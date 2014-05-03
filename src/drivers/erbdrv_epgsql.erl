%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%
%% @copyright 2013 Voalte Inc. <ccorral@voalte.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(erbdrv_epgsql).
-behaviour(erbi_driver).
-behaviour(erbi_temp_db).

-include("erbi.hrl").
-include("erbi_driver.hrl").
-include_lib("epgsql/include/pgsql.hrl").

% erbi_driver API
-export([driver_info/0,
         validate_property/2,
         property_info/0,
         parse_args/1,
         connect/3,
         disconnect/1,
         begin_work/1,
         begin_work/2,
         rollback/1,
         rollback/2,
         commit/1,
         do/3,
         prepare/2,
         bind_params/3,
         execute/3,
         fetch_rows/3,
         finish/2
        ]).

-define(MIN_FETCH,1).
-define(MAX_FETCH,0). % 0 all rows

% erbi_temp_db  API
-export([start_temp/2,
         stop_temp/2,
        get_temp_connect_data/4]).

-define(MIN_PORT,5433).
-define(MAX_PORT,5533).

-spec driver_info() -> erbi_driver_info().
driver_info()->
    #erbi_driver_info
        { driver = epgsql,
          preparse_support = true,
          cursor_support = true,
          transaction_support = true,
          must_preparse = true,
          must_bind = true,
          multiple_bind = false
          }.

-spec validate_property( atom(), any() ) ->
    ok | {ok,[property()]} | {error,any()}.
validate_property(port,Port) when is_list(Port)->
     {ok,[{port,list_to_integer(Port)}]};
validate_property( _,_ ) ->
    ok.

-spec property_info() -> [{atom(),any()}].
property_info()->
    [{aliases, [{db,database},{dbname,database},{hostaddr,host},{hostname,host},{sslmode,ssl}]},
     {defaults,[{host,"localhost"},{port,5432}]},
     {required,[]}
     ].

-spec parse_args([any()]) ->
    declined | {error,any()} | term().
parse_args(_)->
    declined.

-spec connect( DS :: erbi_data_source(),
                   Username :: string(),
                   Password :: string() ) -> erbdrv_return().
connect(#erbi{driver = epgsql, properties=PropList}, Username, Password)->
    Host= proplists:get_value(host,PropList),
    connect(Host, Username, Password, PropList).

connect(Host,Username,Password,PropList)->
    erbdrv_response(pgsql:connect(Host, Username, Password, PropList)).

-spec disconnect( Connection :: erbdrv_connection() ) ->
    ok | {error, erbdrv_error()}.
disconnect(Connection) when is_pid(Connection)->
    pgsql:close(Connection);
disconnect(_) ->
    {error,{erbdrv_general_error,invalid_argument}}.

-spec begin_work( Connection :: erbdrv_connection() ) ->
    erbdrv_return().
begin_work(Connection)->
    erbdrv_response(begin_work_query(Connection)).

begin_work_query(Connection)->
     pgsql:squery(Connection,"BEGIN").

-spec begin_work( Connection :: erbdrv_connection(),
                      Savepoint :: atom | string() ) ->
    erbdrv_return().
begin_work(Connection,Savepoint) when is_list(Savepoint)->
    savepoint(Connection, Savepoint,begin_work_query(Connection));
begin_work(Connection,Savepoint) when is_atom(Savepoint)->
    begin_work(Connection,atom_to_list(Savepoint)).

savepoint(Connection,Savepoint,{ok,[],[]})->
    erbdrv_response(pgsql:squery(Connection, [$S,$A,$V,$E,$P,$O,$I,$N,$T,$  |Savepoint])).

-spec rollback( Connection :: erbdrv_connection() ) ->
    erbdrv_return().
rollback(Connection)->
   erbdrv_response(pgsql:squery(Connection, "ROLLBACK")).

-spec rollback( Connection :: erbdrv_connection(),
                    Savepoint :: atom | string() ) ->
    erbdrv_return().
rollback(Connection,Savepoint) when is_list(Savepoint)->
    erbdrv_response(pgsql:squery(Connection,[$R,$O,$L,$L,$B,$A,$C,$K,$ ,$T,$O, $ | Savepoint]));
rollback(Connection,Savepoint) when is_atom(Savepoint) ->
    rollback(Connection, atom_to_list(Savepoint)).

-spec commit( Connection :: erbdrv_connection() ) ->
    erbdrv_return().
commit(Connection)->
    erbdrv_response(pgsql:squery(Connection,"END")).

-spec do( Connection :: erbdrv_connection(),
              Query :: string(),
              Params ::  erbi_bind_values() ) ->
    erbdrv_return().
do(Connection,Query,Params)->
    erbdrv_squery_response(pgsql:equery(Connection,Query,erbi_bind_values_to_epgsql(Params))).

-spec prepare( Connection :: erbdrv_connection(), Query :: string() ) -> erbdrv_return().
prepare(Connection,Query) when is_list(Query)->
    erbdrv_response(pgsql:parse(Connection,[],Query,[])).

-spec bind_params( Connection :: erbdrv_connection(),
                       Statement :: erbdrv_statement(),
                       Params :: erbi_bind_values() ) ->
    erbdrv_return().
bind_params(Connection,Statement,Params) when is_record(Statement,statement)->
    erbdrv_response(check_bindable_statement(Connection,Statement,Params)).

check_bindable_statement(_Connection,#statement{types=Types},Params)
  when length(Types)=/=length(Params)->
    {error,{missing_parameter,Types}};
check_bindable_statement(Connection,Statement,Params)->
    bind_params_query(Connection,Statement,Params).

bind_params_query(Connection,Statement,Params)->
   pgsql:bind(Connection,Statement,"",erbi_bind_values_to_epgsql(Params)).

-spec execute( Connection :: erbdrv_connection(),
                   Statement :: erbdrv_statement() | string(),
                   Params :: erbi_bind_values() ) ->
    erbdrv_return().
execute(Connection,Statement,_Params)  when is_record(Statement,statement)->
    erbdrv_cols_rows_response(pgsql:execute(Connection,Statement,"", ?MIN_FETCH),Statement).

-spec fetch_rows( Connection :: erbdrv_connection(),
                      Statement :: erbdrv_statement(),
                      Amount :: one | all ) ->
    erbdrv_return().
fetch_rows(Connection,Statement,one)->
    fetch_rows_number(Connection,Statement,?MIN_FETCH);
fetch_rows(Connection,Statement,all) ->
    fetch_rows_number(Connection,Statement,?MAX_FETCH).

fetch_rows_number(Connection,Statement, Amount) when is_integer(Amount), is_record(Statement,statement) ->
    erbdrv_only_rows_response(pgsql:execute(Connection,Statement,"",Amount),Statement#statement.columns).

-spec finish( Connection :: erbdrv_connection(),
                 Statement :: erbdrv_statement() ) ->
    erbdrv_return().
finish(Connection,Statement) when is_record(Statement,statement)->
    pgsql:sync(Connection),
    erbdrv_response(pgsql:close(Connection,Statement));
finish(Connection,_) ->
    erbdrv_response(pgsql:sync(Connection)).

%----------------------------------------------------
% erbi_temp_db API
%-----------------------------------------------------
-define(PID_FILE,"tmp_db.pid").
-define(PORT_FILE,"tmp_db.port").
-define(POSSIBLE_BIN_DIRS,["/usr/bin/pgsql/bin/",
                          "/usr/sbin/pgsql/bin/",
                          "/usr/lib/postgresql/9.3/bin/",
                          "/usr/lib/postgresql/9.2/bin/",
                          "/usr/lib/postgresql/9.1/bin/",
                          "/usr/local/pgsql/bin/",
                          "/usr/local/bin/pgsql/bin/",
                          "/usr/pgsql-9.3/bin",
                          "/usr/pgsql-9.2/bin",
                          "/usr/pgsql-9.1/bin",
                          "/Library/PostgreSQL/9.3/bin/",
                          "/Library/PostgreSQL/9.2/bin/",
                          "/Library/PostgreSQL/9.1/bin/"
                          ]).

-spec start_temp(ErbiDataSource::erbi_data_source(),
                DataDir::unicode:chardata())->
    ok.
start_temp(#erbi{properties=PropList}=DataSource,DataDir)->
    {ok,PathBin}= erbi_temp_db_helpers:find_bin_dir(DataSource,?POSSIBLE_BIN_DIRS,"postgres"),
    {ok, Port}=get_free_db_port(),
    ok = configure_datadir(PathBin,DataDir),
    DBPid = start_db_instance(PathBin,DataDir,Port),
    ok = initialize_db(PropList,PathBin,Port),
    ok = erbi_temp_db_helpers:save_in_db_data_file(DBPid,DataDir,?PID_FILE),
    ok = erbi_temp_db_helpers:save_in_db_data_file(Port,DataDir,?PORT_FILE),
    ok.

-spec stop_temp(ErbiDataSource::erbi_data_source(),
                DataDir::unicode:chardata())->
    ok | {error, term()}.
stop_temp(#erbi{},DataDir)->
    erbi_temp_db_helpers:kill_db_pid(DataDir,?PID_FILE,[{term,2},{int,2},{quit,2},{kill,2}]).

-spec get_temp_connect_data(ErbiDataSource::erbi_data_source(),
                            DataDir::unicode:chardata(),
                                Username::unicode:chardata(),
                                Password::unicode:chardata())->
    {erbi_data_source(),
     unicode:chardata(),
     unicode:chardata()}.
get_temp_connect_data(ErbiDataSource,DataDir,UserName,Password)->
    {get_temp_proplist(ErbiDataSource,DataDir),
     get_temp_username(UserName),
     get_temp_password(Password)}.

%%------------------------------------
%% Create Responses Functions
%%------------------------------------
erbdrv_cols_rows_response({Atom,Rows},Statement) when is_list(Rows) andalso (Atom=:= ok orelse Atom=:= partial) ->
    erbdrv_data_response(unknown,{erbdrv_cols_response(Statement#statement.columns),erbdrv_rows_response({Atom,Rows},Statement#statement.columns)});
erbdrv_cols_rows_response(Response,_) ->
    erbdrv_response(Response).

erbdrv_only_rows_response({error,_Reason}=Response, _Columns)->
     erbdrv_response(Response);
erbdrv_only_rows_response({Atom,Count}, Columns) when is_integer(Count)->
    erbdrv_data_response(Count,erbdrv_rows_response({Atom,[]}, Columns));
erbdrv_only_rows_response({Atom,Rows}, Columns)->
    erbdrv_data_response(unknown,erbdrv_rows_response({Atom,Rows}, Columns));
erbdrv_only_rows_response({Atom,Count,Rows},Columns) ->
    erbdrv_data_response(Count,erbdrv_rows_response({Atom,Rows}, Columns)).

erbdrv_cols_response(undefined)->
    [];
erbdrv_cols_response(ColumnsList)->
    lists:map(fun epgsql_column_to_erbdrv_field/1, ColumnsList).

erbdrv_rows_response({ok,[]}, _Columns)->
    final;
erbdrv_rows_response({Atom,Rows}, Columns)->
    is_final(Atom,lists:map(fun(Row) ->
                                    erbdrv_row_response(Row,Columns)
                                        end,Rows)).
erbdrv_row_response([], _Columns)->
    [];
erbdrv_row_response(Row, Columns)->
    Zipped=lists:zip(tuple_to_list(Row),Columns),
    lists:map(fun epgsql_value_to_erbdrv/1, Zipped).

-spec is_final(atom(),any())-> any() | {final,any()}.
is_final(ok,Data)->
    {final,Data};
is_final(partial,Data) ->
    Data.

erbdrv_response(ok)->
    erbdrv_simple_ok_response();
erbdrv_response({ok,[],[]})->
    erbdrv_simple_ok_response();
erbdrv_response({ok,Connection}) when is_pid(Connection)->
    erbdrv_connection_response(Connection);
erbdrv_response({ok,Statement}) when is_record(Statement,statement)->
    erbdrv_statement_response(Statement);
erbdrv_response({ok,Count}) when is_integer(Count) ->
    erbdrv_count_response(Count);
erbdrv_response({'EXIT',Reason}) ->
    erbdrv_error_response(Reason);
erbdrv_response({error,Reason}) ->
    erbdrv_error_response(Reason).

erbdrv_squery_response({ok,[],[]})->
    erbdrv_response(ok,same,same,unknown,[]);
erbdrv_squery_response(Sth) ->
 erbdrv_response(Sth).

erbdrv_count_response(Count)->
    erbdrv_response(ok,same,same,Count,[]).

erbdrv_data_response(_Count,Rows)->
    erbdrv_response(ok,same,same,unknown,Rows).

erbdrv_statement_response(Statement)->
    #erbdrv
        {status=ok,
         conn=same,
         stmt=Statement,
        data=erbdrv_cols_response(Statement#statement.columns)}.

erbdrv_response(Status,Connection,Statement,Rows,Data)->
    #erbdrv
        { status = Status,
          conn = Connection,
          stmt = Statement,
          rows= Rows,
          data = Data }.

erbdrv_simple_ok_response()->
    #erbdrv
        {status = ok,
         conn = same
         }.

erbdrv_connection_response(Connection) ->
    #erbdrv
        {status = ok,
         conn = Connection
        }.

erbdrv_error_response(Error) ->
    #erbdrv
        {status = error,
         data = translate_error(Error)
         }.


%--------------------------------------------
% ERBI <-> Driver Conversions
%--------------------------------------------



erbi_bind_values_to_epgsql(Params)->
    lists:map(fun erbi_bind_value_to_epgsql/1, Params).

erbi_bind_value_to_epgsql({Type,Value}) when is_atom(Type)->
    erbi_type_to_epgsql(Type,Value);
erbi_bind_value_to_epgsql({Type,Value}) when is_list(Type) ->
    erbi_type_to_epgsql(list_to_atom(Type),Value);
erbi_bind_value_to_epgsql({_Id,Type,Value}) ->
    erbi_type_to_epgsql(Type,Value);
erbi_bind_value_to_epgsql(Value) ->
    erbi_value_to_epgsql(Value).

erbi_value_to_epgsql(Value) when is_list(Value)->
    list_to_binary(Value);
erbi_value_to_epgsql(Value) ->
    Value.

erbi_type_to_epgsql(text,Value)->
    list_to_binary(Value);
erbi_type_to_epgsql(varchar,Value) ->
    list_to_binary(Value);
erbi_type_to_epgsql(_,Value) ->
    Value.

epgsql_value_to_erbdrv({Value,#column{type=varchar} })->
    binary_value_to_list(Value);
epgsql_value_to_erbdrv({Value, #column{type=text}}) ->
    binary_value_to_list(Value);
epgsql_value_to_erbdrv({Value,_}) ->
    Value.

binary_value_to_list(null)->
    null;
binary_value_to_list(Value) ->
     binary_to_list(Value).

epgsql_column_to_erbdrv_field(Column)->
    #erbdrv_field
        {name = binary_to_list(Column#column.name),
         type = Column#column.type,
         length = get_size(Column#column.size),
         precision = 1 %% Get precision by type?
         }.

get_size(N) when N < 0 ->
    unlimited;
get_size(N) ->
    N.


%-----------------------------------------------
% Erbi temp driver internal functions
%-----------------------------------------------
get_temp_proplist(#erbi{properties=PropList}=DS,DataDir)->
    DS#erbi{properties = [get_temp_port_prop(DataDir),
                         get_temp_db_prop(PropList)]}.

get_temp_port_prop(DataDir)->
    Port = erbi_temp_db_helpers:read_integer(DataDir,?PORT_FILE),
    {port,Port}.

%Assumed that if a db is provided
% it was created in db initialization.
get_temp_db_prop(PropList)->
    NewValue=proplists:get_value(database,PropList,get_db_name()),
    {database,NewValue}.

% Assumed that if username is provided,
% it was created in db initialization.
get_temp_username(undefined) ->
    get_db_user();
get_temp_username("")->
    get_db_user();
get_temp_username(User) ->
    User.

get_temp_password(undefined) ->
    "";
get_temp_password(Passwd) ->
    Passwd.

% Creates datadir defaults->user=$USER;authmode=trust;db=postgres
configure_datadir(PathBin,PathData)->
    {ok,{exit_status,0},_} = erbi_temp_db_helpers:exec_cmd(PathBin++"/initdb",["-D",PathData],quiet),
    ok.

start_db_instance(PathBin,PathData,Port)->
    
    Postgres = filename:join([PathBin, "postgres"]),
    Ver = get_postgress_version(Postgres),
    io:format(user,"Starting temp postgres ~p from ~p on port ~p~n",[Ver,PathData,Port]),
    %io:format(user,"Detected postgres version: ~p~n", [Ver]),
    Args = db_instance_args(Ver,PathData,Port),
    {ok,{os_pid,Pid},_} = 
        erbi_temp_db_helpers:exec_cmd
          (Postgres,Args,nowait,
           erbi_temp_db_helpers:filter_logger(nomatch,["^LOG:"])
          ),
    ok=wait_for_db_started(PathBin,Port),
    Pid.

to_int(Value) ->
    try list_to_integer(Value)
    catch error:badarg -> undefined
    end.

get_postgress_version(Postgres) ->
    ParseFun = fun(Output,_Res) ->
        RegEx = "^postgres[^0-9]*\([0-9]+\)\.\([0-9]+\)\.?\([0-9]*\)[ \n\r]*$",
        case re:run(Output,RegEx,[{capture,all,list}]) of
            {match,[_,H,M,L]} -> {to_int(H),to_int(M),to_int(L)};
            _ -> undefined
        end
    end,
    Args = ["--version"],
    {ok,{exit_status,0},Ver} =
        erbi_temp_db_helpers:exec_cmd(Postgres,Args,{ParseFun,""},none),
    Ver.

db_instance_args(Ver, Path, Port) when Ver =/= undefined, Ver >= {9, 3, 0} ->
   ["-p", integer_to_list(Port),
     "-c", "unix_socket_directories="++Path,
     "-D", Path];
db_instance_args(_Ver, Path, Port) ->
   ["-p", integer_to_list(Port),
     "-c", "unix_socket_directory="++Path,
     "-D", Path].

initialize_db(PropList,PathBin,Port)->
    InitFiles= proplists:get_value(init_files,PropList,[]),
    lists:foreach(fun(File)->
                      Cmd = PathBin++"/psql",
                      Args = ["-p", integer_to_list(Port),
                              "-h", "localhost",
                              "-U", get_db_user(),
                              "-d", get_db_name(),
                              "-f", File],
                      %io:format(user,"Running pg init file: ~p~n~p:~n",[File,Cmd]),
                      {ok,{exit_status,0},_} = erbi_temp_db_helpers:exec_cmd(Cmd,Args,quiet)
              end,InitFiles),
    ok.

wait_for_db_started(PathBin,Port)->
    Cmd = PathBin++"/psql",
    Args = ["-d",get_db_name(),
            "-h", "localhost",
            "-f","/dev/null",
            "-p",integer_to_list(Port)],
    Fun = fun() ->
                 case erbi_temp_db_helpers:exec_cmd(Cmd,Args,wait,none) of
                     {ok,{exit_status,0},_}->
                         ok;
                     _ ->
                         wait
                 end
         end,
    erbi_temp_db_helpers:wait_for(Fun,{error,db_not_started},500,10).

get_db_user()->
     os:cmd("echo $USER")--"\n".

get_db_name()->
    "postgres".

get_free_db_port()->
    erbi_temp_db_helpers:get_free_db_port(?MIN_PORT,?MAX_PORT).

%% -----------------------------------------------
%% Postgres Error Code Mapping
%% -----------------------------------------------
%% Keep this code at the end.

translate_error(invalid_password=Error) ->
    {invalid_credentials,Error};
translate_error(invalid_authorization_specification=Error)->
    {invalid_credentials, Error};
translate_error({{badmatch,{error,econnrefused}},_}=Error) ->
    {connection_refused,Error};
translate_error({{badmatch,{error,nxdomain}},_}=Error) ->
    {unknown_host,Error};
translate_error({missing_parameter,_}=Err) ->
    Err;
translate_error(#error{ severity = _Severity,
                        code = CodeBin,
                        message = Message,
                        extra = _Extra }) ->

    Code = binary_to_list(CodeBin),
    {ErrGroup,ErbiErr0} =
        case translate_error_group(Code) of
            {_,_}=Tg -> Tg;
            G -> {G,undefined}
        end,
    {Err,ErbiErr1} =
        case translate_error_code(Code) of
            {_,_}=Te -> Te;
            E -> {E,undefined}
        end,
    {coalesce([ErbiErr1,ErbiErr0,unmapped_error]),
     {ErrGroup,coalesce([Err,ErrGroup]),Message}}.

coalesce([undefined|Rest]) ->
    coalesce(Rest);
coalesce([X|_]) ->
    X;
coalesce([]) ->
    undefined.

translate_error_group(Code) ->
    case lists:sublist(Code,2) of
        "00" -> successful_completion;
        "01" -> warning;
        "02" -> {no_data,                               no_more_rows};
        "03" -> {sql_statement_not_yet_complete,        execution_error};
        "08" -> {connection_exception,                  communication_error} ;
        "09" -> {triggered_action_exception,            execution_error};
        "0A" -> feature_not_supported;
        "0B" -> {invalid_transaction_initiation,        transaction_error};
        "0F" -> locator_exception;
        "0L" -> {invalid_grantor,                       insufficient_permission};
        "0P" -> invalid_role_specification;
        "0Z" -> diagnostics_exception;
        "20" -> case_not_found;
        "21" -> cardinality_violation;
        "22" -> {data_exception,                        execution_error};
        "23" -> {integrity_constraint_violation,        constraint_violation};
        "24" -> {invalid_cursor_state,                  execution_error};
        "25" -> {invalid_transaction_state,             transaction_errror};
        "26" -> invalid_sql_statement_name;
        "27" -> {triggered_data_change_violation,       execution_error};
        "28" -> {invalid_authorization_specification,   invalid_credentials};
        "2B" -> {dependent_privilege_descriptors_still_exist, constraint_violation};
        "2D" -> {invalid_transaction_termination,       transaction_error};
        "2F" -> {sql_routine_exception,                 execution_error};
        "34" -> invalid_cursor_name;
        "38" -> external_routine_exception;
        "39" -> external_routine_invocation_exception;
        "3B" -> {savepoint_exception,                   transaction_error};
        "3D" -> invalid_catalog_name;
        "3F" -> invalid_schema_name;
        "40" -> {transaction_rollback,                  transaction_error};
        "42" -> {syntax_error_or_access_rule_violation, syntax_error};
        "44" -> with_check_option_violation;
        "53" -> {insufficient_resources,                insufficient_resources};
        "54" -> {program_limit_exceeded,                insufficient_resources};
        "55" -> object_not_in_prerequisite_state;
        "57" -> operator_intervention;
        "58" -> system_error;
        "F0" -> config_file_error;
        "HV" -> fdw_error;
        "P0" -> plpgsql_error;
        "XX" -> internal_error
    end.

translate_error_code(Code) ->
    case Code of
        "01000" -> warning;
        "0100C" -> dynamic_result_sets_returned;
        "01008" -> implicit_zero_bit_padding;
        "01003" -> null_value_eliminated_in_set_function;
        "01007" -> privilege_not_granted;
        "01006" -> privilege_not_revoked;
        "01004" -> string_data_right_truncation;
        "01P01" -> deprecated_feature;
        "02000" -> no_data;
        "02001" -> no_additional_dynamic_result_sets_returned;
        "08000" -> connection_exception;
        "08003" -> connection_does_not_exist;
        "08006" -> connection_failure;
        "08001" -> sqlclient_unable_to_establish_sqlconnection;
        "08004" -> sqlserver_rejected_establishment_of_sqlconnection;
        "08007" -> transaction_resolution_unknown;
        "08P01" -> protocol_violation;
        "0F000"	-> locator_exception;
        "0F001" -> invalid_locator_specification;
        "0L000" -> invalid_grantor;
        "0LP01" -> invalid_grant_operation;
        "0Z000" -> diagnostics_exception;
        "0Z002" -> stacked_diagnostics_accessed_without_active_handler;
        "22000" -> data_exception;
        "2202E" -> array_subscript_error;
        "22021" -> character_not_in_repertoire;
        "22008" -> datetime_field_overflow;
        "22012" -> division_by_zero;
        "22005" -> error_in_assignment;
        "2200B" -> escape_character_conflict;
        "22022" -> indicator_overflow;
        "22015" -> interval_field_overflow;
        "2201E" -> invalid_argument_for_logarithm;
        "22014" -> invalid_argument_for_ntile_function;
        "22016" -> invalid_argument_for_nth_value_function;
        "2201F" -> invalid_argument_for_power_function;
        "2201G" -> invalid_argument_for_width_bucket_function;
        "22018" -> invalid_character_value_for_cast;
        "22007" -> invalid_datetime_format;
        "22019" -> invalid_escape_character;
        "2200D" -> invalid_escape_octet;
        "22025" -> invalid_escape_sequence;
        "22P06" -> nonstandard_use_of_escape_character;
        "22010" -> invalid_indicator_parameter_value;
        "22023" -> invalid_parameter_value;
        "2201B" -> invalid_regular_expression;
        "2201W" -> invalid_row_count_in_limit_clause;
        "2201X" -> invalid_row_count_in_result_offset_clause;
        "22009" -> invalid_time_zone_displacement_value;
        "2200C" -> invalid_use_of_escape_character;
        "2200G" -> most_specific_type_mismatch;
        "22004" -> null_value_not_allowed;
        "22002" -> null_value_no_indicator_parameter;
        "22003" -> numeric_value_out_of_range;
        "22026" -> string_data_length_mismatch;
        "22001" -> string_data_right_truncation;
        "22011" -> substring_error;
        "22027" -> trim_error;
        "22024" -> unterminated_c_string;
        "2200F" -> zero_length_character_string;
        "22P01" -> floating_point_exception;
        "22P02" -> invalid_text_representation;
        "22P03" -> invalid_binary_representation;
        "22P04" -> bad_copy_file_format;
        "22P05" -> untranslatable_character;
        "2200L" -> not_an_xml_document;
        "2200M" -> invalid_xml_document;
        "2200N" -> invalid_xml_content;
        "2200S" -> invalid_xml_comment;
        "2200T" -> invalid_xml_processing_instruction;
        "23000" -> integrity_constraint_violation;
        "23001" -> restrict_violation;
        "23502" -> not_null_violation;
        "23503" -> foreign_key_violation;
        "23505" -> unique_violation;
        "23514"	-> check_violation;
        "23P01" -> exclusion_violation;
        "25000" -> invalid_transaction_state;
        "25001" -> active_sql_transaction;
        "25002" -> branch_transaction_already_active;
        "25008" -> held_cursor_requires_same_isolation_level;
        "25003" -> inappropriate_access_mode_for_branch_transaction;
        "25004" -> inappropriate_isolation_level_for_branch_transaction;
        "25005" -> no_active_sql_transaction_for_branch_transaction;
        "25006" -> read_only_sql_transaction;
        "25007" -> schema_and_data_statement_mixing_not_supported;
        "25P01" -> no_active_sql_transaction;
        "25P02" -> in_failed_sql_transaction;
        "28000" -> invalid_authorization_specification;
        "28P01" -> invalid_password;
        "2B000" -> dependent_privilege_descriptors_still_exist;
        "2BP01" -> dependent_objects_still_exist;
        "2F000" -> sql_routine_exception;
        "2F005" -> function_executed_no_return_statement;
        "2F002" -> modifying_sql_data_not_permitted;
        "2F003" -> prohibited_sql_statement_attempted;
        "2F004" -> reading_sql_data_not_permitted;
        "38000" -> external_routine_exception;
        "38001" -> containing_sql_not_permitted;
        "38002" -> modifying_sql_data_not_permitted;
        "38003" -> prohibited_sql_statement_attempted;
        "38004" -> reading_sql_data_not_permitted;
        "39000" -> external_routine_invocation_exception;
        "39001" -> invalid_sqlstate_returned;
        "39004" -> null_value_not_allowed;
        "39P01" -> trigger_protocol_violated;
        "39P02" -> srf_protocol_violated;
        "3B000" -> savepoint_exception;
        "3B001" -> invalid_savepoint_specification;
        "40000" -> transaction_rollback;
        "40002" -> transaction_integrity_constraint_violation;
        "40001" -> serialization_failure;
        "40003" -> statement_completion_unknown;
        "40P01" -> deadlock_detected;
        "42000" -> syntax_error_or_access_rule_violation;
        "42601" -> syntax_error;
        "42501" -> {insufficient_privilege,                 insufficient_permissions};
        "42846" -> cannot_coerce;
        "42803" -> grouping_error;
        "42P20" -> windowing_error;
        "42P19" -> invalid_recursion;
        "42830" -> invalid_foreign_key;
        "42602" -> invalid_name;
        "42622" -> name_too_long;
        "42939" -> reserved_name;
        "42804" -> datatype_mismatch;
        "42P18" -> indeterminate_datatype;
        "42P21" -> collation_mismatch;
        "42P22" -> indeterminate_collation;
        "42809" -> wrong_object_type;
        "42703" -> {undefined_column,                       unknown_column};
        "42883" -> {undefined_function,                     unknown_object};
        "42P01" -> {undefined_table,                        unknown_table};
        "42P02" -> {undefined_parameter,                    missing_parameter};
        "42704" -> {undefined_object,                       unknown_object};
        "42701" -> duplicate_column;
        "42P03" -> duplicate_cursor;
        "42P04" -> duplicate_database;
        "42723" -> duplicate_function;
        "42P05" -> duplicate_prepared_statement;
        "42P06" -> duplicate_schema;
        "42P07" -> duplicate_table;
        "42712" -> duplicate_alias;
        "42710" -> duplicate_object;
        "42702" -> {ambiguous_column,                       unknown_column};
        "42725" -> {ambiguous_function,                     unknown_object};
        "42P08" -> {ambiguous_parameter,                    missing_parameter};
        "42P09" -> {ambiguous_alias,                        unknown_object};
        "42P10" -> invalid_column_reference;
        "42611" -> invalid_column_definition;
        "42P11" -> invalid_cursor_definition;
        "42P12" -> invalid_database_definition;
        "42P13" -> invalid_function_definition;
        "42P14" -> invalid_prepared_statement_definition;
        "42P15" -> invalid_schema_definition;
        "42P16" -> invalid_table_definition;
        "42P17" -> invalid_object_definition;
        "53000" -> insufficient_resources;
        "53100" -> disk_full;
        "53200" -> out_of_memory;
        "53300" -> too_many_connections;
        "53400" -> configuration_limit_exceeded;
        "54000" -> program_limit_exceeded;
        "54001" -> statement_too_complex;
        "54011" -> too_many_columns;
        "54023" -> too_many_arguments;
        "55000" -> object_not_in_prerequisite_state;
        "55006" -> object_in_use;
        "55P02" -> cant_change_runtime_param;
        "55P03" -> lock_not_available;
        "57000" -> operator_intervention;
        "57014" -> query_canceled;
        "57P01" -> admin_shutdown;
        "57P02" -> crash_shutdown;
        "57P03" -> cannot_connect_now;
        "57P04" -> database_dropped;
        "58000" -> system_error;
        "58030" -> io_error;
        "58P01"	-> undefined_file;
        "58P02" -> duplicate_file;
        "F0000" -> config_file_error;
        "F0001" -> lock_file_exists;
        "HV000" -> fdw_error;
        "HV005" -> fdw_column_name_not_found;
        "HV002" -> fdw_dynamic_parameter_value_needed;
        "HV010" -> fdw_function_sequence_error;
        "HV021" -> fdw_inconsistent_descriptor_information;
        "HV024" -> fdw_invalid_attribute_value;
        "HV007" -> fdw_invalid_column_name;
        "HV008" -> fdw_invalid_column_number;
        "HV004" -> fdw_invalid_data_type;
        "HV006" -> fdw_invalid_data_type_descriptors;
        "HV091" -> fdw_invalid_descriptor_field_identifier;
        "HV00B" -> fdw_invalid_handle;
        "HV00C" -> fdw_invalid_option_index;
        "HV00D" -> fdw_invalid_option_name;
        "HV090" -> fdw_invalid_string_length_or_buffer_length;
        "HV00A" -> fdw_invalid_string_format;
        "HV009" -> fdw_invalid_use_of_null_pointer;
        "HV014" -> fdw_too_many_handles;
        "HV001" -> fdw_out_of_memory;
        "HV00P" -> fdw_no_schemas;
        "HV00J" -> fdw_option_name_not_found;
        "HV00K" -> fdw_reply_handle;
        "HV00Q" -> fdw_schema_not_found;
        "HV00R" -> fdw_table_not_found;
        "HV00L" -> fdw_unable_to_create_execution;
        "HV00M" -> fdw_unable_to_create_reply;
        "HV00N" -> fdw_unable_to_establish_connection;
        "P0000" -> plpgsql_error;
        "P0001" -> raise_exception;
        "P0002" -> no_data_found;
        "P0003" -> too_many_rows;
        "XX000" -> internal_error;
        "XX001" -> data_corrupted;
        "XX002" -> index_corrupted
    end.
