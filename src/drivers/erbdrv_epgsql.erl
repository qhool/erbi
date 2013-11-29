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

% erbi_mockable_driver API
-export([start_temp/1,
         stop_temp/1,
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
     {required,[database]}
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

-spec start_temp(PropList::[property()])->
    ok.
start_temp(PropList)->
    {ok,PathBin}=get_db_binaries_path(PropList),
    PathData= proplists:get_value(data_dir,PropList),
    {ok, Port}=get_free_db_port(),
    ok = configure_datadir(PathBin,PathData),
    DBPid = start_db_instance(PathBin,PathData,Port),
    ok = initialize_db(PropList,Port),
    ok = save_in_file(DBPid,PathData,?PID_FILE),
    ok = save_in_file(Port,PathData,?PORT_FILE),
    ok.

-spec stop_temp(PropList::[property()])->
    ok.
stop_temp(PropList)->
    PathData= proplists:get_value(data_dir,PropList),
    Pid=read_from_file(PathData,?PID_FILE),
    kill_db_pid(Pid),
    ok=del_dir(PathData),
    ok.

-spec get_temp_connect_data(PropList::[property()],
                                Args::[any()] | atom(),
                                Username::unicode:chardata(),
                                Password::unicode:chardata())->
    {[property()],
     [any()] | atom(),
     unicode:chardata(),
     unicode:chardata()}.
get_temp_connect_data(PropList,_Args,UserName,Password)->
    {get_temp_proplist(PropList),
     [],
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
         data = epgsql_error_to_erbdrv_error(Error)
         }.
    

%--------------------------------------------
% ERBI <-> Driver Conversions
%--------------------------------------------

epgsql_error_to_erbdrv_error(Error) when is_record(Error,error)->
    {epgsql_code_to_erbdrv_error_code(binary_to_list(Error#error.code)),
     binary_to_list(Error#error.message)};
epgsql_error_to_erbdrv_error(invalid_password=Error) ->
    {invalid_credentials,Error};
epgsql_error_to_erbdrv_error({{badmatch,{error,econnrefused}},_}=Error) ->
    {connection_refused,Error};
epgsql_error_to_erbdrv_error({{badmatch,{error,nxdomain}},_}=Error) ->
    {unknown_host,Error};
epgsql_error_to_erbdrv_error({missing_parameter,_}=Err) ->
    Err;
epgsql_error_to_erbdrv_error(Error) ->
    {invalid_datasource,Error}.

epgsql_code_to_erbdrv_error_code("0B000")->
    transaction_error;
epgsql_code_to_erbdrv_error_code("42P01") ->
    unknown_table;
epgsql_code_to_erbdrv_error_code("42703") ->
    unknown_column;
epgsql_code_to_erbdrv_error_code("42704") ->
    unknown_object;
epgsql_code_to_erbdrv_error_code("42601") ->
    syntax_error;
epgsql_code_to_erbdrv_error_code("53000") ->
    insufficient_resources;
epgsql_code_to_erbdrv_error_code(_) ->
    invalid_datasource.


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
get_temp_proplist(PropList)->
    [get_temp_port_prop(PropList),
    get_temp_db_prop(PropList)].

get_temp_port_prop(PropList)->
    PathData= proplists:get_value(data_dir,PropList),
    Port=read_from_file(PathData,?PORT_FILE),
    {port,Port}.

%Assumed that if a db is provided
% it was created in db initialization.
get_temp_db_prop(PropList)->
    NewValue=proplists:get_value(database,PropList,get_db_name()),
    {database,NewValue}.

% Assumed that if username is provided,
% it was created in db initialization.
get_temp_username("")->
    get_db_user();
get_temp_username(User) ->
    User.

get_temp_password(Passwd) ->
    Passwd.

get_db_binaries_path(PropList)->
    case proplists:get_value(bin_dir,PropList) of
        undefined ->
            search_db_binaries();
        Path ->
            {ok,Path}
    end.

-define(POSSIBLE_BIN_DIRS,["/usr/bin/pgsql/bin/",
                          "/usr/sbin/pgsql/bin/",
                          "/usr/local/pgsql/bin/",
                          "/usr/local/bin/pgsql/bin/",
                          "/Library/PostgreSQL/9.2/bin/"]).

search_db_binaries()->
    case lists:filter(fun(Path)->
                              filelib:is_dir(Path)
                      end,?POSSIBLE_BIN_DIRS) of
        []->
            {error,binaries_not_found};
        [H|_]->         
            {ok,H}
    end.

configure_datadir(PathBin,PathData)->
    case filelib:is_dir(PathData) of
        true ->    % database instance already initialized
            {error,already_created};
        false ->
            initialize_datadir(PathBin,PathData)
    end.

% Creates datadir defaults->user=$USER;authmode=trust;db=postgres
initialize_datadir(PathBin,PathData)->
    os:cmd(PathBin++"/initdb -D "++PathData),
    ok.

start_db_instance(PathBin,PathData,Port)->
    StartDbCmd=PathBin++"/postgres -p "++integer_to_list(Port)++" -D "++PathData,
    StrPid=os:cmd(StartDbCmd++" & echo $!")--"\n",
    list_to_integer(StrPid).

initialize_db(PropList,Port)->
    InitFiles= proplists:get_value(init_files,PropList),
    ok=wait_for_db_started(Port, 0),
    lists:map(fun(File)->
                              os:cmd("psql -p "++integer_to_list(Port)++
                                 " -U "++get_db_user()++
                                 " -d "++get_db_name()++
                                 " -f "++File)
              end,InitFiles),
    ok.

wait_for_db_started(_Port,N) when N >=10 ->
    {error,db_not_started};
wait_for_db_started(Port,N)->
    case os:cmd("psql -d "++get_db_name()++
                    " -f /dev/null -p "++integer_to_list(Port)) of
        "psql:"++_->
            receive
            after 500->
                    wait_for_db_started(Port,N+1)
            end;
        _ ->
            ok
    end.

get_db_user()->
     os:cmd("echo $USER")--"\n".

get_db_name()->
    "postgres".

get_free_db_port()->
    StartingPort=trunc(random:uniform()*(?MAX_PORT-?MIN_PORT))+?MIN_PORT,
    get_free_db_port(StartingPort).

get_free_db_port(StartingPort)->
    get_free_db_port(StartingPort,undefined).

get_free_db_port(Port,undefined) when Port > ?MAX_PORT->
    get_free_db_port(?MIN_PORT,restarted);
get_free_db_port(Port,restarted) when Port >?MAX_PORT ->
    {error,no_free_port};
get_free_db_port(Port,Tag) ->
    case gen_tcp:listen(Port,[]) of
       {ok,TmpSock}->
            gen_tcp:close(TmpSock),
            {ok,Port};
        _ ->
            get_free_db_port(Port+1,Tag)
      end.
  
save_in_file(Term,Path,File)->
    file:write_file(Path++"/"++File,term_to_binary(Term)).
                         
read_from_file(Path,File)->
    {ok,BinaryTerm} = file:read_file(Path++"/"++File),
    binary_to_term(BinaryTerm).

del_dir(Dir) ->
   lists:foreach(fun(D) ->
                    ok = file:del_dir(D)
                 end, del_all_files([Dir], [])).
 
del_all_files([], EmptyDirs) ->
    EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
    {ok, FilesInDir} = file:list_dir(Dir),
    {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                        Path = Dir ++ "/" ++ F,
                                        case filelib:is_dir(Path) of
                                            true ->
                                                {Fs, [Path | Ds]};
                                            false ->
                                                {[Path | Fs], Ds}
                                        end
                                end, {[],[]}, FilesInDir),
    lists:foreach(fun(F) ->
                          ok = file:delete(F)
                  end, Files),
    del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

kill_db_pid(Pid)->
    os:cmd("kill -9 "++integer_to_list(Pid)).

            
    
    
   


    
