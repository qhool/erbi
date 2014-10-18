%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% @copyright 2013 Voalte Inc. <jburroughs@voalte.com>
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
%%
%% @doc
%% erbi neo4j driver, using the neo4j REST interface.
%%
%%
%% @end
-module(erbdrv_neo4j).
-behaviour(erbi_driver).
-behaviour(erbi_temp_db).

-include("erbi.hrl").
-include("erbi_driver.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-export([driver_info/0,
         validate_property/2,
         property_info/0,
         parse_args/1,
         connect/3,
         disconnect/1,
         reset/1,
         begin_work/1, begin_work/2,
         rollback/1, rollback/2,
         commit/1,
         do/3,
         prepare/2,
         bind_params/3,
         execute/3,
         fetch_rows/3,
         finish/2
        ]).


% erbi_temp_db  API
-export([start_temp/2,
         stop_temp/2,
         get_temp_connect_data/4,
         temp_run_script/3]).

-record(neocon, % ;-)
        { type = transaction :: atom(),
          trans = undefined :: undefined | string(),
          rcon = undefined :: term(),
          headers :: [{binary(),binary()}],
          endpoint :: string()
        }).

driver_info() ->
    #erbi_driver_info{ driver = neo4j,
                       preparse_support = false,
                       cursor_support = false,
                       transaction_support = true }.

validate_property(Prop,Val) when is_list(Val) and ((Prop =:= scheme) or (Prop =:= endpoint)) ->
    validate_property(Prop,list_to_atom(Val));
validate_property(port,Port) when is_list(Port) ->
    {ok,[{port,list_to_integer(Port)}]};
validate_property(scheme,S) ->
    case S of
        http ->
            {ok,{scheme,S}};
        https ->
            {ok,{scheme,S}};
        _ ->
            {error,{unsupported_scheme,S}}
    end;
validate_property(endpoint,E) ->
    case E of
        cypher ->
            {ok,{endpoint,E}};
        transaction ->
            {ok,{endpoint,E}};
        _ ->
            {error,{unknown_endpoint,E}}
    end;
validate_property(_,_) ->
    ok.

property_info() ->
    [{defaults,[{scheme,http},
                {host,"localhost"},
                {port,7474},
                {endpoint,transaction}
               ]},
     {expand,[{ssl,[{scheme,"https"}]}]}].

parse_args(_) ->
    declined.

connect( #erbi{ properties = Props }, _Username, _Password ) ->
    Scheme = proplists:get_value(scheme, Props),
    Host = proplists:get_value(host, Props),
    Port = proplists:get_value(port, Props),
    Endpoint = proplists:get_value(endpoint, Props),
    EndpointPath = "/db/data/" ++ atom_to_list(Endpoint),
    Info = case Endpoint of
               transaction -> same;
               cypher ->
                   (driver_info())#erbi_driver_info{transaction_support=false}
           end,
    Conn0 = #neocon{ type=Endpoint,
                     endpoint=EndpointPath },
    case rest_connect(Conn0,Scheme,Host,Port,EndpointPath) of
        {ok,Conn} ->
            case rest_check(Conn) of
                {ok,200,_,_} -> #erbdrv{ status = ok, info = Info,
                                         conn = Conn };
                {ok,Status,Headers,_} ->
                    disconnect(Conn),
                    #erbdrv{ status = error, data = {connection_refused,{Status,Headers}} };
                {error,Why} ->
                    disconnect(Conn),
                    #erbdrv{ status = error, data = {connection_refused,Why} }
            end;
        {error,Why} ->
            #erbdrv{ status = error, data = {connection_refused,Why} }
    end.

%% there was no real connection to start with
disconnect( #neocon{ rcon = RConn } ) ->
    hackney:close(RConn),
    ok.

reset( #neocon{} = Conn) ->
    C1 =
        case catch(rollback(Conn)) of
            #erbdrv{ conn = #neocon{} = C } -> C;
            _ -> Conn
        end,
    #erbdrv{ status = ok, conn = C1#neocon{trans=undefined} }.


begin_work( #neocon{type=cypher} ) ->
    declined;
begin_work( C ) ->
    do_req(post,C,[{statements,[]}],
           fun(C1,_S,Headers,_B) ->
                   TransUrl = proplists:get_value(<<"Location">>,Headers),
                   #hackney_url{ path = Trans } = hackney_url:parse_url(TransUrl),
                   #erbdrv{ status = ok, conn = C1#neocon{trans=Trans} }
           end).
begin_work( _C, _ ) ->
    declined.

rollback( #neocon{type=cypher} ) ->
    declined;
rollback( #neocon{trans=undefined} ) ->
    #erbdrv{ status = error, data = {transaction_error,no_transaction} };
rollback( C ) ->
    do_req(delete,C,[],
           fun(C1,_S,_H,_B) ->
                   #erbdrv{ status = ok, conn = C1#neocon{trans=undefined} }
           end).
rollback( _C, _ ) ->
    declined.

commit( #neocon{type=cypher} ) ->
    declined;
commit( #neocon{trans=undefined} ) ->
    #erbdrv{ status = error, data = {transaction_error,no_transaction} };
commit( #neocon{}=C ) ->
    do_req(post,C,<<"/commit">>,[{statements,[]}],
           fun(C1,_S,_H,_B) ->
                   #erbdrv{ status = ok, conn = C1#neocon{trans=undefined} }
           end).

%% no way to do this in neo4j Rest API
prepare( _C, _Q ) ->
    declined.

do( _C, _Q, _P ) ->
    declined.

bind_params( _C , _S, _P ) -> declined.

%% since prepare isn't supported, we should always get the raw query and a list of params
execute( #neocon{type=cypher}=C, Query, Params ) ->
    Q = format_query({'query',Query},{params,Params}),
    do_req(post,C,Q,
           fun(_S,_H,Body) ->
                   Cols = proplists:get_value(<<"columns">>,Body,[]),
                   Rows = extract_rows(Body),
                   RowCount = case proplists:get_value(<<"stats">>,Body) of
                                  undefined ->
                                      length(Rows);
                                  Stats ->
                                      lists:foldl(fun({_,N},Sum) when is_integer(N) ->
                                                          Sum+N;
                                                     (_,Sum) -> Sum
                                                  end,0,Stats)
                              end,
                   #erbdrv{status=ok,rows=RowCount,stmt=final,data={Cols,Rows}}
           end);
execute( #neocon{trans = Trans}=C, Query, Params ) ->
    Path =
        case Trans of
            undefined -> <<"/commit">>; % if there is no open transaction, do a one-shot
            _ -> <<>>
        end,
    Q = format_query({statement,Query},{parameters,Params}),
    do_req(post,C,Path,[{statements,[Q]}],
           fun(_S,_H,Body) ->
                   % Results =
                   [Result|_] = proplists:get_value(<<"results">>,Body),
                   Cols = proplists:get_value(<<"columns">>,Result,[]),
                   %handle different output formats
                   Rows = extract_rows(Result),
                   #erbdrv{status=ok,rows=length(Rows),stmt=final,data={Cols,Rows}}
           end).

fetch_rows(_,_,_) ->
    declined.
finish(_,_) ->
    declined.

%----------------------------------------------------
% erbi_temp_db API
%-----------------------------------------------------
-define(PORT_FILE,"tmp_db.port").
-define(PID_FILE,"/data/neo4j-service.pid").
-define(SHELL_PORT_FILE,"tmp_shell.port").
-define(POSSIBLE_BIN_DIRS,["/usr/share/neo4j/",
                          "/var/lib/neo4j/bin",
                          "/opt/neo4j/bin",
                          "/usr/local/neo4j/bin",
                          "/usr/neo4j/bin",
                          "../deps/neo4j/bin",
                          "../../deps/neo4j/bin",
                          "../../../deps/neo4j/bin"]).

start_temp(#erbi{properties=PropList}=DataSource,DataDir)->
    {ok,BinDir}= erbi_temp_db_helpers:find_bin_dir(DataSource,?POSSIBLE_BIN_DIRS,"neo4j"),
    {ok, Port}=erbi_temp_db_helpers:get_free_db_port(DataSource),
    {ok, ShellPort}=erbi_temp_db_helpers:get_free_db_port(DataSource,shell_port),
    io:format(user,"Creating temp Neo4j DB in ~p on port ~p; shell port ~p~n",[DataDir,Port,ShellPort]),
    ok = copy_binaries(BinDir,DataDir),
    ok = configure_db_instance(DataDir,Port,ShellPort),
    ok = initialize_db(PropList,DataDir), %starts a local neo4j-shell that populates data
    ok = start_db_instance(DataSource,DataDir),
    ok = wait_for_db_started(Port),
    ok = erbi_temp_db_helpers:save_in_db_data_file(Port,DataDir,?PORT_FILE),
    ok = erbi_temp_db_helpers:save_in_db_data_file(ShellPort,DataDir,?SHELL_PORT_FILE),
    ok.

stop_temp(#erbi{},DataDir)->
    case erbi_temp_db_helpers:read_integer(DataDir,?PORT_FILE) of
        {error,_} = Error ->
          Error;
        Port ->
          ok = stop_db_instance(DataDir),
          ok = wait_for_db_stopped(Port),
          ok
  end.

get_temp_connect_data(ErbiDataSource,DataDir,UserName,Password)->
    {get_temp_proplist(ErbiDataSource,DataDir),
     get_temp_username(UserName),
     get_temp_password(Password)}.

temp_run_script(#erbi{}=DataSource,DataDir,File)->
    #erbi{properties=Props} = get_temp_proplist(DataSource,DataDir),
    ShellPort = proplists:get_value(shell_port, Props, 1337),
    erbi_temp_db_helpers:exec_cmd(DataDir++"/bin/neo4j-shell",
                              [ "-port", integer_to_list(ShellPort),
                                "-file", File ],
                              quiet).

%%-- Internals --%%

format_query({QAtom,Query},{PAtom,Params}) ->
    Query1 = case Query of
                 X when is_binary(X) -> X;
                 _ -> iolist_to_binary(Query)
             end,
    Q = [{QAtom,Query1}],
    QParams = case Params of
                  [] -> [];
                  _ ->
                      [{PAtom,Params}]
              end,
    %% neo4j cares about the order
    Q++QParams.

extract_rows(Result) ->
    RawRows = proplists:get_value(<<"data">>,Result,[]),
    lists:map( fun([{<<"row">>,Row}]) -> Row;
                  (Row) -> Row
               end,
               RawRows ).

do_req(Method,Conn,Body,Func) ->
    do_req(Method,Conn,<<>>,Body,Func).

do_req(Method,Conn,Path,Body,Func) ->
    %io:format(user,"~n~n~n~n------~nURL: ~p~n------~nreqbody: ~p~n",[Url,ReqBody]),
    rest_response(Conn,Func,rest_request(Conn,Method,Path,Body)).

rest_response(C,Func,{_,Stat,Headers,Body}) when (Stat >= 200) and (Stat < 300) ->
    %io:format(user,"response ~p:~n~p",[Stat,Body]),
    case proplists:get_value(<<"errors">>,Body) of
        X when X =:= [] ; X =:= undefined ->
            case Func of
                F3 when is_function(F3,3) ->
                    Func(Stat,Headers,Body);
                F4 when is_function(F4,4) ->
                    Func(C,Stat,Headers,Body)
            end;
        [Error|_] ->
            Code = proplists:get_value(<<"code">>,Error),
            Status = proplists:get_value(<<"status">>,Error),
            Message = proplists:get_value(<<"message">>,Error),
            ErbiError =
                case Code of
                    42000 -> execution_error;
                    42001 -> syntax_error;
                    42002 -> missing_parameter;
                    %% error format changed with 2.0.0
                    <<"Neo.ClientError.Statement.InvalidSyntax">> ->
                        syntax_error;
                    <<"Neo.ClientError.Statement.InvalidType">> ->
                        type_error;
                    <<"Neo.DatabaseError.Statement.ExecutionFailure">> ->
                        execution_error;
                    econnrefused ->
                        connection_refused;
                    closed ->
                        connection_closed;
                    _ -> unmapped_error
                end,
            case Status of
                undefined ->
                    #erbdrv{status=error,data={ErbiError,{Code,Message}}};
                _ ->
                    #erbdrv{status=error,data={ErbiError,{Code,Status,Message}}}
            end
    end;
rest_response(_,_Func,{_, 400, _H, Body}) ->
    ExcpName = proplists:get_value(<<"exception">>,Body),
    FullName = proplists:get_value(<<"fullname">>,Body,ExcpName),
    Message = proplists:get_value(<<"message">>,Body),
    ErbiError =
        case ExcpName of
            <<"SyntaxException">> ->
                syntax_error;
            <<"ParameterNotFoundException">> ->
                missing_parameter;
            <<"EntityNotFoundException">> ->
                unknown_object;
            _ -> unmapped_error
        end,
    #erbdrv{ status = error, data = {ErbiError, {FullName,Message}} };
rest_response(_,_,{_, 401, H,_}) ->
    header_error(unauthorized,<<"www-authenticate">>,H);
rest_response(_,_,{_, 407, H,_}) ->
    header_error(unauthorized,<<"proxy-authenticate">>,H);
rest_response(_,_,{_, 403, H,_}) ->
    header_error(unauthorized,H);
rest_response(_,_,{_, 408, H,_}) ->
    header_error(timeout,none,H);
rest_response(_,_,{_, Stat, H, _}) when (Stat >= 300) and (Stat < 500) ->
    header_error(communication_error,H);
rest_response(_,_,{_, Stat, H, _}) when (Stat >= 300) and (Stat < 500) ->
    header_error(communication_error,H);
rest_response(_,_,{_, Stat, H, _}) when (Stat >= 500) ->
    header_error(server_error,H);
rest_response(_,_,{error,Reason}) ->
    #erbdrv{ status = error, data = {unmapped_error,Reason} }.

header_error( Code, Headers ) ->
    header_error( Code, <<"status">>, Headers ).
header_error( Code, HeaderName, Headers ) ->
    header_error( Code, HeaderName, Headers, undefined ).
header_error( Code, HeaderName, Headers, Default ) ->
    Data = case proplists:get_value(HeaderName,Headers,Default) of
               undefined ->
                   Code;
               Val ->
                   {Code,Val}
           end,
    #erbdrv{ status = error, data = Data }.

%-----------------------------------------------
% REST functions
%-----------------------------------------------
rest_connect(#neocon{}=Conn,Scheme,Host,Port,Endpoint) ->
    ensure_started(hackney),
    Transport = case Scheme of
                    http -> hackney_tcp_transport;
                    https -> hackney_ssl_transport
                end,
    BinHost = iolist_to_binary(Host),
    BinPort = iolist_to_binary(integer_to_list(Port)),
    Headers =
        [{<<"Content-Type">>, <<"application/json">>},
         {<<"Accept">>, <<"application/json, */*;q=0.9">>},
         {<<"Host">>,<<BinHost/binary,":",BinPort/binary>>}],
    case hackney:connect(Transport,BinHost,Port,[]) of
        {ok,RConn} ->
            {ok,Conn#neocon{ rcon = RConn,
                             headers = Headers,
                             endpoint = iolist_to_binary(Endpoint)
                           }};
        E -> E
    end.

rest_request(#neocon{ trans = undefined,
                      endpoint = EndPoint }=Conn, Method, Path, JsonBody) ->
    rest_request(Conn,Method,EndPoint,Path,JsonBody);
rest_request(#neocon{ trans = Trans }=Conn, Method, Path, JsonBody) ->
    rest_request(Conn,Method,Trans,Path,JsonBody).
rest_request(#neocon{ rcon = RConn, headers = ReqHeaders },Method,Base,Path,JsonBody) ->
    BinBody = jsx:encode(JsonBody),
    case hackney:send_request(RConn,{Method,<<Base/binary,Path/binary>>,ReqHeaders,BinBody}) of
        {ok, Status, Headers, RConn} ->
            JsonResp =
                case hackney:body(RConn) of
                    {ok,<<>>} -> [];
                    {ok,BinResp} ->
                        jsx:decode(BinResp)
                end,
            {ok, Status, Headers, JsonResp};
        {error,_}=Err -> Err
    end.

rest_check(Scheme,Host,Port)->
    case rest_connect(#neocon{},Scheme,Host,Port,"/db/data/") of
        {ok,Conn} ->
            Ret = rest_check(Conn),
            disconnect(Conn),
            Ret;
        E -> E
    end.

rest_check(Conn) ->
    rest_request(Conn,get,<<"/db/data/">>,<<>>,[]).

%-----------------------------------------------
% Erbi temp driver internal functions
%-----------------------------------------------
get_temp_proplist(#erbi{properties=PropList}=DS,DataDir)->
    DS#erbi{properties = [get_temp_port_prop(DataDir), 
                          get_temp_shell_port_prop(DataDir)]++ add_endpoint_if_needed(PropList)}.

get_temp_port_prop(DataDir)->
    Port=erbi_temp_db_helpers:read_integer(DataDir,?PORT_FILE),
    {port,Port}.

get_temp_shell_port_prop(DataDir)->
    Port=erbi_temp_db_helpers:read_integer(DataDir,?SHELL_PORT_FILE),
    {shell_port,Port}.

add_endpoint_if_needed(PropList)->
    case proplists:get_value(endpoint,PropList) of
        undefined ->
            [];
        Val ->
            [{endpoint,Val}]
    end.

get_temp_username(_) ->
    undefined.

get_temp_password(_) ->
    undefined.

copy_binaries(Source,Dest)->
    ok=filelib:ensure_dir(Dest),
    lists:foreach(fun(SubDir)->
                          ok=filelib:ensure_dir(Dest++"/"++SubDir),
                          os:cmd("cp -p -r "++
                                     Source++"/../"++SubDir++" "++
                                     Dest++"/"++SubDir)
                  end,
                  ["bin","conf","lib","plugins","system"]),
    os:cmd("mkdir "++Dest++"/data"),
    ok.

configure_db_instance(PathData,Port,ShellPort)->
    substitute_properties_in_file(PathData,Port,ShellPort),
    enable_remote_shell_cmd(PathData,ShellPort),
    ok.

substitute_properties_in_file(PathData,Port,ShellPort)->
   lists:foreach(fun({Config,SubsCmd})->
              os:cmd("mv "++
               PathData++Config++" "++
               PathData++Config++"_tmp"),
               os:cmd("sed "++SubsCmd++
               " "++PathData++Config++"_tmp > "++
               PathData++Config)
                  end,
       [{"/conf/neo4j-server.properties",
         get_substitute_server_config_cmd(Port)},
        {"/conf/neo4j.properties",
         "s/enable_remote_shell=.*\$/enable_remote_shell=port" ++ integer_to_list(ShellPort) ++ "/g"},
        {"/conf/neo4j.properties",
         "s/remote_shell_port=.*\$/remote_shell_port" ++ integer_to_list(ShellPort) ++ "/g"}
       ]).

-define(DB_DATA_DIR,"data/erbi_tmp.db").
get_substitute_server_config_cmd(Port)->
    lists:flatten(lists:map(fun({Key,Val})->
                    " -e \"s/"++Key++"=.*\$/"++Key++"="++Val++"/g\""
            end,
    [
     {"org.neo4j.server.database.location",esc_slashes(?DB_DATA_DIR)},
     {"org.neo4j.server.webserver.port",integer_to_list(Port)},
     {"org.neo4j.server.webserver.https.enabled","false"}
    ])).
esc_slashes(Str) ->
    esc_slashes(Str,[]).
esc_slashes([$/|Str],Acc) ->
    esc_slashes(Str,"/\\"++Acc);
esc_slashes([C|Str],Acc) ->
    esc_slashes(Str,[C|Acc]);
esc_slashes([],Acc) ->
    lists:reverse(Acc).

enable_remote_shell_cmd(PathData,ShellPort)->
    os:cmd("echo 'enable_remote_shell=port=" ++ integer_to_list(ShellPort) ++ "' >> "++
               PathData++"/conf/neo4j.properties"),
    os:cmd("echo 'remote_shell_port=" ++ integer_to_list(ShellPort) ++ "' >> "++
               PathData++"/conf/neo4j.properties").

start_db_instance(DataSource,DataDir)->
    {ok,_,_} =  erbi_temp_db_helpers:exec_start(DataSource,term,DataDir++"/bin/neo4j",["start"]),
    ok.

stop_db_instance(DataDir)->
    case erbi_temp_db_helpers:exec_cmd(DataDir++"/bin/neo4j",["stop"],quiet) of
      {ok,_,_} ->
        ok;
      {error,_} ->
        erbi_temp_db_helpers:kill_db_pid(DataDir, ?PID_FILE)
    end.

wait_for_db_started(Port)->
    wait_for_db_state(Port,started,{error,db_not_started}).

wait_for_db_stopped(Port)->
    wait_for_db_state(Port,stopped,{error,db_not_stopped}).

wait_for_db_state(Port,ExpectedState,Error)->
    Fun = fun() ->
                  case {ExpectedState,rest_check(http,"localhost",Port)} of
                       {started,{ok,200,_,_}} ->
                          ok;
                       {stopped,{error,{failed_connect,_}}} ->
                          ok;
                       {stopped,{error,econnrefused}} ->
                          ok;
                       _Any ->
                          wait
                  end
          end,
   erbi_temp_db_helpers:wait_for(Fun,Error,500,50).

ensure_started(App) when not is_list(App) ->
    ensure_started([App]);
ensure_started([]) ->
    ok;
ensure_started([App|Apps]) ->
    case application:start(App) of
        ok -> ensure_started(Apps);
        {error,{already_started,App}} -> ensure_started(Apps);
        {error,{not_started,Dep}} -> ensure_started([Dep,App|Apps]);
        Other -> Other
    end.

initialize_db(PropList,PathData)->
    InitFiles= proplists:get_value(init_files,PropList,[]),
    lists:map(fun(File)->
                      erbi_temp_db_helpers:exec_cmd(PathData++"/bin/neo4j-shell",
                                                    [ "-path", PathData++"/"++?DB_DATA_DIR,
                                                      " -config", PathData++"/conf/", "-file", File ],
                                                    quiet
                                                   )
              end,InitFiles),
    ok.


