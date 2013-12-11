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
%% @end 
-module(erbdrv_neo4j).
-behaviour(erbi_driver).
-behaviour(erbi_temp_db).

-include("erbi.hrl").
-include("erbi_driver.hrl").

-export([driver_info/0,
         validate_property/2,
         property_info/0,
         parse_args/1,
         connect/3,
         disconnect/1,
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
-export([start_temp/1,
         stop_temp/1,
        get_temp_connect_data/3]).

-record(neocon, % ;-)
        { type = transaction :: atom(),
          trans = undefined :: undefined | string(),
          url :: string()
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
    BaseUrl = atom_to_list(Scheme) ++ "://" ++ Host ++ ":" ++ integer_to_list(Port),
    EndpointUrl = BaseUrl ++ "/db/data/" ++ atom_to_list(Endpoint),
    Info = case Endpoint of
               transaction -> same;
               cypher ->
                   (driver_info())#erbi_driver_info{transaction_support=false}
           end,
    case check_db_status(Scheme,Host,Port, [200]) of
        {ok,_Status,_,_} ->
            #erbdrv{ status = ok, info = Info, conn = #neocon{type=Endpoint,url=EndpointUrl} };
        {error,Status,Headers,_} ->
            #erbdrv{ status = error, data = {connect_failed,Status,Headers} }
    end.

%% there was no real connection to start with
disconnect( _ ) ->
    ok.

begin_work( #neocon{type=cypher} ) ->
    declined;
begin_work( C ) ->
    do_req(post,C,[201],[{statements,[]}],
           fun(_S,Headers,_B) ->
                   Trans = proplists:get_value("location",Headers),
                   #erbdrv{ status = ok, conn = C#neocon{trans=Trans} }
           end).
begin_work( _C, _ ) ->
    declined.

rollback( #neocon{type=cypher} ) ->
    declined;
rollback( #neocon{trans=undefined} ) ->
    #erbdrv{ status = error, data = no_transaction };
rollback( C ) ->
    do_req(delete,C,[200],[],
           fun(_S,_H,_B) ->
                   #erbdrv{ status = ok, conn = C#neocon{trans=undefined} }
           end).
rollback( _C, _ ) ->
    declined.

commit( #neocon{type=cypher} ) ->
    declined;
commit( #neocon{trans=undefined} ) ->
    #erbdrv{ status = error, data = no_open_transaction };
commit( #neocon{trans=Trans}=C ) ->
    do_req(post,C,Trans ++ "/commit",[200],[{statements,[]}],
           fun(_S,_H,_B) ->
                   #erbdrv{ status = ok, conn = C#neocon{trans=undefined} }
           end).

%% no way to do this in neo4j Rest API
prepare( _C, _Q ) ->
    declined.

do( _C, _Q, _P ) ->
    declined.

bind_params( _C , _S, _P ) -> declined.

%% since prepare isn't supported, we should always get the raw query and a list of params
execute( #neocon{type=cypher, url=Url}=C, Query, Params ) ->
    Q = format_query({'query',Query},{params,Params}),
    do_req(post,C,Url,[200],Q,
           fun(_S,_H,Body) ->
                   Cols = proplists:get_value(<<"columns">>,Body,[]),
                   Rows = proplists:get_value(<<"data">>,Body,[]),
                   RowCount = case proplists:get_value(<<"stats">>,Body) of
                                  undefined ->
                                      length(Rows);
                                  Stats ->
                                      lists:foldl(fun({_,N},Sum) when is_integer(N) ->
                                                          Sum+N;
                                                     (_,Sum) -> Sum
                                                  end,0,Stats)
                              end,
                   #erbdrv{status=ok,rows=RowCount,data={Cols,Rows}}
           end);
execute( #neocon{trans = Trans, url=Url}=C, Query, Params ) ->
    ToUrl = 
        case Trans of
            undefined ->
                Url ++ "/commit"; % if there is no open transaction, do a one-shot
            _ ->
                Trans
        end,
    Q = format_query({statement,Query},{parameters,Params}),
    do_req(post,C,ToUrl,[200],[{statements,[Q]}],
           fun(_S,_H,Body) ->
                   % Results = 
                   case proplists:get_value(<<"errors">>,Body) of
                       X when X =:= [] ; X =:= undefined ->
                           [Result|_] = proplists:get_value(<<"results">>,Body),
                           %io:format(user,"~n~nResult: ~p~n",[Result]),
                           Cols = proplists:get_value(<<"columns">>,Result,[]),
                           %io:format(user,"columns: ~p~n",[Cols]),
                           %% my neo4j differs from the docs here:
                           %Data = proplists:get_value(<<"data">>,Result,[]),
                           %Rows = proplists:get_all_values(<<"row">>,Data),
                           Rows = proplists:get_value(<<"data">>,Result,[]),
                           #erbdrv{status=ok,rows=length(Rows),data={Cols,Rows}};
                       Errors ->
                           #erbdrv{status=error,data=Errors}
                   end
           end).  

fetch_rows(_,_,_) ->
    declined.
finish(_,_) ->
    decline.

%----------------------------------------------------
% erbi_temp_db API
%-----------------------------------------------------
-define(PORT_FILE,"tmp_db.port").
-define(MIN_PORT, 7475).
-define(MAX_PORT, 8475).
-define(POSSIBLE_BIN_DIRS,[]).

-spec start_temp(ErbiDataSource::erbi_data_source())->
    ok.
start_temp(#erbi{properties=PropList})->
    {ok,PathBin}= erbi_temp_db_helpers:search_db_binaries(
                    [proplists:get_value(bin_dir,PropList,"") |
                     ?POSSIBLE_BIN_DIRS]
                    ,"neo4j"),
    PathData = proplists:get_value(data_dir,PropList),
    {ok, Port}=erbi_temp_db_helpers:get_free_db_port(?MIN_PORT,?MAX_PORT),
    ok = get_needed_binaries_copies(PathBin,PathData),
    ok = configure_db_instance(PathData,Port),
    ok = initialize_db(PropList,PathData), %starts a local neo4j-shell that populates data
    ok = start_db_instance(PathData),
    ok = wait_for_db_started(Port ),
    ok = erbi_temp_db_helpers:save_in_db_data_file(Port,PathData,?PORT_FILE),
    ok.

-spec stop_temp(ErbiDataSource::erbi_data_source())->
    ok.
stop_temp(#erbi{properties=PropList})->
    PathData = proplists:get_value(data_dir,PropList),
    Port = erbi_temp_db_helpers:read_from_db_data_file(PathData,?PORT_FILE),
    ok = stop_db_instance(PathData),
    ok = wait_for_db_stopped(Port),
    ok = erbi_temp_db_helpers:del_data_dir(PathData),
    ok.

-spec get_temp_connect_data(ErbiDataSource::erbi_data_source(),
                                Username::unicode:chardata(),
                                Password::unicode:chardata())->
    {erbi_data_source(),
     unicode:chardata(),
     unicode:chardata()}.
get_temp_connect_data(ErbiDataSource,UserName,Password)->
    {get_temp_proplist(ErbiDataSource),
     get_temp_username(UserName),
     get_temp_password(Password)}.



%%-- Internals --%%

format_query({QAtom,Query},{PAtom,Params}) ->
    Query1 = case Query of
                 X when is_binary(X) -> X;
                 _ -> list_to_binary(Query)
             end,
    Q = [{QAtom,Query1}],
    QParams = case Params of
                  [] -> [];
                  _ ->
                      BinParams = 
                          lists:map( fun ({P,V}) when is_list(V) ->
                                             {P,list_to_binary(V)};
                                         (X) -> X
                                     end, Params ),
                      [{PAtom,BinParams}]
              end,
    %% neo4j cares about the order
    Q++QParams.


do_req(Method,#neocon{trans=undefined,url=Url}=Conn,Statuses,Body,Func) ->
    do_req(Method,Conn,Url,Statuses,Body,Func);
do_req(Method,#neocon{trans=Trans}=Conn,Statuses,Body,Func) ->
    do_req(Method,Conn,Trans,Statuses,Body,Func).

do_req(Method,Conn,Url,Statuses,ReqBody,Func) ->
    %io:format(user,"~n~n~n~n------~nURL: ~p~n------~nreqbody: ~p~n",[Url,ReqBody]),
    rest_response(Conn,Func,restc:request(Method,json,Url,Statuses,[],ReqBody)).
         
rest_response(#neocon{type=Type},Func,RestStat) ->
    rest_response(Type,Func,RestStat);
rest_response(_,Func,{_,Stat,Headers,Body}) when (Stat >= 200) and (Stat < 300) ->
    case proplists:get_value(<<"errors">>,Body) of
        X when X =:= [] ; X =:= undefined ->
            Func(200,Headers,Body);
        [Error|_] ->
            Code = proplists:get_value(<<"code">>,Error),
            Status = proplists:get_value(<<"status">>,Error),
            Message = proplists:get_value(<<"message">>,Error),
            ErbiError = 
                case Code of
                    42000 -> execution_error;
                    42001 -> syntax_error;
                    42002 -> missing_parameter;
                    _ -> unmapped_error
                end,
            #erbdrv{status=error,data={ErbiError,{Code,Status,Message}}}
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
% Erbi temp driver internal functions
%-----------------------------------------------
get_temp_proplist(#erbi{properties=PropList}=DS)->
    DS#erbi{properties = [get_temp_port_prop(PropList)]++
                         add_endpoint_if_needed(PropList)}.

get_temp_port_prop(PropList)->
    PathData= proplists:get_value(data_dir,PropList),
    Port=erbi_temp_db_helpers:read_from_db_data_file(PathData,?PORT_FILE),
    {port,Port}.

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

get_needed_binaries_copies(PathBin,PathData)->
    ok=filelib:ensure_dir(PathData),
    lists:foreach(fun(SubDir)->
                          ok=filelib:ensure_dir(PathData++"/"++SubDir),
                          os:cmd("cp -p -r "++
                                     PathBin++"../"++SubDir++" "++
                                     PathData++"/"++SubDir)
                  end,
                  ["bin","conf","lib","plugins","system"]),
    os:cmd("mkdir "++PathData++"/data"),
    ok.

configure_db_instance(PathData,Port)->
    substitute_properties_in_file(PathData,Port),
    check_disabled_remote_shell_cmd(PathData),
    ok.

substitute_properties_in_file(PathData,Port)->
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
         "s/enable_remote_shell=.*\$/enable_remote_shell=false/g"}]).

-define(DB_DATA_DIR,"data\\/erbi_tmp.db").
get_substitute_server_config_cmd(Port)->
    lists:flatten(lists:map(fun({Key,Val})->
                    " -e \"s/"++Key++"=.*\$/"++Key++"="++Val++"/g\""
            end,
    [
     {"org.neo4j.server.database.location",?DB_DATA_DIR},
     {"org.neo4j.server.webserver.port",integer_to_list(Port)},
     {"org.neo4j.server.webserver.https.enabled","false"}
    ])).

check_disabled_remote_shell_cmd(PathData)->
    os:cmd("echo 'enable_remote_shell=false' >> "++
               PathData++"/conf/neo4j.properties").

start_db_instance(PathData)->
    exec_neo4j_server_cmd(PathData,"start &").

stop_db_instance(PathData)->
    exec_neo4j_server_cmd(PathData,"stop").

exec_neo4j_server_cmd(PathData,NeoCmd) ->
    DbCmd=PathData++"/bin/neo4j "++NeoCmd,
    os:cmd(DbCmd),
    ok.

wait_for_db_started(Port)->
    wait_for_db_state(Port,started,[200],{error,db_not_started}).

wait_for_db_stopped(Port)->
    wait_for_db_state(Port,stopped,[],{error,db_not_stopped}).

wait_for_db_state(Port,ExpectedState,ExpectedHttpStatus,Error)->
    Fun = fun() ->
                  case {ExpectedState,check_db_status(http,"localhost",Port, ExpectedHttpStatus)} of
                       {started,{ok,_,_,_}} ->
                          ok;
                       {stopped,{error,{failed_connect,_}}} ->
                          ok;
                      Any ->
                          wait
                  end
          end,
   erbi_temp_db_helpers:wait_for(Fun,Error,500,50).

check_db_status(Scheme,Host,Port, ExpectedHttpCode)->
    BaseUrl = atom_to_list(Scheme) ++ "://" ++ Host ++ ":" ++ integer_to_list(Port),
    CheckUrl = BaseUrl ++ "/db/data/",
    ok = application:ensure_started(inets),
    ok = case Scheme of
             https ->
                 application:ensure_started(restc);
             _ ->
                 ok
         end,
    restc:request( get, json, CheckUrl, ExpectedHttpCode ).

initialize_db(PropList,PathData)->
    InitFiles= proplists:get_value(init_files,PropList,[]),
    lists:map(fun(File)->
                         os:cmd(PathData++"/bin/neo4j-shell -path "++PathData++
                                      "/"++?DB_DATA_DIR++
                                      " -config "++PathData++"/conf/ -file "++File)
              end,InitFiles),
    ok.


