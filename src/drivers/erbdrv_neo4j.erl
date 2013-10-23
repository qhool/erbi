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
         prepare/2,
         bind_params/3,
         execute/3,
         fetch_rows/3,
         finish/2
        ]).

-record(neocon, % ;-)
        { type = transaction :: atom(),
          trans = undefined :: undefined | string(),
          url :: string()
        }).

driver_info() ->
    #erbi_driver_info{ driver = neo4j }.

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
    % make sure the server is there:
    CheckUrl = BaseUrl ++ "/db/data/",
    ok = application:ensure_started(inets),
    ok = case Scheme of
             https ->
                 application:ensure_started(restc);
             _ ->
                 ok
         end,
    case restc:request( get, json, CheckUrl, [200] ) of
        {ok,_Status,_,_} ->
            #erbdrv{ status = ok, conn = #neocon{type=Endpoint,url=EndpointUrl} };
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
    do_req(post,Trans ++ "/commit",[200],[{statements,[]}],
           fun(_S,_H,_B) ->
                   #erbdrv{ status = ok, conn = C#neocon{trans=undefined} }
           end).

%% no way to do this in neo4j Rest API
prepare( _C, _Q ) ->
    declined.

bind_params( _C , _S, _P ) ->
    declined.

%% since prepare isn't supported, we should always get the raw query and a list of params
execute( #neocon{type=cypher, url=Url}, Query, Params ) ->
    Q = format_query({'query',Query},{params,Params}),
    do_req(post,Url,[200],Q,
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
execute( #neocon{trans = Trans, url=Url}, Query, Params ) ->
    ToUrl = 
        case Trans of
            undefined ->
                Url ++ "/commit"; % if there is no open transaction, do a one-shot
            _ ->
                Trans
        end,
    Q = format_query({statement,Query},{parameters,Params}),
    do_req(post,ToUrl,[200],[{statements,[Q]}],
           fun(_S,_H,Body) ->
                   % Results = 
                   case proplists:get_value(<<"errors">>,Body) of
                       X when X =:= [] ; X =:= undefined ->
                           [Result|_] = proplists:get_value(<<"results">>,Body),
                           io:format(user,"~n~nResult: ~p~n",[Result]),
                           Cols = proplists:get_value(<<"columns">>,Result,[]),
                           io:format(user,"columns: ~p~n",[Cols]),
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


do_req(Method,#neocon{trans=undefined,url=Url},Statuses,Body,Func) ->
    do_req(Method,Url,Statuses,Body,Func);
do_req(Method,#neocon{trans=Trans},Statuses,Body,Func) ->
    do_req(Method,Trans,Statuses,Body,Func);
do_req(Method,Url,Statuses,ReqBody,Func) ->
    io:format(user,"~n~n~n~n------~nURL: ~p~n------~nreqbody: ~p~n",[Url,ReqBody]),
    case restc:request(Method,json,Url,Statuses,[],ReqBody) of
        {error, Status, _H, B } when Status == 400 ->
            io:format(user,"400Status: ~n~p~n",[B]),
            ErrMsg = proplists:get_value(<<"message">>,B),
            #erbdrv{ status = error, data = { cypher_error, ErrMsg } };
        {error, Status, H, B } -> 
            io:format(user,"got error status (~p): ~n~p~n~n",[Status,B]),
            #erbdrv{ status = error, data = { unexpected_status, {Status,H,B} } };
        { error, Reason } -> 
            io:format(user,"{error,~p}~n",[Reason]),
            #erbdrv{ status = error, data = { rest_error, Reason } };
        { ok, Status, Headers, Body } -> 
            io:format(user,"got body:~n~p~n",[Body]),
            Func(Status,Headers,Body);
        Other ->
            io:format(user, "Other: ~n~p~n",[Other]),
            {error,Other}
    end.  
              

                
