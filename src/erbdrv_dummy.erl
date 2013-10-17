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
%% erbi dummy driver
%% @end 
-module(erbdrv_dummy).
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
         finish/2,
         params/2
        ]).

driver_info() ->
    #erbi_driver_info
        { driver = dummy,
          preparse_support = false,
          cursor_support = false,
          transaction_support = false
        }.

validate_property(queries,Queries) ->
    {ok,[{queries,lists:map( fun({Re,Cols,Rows}) ->
                                    {ok,Pattern} = re:compile(Re),
                                    {Pattern,Cols,Rows}
                            end, Queries )
        }]};    
validate_property(Prop,Val) when is_list(Val) ->
    {ok,[{Prop,list_to_atom(Val)}]};
validate_property(_,_) ->
    ok.

property_info() ->
    [{defaults, [%whether each operation should succeed
                {connect,success},
                {disconnect,success},
                {prepare,success},
                {transaction,success},
                {begin_work,fallthrough},
                {rollback,fallthrough},
                {bind,success},
                {fetch,success},
                {finish,success},
                %other behaviours:
                % whether to return columns after prepare
                {cols_on_prepare,false},
                {rows_on_execute,true},
                {simulate_fetch,true},
                {queries,[{re:compile(".*"),[dummy],[[1]]}]}
               ]
     }].

parse_args(_) ->
    declined.

connect( #erbi{ properties = Props } = DataSource, _Username, _Password ) ->
    io:format(user, "Dummy connect: DS = ~n~p~n", [DataSource] ),
    on_success( Props, connect,
                #erbdrv{ status = ok, conn = Props } ).

disconnect( Props ) ->
    on_success( Props, disconnect, ok ).

begin_work( Props ) ->
    begin_work( Props, undefined ).
begin_work( Props, _ ) ->
    on_success( Props, [begin_work,transaction], #erbdrv{status=ok} ).

rollback( Props ) ->
    rollback( Props, undefined ).
rollback( Props, _ ) ->
    on_success( Props, [rollback,transaction], #erbdrv{status=ok} ).

commit( Props ) ->
    on_success( Props, [commit,transaction], #erbdrv{status=ok} ).

prepare( Props, Q ) ->
    on_success( Props, prepare, 
                fun() ->
                        Data = 
                            case proplists:get_value(cols_on_prepare,Props) of
                                true ->
                                    {Cols,_} = match_query(Props,Q),
                                    Cols;
                                _ -> undefined
                            end,
                        #erbdrv{status=ok,stmt={Q,[],undefined},data=Data}
                end ).

bind_params( Props, {Q,OldParams,R}, Params ) ->
    on_success( Props, bind, #erbdrv{status=ok,stmt={Q,OldParams++Params,R}} ).

execute( Props, Q, Params ) ->
    on_success( Props, execute, 
                fun() ->
                        {Cols,Rows} = match_query(Props,Q),
                        %if we aren't returning the whole set of rows now,
                        %stash them in the statement
                        {StmtRows,DataRows} =
                            case proplists:get_value(rows_on_execute,Props) of
                                true -> {[],{final,Rows}};
                                false -> {Rows,[]}
                            end,
                        Data = 
                            case proplists:get_value(cols_on_prepare,Props) of
                                true -> DataRows;
                                false -> {Cols,DataRows}
                            end,
                        Stmt = case Q of
                                   {Query,StParams,_} -> {Query,StParams++Params,StmtRows};
                                   Query -> {Query,Params,StmtRows}
                               end,
                        #erbdrv{status=ok,stmt=Stmt,data=Data}
                end ).

fetch_rows( Props, {Query,_Params,Rows}, Amount ) ->
    on_success( Props, fetch,
                fun() ->
                        RowsOnExec = proplists:get_value(rows_on_execute,Props),
                        SimFetch = proplists:get_value(simulate_fetch,Props),
                        {RetRows,StmtRows} = 
                            case Rows of
                                [] -> final;
                                _ ->
                                    if (RowsOnExec and SimFetch and (Amount =:= one)) ->
                                            [Row|Rows1] = Rows,
                                            case Rows1 of 
                                                [] ->
                                                    {{final,[Row]},[]};
                                                _ ->
                                                    {[Row],Rows1}
                                            end;
                                       true ->
                                            {{final,Rows},[]}
                                    end                  
                            end,
                        #erbdrv{status=ok,stmt={Query,StmtRows},data=RetRows}
                end ).
                      
finish( Props, {Query,_,_} ) ->
    on_success( Props, finish, #erbdrv{status=ok,stmt={Query,[],undefined}} ).

%% -----------------------------------
%% @doc get bind parameters passed in
%%
%% example:
%% <pre>
%%  {ok,Params} = erbi_statement:driver_call(Statement,params,[])
%% </pre>
%% -----------------------------------
params( _Props, {_Q,Params,_} ) ->
    #erbdrv{status=ok,data=Params}.


%% ---
%% Internal functions
%% ---

on_success( Props, Ops, OnSuccess ) when is_list(Ops) ->
    SV = lists:foldl(fun(Op,fallthrough) ->
                             proplists:get_value(Op,Props);
                        (_,V) -> V
                     end,fallthrough,Ops),
    on_success(SV,OnSuccess);
on_success( Props, Op, OnSuccess ) ->
    SV = proplists:get_value(Op,Props),
    on_success(SV,OnSuccess).
on_success( SuccessVal, OnSuccess ) ->
    case SuccessVal of
        success ->
            if is_function(OnSuccess) ->
                    OnSuccess();
               true ->
                    OnSuccess
            end;
        declined ->
            declined;
        Reason ->
            #erbdrv{ status = error, data = Reason }
    end.

match_query(Props,QStr) ->
    QueryList = proplists:get_value(queries,Props),
    Found = 
        lists:foldl( fun({Rgx,C,R},none) ->
                             case re:run(QStr,Rgx) of
                                 {match,_} -> {C,R};
                                 _ -> none
                             end;
                        (_,{C,R}) -> {C,R}
                     end, none, QueryList ),
    case Found of
        none ->
            {[],[]};
        _ -> Found
    end.
                              
