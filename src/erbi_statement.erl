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
%% Erbi statement handle functions: perform queries, fetch rows, etc. 
%% @end 
-module(erbi_statement).

-export([bind_params/2,
         execute/1, execute/2,
         fetchrow_list/1,
         fetchrow_proplist/1,
         fetchrow_dict/1,
         finish/1,
         fetchall_list/1,
         fetchall_proplist/1,
         fetchall_dict/1,
         driver_call/3
        ]).

-include("erbi.hrl").
-include("erbi_driver.hrl").
-include("erbi_private.hrl").

-define(STMT(StmtID),{erbi_statement,_,#stmt{ id = StmtID }}).

%% --------------------------------------
%% @doc Bind given parameters to this statement.
%% May be supplied as a positional list, or by name (contingent on driver support).
%% - Statement  - erbi statement handle
%% - BindValues - List of parameter values 
%% @end
%% --------------------------------------
-spec bind_params( Statement :: erbi_statement(),
                   Params :: erbi_bind_values() ) ->
                         ok | { error, any() }.
bind_params( ?STMT(StmtID)=Statement, Params ) ->
    erbi_driver:call(Statement,{bind,StmtID,Params}).

%% --------------------------------------
%% @doc Begin execution of this statement
%% @end
%% --------------------------------------
-spec execute( Statement :: erbi_statement() ) -> {ok,erbi_row_count()} | { error, any() }.
execute( Statement ) ->
    execute(Statement,[]).

-spec execute( Statement :: erbi_statement(),
               Params :: erbi_bind_values() ) ->
                     {ok,erbi_row_count()} | { error, any() }.
execute( ?STMT(StmtID)=Statement, Params ) ->
    erbi_driver:call(Statement,{execute,StmtID,Params}).

%% --------------------------------------
%% @doc Fetch a single record from the result set.
%% @end
%% --------------------------------------
-spec fetchrow_list( Statement :: erbi_statement() ) ->
                           { ok, [any()] } | exhausted | { error, any() }.
fetchrow_list(Statement) ->
    get_row_as(list,Statement).
             
-spec fetchrow_proplist( Statement :: erbi_statement() ) ->
                           { ok, [property()] } | exhausted | { error, any() }.
fetchrow_proplist(Statement) ->
    get_row_as(proplist,Statement).

-spec fetchrow_dict( Statement :: erbi_statement() ) ->
                           { ok, dict() } | exhausted | { error, any() }.
fetchrow_dict(Statement) ->
    get_row_as(dict,Statement).

%% --------------------------------------
%% @doc Complete processing with this statement.
%% Closes associated cursor.
%% @end
%% --------------------------------------
-spec finish( Statement :: erbi_statement() ) ->
                    ok | {error, any()}.
finish(?STMT(StmtID) = Statement) ->
    erbi_driver:call(Statement,{finish,StmtID}).


%% --------------------------------------
%% @doc 
%% Fetch all remaining records and return a list.
%% @end
%% --------------------------------------
-spec fetchall_list( Statement :: erbi_statement() ) ->
                           { ok, [[any()]] } | { error, any() }.
fetchall_list(Statement) ->
    get_rows_as(list,Statement,all).

-spec fetchall_proplist( Statement :: erbi_statement() ) ->
                           { ok, [[property()]] } | { error, any() }.
fetchall_proplist(Statement) ->
    get_rows_as(proplist,Statement,all).
           
-spec fetchall_dict( Statement :: erbi_statement() ) ->
                           { ok, [dict()] } | { error, any() }.
fetchall_dict(Statement) ->
    get_rows_as(dict,Statement,all).

%% --------------------------------------
%% @doc Call arbitrary driver function
%%
%% This function calls an arbitrary function in the driver module.
%% Similar to {@link erbi:call_driver/3} and {@link erbi_connection:call_driver/3},
%% this version prepends the driver's connection object and
%% the statement handle to the argument list you supply,
%% and allows the driver to update its state information.
%%
%% See the documentation for your driver for what functions are available
%% and how to use them.
%% @end
%% --------------------------------------
-spec driver_call( Statement :: erbi_statement(),
                   Function :: atom(),
                   Args :: [any()] ) ->
                         any().
driver_call( ?STMT(StmtID)=Statement, Function, Args ) ->
    erbi_driver:call(Statement,{driver_call,StmtID,Function,Args}).

%%==== Internals ====%%
get_row_as(Type,Statement) ->
    case get_rows_as(Type,Statement,one) of
        {error,_}=E -> E;
        {ok,[]} -> exhausted;
        {ok,[Row]} -> {ok,Row}
    end.
            
get_rows_as(Type,?STMT(StmtID)=Statement,Amount) ->
    case get_rows(Statement,Amount,[],
                  erbi_driver:call(Statement,{start_fetch,StmtID,Amount})) of
        {error,Reason} ->
            {error,Reason};
        {Store,Acc} ->
            Rows = lists:concat(lists:reverse(Acc)),
            case Type of
                list ->
                    {ok,Rows};
                _ ->
                    case erbi_stmt_store:get_cols(Store,StmtID) of
                        undefined ->
                            {error, no_metadata};
                        Cols ->
                            ColNames = lists:map( fun(#erbdrv_field{name=Name}) -> Name end, Cols ),
                            Proplists = lists:map( fun(Row) -> lists:zip(ColNames,Row) end, Rows ),
                            case Type of
                                proplist ->
                                    {ok,Proplists};
                                dict ->
                                    {ok,lists:map( fun dict:from_list/1, Proplists )}
                            end
                    end
            end
    end.

get_rows(_,_,_,{error,Reason}) ->
    {error,Reason};
get_rows(?STMT(StmtID)=Statement,_,Acc,{ok,#erbdrv_stmt_counters{current=C,last=L},Store}) 
  when C > L ->
    erbi_driver:call(Statement,{end_fetch,StmtID,0},{Store,Acc});
get_rows(?STMT(StmtID)=Statement,one,Acc,{ok,#erbdrv_stmt_counters{current=Current},Store}) ->
    Row = erbi_stmt_store:get(Store,StmtID,Current),
    erbi_driver:call(Statement,{end_fetch,StmtID,1},{Store,[[Row]|Acc]});
get_rows(?STMT(StmtID)=Statement,all,Acc,
         {ok,#erbdrv_stmt_counters{current=Current,last=Last,is_final=IsFinal},Store}) ->
    Rows = lists:map( fun(N) -> erbi_stmt_store:get(Store,StmtID,N) end, lists:seq(Current,Last) ),
    NumRead = Last-Current+1,
    case IsFinal of
        true ->
            erbi_driver:call(Statement,{end_fetch,StmtID,NumRead},{Store,[Rows|Acc]});
        false ->
            get_rows(Statement,all,Acc,
                     erbi_driver:call(Statement,{continue_fetch,StmtID,NumRead}))
    end.


                               
