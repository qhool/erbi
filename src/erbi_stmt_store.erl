%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%% @copyright 2013 Voalte Inc. <jburroughs@voalte.com>
%%% 
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @private
%%% @doc
%%% Storage for information about statements, including a row-cache.
%%%
%%% The statement store is an ETS table.  Each statement gets an ID
%%% the store consists of:
%%% <ul>
%%% <li>{ {StatementID, counters}, First, Current, Last, IsFinal }</li>
%%% <li>{ {StatementID, Key}, Val }</li>
%%% <li>{ {StatementID, N}, Row }</li>
%%%
%%% keys can be: handle, counters, metadata
%%% @end
-module(erbi_stmt_store).
-include("erbi_driver.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
        
-export([init_store/0,
         add_statement/2,
         reset_statement/2,
         reset_all/1,
         get/3,
         lookup/4,
         all_handles/1,
         set/4,
         incr/4,
         counters/2,
         set_cols/3,
         get_cols/2,
         add_rows/3,
         add_rows/4
        ]).

-ifdef(TEST).
-export([init_unprotected/0]).
-endif.

-define(COUNTERS_INIT(ID,FIN),  {{ID,counters},0, 0, -1, FIN} ).

init_store() ->
    init_store([protected]).
init_store(Opts) ->
    Tbl = ets:new(statements,[set]++Opts),
    true = ets:insert( Tbl, {next_id,0} ),
    Tbl.
-ifdef(TEST).
init_unprotected() ->
    init_store([public]).
-endif.

add_statement(Tbl,Handle) ->
    NextID = ets:update_counter(Tbl, next_id, {2,1}),
    % if there's no handle, statement is final by default
    IsFinal = Handle =:= undefined,
    true = ets:insert( Tbl, [{{NextID,handle},Handle},
                             ?COUNTERS_INIT(NextID,IsFinal)] ),
    NextID.

-spec reset_statement( Tbl :: ets:tid(),
                       StmtID :: pos_integer() ) ->
                             erbdrv_statement() | undefined.

reset_statement(Tbl,StmtID) ->
    Handle = get(Tbl,StmtID,handle),
    IsFinal = Handle =:= undefined,
    % really an update
    ets:insert( Tbl, ?COUNTERS_INIT(StmtID,IsFinal) ),
    %delete cached rows
    ets:select_delete( Tbl, ets:fun2ms(fun({{ID,N},_}) when ID == StmtID, is_integer(N) -> true end) ),
    %delete cached params
    ets:delete( Tbl, [{StmtID,params}] ),
    Handle.

reset_all(Tbl) ->
    IDHandles = ets:select( Tbl, ets:fun2ms(fun({{ID,handle},H}) -> {ID,H} end) ),
    %delete rows and params
    ets:select_delete( Tbl, ets:fun2ms(fun({{_,X},_}) when is_integer(X); X =:= params -> true end) ),
    %% reset counts
    lists:foreach( fun({ID,H}) -> 
                           IsFinal = H =:= undefined,
                           ets:insert( Tbl, ?COUNTERS_INIT(ID,IsFinal) )
                   end, IDHandles ),
    IDHandles.


-define(KEY_TO_IDX(X),case X of 
                          first -> {counters,2};
                          current -> {counters,3};
                          last -> {counters,4};
                          is_final -> {counters,5};
                          X -> {X,2}
                       end).

get( Tbl, StatementID, Key ) ->
    {K,I} = ?KEY_TO_IDX(Key),
    ets:lookup_element( Tbl, {StatementID,K}, I ).

lookup( Tbl, StatementID, Key, Default ) ->
    {K,I} = ?KEY_TO_IDX(Key),
    case ets:lookup( Tbl, {StatementID,K} ) of
        [Item] ->
            element(I,Item);
        X -> io:format(user,"lookup: ~p~n",[X]), Default
    end.    

all_handles( Tbl ) ->
    ets:select( Tbl, ets:fun2ms(fun({{ID,handle},H}) when is_integer(ID) -> H end) ).


set( Tbl, StatementID, Key, Val ) ->
    {K,I} = ?KEY_TO_IDX(Key),
    case ets:update_element( Tbl, {StatementID,K}, {I,Val} ) of
        false ->
            I = 2, %should always be 2 except for counters
            ets:insert( Tbl, {{StatementID,K},Val} );
        _ -> ok
    end.

incr( Tbl, StatementID, Key, Incr ) ->
    {K,I} = ?KEY_TO_IDX(Key),
    ets:update_counter( Tbl, {StatementID,K}, {I,Incr} ).
        
counters( Tbl, StatementID ) ->
    [Counters] = ets:lookup( Tbl, {StatementID,counters} ),
    % turn it into a counters record
    #erbdrv_stmt_counters{} = setelement(1,Counters,erbdrv_stmt_counters).

set_cols( Tbl, StmtID, Cols ) ->
    set( Tbl, StmtID, metadata, Cols ).
get_cols( Tbl, StmtID ) ->
    lookup( Tbl, StmtID, metadata, [] ).


%TODO: remove old rows
add_rows( Tbl, StmtID, Data ) ->
    Counters = counters( Tbl, StmtID ),
    add_rows(Tbl,StmtID,Data,Counters).

add_rows( Tbl, StmtID, {Cols,Rows}, Counters ) ->
    set_cols(Tbl,StmtID, Cols),
    add_rows(Tbl,StmtID, Rows, Counters);
add_rows( Tbl, StmtID, Rows, 
          #erbdrv_stmt_counters{last=Last} = Counters ) ->
    NewLast = Last + length(Rows),
    RowRecs = lists:map( fun({Idx,Row}) ->
                                 {{StmtID,Idx},Row}
                         end, lists:zip( lists:seq(Last+1,NewLast), Rows ) ),
    ets:insert( Tbl, RowRecs ),
    set( Tbl, StmtID, last, NewLast ),
    Counters1 = Counters#erbdrv_stmt_counters{last=NewLast},
    {ok,Counters1,Tbl}.
