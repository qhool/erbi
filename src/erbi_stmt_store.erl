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

-define(INIT_FIRST,0).
-define(INIT_CURRENT,0).
-define(INIT_LAST,-1).
-define(STMT_BATCH_SIZE,20).

-record(stmt,{
          tid                           :: ets:tid(),
          first     = ?INIT_FIRST       :: integer(),  %% <-+-counters
          current   = ?INIT_CURRENT     :: integer(),  %% <-|
          last      = ?INIT_LAST        :: integer(),  %% <-|
          is_final                      :: boolean(),  %% <-+
          handle                        :: any(),
          raw_query                     :: binary() | string(),
          bound     = false             :: boolean(),
          columns                       :: list(),
          params    = []                :: list()
         }).


-define(KEY_TO_IDX(X),case X of
                          tid ->        #stmt.tid;
                          first ->      #stmt.first;
                          current ->    #stmt.current;
                          last ->       #stmt.last;
                          is_final ->   #stmt.is_final;
                          handle ->     #stmt.handle;
                          raw_query ->  #stmt.raw_query;
                          bound ->      #stmt.bound;
                          columns ->    #stmt.columns;
                          params ->     #stmt.params
                      end).


%-define(COUNTERS_INIT(ID,FIN),  {{ID,counters},0, 0, -1, FIN} ).
init_store() ->
    init_store([protected]).
init_store(Opts) ->
    ets:new(statements,[set,{keypos,2}]++Opts).
-ifdef(TEST).
init_unprotected() ->
    init_store([public]).
-endif.

-ifdef(TEST).
-define(STMT_TBL_OPTS(Tbl),[proplists:get_value(protection,ets:info(Tbl)),set]).
-else.
-define(STMT_TBL_OPTS(_),[protected,set]).
-endif.



add_statement(Tbl,Handle) ->
    % if there's no handle, statement is final by default
    IsFinal = Handle =:= undefined,
    Statement = ets:new(rows,?STMT_TBL_OPTS(Tbl)),
    true = ets:insert( Tbl, [#stmt{ tid = Statement,
                                    handle = Handle,
                                    is_final = IsFinal }] ),
    Statement.

-spec reset_statement( Tbl :: ets:tid(),
                       Statement :: ets:tid() ) ->
                             erbdrv_statement() | undefined.

reset_statement(Tbl,Statement) ->
    case ets:lookup(Tbl,Statement) of
        [#stmt{ tid = Statement,
                handle = Handle }] ->
            ets:delete(Statement),
            ets:update_element(Tbl,Statement, [{#stmt.first, ?INIT_FIRST},
                                               {#stmt.current, ?INIT_CURRENT},
                                               {#stmt.last, ?INIT_LAST},
                                               {#stmt.is_final, Handle =:= undefined},
                                               {#stmt.bound, false},
                                               {#stmt.params, []}]),
            Handle;
        _ ->
            undefined
    end.

reset_all(Tbl) ->
    Stmts = ets:tab2list(Tbl),
    reset_all(Tbl,Stmts,[]).
reset_all(Tbl,[#stmt{tid=StmtTbl,handle=Handle}|Rest],Handles) ->
    catch(ets:delete(StmtTbl)),
    case Handle of
        undefined -> reset_all(Tbl,Rest,Handles);
        _ -> reset_all(Tbl,Rest,[Handle|Handles])
    end;
reset_all(Tbl,[],Handles) ->
    ets:delete_all_objects(Tbl),
    Handles.

get( _Tbl, Statement, Row ) when is_integer(Row) ->
    ets:lookup_element( Statement, Row, 2 );
get( Tbl, Statement, Key ) ->
    ets:lookup_element( Tbl, Statement, ?KEY_TO_IDX(Key) ).
lookup( Tbl, Statement, Key, Default ) ->
    case catch(get(Tbl,Statement,Key)) of
        {'EXIT',{badarg,_}} ->
            Default;
        Item -> Item
    end.

all_handles( Tbl ) ->
    ets:select( Tbl, ets:fun2ms(fun(#stmt{handle=H}) -> H end) ).

set( Tbl, Statement, Key, Val ) ->
    true = ets:update_element( Tbl, Statement, {?KEY_TO_IDX(Key),Val} ),
    ok.

incr( Tbl, Statement, Key, Incr ) ->
    ets:update_counter( Tbl, Statement, {?KEY_TO_IDX(Key),Incr} ).

counters( Tbl, Statement ) ->
    [#stmt{ first = First,
            current = Current,
            last = Last,
            is_final = IsFinal }] = ets:lookup( Tbl, Statement ),
    % turn it into a counters record
    #erbdrv_stmt_counters{ first = First, current = Current, last = Last, is_final = IsFinal }.

set_cols( Tbl, Statement, Cols ) ->
    set( Tbl, Statement, columns, Cols ).
get_cols( Tbl, Statement ) ->
    lookup( Tbl, Statement, columns, undefined ).


%TODO: remove old rows
add_rows( Tbl, Statement, Data ) ->
    Counters = counters( Tbl, Statement ),
    add_rows(Tbl,Statement,Data,Counters).

add_rows( Tbl, Statement, Rows,
          #erbdrv_stmt_counters{last=Last} = Counters ) ->
    NewLast = Last + length(Rows),
    RowRecs = lists:map( fun({Idx,Row}) ->
                                 {Idx,Row}
                         end, lists:zip( lists:seq(Last+1,NewLast), Rows ) ),
    ets:insert( Statement, RowRecs ),
    set( Tbl, Statement, last, NewLast ),
    Counters1 = Counters#erbdrv_stmt_counters{last=NewLast},
    {ok,Counters1,Tbl}.
