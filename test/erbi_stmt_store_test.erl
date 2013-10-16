%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(erbi_stmt_store_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").
-include("erbi_driver.hrl").

basic_test_() ->
    {foreach,
     fun mk_store/0,
     fun cleanup_store/1,
     [ fun(Store) ->
               [ ?_test( init_with_handle(Store) ),
                 ?_test( init_no_handle(Store) ),
                 ?_assert( 2 =< erbi_stmt_store:all_handles(Store) ) ]
       end,
       fun(Store) ->
               [ ?_test( add_rows_and_reset(Store) ),
                 ?_test( init_with_handle(Store) ),
                 ?_test( keys_vals(Store) )
               ]
       end,
       fun(Store) ->
               [ ?_test( reset_all(Store) ) ]
       end
                 
     ]
    }.

init_with_handle(Store) ->
    Stmt = erbi_stmt_store:add_statement(Store,fake_handle),
    ?debugFmt("stmt: ~p~n",[Stmt]),
    fake_handle = check_init_state(Store,Stmt),
    ok.
init_no_handle(Store) ->
    Stmt = erbi_stmt_store:add_statement(Store,undefined),
    undefined = check_init_state(Store,Stmt),
    ok.
add_rows_and_reset(Store) ->
    Stmt = ?debugVal(erbi_stmt_store:add_statement(Store,fake)),
    fake = check_init_state(Store,Stmt),
    {ok,#erbdrv_stmt_counters{last=0},Store} =
        ?debugVal(erbi_stmt_store:add_rows( Store, Stmt, [[0,0,0]] )),
    [0,0,0] = ?debugVal(erbi_stmt_store:get(Store,Stmt,0)),
    1 = Curr = ?debugVal(erbi_stmt_store:incr( Store, Stmt, current, 1 )),
    {ok,#erbdrv_stmt_counters{last=3},Store} = 
        ?debugVal(erbi_stmt_store:add_rows( Store, Stmt, [[1,2,3],[4,5,6],[7,8,9]] )),
    [1,2,3] = ?debugVal(erbi_stmt_store:get(Store,Stmt,Curr)),
    [4,5,6] = ?debugVal(erbi_stmt_store:get(Store,Stmt,Curr+1)),
    [7,8,9] = ?debugVal(erbi_stmt_store:get(Store,Stmt,Curr+2)),
    4 = ?debugVal(erbi_stmt_store:incr( Store, Stmt, current, 3 )),
    fake = ?debugVal(erbi_stmt_store:reset_statement( Store,Stmt )),
    fake = check_init_state(Store,Stmt).
keys_vals(Store) ->
    Stmt = ?debugVal(erbi_stmt_store:add_statement(Store,foo)),
    %% test an arbitrary key
    default = erbi_stmt_store:lookup(Store,Stmt,blarg,default),
    ?debugVal(erbi_stmt_store:set(Store,Stmt,blarg,{whatever,thing})),
    {whatever,thing} = ?debugVal(erbi_stmt_store:lookup(Store,Stmt,blarg,default)),
    {whatever,thing} = ?debugVal(erbi_stmt_store:get(Store,Stmt,blarg)),
    %% cols
    [] = ?debugVal(erbi_stmt_store:get_cols(Store,Stmt)),
    ?debugVal(erbi_stmt_store:set_cols(Store,Stmt,[one,two,three])),
    [one,two,three] = ?debugVal(erbi_stmt_store:get_cols(Store,Stmt)).
reset_all(Store) ->
    Stmt1 = ?debugVal(erbi_stmt_store:add_statement(Store,fake_one)),
    Stmt2 = ?debugVal(erbi_stmt_store:add_statement(Store,fake_two)),
    ?assert( Stmt1 =/= Stmt2 ),
    {ok,#erbdrv_stmt_counters{last=1},Store} = 
        ?debugVal(erbi_stmt_store:add_rows( Store, Stmt1, [[1,2,3],[4,5,6]] )),
    {ok,#erbdrv_stmt_counters{last=0},Store} =
        ?debugVal(erbi_stmt_store:add_rows( Store, Stmt2, [[0,0]] )),
    ?debugVal(erbi_stmt_store:reset_all(Store)),
    fake_one = check_init_state(Store,Stmt1),
    fake_two = check_init_state(Store,Stmt2).
    
                 
check_init_state(Store,Stmt) ->
    ?debugFmt("Checking initial state of statement ~p~n",[Stmt]),
    0 = First = ?debugVal(erbi_stmt_store:get( Store, Stmt, first )),
    0 = Curr = ?debugVal(erbi_stmt_store:get( Store, Stmt, current )),
    ?assert( 0 > ?debugVal(erbi_stmt_store:get( Store, Stmt, last )) ),
    Handle = ?debugVal(erbi_stmt_store:get( Store, Stmt, handle )),
    WantFinal = Handle =:= undefined,
    WantFinal = ?debugVal(erbi_stmt_store:get( Store, Stmt, is_final )),
    #erbdrv_stmt_counters{first=0,current=0,is_final=WantFinal} = ?debugVal(erbi_stmt_store:counters(Store,Stmt)),
    ?assertError(badarg,erbi_stmt_store:get(Store,Stmt,0)),
    ?assertError(badarg,erbi_stmt_store:get(Store,Stmt,1)),
    ?assertError(badarg,erbi_stmt_store:get(Store,Stmt,2)),
    Handle.

%% setup/teardown:
mk_store() ->
    Store = erbi_stmt_store:init_unprotected(),
    ?debugFmt("Created stmt_store: ~p~n",[Store]),
    Store.
cleanup_store(Store) ->
    ets:delete(Store).
