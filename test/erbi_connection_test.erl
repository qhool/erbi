%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(erbi_connection_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

prepare1_test() ->
    { ok, Conn } = erbi:connect( { erbi, dummy,
                                   [{connect,success},
                                    {prepare,failure}] }, undefined, undefined ),
    { error, _ } = erbi_connection:prepare( Conn, "anything" ).
prepare2_test() ->
    { ok, Conn } = erbi:connect( { erbi, dummy,
                                   [{connect,success},
                                    {prepare,success}] }, undefined, undefined ),
    { ok, _ } = erbi_connection:prepare( Conn, "anything" ).

do_test() ->
    { ok, Conn } = erbi:connect( { erbi, dummy,
                                   [{connect,success},
                                    {do,success}] }, undefined, undefined ),
    { ok, _ } = erbi_connection:do( Conn, "whatever", ["A","n"] ).

selectall_test_() ->
    PropVariants =
        lists:map(
          fun(N) ->
                  lists:flatten(
                    erbi_test_util:bitmap_sublist(
                      [[{prepare,declined},{cols_on_prepare,false}],{bind,declined},
                       [{prepare,success},{cols_on_prepare,true},{rows_on_execute,false},{simulate_fetch,true}],
                       must_preparse,must_bind,multiple_bind],N) )
            end, lists:seq(0,63) ),
    lists:flatmap(
      fun(P) ->
              [ ?_test( ?debugFmt("~n----------~nTesting with: ~n~p~n------------~n~n",[P]) ),
                ?_assert( erbi_test_util:equal_rows_dict(test,get_select(P,test,selectall_dict)) ),
                ?_assert( erbi_test_util:equal_rows_list(test,get_select(P,test,selectall_list)) ),
                ?_assert( erbi_test_util:equal_rows_proplist(test,get_select(P,test,selectall_proplist)) )
              ]
      end, PropVariants ).

selectrow_test_() ->
    [ ?_assert( erbi_test_util:equal_rows_list(test,[get_select(test,selectrow_list)],1) ),
      ?_assert( erbi_test_util:equal_rows_proplist(test,[get_select(test,selectrow_proplist)],1) ),
      ?_assert( erbi_test_util:equal_rows_dict(test,[get_select(test,selectrow_dict)],1) )
    ].

transaction_test() ->
    { ok, Conn } = erbi:connect( { erbi, dummy,
                                   [{default,success}] } ),
    ok = erbi_connection:begin_work(Conn),
    ok = erbi_connection:rollback(Conn),
    ok = erbi_connection:begin_work(Conn),
    ok = erbi_connection:commit(Conn).

disconnect_test() ->
    { ok, Conn } = erbi:connect( { erbi, dummy,
                                   [{default,success}] } ),
    ok = erbi_connection:disconnect(Conn),
    ?assertExit({normal,_}, erbi_connection:selectall_list(Conn,"foo")).

get_select(Dataset,Func) ->
    get_select([],Dataset,Func).
get_select(Props,Dataset,Func) ->
    Conn = fetch_conn(Props,Dataset),
    { ok, Result } = erbi_connection:Func(Conn,"whatever"),
    Result.

fetch_conn(Dataset) ->
    fetch_conn([],Dataset).
fetch_conn(Props,Dataset) ->
    {Cols,Rows} = erbi_test_util:dataset(Dataset),
    { ok, Conn } = erbi:connect( { erbi, dummy,
                                   Props ++
                                       [{connect,success},
                                        {prepare,success},
                                        {queries,
                                         [{".*",Cols,Rows}
                                              ]
                                        }
                                       ]
                                 } ),
    Conn.
