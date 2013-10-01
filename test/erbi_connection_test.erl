%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(erbi_connection_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

prepare1_test_() ->
    { ok, Conn } = erbi:connect( { erbi, dummy, 
                                   [{connect,success},
                                    {prepare,failure}] }, undefined, undefined ),
    { error, _ } = Conn:prepare( "anything" ).
prepare2_test() ->
    { ok, Conn } = erbi:connect( { erbi, dummy, 
                                   [{connect,success},
                                    {prepare,success}] }, undefined, undefined ),
    { ok, _ } = Conn:prepare( "anything" ).

selectall_test_() ->
    [ ?_assert( erbi_test_util:equal_rows_dict(test,get_select(test,selectall_dict)) ),
      ?_assert( erbi_test_util:equal_rows_list(test,get_select(test,selectall_list)) ),
      ?_assert( erbi_test_util:equal_rows_proplist(test,get_select(test,selectall_proplist)) )
    ].

selectrow_test_() ->
    [ ?_assert( erbi_test_util:equal_rows_list(test,[get_select(test,selectrow_list)],1) ),
      ?_assert( erbi_test_util:equal_rows_proplist(test,[get_select(test,selectrow_proplist)],1) ),
      ?_assert( erbi_test_util:equal_rows_dict(test,[get_select(test,selectrow_dict)],1) )
    ].

transaction_test() ->
    Conn = { ok, Conn } = erbi:connect( { erbi, dummy,
                                          [{default,success}] } ),
    ok = Conn:begin_work(),
    ok = Conn:rollback(),
    ok = Conn:begin_work(),
    ok = Conn:commit().

disconnect_test() ->
    Conn = { ok, Conn } = erbi:connect( { erbi, dummy,
                                          [{default,success}] } ),
    ok = Conn:disconnect(),
    {error,_} = Conn:selectall_list("foo").

get_select(Dataset,Func) ->
    Conn = fetch_conn(Dataset),
    { ok, Result } = Conn:Func("whatever"),
    Result.

fetch_conn(Dataset) ->
    {Cols,Rows} = erbi_test_util:dataset(Dataset),
    { ok, Conn } = erbi:connect( { erbi, dummy,
                                   [{connect,success},
                                    {prepare,success},
                                    {fetch,[{columns,Cols},
                                            {rows,Rows}]}]
                                 } ),
    Conn.
