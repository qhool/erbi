%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(erbi_statement_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

bind_params_test() ->
    Stmt = ?debugVal(get_stmt(test,[])),
    ?debugVal(erbi_statement:bind_params(Stmt,["one","two","three"])),
    {ok,["one","two","three"]} = erbi_statement:driver_call(Stmt,params,[]).

finish_test() ->
    Stmt = get_stmt(test,[]),
    ok = Stmt:finish().

fetchall_test_() ->
    [ ?_assert( erbi_test_util:equal_rows_list(test,exec_on_stmt(test,fetchall_list)) ),
      ?_assert( erbi_test_util:equal_rows_proplist(test,exec_on_stmt(test,fetchall_proplist)) ),
      ?_assert( erbi_test_util:equal_rows_dict(test,exec_on_stmt(test,fetchall_dict)) )
    ].

fetchrow_test_() ->
    [ fetchrow_meta(test,equal_rows_list,fetchrow_list),
      fetchrow_meta(test,equal_rows_proplist,fetchrow_proplist),
      fetchrow_meta(test,equal_rows_dict,fetchrow_dict)
    ].

fetchrow_meta(Dataset,EqualFunc,FetchFunc) ->
    Rowgetter = fun(Self,Stmt,Out) ->
                        case erbi_statement:FetchFunc(Stmt) of
                            exhausted -> lists:reverse(Out);
                            { ok, Row } -> Self(Self,Stmt,[Row|Out])
                        end
                end,
    fun() ->
            Stmt = get_stmt(Dataset,[]),
            {ok,_} = erbi_statement:execute(Stmt),
            Rows = Rowgetter(Rowgetter,Stmt,[]),
            ?_assert( erbi_test_util:EqualFunc(Dataset,Rows) )
    end.

exec_on_stmt(Dataset,Fun) ->
    Stmt = ?debugVal(get_stmt(Dataset,[])),
    {ok,_} = ?debugVal(erbi_statement:execute(Stmt)),
    {ok,Res} = erbi_statement:Fun(Stmt),
    Res.

get_stmt(Dataset,Props) ->
    {Cols,Rows} = erbi_test_util:dataset(Dataset),
    { ok, Conn } = erbi:connect( #erbi{ driver = dummy,
                                        properties = Props++ 
                                            [{queries,
                                              [{".*",Cols,Rows}
                                              ]
                                             }
                                            ]
                                      } ),
    {ok,Stmt} = erbi_connection:prepare(Conn,"whatever"),
    Stmt.

