    %%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(erbi_pool_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").
-include("erbi_driver.hrl").

-export([
    start/0,
    stop/1,
    check_pools_operations/1
]).

init_test_() ->
    {setup,
     fun start/0,               % setup function
     fun stop/1,                % teardown function
     fun check_pools_operations/1}.  % instantiator

start() ->
%%     dbg:tracer(),
%%     dbg:p(all, c),
%%     dbg:tpl(erbi_sup, x),
    {ok, SPid} = erbi_sup:start_link([
        {"erbi:dummy:pool_name=test0;pool_size=2;pool_max_overflow=0"},
        {"erbi:dummy:pool_name=test1;pool_size=1;pool_max_overflow=1"}]),
    SPid.

stop(_SPid) ->
    ok.

check_pools_operations(_SPid) ->
    [
        [check_initialized_names()],
        [check_no_overflow_pool_state()],
        [check_overflow_pool_state()]
    ].

check_initialized_names() ->
    [?_assert(true =:= lists:all(fun (test0) -> true;
                                    (test1) -> true;
                                    (_) -> false end, erbi_pool:list_pool_names()))].

check_no_overflow_pool_state() ->
    S0 = erbi_pool:status(test0),
    {ok, C0} = erbi_pool:checkout(test0),
    {ok,  C1} = erbi_pool:checkout(test0),
    S1 = erbi_pool:status(test0),
    E = erbi_pool:checkout(test0),
    erbi_pool:checkin(C0),
    erbi_pool:checkin(C1),
    S2 = erbi_pool:status(test0),
    [
     ?_assertMatch({error, no_available_connections}, E),
     ?_assertMatch({ready,2,0,0},S0),
     ?_assertMatch({full,0,0,2},S1),
     ?_assertMatch({ready,2,0,0},S2)
    ].

check_overflow_pool_state() ->
    S0 = erbi_pool:status(test1),
    {ok, C0} = erbi_pool:checkout(test1),
    {ok, C1} = erbi_pool:checkout(test1),
    S1 = erbi_pool:status(test1),
    E = erbi_pool:checkout(test1),
    erbi_pool:checkin(C0),
    erbi_pool:checkin(C1),
    S2 = erbi_pool:status(test1),
    [
     ?_assertMatch({ready,1,0,0},S0),
     ?_assertMatch({full,0,1,2},S1),
     ?_assertMatch({error, no_available_connections}, E),
     ?_assertMatch({ready,1,0,0},S2)
    ].

