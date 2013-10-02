%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(erbi_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

basic_test_() ->
    [ ?_test( { ok, _ } = erbi:connect( "erbi:dummy:connect=success", undefined, undefined ) ),
      ?_test( { error, _ } = erbi:connect( "erbi:dummy:connect=failure", undefined, undefined ) )
    ].

parse_datasource_test_() ->
    [ ?_assertEqual( #erbi{ driver = test, properties = [ {database,"foo"},{host, "bar"} ] },
                     erbi:parse_data_source( "erbi:test:database=foo;host=bar" ) ),
      ?_assertEqual( #erbi{ driver = test, args = [ "generic arg", "another_arg", "7" ] },
                     erbi:parse_data_source( "erbi:test::\"generic arg\":another_arg:7" ) ),
      ?_assertEqual( #erbi{ driver = 'foo:bar', properties = [ {database,"my database name"} ], args = ["","a"] },
                     erbi:parse_data_source( "erbi:foo\\:bar:database=\"my database name\"::a" ) ),
      ?_test( {error,_} = erbi:parse_data_source( "test:database=\"foo bar\"" ) ),
      ?_test( {error,_} = erbi:parse_data_source( "erbi::" ) ),
      ?_test( {error,_} = erbi:parse_data_source( "erbi:test:database=\"foo\"host=bat" ) ),
      ?_test( {error,_} = erbi:parse_data_source( "erbi:test:database=foohost=bat" ) )
    ].
