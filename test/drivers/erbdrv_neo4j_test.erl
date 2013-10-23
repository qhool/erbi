%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(erbdrv_neo4j_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").
-include("erbi_driver.hrl").

params_test_() ->
    [ { "normalize 1", 
        ?_assertEqual( #erbi{driver=neo4j,
                             properties=[{endpoint,transaction},
                                         {host,"localhost"},
                                         {port,7474},
                                         {scheme,http}],
                             args=undefined},
                       ?debugVal(erbi:normalize_data_source( "erbi:neo4j:" )) ) },
      { "normalize 2", 
        ?_assertEqual(  #erbi{driver=neo4j,
                              properties=[{endpoint,cypher},
                                          {host,"www.xxx"},
                                          {port,1234},
                                          {scheme,https}],
                              args=undefined},
                        ?debugVal
                           (erbi:normalize_data_source( 
                              "erbi:neo4j:scheme=https;endpoint=cypher;host=www.xxx;port=1234" 
                             ) ) ) },
      { "bad datasource 1", 
        ?_assertEqual( {error,{invalid_datasource,{unsupported_scheme,ftp}}},
                       ?debugVal(erbi:normalize_data_source( "erbi:neo4j:scheme=ftp" ) ) ) },
      { "bad datasource 2",
        ?_assertEqual( {error,{invalid_datasource,{unknown_endpoint,foo}}},
                       ?debugVal(erbi:normalize_data_source( "erbi:neo4j:endpoint=foo" ) ) ) }
    ].

main_test_() ->
    {setup,
     fun() ->
             Config = erbi_test_util:config(neo4j),
             TransDS = proplists:get_all_values(transaction_datasource,Config),
             CypherDS = proplists:get_all_values(cypher_datasource,Config),
             {Config,TransDS,CypherDS}
     end,
     fun({Config,TransDS,CypherDS}) ->
             DoCommitted = proplists:get_value(nontransactional_write,Config,false),
             DoUncommitted = proplists:get_value(transactional_write,Config,false),
             lists:flatmap(
               fun({Type,DS}) ->
                       [ { "connect to " ++ DS, 
                           ?_test( { ok, _ } = ?debugVal(erbi:connect( DS )) ) },
                         {foreach,
                          fun() ->
                                  {ok,Conn} = erbi:connect( DS ),
                                  {Config,Type,Conn,DS}
                          end,
                          fun({_,_,Conn,_}) ->
                                  erbi_connection:disconnect(Conn)
                          end,
                          [ fun mktests_read_only/1 ]
                          ++ case DoCommitted of
                                 true ->
                                     [fun mktests_non_transactional/1];
                                 false -> []
                             end
                          ++ case DoUncommitted of
                                 true ->
                                     [fun mktests_transactional/1];
                                 false ->
                                     []
                             end
                         }
                       ]
               end, lists:map(fun(X) -> {transaction,X} end, TransDS) ++
                   lists:map(fun(X) -> {cypher,X} end, CypherDS ) 
              )
     end   
    }.

mktests_read_only({_Config,_Type,Conn,_DS}) ->
    [ { "row ids query", fun() ->
                           {ok,Rows} = erbi_connection:selectall_list
                                         (Conn,"start n=node(*) return id(n) limit 5"),
                           lists:all(fun([R]) when is_integer(R) -> true;
                                        (_) -> false
                                     end, Rows )
                   end },
      { "queries with parameter",
        fun() ->
                lists:map(fun(N) ->
                                  Res = erbi_connection:selectrow_list
                                          (Conn,"start n=node(*) where id(n) = {id} return id(n)",[{id,N}]),
                                  ?assert( (Res =:= exhausted) orelse (Res =:= {ok,[N]}) )
                          end, lists:seq(0,2))
        end  }
    ].
                                
mktests_non_transactional({Config,Type,Conn,_DS}) ->
    { setup,
      fun() ->
              random:seed(now()),
              TestKey = gen_test_key(),
              {Config,Type,Conn,TestKey}
      end,
      fun({_Config,_Type,Conn,TestKey}) ->
              erbi_connection:do(Conn,"start n=node(*) where n.erbi_test! = {k} delete n",[{k,TestKey}]),
              erbi_connection:do(Conn,"start r=relationship(*) where r.erbi_test! = {k} delete r",[{k,TestKey}])
      end,
      fun({_Config,_Type,Conn,TestKey}) ->
              [ { "create node (non-transactional)",
                  ?_test( erbi_connection:do(Conn,"create (n:Foo {erbi_test:{key},val: 7})",[{key,TestKey}]) )
                },
                { "check node (non-transactional)",
                  ?_assertEqual
                     ( {ok,[7]},
                       ?debugVal( erbi_connection:selectrow_list
                                    (Conn,"start n=node(*) where n.erbi_test! = {key} return n.val",
                                     [{key,TestKey},{val,7}]) ) )
                     } ]
      end
    }.

mktests_transactional({_,cypher,_,_}) ->
    { "skip transactional tests for cypher endpoint", ?_test(ok) };
mktests_transactional({Config,transaction,Conn,DS}) ->
    { foreach,
      fun() ->
              ok = erbi_connection:begin_work(Conn),
              TestKey = gen_test_key(),
              {Config,Conn,TestKey}
      end,
      fun({_Config,Conn,_TestKey}) ->
              ok = erbi_connection:rollback(Conn)
      end,
      [ fun({_,Conn,TestKey}) ->
                [ { "create node (transactional)",
                    ?_test( erbi_connection:do(Conn,"create (n:Bar {erbi_test:{key},val: 7})",[{key,TestKey}]) )
                  },
                  { "check node (transactional)",
                    ?_assertEqual
                       ( {ok,[7]},
                         ?debugVal( erbi_connection:selectrow_list
                                      (Conn,"start n=node(*) where n.erbi_test! = {key} return n.val",
                                       [{key,TestKey}]) ) )
                  },
                  { "look outside transaction",
                    fun() ->
                            {ok, Conn2} = erbi:connect(DS),
                            ?assertEqual( exhausted,
                                          ?debugVal( erbi_connection:selectrow_list
                                                       (Conn2,"start n=node(*) where n.erbi_test! = {key} return n.val",
                                                        [{key,TestKey}]) ) )
                    end
                  }
                ]
        end
      ] }.

gen_test_key() ->
    random:seed(now()),
    string:join(
      lists:map(
        fun erlang:integer_to_list/1,
        lists:map(
          fun random:uniform/1, lists:duplicate(5,1000))),".").
