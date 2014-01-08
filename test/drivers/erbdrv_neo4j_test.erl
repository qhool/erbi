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
     fun(_) ->
             ok
     end,
     fun({Config,TransDS,CypherDS}) ->
             DoCommitted = proplists:get_value(nontransactional_write,Config,false),
             DoUncommitted = proplists:get_value(transactional_write,Config,false),
             lists:map(
               fun({Type,DS}) ->
                       {setup,
                        fun() ->
                                ?debugMsg("Starting Neo4j"),
                                ok = erbi_test_util:start_db_test(DS)
                        end,
                        fun(_) ->
                                ?debugMsg("Stopping Neo4j"),
                                ok = erbi_test_util:stop_db_test(DS)
                        end,
                        [ { "connect to " ++ DS,
                                    fun()->
                                            ?debugMsg("Connect test"),
                                            ?_test( { ok, _ } = ?debugVal(erbi:connect( DS )) )
                                      end},
                                    {foreach,
                                     fun() ->
                                             ?debugMsg("Foreach SETUP"),
                                             {ok,Conn} = erbi:connect( DS ),
                                             {Config,Type,Conn,DS}
                                     end,
                                     fun({_,_,Conn,_}) ->
                                             ?debugMsg("Foreach DISCONN"),
                                             erbi_connection:disconnect(Conn)
                                     end,
                                     [ fun mktests_read_only/1,
                                       fun mktests_errors/1 ]
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
                        
                       }
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
mktests_errors({_Config,Type,Conn,_DS}) ->
    TypeStr = " (" ++ atom_to_list(Type) ++ ")",
    [ { "syntax err" ++ TypeStr,
        ?_test( {error,{syntax_error,_}} =
                    ?debugVal(erbi_connection:do(Conn,"create (n:foo {")) )
      },
      { "missing param" ++ TypeStr,
        fun() -> 
                case ?debugVal(erbi_connection:selectrow_list(
                                 Conn,"start n=node(*) where id(n)={x} return n",[])) of
                    %% 2.0.0 neo4j doesn't give missing_paramter error
                    exhausted ->
                        ok;
                    {error,{missing_parameter,_}} ->
                        ok;
                    Badness ->
                        ok = {should_not_get,Badness}
                end
        end
      },
      { "negone node" ++ TypeStr,
        fun() ->
                {error,{Err,_}} = ?debugVal(erbi_connection:selectrow_list(
                                Conn,"start n=node(-1) return n")),
                ?assert( (Err =:= unknown_object) or (Err =:= syntax_error) )
        end
      }
    ].
                                
mktests_non_transactional({Config,Type,Conn,_DS}) ->
    { setup,
      fun() ->
              random:seed(now()),
              TestKey = gen_test_key(),
              {Config,Type,Conn,TestKey}
      end,
      fun({_Config,_Type,C,TestKey}) ->
              erbi_connection:do(C,"start n=node(*) where n.erbi_test = {k} delete n",[{k,TestKey}]),
              erbi_connection:do(C,"start r=relationship(*) where r.erbi_test = {k} delete r",[{k,TestKey}])
      end,
      fun({Cfg,_Type,C,TestKey}) ->
              basic_crud("non-transactional",{Cfg,C,TestKey})
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
      fun({_Cfg,C,_TestKey}) ->
              ok = erbi_connection:rollback(C)
      end,
      [ fun({_Cfg,_C,TestKey}=Tparms) ->
                basic_crud("transactional",Tparms) 
                ++ [ { "look outside transaction",
                    fun() ->
                            {ok, Conn2} = erbi:connect(DS),
                            ?assertEqual( exhausted,
                                          ?debugVal( erbi_connection:selectrow_list
                                                       (Conn2,"start n=node(*) where n.erbi_test = {key} return n.val",
                                                        [{key,TestKey}]) ) )
                    end
                  }
                ]
        end
      ] }.

basic_crud(Type,{_Config,Conn,TestKey}) ->
    TypeStr = " (" ++ Type ++ ")",
    [ { "create node" ++ TypeStr,
        ?_assertEqual
           ( {ok,0},
             ?debugVal(erbi_connection:do(Conn,"create (n:Bar {erbi_test:{key},val: 7})",[{key,TestKey}]) ) )
      },
      { "check node" ++ TypeStr,
        ?_assertEqual
           ( {ok,[{<<"n.erbi_test">>,list_to_binary(TestKey)},{<<"n.val">>,7}]},
             ?debugVal( erbi_connection:selectrow_proplist
                          (Conn,"start n=node(*) where n.erbi_test = {key} and n.val = {val} return n.erbi_test, n.val",
                           [{key,TestKey},{val,7}]) ) )
      },
      { "null property" ++ TypeStr,
        ?_assertEqual
           ( {ok,[null]},
             ?debugVal( erbi_connection:selectrow_list
                          (Conn,"start n=node(*) where n.erbi_test = {key} return n.sovnsdojn limit 1",
                           [{key,TestKey}]) ) ) 
      },
      { "create node part 2" ++ TypeStr,
        ?_assertEqual
           ( {ok,0},
             ?debugVal(erbi_connection:do(Conn,"create (n:Bar {erbi_test:{key},val: 101})",[{key,TestKey}]) ) )
      },
      { "check pt 2" ++ TypeStr,
        ?_assertEqual
           ( {ok,[[7],[101]]},
             ?debugVal( erbi_connection:selectall_list
                          (Conn,"start n=node(*) where n.erbi_test = {key} return n.val order by n.val",
                           [{key,TestKey}]) ) )
      },
      { "delete node"++TypeStr,
        ?_assertEqual
           ( {ok,0},
             ?debugVal(erbi_connection:do
                         (Conn,"start n=node(*) where n.erbi_test = {key} and n.val = {val} delete n",
                          [{key,TestKey},{val,101}])) )
      },
      { "update node"++TypeStr,
        ?_assertEqual
           ( {ok,[9]},
             ?debugVal( erbi_connection:selectrow_list
                          (Conn,"start n=node(*) where n.erbi_test = {key} and n.val= {val_old} " ++ 
                               " set n.val = {val_new} return n.val",
                           [{key,TestKey},{val_old,7},{val_new,9}]) ) )
      },
      { "check pt 3"++TypeStr,
        ?_assertEqual
           ( {ok,[[9]]},
             ?debugVal( erbi_connection:selectall_list
                          (Conn,"start n=node(*) where n.erbi_test = {key} return n.val",
                           [{key,TestKey}]) ) )
      }
    ].



gen_test_key() ->
    random:seed(now()),
    string:join(
      lists:map(
        fun erlang:integer_to_list/1,
        lists:map(
          fun random:uniform/1, lists:duplicate(5,1000))),".").

connect_temp_neo4j_test_()->
    {setup,
     fun()->
             Config=erbi_test_util:config(neo4j_temp),
             Datasource1= "erbi:temp:base_driver=neo4j;bin_dir=/opt/neo4j/bin/;data_dir="++
                 proplists:get_value(data_dir,Config,""),
             Datasource2=Datasource1++"2"++
                 ";init_files="++proplists:get_value(neo4j_init_files,Config,""),
             ok=erbi_temp_db:start(Datasource1),
             ok=erbi_temp_db:start(Datasource2),
             {Datasource1,Datasource2}
     end,
     fun({Datasource1,Datasource2})->
             ok=erbi_temp_db:stop(Datasource1),
             ok=erbi_temp_db:stop(Datasource2)

     end,
     fun({Datasource1,Datasource2})->
             [?_test({ok,_}=?debugVal(erbi:connect(Datasource1,undefined,undefined))),
              ?_test({ok,_}=?debugVal(erbi:connect(Datasource2,"","")))]

     end}.

driver_calls_temp_neo4j_test_()->
        {setup,
     fun()->
             Config=erbi_test_util:config(neo4j_temp),
             Datasource= "erbi:temp:base_driver=neo4j;bin_dir=/opt/neo4j/bin/;data_dir="++
                 proplists:get_value(data_dir,Config,"")++
                 "3;init_files="++proplists:get_value(neo4j_init_files,Config,"")++
                 ";endpoint=cypher",
             ok=erbi_temp_db:start(Datasource),
             {ok,Connection}=?debugVal(erbi:connect(Datasource,undefined,undefined)),
             {Datasource,Connection}
     end,
     fun({Datasource,_Connection})->
             ok=erbi_temp_db:stop(Datasource)
     end,
     fun({_Datasource,Conn})->
             [%initialized with script?
              ?_assertEqual( {ok,[[0],[1],[2]]},
                        ?debugVal( erbi_connection:selectall_list(Conn,
                                                       "start n=node(*) return n.val"))),
             % Checking if calls are made to base driver corrctly,
             % Base driver smarts tested in driver test module
             ?_assertEqual({ok,0},
                           ?debugVal(erbi_connection:do(Conn,"create (n {name:\"node3\",val:3})"))),
             ?_assertEqual({ok,[[0],
                                [1],
                                [2],
                                [3]
                               ]},
                           ?debugVal(erbi_connection:selectall_list(Conn,"start n=node(*) return n.val order by n.val"))),
             ?_assertEqual({error,driver_declined} , erbi_connection:begin_work(Conn)),
             ?_assertEqual({error,driver_declined} , erbi_connection:rollback(Conn)),
             ?_assertEqual({error,driver_declined} , erbi_connection:begin_work(Conn,"savepoint")),
             ?_assertEqual({error,driver_declined} , erbi_connection:rollback(Conn,"savepoint")),
             ?_assertEqual(ok , ?debugVal(erbi_connection:disconnect(Conn)))
              
              ]
     end}.

temp_neo4j_autoclean_on_start_test_()->
    [{setup,
     fun()->
             Config = erbi_test_util:config(neo4j_temp),
             Datasource = "erbi:temp:base_driver=neo4j;data_dir="++
                 proplists:get_value(data_dir,Config,""),
             erbi_temp_db:start(Datasource),
             Datasource
     end,
     fun(Datasource)->
        erbi_temp_db:stop(Datasource)
    end,
     fun(Datasource)->
             [{timeout, 500,?_assertEqual(ok, erbi_temp_db:start(Datasource))}, % Cleans previous instance and starts another
              ?_assertEqual(true, filelib:is_dir(erbi_temp_db:data_dir(Datasource)))]
     end},
     {setup,
     fun()->
             Config = erbi_test_util:config(neo4j_temp),
             Datasource = "erbi:temp:base_driver=neo4j;auto_clean=false;
                data_dir="++
                 proplists:get_value(data_dir,Config,""),
              erbi_temp_db:start(Datasource),
             Datasource
     end,
     fun(Datasource)->
        erbi_temp_db:stop(Datasource)
    end,
     fun(Datasource)->
             [{timeout, 500,?_assertException(error,{badmatch,{error,db_not_started}}, erbi_temp_db:start(Datasource))}, % Cleans previous instance and starts another
              ?_assertEqual(true, filelib:is_dir(erbi_temp_db:data_dir(Datasource)))]
     end}].

temp_neo4j_kill_instance_test_()->
  {setup,
     fun()->
             Config = erbi_test_util:config(neo4j_temp),
             DataDir = proplists:get_value(data_dir,Config,""),
             Datasource = "erbi:temp:base_driver=neo4j;data_dir="++
                 DataDir,
             erbi_temp_db:start(Datasource),
             erbi_temp_db_helpers:del_dir(DataDir++"/bin/"), 
             Datasource
     end,
     fun(Datasource)->
        erbi_temp_db:stop(Datasource)
    end,
    fun(Datasource)->
      [{timeout, 500,?_assertEqual(ok, erbi_temp_db:stop(Datasource))},
       {timeout, 500,?_assertEqual(ok, erbi_temp_db:start(Datasource))}]
     end}.


