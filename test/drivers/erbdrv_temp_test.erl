%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbdrv_temp_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

connect_temp_neo4j_test_()->
    {setup,
     fun()->
             {ok,Config}=erbi_test_util:config(),
             Datasource1= "erbi:temp:base_driver=neo4j;bin_dir=/opt/neo4j/bin/;data_dir="++
                 temp_opt(data_dir,Config),
             Datasource2=Datasource1++"2"++";init_files="++temp_opt(neo4j_init_files,Config),
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
             {ok,Config}=erbi_test_util:config(),
             Datasource= "erbi:temp:base_driver=neo4j;bin_dir=/opt/neo4j/bin/;data_dir="++
                 temp_opt(data_dir,Config)++
                 ";init_files="++temp_opt(neo4j_init_files,Config)++
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
    

connect_temp_epgsql_test_()->
    {setup,
     fun()->
             {ok,Config}=erbi_test_util:config(),
             Datasource1= "erbi:temp:base_driver=epgsql;data_dir="++
                 temp_opt(data_dir,Config)++
                 ";init_files="++temp_opt(sql_init_files,Config),
             Datasource2=Datasource1++";bin_dir=/casa/",
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


driver_calls_temp_epgsql_test_()->
    {setup,
     fun()->
             {ok,Config}=erbi_test_util:config(),
             Datasource= "erbi:temp:base_driver=epgsql;data_dir="++
                 temp_opt(data_dir,Config)++
                 ";init_files="++temp_opt(sql_init_files,Config),

             ok=erbi_temp_db:start(Datasource),
             {ok,Conn}=erbi:connect(Datasource,"",""),
             {Datasource,Conn}
     end,
     fun({Datasource,_Conn})->
             ok=erbi_temp_db:stop(Datasource)
     end,
     fun({_Datasource,Conn})->
             [%initialized in scripts?
              ?_assertEqual({ok,[[{"id",0},{"name","Unknown"}],
                                 [{"id",1},{"name","This is a name"}]]},
                            ?debugVal(erbi_connection:selectall_proplist(Conn,"select * from test_temp"))),
             % Checking if calls are made to base driver corrctly,
             % Base driver smarts tested in driver test module
             ?_assertEqual({ok,unknown},
                           ?debugVal(erbi_connection:do(Conn,"CREATE TABLE test_temp2 (id bigserial,name text)"))),
             ?_assertEqual({ok,1},
                           ?debugVal(erbi_connection:do(Conn,"INSERT INTO test_temp2 (id, name) VALUES ($1, $2)",[2,"AnotherName"]))),
             ?_assertEqual({ok,[[{"id",2},
                                 {"name","AnotherName"}]]},
                           ?debugVal(erbi_connection:selectall_proplist(Conn,"select * from test_temp2"))),
             ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn))),
             ?_assertEqual(ok , ?debugVal(erbi_connection:rollback(Conn))),
             ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn,"savepoint"))),
             ?_assertEqual(ok , ?debugVal(erbi_connection:rollback(Conn,"savepoint"))),
             ?_assertEqual(ok , ?debugVal(erbi_connection:disconnect(Conn)))
                 ]
     end}.




temp_opt(Atom,PList)->
    MockPList=proplists:get_value(temp,PList),
    case proplists:get_value(Atom,MockPList) of
        undefined ->
            "";
        Value ->
            Value
    end.

