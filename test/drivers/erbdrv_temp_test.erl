%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbdrv_temp_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

connect_temp_epgsql_test_()->
    {setup,
     fun()->
             {ok,Config}=erbi_test_util:config(),
             Datasource1= "erbi:temp:base_driver=epgsql;data_dir="++
                 temp_opt(data_dir,Config)++
                 ";init_files="++temp_opt(schema_files,Config),
             Datasource2=Datasource1++";bin_dir=/Library/PostgreSQL/9.2/bin/",
             ok=erbdrv_temp:start(Datasource1),
             ok=erbdrv_temp:start(Datasource2),
             {Datasource1,Datasource2}
     end,
     fun({Datasource1,Datasource2})->
             ok=erbdrv_temp:stop(Datasource1),
             ok=erbdrv_temp:stop(Datasource2)

     end,
     fun({Datasource1,Datasource2})->
             [?_test({ok,_}=?debugVal(erbi:connect(Datasource1,"",""))),
              ?_test({ok,_}=?debugVal(erbi:connect(Datasource2,"","")))]
     end}.


driver_calls_temp_epgsql_test_()->
    {setup,
     fun()->
             {ok,Config}=erbi_test_util:config(),
             Datasource= "erbi:temp:base_driver=epgsql;data_dir="++
                 temp_opt(data_dir,Config)++
                 ";init_files="++temp_opt(schema_files,Config),

             ok=erbdrv_temp:start(Datasource),
             {ok,Conn}=erbi:connect(Datasource,"",""),
             {Datasource,Conn}
     end,
     fun({Datasource,_Conn})->
             ok=erbdrv_temp:stop(Datasource)
     end,
     fun({_Datasource,Conn})->
             [%initialized in scripts?
              ?_assertEqual({ok,[[{"id",0},{"name","Unknown"}],
                                 [{"id",1},{"name","This is a name"}]]},
                            ?debugVal(erbi_connection:selectall_proplist(Conn,"select * from test_temp"))),
             % Checkings if calls are made to base driver corrctly,
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


