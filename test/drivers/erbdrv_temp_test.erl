%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbdrv_temp_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

connect_temp_test_()->
    {setup,
     fun()->
             {ok,Config}=erbi_test_util:config(),
             Datasource= "erbi:temp:base_driver=epgsql;data_dir="++
                 temp_opt(data_dir,Config)++
                 ";init_files="++temp_opt(schema_files,Config),
             erbdrv_temp:start(Datasource),
             Datasource
     end,
     fun(Datasource)->
            %% erbdrv_temp:stop(Datasource)
             ok
     end,
     fun(Datasource)->
             {ok,C}=?debugVal(erbi:connect(Datasource,"","")),
             [?_test({ok,_}=?debugVal(erbi_connection:selectall_proplist(C,"select * from test_temp")))]
     end}.




temp_opt(Atom,PList)->
    MockPList=proplists:get_value(temp,PList),
    case proplists:get_value(Atom,MockPList) of
        undefined ->
            "";
        Value ->
            Value
    end.


