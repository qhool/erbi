%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbdrv_temp_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

start_temp_test_()->
    {setup,
     fun()->
             Config = erbi_test_util:config(dummy_temp),
             "erbi:temp:base_driver=dummy;data_dir="++
                 proplists:get_value(data_dir,Config,"")
     end,
     fun(Datasource)->
             [?_assertEqual(ok, erbi_temp_db:start(Datasource)),
              ?_assertEqual(true, filelib:is_dir(get_data_dir(Datasource))),
              ?_assertEqual(ok, erbi_temp_db:stop(Datasource)),
              ?_assertEqual(false, filelib:is_dir(get_data_dir(Datasource)))]
     end}.

datasource_parameters_test()->
    {setup,
     fun()->
             Config = erbi_test_util:config(dummy_temp),
             Datasource="erbi:temp:base_driver=dummy;data_dir="++
                 proplists:get_value(data_dir,Config,"")++";connect=success",
             erbi_temp_db:start(Datasource),
             Datasource
     end,
     fun(Datasource)->
             erbi_temp_db:stop(Datasource)
                 end,
     fun(Datasource)->
             [?_assertEqual(ok, erbi:connect(Datasource)),
              ?_assertEqual(declined, erbi:disconnect(Datasource))]
     end}.



get_data_dir(DataSource)->
    #erbi{properties=PropList} = NormDS = erbi:normalize_data_source(DataSource),
   erbi_temp_db:get_data_dir_name(PropList,NormDS).
