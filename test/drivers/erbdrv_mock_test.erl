%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbdrv_mock_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

-define(FILES,"../test/drivers/mock_db_schema.sql").

connect_mock_test()->
    {ok,Config}=erbi_test_util:config(),
    Datasource= "erbi:mock:base_driver=epgsql;data_dir="++
	mock_opt(data_dir,Config)++
	";init_files="++mock_opt(schema_files,Config),
    {ok,_}=?debugVal(erbi:connect(Datasource,"AUSER","APassword")).




mock_opt(Atom,PList)->
    MockPList=proplists:get_value(mock,PList),
    case proplists:get_value(Atom,MockPList) of
	undefined ->
	    "";
	Value ->
	    Value
		end.
	       
    
