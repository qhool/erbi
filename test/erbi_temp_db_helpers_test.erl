%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbi_temp_db_helpers_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

del_data_dir_test()->
    BaseDir="/tmp/testdir/",
    os:cmd("mkdir -p "++BaseDir++"/subdir"),
    erbi_temp_db_helpers:del_data_dir(BaseDir),
    false=filelib:is_dir(BaseDir).

get_free_db_port_test()->
    {ok,Port}=erbi_temp_db_helpers:get_free_db_port(7777,8777),
    {ok,_}=gen_tcp:listen(Port,[]).

save_read_from_db_data_file_test_()->
    {setup,
     fun()->
             BaseDir="/tmp/testdir/",
             os:cmd("mkdir -p "++BaseDir),
             BaseDir
     end,
     fun(BaseDir)->
             erbi_temp_db_helpers:del_data_dir(BaseDir)
     end,
     fun(BaseDir)->
             Term="This is a term",
             File="termfile",
             [?_assertEqual(ok,erbi_temp_db_helpers:save_in_db_data_file(Term,BaseDir,File)),
              ?_assertEqual(Term,erbi_temp_db_helpers:read_from_db_data_file(BaseDir,File))]
     end
    }.
    
search_db_binaries_test_()->
   %Check that if provided path is invalid, finds it anyway
   [?_test({ok,_Path}= erbi_temp_db_helpers:search_db_binaries(["/invalid/path/"],"ls")),
   % Check that uses system PATH
   ?_test({ok,_Path}= erbi_temp_db_helpers:search_db_binaries([],"ls"))].

wait_for_test_()->
    Error = waited_too_much,
    Interval = 500,
    FunWait = fun()-> wait end,
    FunNoWait = fun()-> no_wait end,
    [?_assertEqual(Error,erbi_temp_db_helpers:wait_for(FunWait,Error,Interval,2)),
     ?_assertEqual(no_wait,erbi_temp_db_helpers:wait_for(FunNoWait,Error,Interval,1)),
     ?_assertEqual(Error,erbi_temp_db_helpers:wait_for(FunNoWait,Error,Interval,0))].
              
                            
                                  
                              
