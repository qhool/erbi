%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbi_temp_db_helpers_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

del_data_dir_test()->
    BaseDir="/tmp/testdir/",
    erbi_temp_db_helpers:create_dir(BaseDir++"/subdir"),
    true = filelib:is_dir(BaseDir),
    erbi_temp_db_helpers:del_dir(BaseDir),
    false = filelib:is_dir(BaseDir).

get_free_port_test()->
    {ok,Port}=erbi_temp_db_helpers:get_free_port(),
    {ok,_}=gen_tcp:listen(Port,[]).

get_free_db_port_test() ->
    true = os:putenv("ERBI_TEMPDB_PORT_FOO","17173"),
    {ok,17173} = erbi_temp_db_helpers:get_free_db_port(#erbi{ driver = foo }).

save_read_integer_test_()->
    {setup,
     fun()->
             BaseDir="/tmp/testdir/",
             erbi_temp_db_helpers:create_dir(BaseDir),
             BaseDir
     end,
     fun(BaseDir)->
             erbi_temp_db_helpers:del_dir(BaseDir)
     end,
     fun(BaseDir)->
             Term = 585,
             File = "termfile",
             [?_assertEqual(ok,erbi_temp_db_helpers:save_in_db_data_file(Term,BaseDir,File)),
              ?_assertEqual(Term,erbi_temp_db_helpers:read_integer(BaseDir,File))]
     end
    }.

search_db_binaries_test_()->
   %Check that if provided path is invalid, finds it anyway
   [?_test({ok,_Path}= erbi_temp_db_helpers:search_dirs(["/invalid/path/"],"ls")),
   % Check that uses system PATH
   ?_test({ok,_Path}= erbi_temp_db_helpers:search_dirs([],"ls"))].

wait_for_test_()->
    Error = waited_too_much,
    Interval = 500,
    WaitCount=5,
    FunWait = fun()-> wait end,
    FunNoWait = fun()-> no_wait end,
    FunWaitN = fun()->
                       case get(wait_count) of
                           undefined ->
                               put(wait_count, 1),
                               wait;
                           N when N == WaitCount->
                               no_wait;
                           N ->
                               put(wait_count, N+1),
                                   wait
                       end
               end,
    [?_assertEqual(Error,erbi_temp_db_helpers:wait_for(FunWait,Error,Interval,2)),
     ?_assertEqual(no_wait,erbi_temp_db_helpers:wait_for(FunNoWait,Error,Interval,1)),
     ?_assertEqual(Error,erbi_temp_db_helpers:wait_for(FunNoWait,Error,Interval,0)),
     ?_assertEqual(no_wait,erbi_temp_db_helpers:wait_for(FunWaitN,Error,Interval,WaitCount+1)),
     ?_assertEqual(WaitCount,erase(wait_count)),
     ?_assertEqual(Error,erbi_temp_db_helpers:wait_for(FunWaitN,Error,Interval,WaitCount-1)),
     ?_assertEqual(WaitCount-1,erase(wait_count))
    ].


kill_os_pid_test_() ->
    [{"Normal kill",fun() -> kill_os_pid(no_trap) end},
     {"Stubborn kill",{timeout,30,fun() -> kill_os_pid(trap) end}}].

kill_os_pid(Trap) ->
    {ok, Dir} = erbi_temp_db_helpers:search_dirs([],"bash"),
    ?debugVal(Me = self()),
    SendMePid = fun(Pid) ->
                        Me ! {os_pid,Pid}
                end,
    SleepLoop = "for ((n=0; $n < 150; n=$n+1)); do sleep 0.1; done",
    {SleepScript,ExpectSignal} =
        case Trap of
            no_trap -> {"echo $$; "++SleepLoop,15};
            trap -> {"echo $$; trap '' SIGTERM SIGINT; "++SleepLoop,9}
        end,
    SleepWithPid =
        fun() ->
                Result = erbi_temp_db_helpers:exec_cmd(Dir++"/bash",["-c",SleepScript],
                                                       {fun scan_for_pid/2,SendMePid},standard_error),
                Me ! {return,?debugVal(Result)}
        end,
    ?assert(is_pid(?debugVal(spawn(SleepWithPid)))),
    KillSeq = [{wait,60},{term,4},{kill,2}],
    receive
        {os_pid,Pid} ->
            ?debugVal(Pid),
            alive = ?debugVal(erbi_temp_db_helpers:check_os_pid(Pid)),
            ok = ?debugVal(erbi_temp_db_helpers:kill_os_pid(Pid,KillSeq)),
            dead = ?debugVal(erbi_temp_db_helpers:check_os_pid(Pid))
    after 1000 ->
            throw(expecting_pid)
    end,
    receive
        {return,{ok,{exit_status,Status},_}} ->
            %%?assertEqual(128+ExpectSignal,Status);
            ?assertEqual(true,Status > 128);
        {return,Err} ->
            ?debugVal({return,Err}),
            throw(Err)
    after 15000 ->
            throw(no_exit_from_exec_cmd)
    end.

scan_for_pid(_,undefined) ->
    undefined;
scan_for_pid(Output,OnPid) ->
    ?debugVal(Output),
    case re:run(Output,"[0-9]+") of
        {match,[{Start,Len}|_]} ->
            OSPid = list_to_integer(lists:sublist(Output,Start+1,Len)),
            OnPid(OSPid),
            undefined;
        _ ->
            OnPid
    end.

exec_test_() ->
    CMDs =
        [{"true",
          fun(True) ->
                  [{ "exec_cmd true",
                     ?_test( {ok,{exit_status,0},_} =
                                 ?debugVal(erbi_temp_db_helpers:exec_cmd(?debugVal(True),[],wait)) )}]
          end},
         {"false",
          fun(False) ->
                  [{ "exec_cmd false",
                     ?_test( {ok,{exit_status,1},_} =
                                 ?debugVal(erbi_temp_db_helpers:exec_cmd(?debugVal(False),[],wait)) )}]
          end},
         {"sleep",
          fun(Sleep) ->
                  [{ "exec_cmd get pid",
                     ?_test( {ok,{os_pid,_Pid},_} =
                                 ?debugVal(erbi_temp_db_helpers:exec_cmd(?debugVal(Sleep),["3"],nowait)) )}]
          end},
         {"echo",
          fun(Echo) ->
                  [{ "exec_cmd read output",
                     ?_test( {ok,{exit_status,0},"foo"++_} =
                                 ?debugVal
                                    (erbi_temp_db_helpers:exec_cmd
                                       (?debugVal(Echo),["foo"],
                                        {fun(Dat,Acc) -> Dat++Acc end,""},
                                        standard_io )) )}]
          end}],
    NormalTests =
        lists:concat(
          [ case erbi_temp_db_helpers:search_dirs([],C) of
                {ok,Cmd} ->
                    F(Cmd++"/"++C);
                _ -> []
            end || {C,F} <- CMDs ]
         ),
    BadCmd =
        { "exec_cmd bad command",
          ?_test( {error,{exec_failed,_}} =
                      ?debugVal(erbi_temp_db_helpers:exec_cmd
                                  ("/never/no/no/uoefnv08234028408",[]))
                )},
    Killer =
        fun(Output,Method) ->
                case re:run(Output,"[0-9]+") of
                    {match,[{Start,Len}|_]} ->
                        OSPid = list_to_integer(lists:sublist(Output,Start+1,Len)),
                        kill_port_by_os_pid(OSPid,Method);
                    _ ->
                        Method
                end
        end,
    MkTerminator =
        fun(Method) ->
                { "exec_cmd " ++ atom_to_list(Method),
                  ?_test( {error,_} =
                              ?debugVal(erbi_temp_db_helpers:exec_cmd
                                          ("/bin/sh",["-c","echo $$; sleep 3"],
                                           {Killer,Method}))
                        )}
        end,
    Terminators = [ MkTerminator(Mth)
                    || Mth <- [port_close,close_by_owner,kill_owner] ],
    NormalTests ++ [BadCmd,Terminators].

kill_port_by_os_pid(OSPid,Method) ->
    [Port] =
        lists:filter(fun(P) ->
                             case erlang:port_info(P,os_pid) of
                                 {os_pid,OSPid} ->
                                     true;
                                 _ -> false
                             end
                     end, erlang:ports()),
    {connected,Owner} = ?debugVal(erlang:port_info(Port,connected)),
    ?debugFmt("Found port: ~p   Owner: ~p",[Port,Owner]),
    case Method of
        port_close ->
            ?debugVal(port_close(Port));
        close_by_owner ->
            Port ! { Owner, close };
        kill_owner ->
            exit(Owner,kill)
    end.

