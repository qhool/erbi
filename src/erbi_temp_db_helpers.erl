%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbi_temp_db_helpers).
-include("erbi.hrl").

% Helper functions for drivers implementing
% erbi_temp_db behaviour
-export([create_dir/1,
         del_dir/1,
         kill_db_pid/2,kill_db_pid/3,kill_pid/1,kill_pid/2,check_pid/1,
         get_free_db_port/2,
         save_in_db_data_file/3,
         read_integer/2,
         find_bin_dir/3,
         search_dirs/2,
         wait_for/4,
         getenv/2,
         filter_scanner/2, filter_scanner/3,
         filter_logger/2, filter_logger/3,
         exec_cmd/2, exec_cmd/3, exec_cmd/4
	]).


% Helper functions for drivers implementing
% erbi_temp_db behaviour

create_dir(Dir)->
    os:cmd("mkdir -p "++ Dir).


del_dir(Dir) ->
   lists:foreach(fun(D) ->
                         ok = file:del_dir(D)
                 end, del_all_files([Dir], [])).

del_all_files([], EmptyDirs) ->
    EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
    {ok, FilesInDir} = file:list_dir(Dir),
    {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                        Path = Dir ++ "/" ++ F,
                                        case filelib:is_dir(Path) of
                                            true ->
                                                {Fs, [Path | Ds]};
                                            false ->
                                                {[Path | Fs], Ds}
                                        end
                                end, {[],[]}, FilesInDir),
    case search_dirs([],"sh") of
        {ok,Path} ->
            exec_cmd(Path++"/sh",["-c","rm -f " ++ Dir ++ "/* " ++ Dir ++ "/.*"], 
                     filter_scanner(nomatch,["Is a directory"]),none);
        _ ->
            lists:foreach(fun(F) ->
                                  ok = file:delete(F)
                          end, Files)
    end,
    del_all_files(T ++ Dirs, [Dir | EmptyDirs]).
%%@doc lookup pid from pidfile, and kill
%%
%% kill_db_pid(Dir,PidFile,[KillSequence])
%% see kill_pid for format of KillSequence
%%@end
kill_db_pid(Dir,PidFile) ->
    kill_db_pid(Dir,PidFile,default).

kill_db_pid(Dir,PidFile,Sequence) ->
    case read_integer(Dir,PidFile) of
        {error,_} = Error ->
            Error;
        Pid ->
            kill_pid(Pid,Sequence)
    end.

%%@doc kill os pid
%%
%% kill_pid(Pid,[KillSequence])
%%
%% Killsequence controls how many attempts and what signals to use.
%% It consists of a list of terms:
%% <ul>
%%   <li>{Signal,NumberOfAttempts}
%%     <ul>Signal: standard signal name, as atom (term,kill,int,quit...)</ul>
%%     <ul>NumberOfAttempts: attempts before moving to the next element</ul>
%%   </li>
%%   <li>{wait,Miliseconds} Set/reset time to wait between attempts.</li>
%% </ul>
%% After each unsucessful attempt to kill the process, the wait time is increased by
%% a factor of 1.5.  Wait time is reset when switching to a new signal.
%%
%% KillSequence defaults to [{wait,600},{term,9},{kill,3}].
%%
%%@end
kill_pid(Pid) ->
    kill_pid(Pid,default).

kill_pid(Pid,default) ->
    kill_pid(Pid,[{term,9},{kill,3}]);
kill_pid(Pid,Sequence) ->
    kill_pid(integer_to_list(Pid),Sequence,600,1.0,check).

kill_pid(Pid,[{wait,W}|Ks],_,_F,State) ->
    kill_pid(Pid,Ks,W,1.0,State);
kill_pid(Pid,[{Sig,N}|Ks],W,F,State) when is_atom(Sig) ->
    kill_pid(Pid,[{string:to_upper(atom_to_list(Sig)),N}|Ks],W,F,State);
kill_pid(Pid,Ks,W,F,check) ->
    kill_pid(Pid,Ks,W,F,check_pid(Pid));
kill_pid(_,[],_,_,alive) ->
    {error,it_will_not_die};
kill_pid(Pid,_,_,_,dead) ->
    io:format(standard_error,"~s killed.~n",[Pid]),
    ok;
kill_pid(Pid,[{Sig,N}|Kills],W,F,alive) ->
    io:format(standard_error,"Killing ~s with SIG~s~n",[Pid,Sig]),
    exec_cmd("/bin/kill",["-s",Sig,Pid],quiet),
    kill_pid(Pid,[{Sig,N-1}|Kills],W,F,wait);
kill_pid(Pid,[{_,0}|Ks],W,_F,wait) ->
    kill_pid(Pid,Ks,W,1.0,wait);
kill_pid(Pid,Ks,W,F,wait) ->
    receive after round(W*F) -> kill_pid(Pid,Ks,W,F*1.5,check) end.

check_pid(Pid) when is_integer(Pid) ->
    check_pid(integer_to_list(Pid));
check_pid(Pid) ->
    {ok,PidPat} = re:compile("^\s*"++Pid,[multiline]),
    Scan = fun(_,{alive,_}) -> {acc,{alive,""}};
              ("",{dead,_}) -> {acc,{dead,""}};
              (Chunk,{dead,LastChunk}) ->
                   case re:run(LastChunk++Chunk,PidPat,[{capture,none}]) of
                       match -> {acc,{alive,""}};
                       _ -> {acc,{dead,Chunk}}
                   end
           end,
    {ok,{exit_status,0},{Found,_}} = exec_cmd("/bin/ps",["ax"],{Scan,{dead,""}},none),
    Found.

get_free_db_port(MinPort,MaxPort)->
    StartingPort=trunc(random:uniform()*(MaxPort-MinPort))+MinPort,
    get_free_db_port(StartingPort+1,StartingPort,MinPort,MaxPort).

get_free_db_port(Port,StartingPort,MinPort,MaxPort) when Port > MaxPort->
    get_free_db_port(MinPort,StartingPort,MinPort,MaxPort);
get_free_db_port(Port,StartingPort,_MinPort,_MaxPort) when Port == StartingPort ->
    {error,no_free_port};
get_free_db_port(Port,StartingPort,MinPort,MaxPort) ->
    case gen_tcp:listen(Port,[]) of
       {ok,TmpSock}->
            gen_tcp:close(TmpSock),
            {ok,Port};
        _ ->
            get_free_db_port(Port+1,StartingPort,MinPort,MaxPort)
      end.

save_in_db_data_file(Term,Path,File)->
                                                %file:write_file(Path++"/"++File,term_to_binary(Term)).
    ok = file:write_file(Path++"/"++File,io_lib:fwrite("~p\n",[Term])).

read_integer(Path,File)->
    case file:read_file(Path++"/"++File) of
        {ok, Binary} ->
            to_integer(Binary);
        Any ->
            Any
    end.

to_integer(Binary)->
    [Value] = string:tokens(binary_to_list(Binary), "\n" ),
    list_to_integer(Value).

find_bin_dir(#erbi{properties=Props}=DataSource,Candidates,File) ->
    case getenv(DataSource,bin) of
        false ->
            search_dirs([proplists:get_value(bin_dir,Props,"") | Candidates],File);
        EnvFile ->
            {ok,EnvFile}
    end.

search_dirs(PossiblePaths,Filename)->
    SearchPath = PossiblePaths ++ get_os_path(),
    case lists:filter(fun(Path)->
                              filelib:is_file(Path++"/"++Filename)
                      end,SearchPath) of
        []->
            {error,{not_found,Filename,{search_path,SearchPath}}};
        [H|_]->
            {ok,H}
    end.

get_os_path()->
    StrPaths=os:getenv("PATH"),
    string:tokens(StrPaths,":").

wait_for(_Fun,Error,_Interval,0 ) ->
    Error;
wait_for(Fun,Error, Interval, Tries) ->
    case Fun() of
        wait ->
            receive
            after Interval->
                    wait_for(Fun,Error,Interval,Tries-1)
            end;
        Any->
            Any
    end.

getenv(#erbi{driver=Driver},Key) ->
    EnvName = "ERBI_TEMPDB_" ++ string:to_upper(atom_to_list(Key) ++ "_" ++ atom_to_list(Driver)),
    os:getenv(EnvName).

%% @doc logger generator for exec_cmd
%%
%%
%% @end

filter_scanner(Mode,Patterns) ->
    filter_scanner(Mode,Patterns,fun(Str) ->
                                        io:format(standard_error,"~s",[Str])
                                end).
filter_scanner(Mode,Patterns,OutFun) ->
    {filter_scanfn(Mode,Patterns,OutFun),""}.

filter_logger(Mode,Patterns) ->
    filter_logger(Mode,Patterns,fun(Str) ->
                                        io:format(standard_error,"~s",[Str])
                                end).

filter_logger(Mode,Patterns,OutFun) ->
    ScanFn = filter_scanfn(Mode,Patterns,OutFun),
    fun("~s",[Line]) ->
            ScanFn(Line,"");
       (_,_) ->
            ok
    end.

filter_scanfn(Mode,Patterns,OutFun) ->
    CPatns = lists:map( fun({Patn,Opts}) ->
                                {ok,C} = re:compile(Patn,Opts),
                                C;
                           (Patn) ->
                                {ok,C} = re:compile(Patn),
                                C
                        end,Patterns ),
    AnyAll = case Mode of
                 match -> any;
                 nomatch -> all
             end,
    {ok,LineRe} = re:compile("\n"),
    fun(Data,Acc) ->
            Acc0 = Acc++Data,
            {Lines,Acc1} = pop(re:split(Acc0,LineRe)),
            lists:foreach(fun(<<>>) ->
                                  ok;
                             (Line) ->
                                  case lists:AnyAll(fun(P) -> Mode =:= re:run(Line,P,[{capture,none}]) end,
                                                    CPatns) of
                                      true -> OutFun(binary_to_list(Line)++"\n");
                                      _ -> ok
                                  end
                          end,Lines),
            binary_to_list(Acc1)
    end.

pop(T) ->
    pop(T,[]).
pop([Last],Front) ->
    {lists:reverse(Front),Last};
pop([A|As],Front) ->
    pop(As,[A|Front]).

%%@doc execute command, returning OS PID
%%
%% Arguments:
%%
%%@end
-type exec_cmd_return() :: {ok,{os_pid,integer()},any()} |
                           {ok,{exit_status,integer()},any()} |
                           {error,any()}.
-type exec_cmd_scanfn() :: fun( (string(),any()) -> any() ).

-spec exec_cmd( Command :: unicode:chardata(),
                Args :: [unicode:chardata()] ) -> exec_cmd_return().

exec_cmd( Command, Args ) ->
    exec_cmd(Command,Args,wait).


-spec exec_cmd( Command :: unicode:chardata(),
                Args :: [unicode:chardata()],
                wait | nowait | {exec_cmd_scanfn(),any()}
              ) -> exec_cmd_return().

exec_cmd( Command, Args, quiet ) ->
    AccWait =
        fun(Data,Acc) ->
                Data++Acc
        end,
    StrCmd = string:join( lists:map( fun unicode:characters_to_list/1,
                                     [Command|Args] ), " " ),
    case exec_cmd(Command, Args, {AccWait,""}, none) of
        {ok,{exit_status,0},_} = Ret -> Ret;
        {ok,{exit_status,Status},Out} = Ret ->
            io:format(standard_error,"Non-zero exit (~p) from ~p:~n~s",[Status,StrCmd,Out]),
            Ret;
        Other -> Other
    end;
exec_cmd( Command, Args, Logger ) when is_function(Logger) ->
    exec_cmd( Command, Args, wait, Logger);
exec_cmd( Command, Args, Scanner ) ->
    exec_cmd( Command, Args, Scanner, standard_error ).

%%@doc execute command, with processing of output
%%
%% Allows a command to be executed, while capturing the OS pid or exit status, and the output.
%% Output can be printed or logged, and can also be scanned or collected.
%% Arguments:
%% <ul>
%%   <li>Command: absolute path to executable</li>
%%   <li>Args: List of arguments; these do not need to be quoted or escaped; command is exec'ed
%%       directly.</li>
%%   <li>Scanner -- output parser/accumulator, and flow control function.  See below.</li>
%%   <li>Logger -- output/logging control.  Can be:
%%     <ul><li>A device, like the first arg of io:format/3</li>
%%         <li>A fun, accepting string(); will be called for all output of Command;
%%             return is ignored.</li>
%%         <li>'none'</li>
%%     </ul></li>
%% </ul>
%% Return values:
%% <ul>
%%   <li>{ok,{os_pid,Pid},Acc} -- Acc is the accumulated output of Scanner (q.v.)</li>
%%   <li>{ok,{exit_status,Status},Acc}</li>
%%   <li>{error,Reason}</li>
%% </ul>
%%
%% Scanner
%%
%% The scanner argument is a pair of {Scanner,InitAcc}.  Scanner takes two arguments:
%% <ul>
%%   <li>Data: string() -- a chunk of output from the command.</li>
%%   <li>Acc: any() -- accumulator; starts with InitAcc</li>
%% </ul>
%% Scanner returns one of:
%% <ul>
%%   <li>{ok,Acc} -- exec_cmd will return immediately, with {ok,{os_pid,Pid},Acc}</li>
%%   <li>{error,Reason} -- exec_cmd returns immediately with this value</li>
%%   <li>{acc,Acc} -- continue processing output, with new accumulator value</li>
%% </ul>
%% Scanner can also accept 'wait' and 'nowait' which make the function wait (or not) for
%% Command to finish; output Acc will be 'undefined'.ls
%%@end

-spec exec_cmd( Command :: unicode:chardata(),
                Args :: [unicode:chardata()],
                wait | nowait | {exec_cmd_scanfn(),any()},
                none | io:device()
              ) -> exec_cmd_return().

exec_cmd( Command, Args, wait, Output ) ->
    WaitScanner =
        fun(_Data,_Acc) ->
                undefined %returning undefined means output processor goes until exit.
        end,
    exec_cmd( Command, Args, {WaitScanner,""}, Output );
exec_cmd( Command, Args, nowait, Output ) ->
    NoWaitScanner =
        fun(_Data,_Acc) ->
                {ok,undefined} % returning {ok,Acc} ends scanning phase
        end,
    exec_cmd( Command, Args, {NoWaitScanner,undefined}, Output );
exec_cmd( Command, Args, {Scanner,Acc}, Output ) ->
    StrCmd = string:join( lists:map( fun unicode:characters_to_list/1,
                                     [Command|Args] ), " " ),
    OutFun =
        case Output of
            none ->
                fun(_,_) ->
                        ok
                end;
            H when is_atom(H) ->
                fun(Fmt,Dat) ->
                        io:format(Output,Fmt,Dat)
                end;
            F1 when is_function(F1,1) ->
                fun(Fmt,Dat) ->
                        F1(io_lib:format(Fmt,Dat))
                end;
            F2 when is_function(F2,2) ->
                F2
        end,
    TopPid = self(),
    {SpawnPid,_Mon} =
        spawn_monitor(
          fun() ->
                  OutFun("Executing ~s: ",[StrCmd]),
                  Port =
                      open_port
                        ( {spawn_executable,
                           unicode:characters_to_list(Command)},
                        %{spawn,StrCmd},
                          [{args,lists:map
                                   ( fun unicode:characters_to_binary/1, Args )},
                           stream,use_stdio,stderr_to_stdout,exit_status,
                           {cd, filename:absname("")} ] ),
                  {os_pid,OSPid} = erlang:port_info(Port,os_pid),
                  TopPid ! {{self(),undefined},{os_pid,OSPid}},
                  Ret = port_loop(Port,TopPid,{self(),OSPid},OutFun),
                  OutFun("'~s' terminated: ~p~n",[StrCmd,Ret])
          end),
    output_loop( SpawnPid, Scanner, Acc ).

output_loop( SpawnPid, Scanner, Acc ) ->
    output_loop( SpawnPid, undefined, Scanner, Acc ).

output_loop(SpawnPid,OSPid,Scanner,Acc) ->
    receive
        {{SpawnPid,OSPid},{os_pid,OSPid1}} ->
            output_loop(SpawnPid,OSPid1,Scanner,Acc,{data,""});
        {{SpawnPid,OSPid},Msg} ->
            output_loop(SpawnPid,OSPid,Scanner,Acc,Msg);
        {'DOWN',_,process,SpawnPid,Reason} ->
            case OSPid of
                undefined -> {error,{exec_failed,Reason}};
                _ ->         {error,{unexpected_termination,Reason}}
            end;
        {'DOWN',_,process,_,_} ->
            output_loop(SpawnPid,OSPid,Scanner,Acc);
        {{_OtherPid,_},_} ->
            output_loop(SpawnPid,OSPid,Scanner,Acc)
    after 1000 ->
            case OSPid of
                undefined -> {error,timeout};
                _ ->
                    output_loop(SpawnPid,OSPid,Scanner,Acc)
            end
    end.

output_loop(SpawnPid,OSPid,Scanner,Acc,Msg) ->
    case Msg of
        {data,Data} ->
            case Scanner(Data,Acc) of
                {ok,Acc1} ->
                    {ok,{os_pid,OSPid},Acc1};
                {error,Reason} ->
                    {error,Reason};
                {acc,Acc2} ->
                    output_loop(SpawnPid,OSPid,Scanner,Acc2);
                Acc2 ->
                    output_loop(SpawnPid,OSPid,Scanner,Acc2)
            end;
        {exit_status,Status} ->
            {ok,{exit_status,Status},Acc};
        Other ->
            Other
    end.

port_loop(Port,Parent,Ident,Logger) ->
    receive
        {Port,{data,Data}} ->
            Parent ! {Ident,{data,Data}},
            Logger("~s",[Data]),
            port_loop(Port,Parent,Ident,Logger);
        {Port,{exit_status,Status}} ->
            Parent ! {Ident,{exit_status,Status}},
            {exit_status,Status};
        {'EXIT',Port,Reason}->
            Parent ! {Ident,{error,Reason}},
            {port_terminated,Reason};
        {Port,closed} ->
            Parent ! {Ident,{error,port_closed}},
            {port_terminated,closed};
        Other ->
            Parent ! {Ident,{error,{unhandled_message,Other}}},
            {unhandled_message,Other}
    after 1000 ->
            case erlang:port_info(Port) of
                undefined ->
                    Parent ! {Ident,{error,port_gone}},
                    {error,port_gone};
                _Info ->
                    port_loop(Port,Parent,Ident,Logger)
            end
    end.

