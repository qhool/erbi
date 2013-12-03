%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbi_temp_db_helpers).
-include("erbi.hrl").

% Helper functions for drivers implementing
% erbi_temp_db behaviour
-export([del_data_dir/1,
         kill_db_pid/1,
 	 get_free_db_port/2,
	 save_in_db_data_file/3,
	 read_from_db_data_file/2,
	 search_db_binaries/1,
	 get_string_from_ds/1
	]).


% Helper functions for drivers implementing
% erbi_temp_db behaviour
del_data_dir(Dir) ->
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
    lists:foreach(fun(F) ->
                          ok = file:delete(F)
                  end, Files),
    del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

kill_db_pid(Pid)->
    os:cmd("kill -9 "++integer_to_list(Pid)).

get_free_db_port(MinPort,MaxPort)->
    StartingPort=trunc(random:uniform()*(MaxPort-MinPort))+MinPort,
    get_free_db_port(StartingPort,MinPort,MaxPort).

get_free_db_port(StartingPort,MinPort,MaxPort)->
    get_free_db_port(StartingPort,undefined,MinPort,MaxPort).

get_free_db_port(Port,undefined,MinPort,MaxPort) when Port > MaxPort->
    get_free_db_port(MinPort,restarted);
get_free_db_port(Port,restarted,_MinPort,MaxPort) when Port > MaxPort ->
    {error,no_free_port};
get_free_db_port(Port,Tag,MinPort,MaxPort) ->
    case gen_tcp:listen(Port,[]) of
       {ok,TmpSock}->
            gen_tcp:close(TmpSock),
            {ok,Port};
        _ ->
            get_free_db_port(Port+1,Tag,MinPort,MaxPort)
      end.

save_in_db_data_file(Term,Path,File)->
    file:write_file(Path++"/"++File,term_to_binary(Term)).
                         
read_from_db_data_file(Path,File)->
    {ok,BinaryTerm} = file:read_file(Path++"/"++File),
    binary_to_term(BinaryTerm).

search_db_binaries(PossiblePaths)->
    case lists:filter(fun(Path)->
                              filelib:is_dir(Path)
                      end,PossiblePaths) of
        []->
            {error,binaries_not_found};
        [H|_]->         
            {ok,H}
    end.


get_string_from_ds(#erbi{driver = DriverName, properties=PropList, args=Args})->
    io_lib:format("erbi:~p:~p:~p",[DriverName,PropList,Args]).
