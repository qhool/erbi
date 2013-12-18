%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbi_temp_db_helpers).
-include("erbi.hrl").

% Helper functions for drivers implementing
% erbi_temp_db behaviour
-export([create_dir/1,
         del_dir/1,
         kill_db_pid/1,
         get_free_db_port/2,
         save_in_db_data_file/3,
         read_from_db_data_file/2,
         find_bin_dir/3,
         search_dirs/2,
         wait_for/4,
         getenv/2
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
    lists:foreach(fun(F) ->
                          ok = file:delete(F)
                  end, Files),
    del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

kill_db_pid(Pid)->
    os:cmd("kill -9 "++integer_to_list(Pid)).

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
    ok = file:write_file(Path++"/"++File,io_lib:fwrite("~p.\n",[Term])).
                         
read_from_db_data_file(Path,File)->
    %{ok,BinaryTerm} = file:read_file(Path++"/"++File),
    %binary_to_term(BinaryTerm).
    {ok,[Term]} = file:consult(Path++"/"++File),
    Term.

find_bin_dir(#erbi{properties=Props}=DataSource,Candidates,File) ->
    case getenv(DataSource,bin) of
        false ->
            search_dirs([proplists:get_value(bin_dir,Props,"") | Candidates],File);
        File ->
            {ok,File}
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
