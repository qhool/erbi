-module(erbi_test_util).
-export([config/0,config/1,
         dataset/1,
         equal_rows_list/2,equal_rows_list/3,
         equal_rows_proplist/2,equal_rows_proplist/3,
         equal_rows_dict/2,equal_rows_dict/3,
         dicts_equal/2,proplists_equal/2]).
-include_lib("eunit/include/eunit.hrl").

config() ->
    ErbiDir = get_base_dir(),
    ConfigFile = filename:join(ErbiDir,"test.config"),
    file:consult(ConfigFile).

config(Section) ->
    {ok,Cfg} = config(),
    proplists:get_value(Section,Cfg).

dataset(Name) ->
    ErbiDir = get_base_dir(),
    FilePatterns = lists:map( fun(N) ->
                                      Stars = lists:duplicate(N,"*"),
                                      filename:join([ErbiDir|Stars]++[atom_to_list(Name)++".dat"])
                              end, lists:seq(0,3) ),
    [FileName|_] = lists:concat( lists:map( fun filelib:wildcard/1, FilePatterns ) ),
    case file:consult(FileName) of
        {ok,Terms} ->
            ?debugFmt("Read from ~p:~n~p~n",[FileName,Terms]),
            [Cols|Rows]=Terms,
            {Cols,Rows};
        {error,Reason} ->
            ?debugFmt("Can't read ~p: ~p~n",[FileName,Reason]),
            {error,Reason}
    end.

equal_rows_list(Dataset,Results) when is_atom(Dataset) ->
    ?debugFmt("list dataset: ~n~p~n",[Results]),
    equal_rows_list(Dataset,Results,0).
equal_rows_list(Dataset,Results,Limit) -> 
    rows_equal_by(Dataset,Results,Limit,fun(_,Rows) -> Rows end,fun(A,B) -> A =:= B end).

equal_rows_proplist(Dataset,Results) when is_atom(Dataset) ->
    equal_rows_proplist(Dataset,Results,0).
equal_rows_proplist(Dataset,Results,Limit) -> 
    rows_equal_by(Dataset,Results,Limit,fun rows_to_proplists/2,fun proplists_equal/2).

equal_rows_dict(Dataset,Results) when is_atom(Dataset) ->
    equal_rows_dict(Dataset,Results,0).
equal_rows_dict(Dataset,Results,Limit) -> 
    rows_equal_by(Dataset,Results,Limit,fun rows_to_dicts/2,fun dicts_equal/2).



apply_lim(List,0) ->
    List;
apply_lim(List,Lim) ->
    lists:sublist(List,Lim).

rows_equal_by(DatasetName,Results,Limit,DatasetToRows,RowComparator) ->
    {Cols,Rows} = dataset(DatasetName),
    %ColAtoms = lists:map(fun atom_to_list/1,Cols),
    Rows1 = apply_lim(Rows,Limit),
    Results1 = apply_lim(Results,Limit),
    Expected = DatasetToRows(Cols,Rows1),
    lists:all( fun(I) ->
                       RowComparator( ?debugVal(lists:nth(I,Expected)), ?debugVal(lists:nth(I,Results1)) )
               end,
               lists:seq(1,length(Results1)) ).
                                        
rows_to_dicts(Cols,Rows) ->
    lists:map( fun(Row) ->
                       dict:from_list(lists:zip(Cols,Row))
               end, Rows ).

rows_to_proplists(Cols,Rows) ->
    lists:map( fun(Row) -> lists:zip(Cols,Row) end, Rows ).
                 
       
gen_d_eq(Dict) ->
    fun({K,V}) ->
            case dict:find(K,Dict) of
                error -> false;
                {ok,V2} -> V =:= V2
            end
    end.
dicts_equal(Dict1,Dict2) ->
    Entries1 = dict:to_list(Dict1),
    Entries2 = dict:to_list(Dict2),
    lists:all( gen_d_eq(Dict1), Entries2 ) andalso
        lists:all( gen_d_eq(Dict2), Entries1 ).

proplists_equal(PL1,PL2) ->
    lists:sort(PL1) =:= lists:sort(PL2).

get_base_dir() ->
    {file,ThisFile} = code:is_loaded(?MODULE),
    filename:join(
      lists:reverse(
        lists:nthtail(
          2,  lists:reverse(
                filename:split(ThisFile)
               )))).
