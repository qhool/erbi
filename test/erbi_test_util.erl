-module(erbi_test_util).
-export([start_db_test/1,
	 stop_db_test/1,
	 config/0,config/1,
         dataset/1,
         equal_rows_list/2,equal_rows_list/3,
         equal_rows_proplist/2,equal_rows_proplist/3,
         equal_rows_dict/2,equal_rows_dict/3,
         rows_equal_by/3,rows_equal_by/4,rows_equal_by/5,
         dicts_equal/2,proplists_equal/2,
         bitmap_sublist/2 ]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erbi/include/erbi.hrl").

start_db_test(DataSource)->
    Fun=fun(DS)->
    erbi_temp_db:start(DS)
    end,
    apply_if_temp(DataSource,Fun).
    
stop_db_test(DataSource)->
    Fun=fun(DS)->
    erbi_temp_db:stop(DS)
    end,
    apply_if_temp(DataSource,Fun).

apply_if_temp(DataSource,Fun)->
    case erbi:parse_data_source(DataSource) of
	#erbi{driver=temp}->
	    Fun(DataSource);
	_->
	    ok
    end.
    
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

equal_rows_list(Dataset,Results) ->
    ?debugFmt("list dataset: ~n~p~n",[Results]),
    equal_rows_list(Dataset,Results,0).
equal_rows_list(Dataset,Results,Limit) -> 
    rows_equal_by(Dataset,Results,Limit,fun(_,Rows) -> Rows end,fun(A,B) -> A =:= B end).

equal_rows_proplist(Dataset,Results)  ->
    equal_rows_proplist(Dataset,Results,0).
equal_rows_proplist(Dataset,Results,Limit) -> 
    rows_equal_by(Dataset,Results,Limit,fun rows_to_proplists/2,fun proplists_equal/2).

equal_rows_dict(Dataset,Results) ->
    equal_rows_dict(Dataset,Results,0).
equal_rows_dict(Dataset,Results,Limit) -> 
    rows_equal_by(Dataset,Results,Limit,fun rows_to_dicts/2,fun dicts_equal/2).



apply_lim(List,0) ->
    List;
apply_lim(List,Lim) ->
    lists:sublist(List,Lim).

rows_equal_by(DatasetName,Results,Limit,DatasetToRows,RowComparator) when is_atom(DatasetName) ->
    {Cols,Rows} = dataset(DatasetName),
    rows_equal_by({Cols,Rows},Results,Limit,DatasetToRows,RowComparator);
rows_equal_by({Cols,Rows},Results,Limit,DatasetToRows,RowComparator) -> 
    Rows1 = apply_lim(Rows,Limit),
    Dataset = DatasetToRows(Cols,Rows1),
    rows_equal_by(Dataset,Results,Limit,DatasetToRows,RowComparator);
rows_equal_by(Dataset,Results,Limit,_,RowComparator) when is_list(Dataset) ->
    rows_equal_by(Dataset,Results,Limit,RowComparator).

rows_equal_by(Expected,Results,RowComparator) ->    
    rows_equal_by(Expected,Results,0,RowComparator).
rows_equal_by(Expected,Results,Limit,RowComparator) ->
    Expected1 = apply_lim(Expected,Limit),
    Results1 = apply_lim(Results,Limit),
    lists:all( fun(I) ->
                       RowComparator( ?debugVal(lists:nth(I,Expected1)), 
                                      ?debugVal(lists:nth(I,Results1)) )
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

bitmap_sublist(List,Map) when is_integer(Map) ->
    bitmap_sublist(List,binary:encode_unsigned(Map));
bitmap_sublist(List,Map) when is_binary(Map) ->
    bitmap_sublist(List,bits_to_bools(Map,[]),[]).
bitmap_sublist([],_,Acc) ->
    lists:reverse(Acc);
bitmap_sublist(_,[],Acc) ->
    lists:reverse(Acc);
bitmap_sublist([L|List],[true|Map],Acc) ->
    bitmap_sublist(List,Map,[L|Acc]);
bitmap_sublist([_|List],[false|Map],Acc) ->
    bitmap_sublist(List,Map,Acc).

bits_to_bools(<<>>,Acc) ->
    Acc;
bits_to_bools(<<1:1,B/bitstring>>,Acc) ->
    bits_to_bools(B,[true|Acc]);
bits_to_bools(<<0:1,B/bitstring>>,Acc) ->
    bits_to_bools(B,[false|Acc]).
