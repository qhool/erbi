%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% @copyright 2013 Voalte Inc. <jburroughs@voalte.com>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% erbi dummy driver
%% @end 
-module(erbdrv_dummy).
-behaviour(erbi_driver).
-behaviour(erbi_temp_db).
-include("erbi.hrl").
-include("erbi_driver.hrl").
-export([driver_info/0,
         validate_property/2,
         property_info/0,
         parse_args/1,
         connect/3,
         disconnect/1,
         begin_work/1, begin_work/2,
         rollback/1, rollback/2,
         commit/1,
         do/3,
         prepare/2,
         bind_params/3,
         execute/3,
         fetch_rows/3,
         finish/2,
         params/2
        ]).

% erbi_temp_db  API
-export([start_temp/1,
         stop_temp/1,
        get_temp_connect_data/3]).

driver_info() ->
    #erbi_driver_info
        { driver = dummy,
          preparse_support = false,
          cursor_support = false,
          transaction_support = false
        }.

validate_property(queries,Queries) ->
    {ok,[{queries,lists:map( fun({Re,Cols,Rows}) ->
                                    {ok,Pattern} = re:compile(Re),
                                    {Pattern,Cols,Rows}
                            end, Queries )
        }]};    
validate_property(Prop,Val) when is_list(Val) ->
    {ok,[{Prop,list_to_atom(Val)}]};
validate_property(_,_) ->
    ok.

property_info() ->
    {ok,QR} = re:compile(".*"),
    [{defaults, [%whether each operation should succeed
                 {connect,fallthrough},
                 {disconnect,fallthrough},
                 {do,declined},
                 {prepare,fallthrough},
                 {execute,fallthrough},
                 {transaction,fallthrough},
                 {begin_work,fallthrough},
                 {rollback,fallthrough},
                 {commit,fallthrough},
                 {bind,fallthrough},
                 {fetch,fallthrough},
                 {finish,fallthrough},
                 {default,success},
                 %driver info flags:
                 {preparse_support,false},
                 {cursor_support,false},
                 {transaction_support,false},
                 {must_preparse,false},
                 {must_bind,false},
                 {multiple_bind,false},
                 %other behaviours:
                 % whether to return columns after prepare
                 {cols_on_prepare,false},
                 % whether to return all rows at execute
                 {rows_on_execute,true},
                 % whether to spread rows out over fetches
                 % fetch_rows will return 2 rows when amount is 'all', 1 for 'one' when this is on
                 {simulate_fetch,true},
                 {queries,[{QR,[dummy],[[1]]}]}
                ]},
     {unique,true}
    ].

parse_args(_) ->
    declined.

-define( CHKSUP(Sup), Sup = proplists:get_value(Sup,Props) ).

connect( #erbi{ properties = Props }, _Username, _Password ) ->
    Info = #erbi_driver_info
        { driver = dummy,
          ?CHKSUP(preparse_support),
          ?CHKSUP(cursor_support),
          ?CHKSUP(transaction_support),
          ?CHKSUP(must_preparse),
          ?CHKSUP(must_bind),
          ?CHKSUP(multiple_bind)
        },
    on_success( Props, connect,
                #erbdrv{ status = ok, info = Info, conn = Props } ).

disconnect( Props ) ->
    on_success( Props, disconnect, ok ).

begin_work( Props ) ->
    begin_work( Props, undefined ).
begin_work( Props, _ ) ->
    on_success( Props, [begin_work,transaction], #erbdrv{status=ok} ).

rollback( Props ) ->
    rollback( Props, undefined ).
rollback( Props, _ ) ->
    on_success( Props, [rollback,transaction], #erbdrv{status=ok} ).

commit( Props ) ->
    on_success( Props, [commit,transaction], #erbdrv{status=ok} ).

do( Props, Q, Params ) ->
    on_success( Props, do, 
                fun() ->
                        %hand off to execute, overriding some behavior
                        execute( [{execute,success},
                                  {cols_on_prepare,false},
                                  {rows_on_execute,false}|Props], Q, Params )
                end ).

prepare( Props, Q ) ->
    on_success( Props, prepare, 
                fun() ->
                        Data = 
                            case proplists:get_value(cols_on_prepare,Props) of
                                true ->
                                    {Cols,_} = match_query(Props,Q),
                                    Cols;
                                _ -> undefined
                            end,
                        #erbdrv{status=ok,stmt={Q,[],undefined},data=Data}
                end ).

bind_params( Props, {Q,OldParams,R}, Params ) ->
    on_success( Props, bind, #erbdrv{status=ok,stmt={Q,OldParams++Params,R}} ).

execute( Props, Q, Params ) ->
    on_success( Props, execute, 
                fun() ->
                        {Query,StParams,_} = 
                            case Q of
                                {_,_,_} -> Q;
                                _ -> {Q,[],undefined}
                            end,
                        {Cols,Rows} = match_query(Props,Query),
                        %if we aren't returning the whole set of rows now,
                        %stash them in the statement
                        {StmtRows,DataRows} =
                            case proplists:get_value(rows_on_execute,Props) of
                                true -> {[],{final,Rows}};
                                false -> {Rows,[]}
                            end,
                        Data = 
                            case proplists:get_value(cols_on_prepare,Props) of
                                true -> DataRows;
                                false -> {Cols,DataRows}
                            end,
                        Stmt = {Query,StParams++Params,StmtRows},
                        #erbdrv{status=ok,stmt=Stmt,data=Data}
                end ).

fetch_rows( Props, {Query,Params,Rows}, Amount ) ->
    on_success( Props, fetch,
                fun() ->
                        SimFetch = proplists:get_value(simulate_fetch,Props),
                        {RetRows,StmtRows} = 
                            case Rows of
                                [] -> {final,[]};
                                _ ->
                                    {Ret,Rmdr} = 
                                        if (SimFetch and (Amount =:= one)) ->
                                                [Row|Rows1] = Rows,
                                                {[Row],Rows1};
                                           SimFetch -> %amount not one, return more than one, but not all
                                                case Rows of
                                                    [R1,R2|Rows1] ->
                                                        {[R1,R2],Rows1};
                                                    _ ->
                                                        {Rows,[]}
                                                end;
                                           true ->
                                                {Rows,[]}
                                        end,
                                    case Rmdr of 
                                        [] ->
                                            {{final,Ret},[]};
                                        _ ->
                                            {Ret,Rmdr}
                                    end     
                            end,
                        #erbdrv{status=ok,stmt={Query,Params,StmtRows},data=RetRows}
                end ).
                      
finish( Props, {Query,_,_} ) ->
    on_success( Props, finish, #erbdrv{status=ok,stmt={Query,[],undefined}} ).

%----------------------------------------------------
% erbi_temp_db API
%-----------------------------------------------------
-define(PORT_FILE,"tmp_db.port").
-define(MIN_PORT, 8888).
-define(MAX_PORT, 9888).
-define(POSSIBLE_BIN_DIRS,[]).

-spec start_temp(ErbiDataSource::erbi_data_source())->
    ok.
start_temp(#erbi{properties=PropList})->
    PathData = proplists:get_value(data_dir,PropList),
    os:cmd("mkdir -p "++PathData),
    {ok, Port}=erbi_temp_db_helpers:get_free_db_port(?MIN_PORT,?MAX_PORT),
    ok = erbi_temp_db_helpers:save_in_db_data_file(Port,PathData,?PORT_FILE),
    ok.

-spec stop_temp(ErbiDataSource::erbi_data_source())->
    ok.
stop_temp(#erbi{properties=PropList})->
    PathData = proplists:get_value(data_dir,PropList),
    _Port = erbi_temp_db_helpers:read_from_db_data_file(PathData,?PORT_FILE),
    ok = erbi_temp_db_helpers:del_data_dir(PathData),
    ok.

-spec get_temp_connect_data(ErbiDataSource::erbi_data_source(),
                                Username::unicode:chardata(),
                                Password::unicode:chardata())->
    {erbi_data_source(),
     unicode:chardata(),
     unicode:chardata()}.
get_temp_connect_data(ErbiDataSource,_UserName,_Password)->
    {ErbiDataSource,
     "",
     ""}.


%% -----------------------------------
%% @doc get bind parameters passed in
%%
%% example:
%% <pre>
%%  {ok,Params} = erbi_statement:driver_call(Statement,params,[])
%% </pre>
%% -----------------------------------
params( _Props, {_Q,Params,_} ) ->
    #erbdrv{status=ok,data=Params}.



%% ---
%% Internal functions
%% ---

on_success( Props, Ops, OnSuccess ) when is_list(Ops) ->
    SV = lists:foldl(fun(Op,fallthrough) ->
                             proplists:get_value(Op,Props);
                        (_,V) -> V
                     end,fallthrough,Ops++[default]),
    on_success(SV,OnSuccess);
on_success( Props, Op, OnSuccess ) ->
    on_success( Props, [Op], OnSuccess ).
on_success( SuccessVal, OnSuccess ) ->
    case SuccessVal of
        success ->
            if is_function(OnSuccess) ->
                    OnSuccess();
               true ->
                    OnSuccess
            end;
        declined ->
            declined;
        Reason ->
            #erbdrv{ status = error, data = Reason }
    end.

match_query(Props,QStr) ->
    QueryList = proplists:get_value(queries,Props),
    Found = 
        lists:foldl( fun({Rgx,C,R},none) ->
                             case re:run(QStr,Rgx) of
                                 {match,_} -> {C,R};
                                 _ -> none
                             end;
                        (_,{C,R}) -> {C,R}
                     end, none, QueryList ),
    {C,R} = case Found of
                none ->
                    {[],[]};
                _ -> Found
            end,
    {C,R}.
    
