%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%
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
%% @private
%% @doc
%% This module defines the erbi_driver behaviour, and provides
%% a private interface for use by drivers. 
%% @end

-module(erbi_driver).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3
        ]).

-include("erbi.hrl").

-include("erbi_driver.hrl").

-behaviour(gen_server).

%%-----------------------------------------------
%% TOP LEVEL DRIVER CALLS
%%-----------------------------------------------

%% @doc Return an erbi_driver_info record
%% @end
-callback driver_info() -> erbi_driver_info().

%% @doc Establish a new connection to the database
%% @end
-callback connect( DS :: erbi_data_source(),
                   Username :: string(),
                   Password :: string() ) -> erbdrv_return().

%%-----------------------------------------------
%% CONNECTION LEVEL DRIVER CALLS
%%-----------------------------------------------

%% @doc tear-down existing database connection.
%%
%% Regardless of the return, all existing handles 
%% associated with this connection are assuemed to be invalid after this call.
%% @end
-callback disconnect( Connection :: erbdrv_connection() ) -> 
    ok | {error, erbdrv_error()}.

%% @doc begin transaction or create savepoint
%%
%% Optional Savepoint parameter should create a named savepoint, if supported.
%% Should not return data.
%% @end
-callback begin_work( Connection :: erbdrv_connection() ) ->
    erbdrv_return().
-callback begin_work( Connection :: erbdrv_connection(),
                      Savepoint :: atom | string() ) ->
    erbdrv_return().

%% @doc undo changes
%%
%% Rollback to beginning of transaction, or to optional named savepoint.
%% @end
-callback rollback( Connection :: erbdrv_connection() ) -> 
    erbdrv_return().
 -callback rollback( Connection :: erbdrv_connection(),
                    Savepoint :: atom | string() ) ->  
    erbdrv_return().

%% @doc commit changes
%%
%% Finalize current transaction.
%% @end
-callback commit( Connection :: erbdrv_connection() ) ->
    erbdrv_return().

%% @doc parse query/statement
%%
%% Should parse the supplied query and return a handle to the pre-parsed form.  
%% If the driver does not support pre-parsing, this function will not be called;
%% instead, the statement string will be cached until {@link execute/3} is called.
%%
%% Column info can be returned in data section.
%% @end
-callback prepare( Connection :: erbdrv_connection(), Query :: string() ) -> erbdrv_return().

%%-----------------------------------------------
%% STATEMENT LEVEL DRIVER CALLS
%%-----------------------------------------------

%% @doc bind parameters to a statement
%%
%% Will only be called if the driver supports parameter binding.
%% Otherwise, params will be passed to {@link execute/3}.
%% @end
-callback bind_params( Connection :: erbdrv_connection(),
                       Statement :: erbdrv_statement(),
                       Params :: erbi_bind_values() ) ->
    erbdrv_return().


%% @doc execute statement
%% 
%% Begin execution of given statement handle, returned from {@link prepare/2}.
%%
%% Should return a statement handle, and may return column info and/or 
%% any rows which are ready immediately. If no rows are ready, that's fine --
%% the intent is only to prevent the driver from needing to implement caching of complete rows.
%% 
%% If driver does not indicate support for pre-parsing, statement will
%% be a string.  If driver doesn't support cursors, data will be assumed to contain all
%% rows; Neither {@link fetch_rows/3} nor {@link finish/2} will be called.
-callback execute( Connection :: erbdrv_connection(),
                   Statement :: erbdrv_statement() | string(),
                   Params :: erbi_bind_values() ) ->
    erbdrv_return().

%% @doc retrieve rows from open cursor
%%
%% Amount indicates how many rows are desired; if it is 'one' driver should 
%% request the minimal amount from the database, but the actual amount returned may
%% be more than one; likewise, if it is 'all', request a maximal amount, but the driver is not
%% assumed to have returned all rows.
%% @end
-callback fetch_rows( Connection :: erbdrv_connection(),
                      Statement :: erbdrv_statement(),
                      Amount :: one | all ) ->
    erbdrv_return().

%% @doc close open cursor
%%
%% Will only be called on cursor-supporting drivers.
%% @end
-callback finish( Connection :: erbdrv_connection(),
                 Statement :: erbdrv_statement() ) ->
    erbdrv_return().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Driver-connection gen-server. %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(connect_state,
        { module :: atom(),
          connection :: erbdrv_connection(),
          statements :: ets:tid(),
          info :: erbi_driver_info()
        }).

-spec init( {atom(),undefined|erbi_driver_info(),any(),any(),any()} ) ->
                  { stop, any() } | { ok, #connect_state{} }.

init({Module,Info,DataSource,Username,Password}) ->
    io:format( user, "erbi_driver:init~n", [] ),
    #erbdrv{status = Status} = Ret = Module:connect( DataSource, Username, Password ),
    io:format( user, "driver ret: ~n~p~n", [Ret] ),
    {State,_} = update_state( Ret, 
                              #connect_state{ module = Module, info = Info }, 
                              undefined ),
    io:format( user, "State: ~n~p~n", [State] ),
    case Status of
        ok -> 
            Tbl = erbi_stmt_store:init_store(),
            State1 = State#connect_state{ statements = Tbl },
            {ok,State1};
        declined ->
            %% ????? should never happen
            {stop,"Module declined connect"};
        error ->
            #erbdrv{data = Reason} = Ret,
            io:format( user, "Stop: ~p~n", [Reason] ),
            {stop,Reason}
    end.

% disconnect from driver
% statement cache should cleanup on its own when process terminates
-spec terminate(_,#connect_state{}) ->
                       any().
terminate(_Reason,State) ->
    call_driver(State,disconnect).

%% connection-level  
handle_call(begin_work, _From, State) ->
    proc_return(call_driver(State,begin_work), State,undefined);
handle_call({begin_work,Savepoint},_From, State) ->
    proc_return(call_driver(State,begin_work, Savepoint), State,undefined );
handle_call(rollback, _From, State) ->
    proc_return(call_driver(State,rollback), State,undefined);
handle_call({rollback,Savepoint}, _From, State) ->
    proc_return(call_driver(State,rollback, Savepoint ), State, undefined);
handle_call(commit,_From,State) ->
    proc_return(call_driver(State,commit), State, undefined);
handle_call({prepare,Query},_From,State) ->
    proc_return(call_driver(State,prepare,Query), State, undefined,
                % ok status
                fun opt_cols/3,
                % declined status -- driver does not support prepare; store the statement instead
                fun(#connect_state{ statements = Tbl }=State1,_,_) ->
                        StmtID = erbi_stmt_store:add_statement(Tbl,undefined),
                        erbi_stmt_store:set(Tbl,StmtID,raw_query,Query),
                        {reply,{ok,StmtID},State1}
                end
               );
%% statement-level
handle_call({bind,StmtID,Params},_From,#connect_state{statements=Tbl}=State) ->
    case get_stmt_handle(State,StmtID) of
        undefined -> % no handle, just store params
            erbi_stmt_store:set(Tbl,StmtID,params,Params),
            {reply,ok,State};
        Handle ->
            proc_return(call_driver(State,bind_params,Handle,Params), State, StmtID,
                        %ok:
                        fun opt_cols/3,
                        %declined:
                        fun(#connect_state{}=State1,StmtID1,_) ->
                                erbi_stmt_store:set(Tbl,StmtID1,params,Params),
                                {reply,ok,State1}
                        end)
    end;
handle_call({execute,StmtID,Params},_From,#connect_state{statements=Tbl}=State)
  when is_integer(StmtID) ->
    case get_stmt_handle(State,StmtID) of
        undefined -> % no handle; execute stored query text
            Query = erbi_stmt_store:get(Tbl,StmtID,raw_query),
            Params1 = Params ++ erbi_stmt_store:lookup(Tbl,StmtID,params,[]),
            proc_return(call_driver(State,execute,Query,Params1), State, StmtID,fun add_rows/3);
        Handle ->
            proc_return(call_driver(State,execute,Handle,Params),State,StmtID,fun add_rows/3)
    end;
handle_call({execute,Query,Params},_From,State) ->
    proc_return(call_driver(State,execute,Query,Params), State, undefined, fun add_rows/3);
handle_call({start_fetch,StatementID,Amount},_From,State) ->
    do_fetch(State,StatementID,Amount);
handle_call({continue_fetch,StatementID,Amount,RowsRead},_From,#connect_state{statements=Tbl}=State) ->
    erbi_stmt_store:incr(Tbl,StatementID,current,RowsRead),
    do_fetch(State,StatementID,Amount);
handle_call({end_fetch,StatementID,RowsRead},_From,#connect_state{statements=Tbl}=State) ->
    erbi_stmt_store:incr(Tbl,StatementID,current,RowsRead),
    {reply,ok,State};
handle_call({finish,StatementID},_From,#connect_state{statements=Tbl}=State) ->
    erbi_stmt_store:reset_statement(Tbl,StatementID),
    case erbi_stmt_store:reset_statement(Tbl,StatementID) of
        undefined -> {reply,ok,State};
        Handle -> proc_return(call_driver(State,finish,Handle),State,StatementID)
    end;
handle_call(reset,_From,#connect_state{statements=Tbl}=State) ->
    IDHandles = erbi_stmt_store:reset_all(Tbl),
    lists:foldl( fun(_,{stop,Reason}) ->
                         {stop,Reason}; %after one error, just shut the whole thing down
                    ({ID,H},{reply,_,State1}) ->
                         proc_return(call_driver(State1,finish,H),State1,ID)
                 end, {reply,ok,State}, IDHandles ).

handle_cast(_,State) ->
    {noreply,State}.

handle_info(_,State) ->
    {noreply,State}.

code_change(_OldVsn, _State, _Extra) ->
    {error, i_cant_do_that}.
    
%% row fetching
%% if there are already rows in the buffer, just pass back the counters and the ets table
%% if there are no cached rows, fetch from the driver
do_fetch( #connect_state{statements=Tbl} = State,StatementID,Amount) -> 
    case erbi_stmt_store:counters( Tbl, StatementID ) of
        #erbdrv_stmt_counters{current=C,last=L,is_final=F} when ((not F) and (C > L)) ->
            Handle = get_stmt_handle(State,StatementID),
            proc_return(call_driver(State,fetch_rows,Handle,Amount),State,StatementID,fun add_rows/3);
        Counters -> {reply,{ok,Counters,Tbl},State}
    end.
opt_cols( State,StmtID,undefined ) ->
    {reply,{ok,StmtID},State};
opt_cols( #connect_state{statements=Tbl}=State,StmtID,Cols ) ->
    erbi_stmt_store:set_cols(Tbl,StmtID,Cols),
    {reply,{ok,StmtID},State}.

add_rows( #connect_state{statements=Tbl}=State,StmtID,
          {Cols,Rows} ) when is_list(Cols)->
    erbi_stmt_store:set_cols(Tbl,StmtID,Cols),
    add_rows(State,StmtID,Rows);
add_rows( #connect_state{statements=Tbl}=State,StmtID,
          [#erbdrv_field{}|_]=Cols ) ->
    erbi_stmt_store:set_cols(Tbl,StmtID,Cols),
    Counters = erbi_stmt_store:counters(Tbl,StmtID),
    {reply,{ok,Counters,Tbl},State};
add_rows( #connect_state{statements=Tbl}=State,StmtID,final ) ->
    erbi_stmt_store:set( Tbl, StmtID, is_final, true ),
    Counters = erbi_stmt_store:counters(Tbl,StmtID),
    {reply,{ok,Counters,Tbl},State};
add_rows( #connect_state{statements=Tbl}=State,StmtID,{final,Rows} ) ->
    erbi_stmt_store:set( Tbl, StmtID, is_final, true ),
    add_rows(State,StmtID,Rows);
add_rows( #connect_state{statements=Tbl}=State,StmtID,Rows ) ->
    {ok,Counters,Tbl} = erbi_stmt_store:add_rows(Tbl,StmtID,Rows),
    {reply,{ok,Counters,Tbl},State}.
        
get_stmt_handle(#connect_state{statements=Tbl},StmtID) ->
    erbi_stmt_store:get(Tbl,StmtID,handle).

%% simple function just to keep all the handle-call clauses a bit cleaner
call_driver( #connect_state{ module = Module, connection = Conn }, Function ) ->
    Module:Function(Conn).
call_driver( #connect_state{ module = Module, connection = Conn }, Function, Arg1 ) ->
    Module:Function(Conn,Arg1).
call_driver( #connect_state{ module = Module, connection = Conn }, Function, Arg1, Arg2 ) ->
     Module:Function(Conn,Arg1,Arg2).

%% Drivers are allowed to update connection & statement handles, so    
%% the following functions first update those in the state, then call         
%% the supplied continuation[s].            
proc_return( DriverReturn, State, StmtID ) ->
    proc_return( DriverReturn, State, StmtID, fun standard_on_ok/3, fun standard_on_declined/3, fun standard_on_error/3 ).
proc_return( DriverReturn, State, StmtID, OnOK ) ->
    proc_return( DriverReturn, State, StmtID, OnOK, fun standard_on_declined/3, fun standard_on_error/3 ).
proc_return( DriverReturn, State, StmtID, OnOK, OnDeclined ) ->
    proc_return( DriverReturn, State, StmtID, OnOK, OnDeclined, fun standard_on_error/3 ).
proc_return( declined, State, StmtID, _, OnDeclined, _ ) ->
    OnDeclined( State, StmtID, undefined );
proc_return( #erbdrv{ status = Status, data = Data} = DriverReturn, State, StmtID, OnOK, OnDeclined, OnError ) ->
    {State1,StmtID1} = update_state( DriverReturn, State, StmtID ),
    case Status of
        ok -> OnOK( State1, StmtID1, Data );
        error -> OnError( State1, StmtID1, Data );
        declined -> OnDeclined( State1, StmtID1, Data )
    end.

%% takes an #erbdrv{} return record and updates the state information as needed
update_state( #erbdrv{ conn = NewConn, stmt = Stmt },
              #connect_state{statements=Tbl}=State, StmtID ) ->
    State1 = case NewConn of 
                 same ->
                     State;
                 _ ->
                     State#connect_state{ connection = NewConn }
             end,
    StmtID1 = 
        case {StmtID,Stmt} of
            {undefined,undefined} -> StmtID;
            {_,same} -> StmtID;
            {ID,Handle} when ID =:= new ; Handle =/= undefined -> %new statement
                erbi_stmt_store:add_statement( Tbl, Handle );
            {ID,NewHandle} when is_integer(ID) ->
                true = erbi_stmt_store:set( Tbl, ID, handle, NewHandle ),
                case NewHandle of
                    undefined ->
                        erbi_stmt_store:set( Tbl, ID, is_final, true );
                    _ -> true
                end,
                StmtID
        end,
    {State1,StmtID1}.
  

standard_on_ok( State2, _StmtID, nothing ) -> 
    {reply,ok,State2}; 
standard_on_ok( State2, _StmtID, Data1 ) -> 
    {reply,{ok,Data1},State2}.
standard_on_declined( State2, _StmtID, _Data ) -> 
    {reply,{error,driver_declined},State2}.
standard_on_error( State2, _StmtID, Reason ) -> 
    {reply,{error,Reason},State2}.
                    
