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
%% Functions for manipulating erbi connection state, and db convenience functions.
%% @end
-module(erbi_connection).

-include("erbi_private.hrl").

-export([prepare/2,
         prepare_cached/2,
         do/2, do/3,
         selectall_list/2, selectall_list/3,
         selectall_proplist/2, selectall_proplist/3,
         selectall_dict/2, selectall_dict/3,
         selectrow_list/2, selectrow_list/3,
         selectrow_proplist/2, selectrow_proplist/3,
         selectrow_dict/2, selectrow_dict/3,
         begin_work/1, begin_work/2,
         commit/1,
         rollback/1, rollback/2,
         disconnect/1,
         driver_call/3
        ]).

-include("erbi.hrl").
%% @headerfile "erbi.hrl"

%% --------------------------------------
%% @doc Prepare query/statement for execution.
%%
%% Parses statement and returns a handle which can be used to execute and retrieve rows.
%% @end
%% --------------------------------------
-spec prepare( Connection :: erbi_connection(),
               Query :: any() ) ->
                     { ok, erbi_statement() } | { error, any() }.
prepare({erbi_connection,Conn}=Connection,Query) ->
    erbi_driver:call(Connection,{prepare,Query},
                     fun({ok,StmtID}) ->
                             {ok,{erbi_statement,Conn,#stmt{ id = StmtID }}}
                     end).

%% --------------------------------------
%% @doc Prepare with cacheing.
%%
%% This function is the same as prepare/2 except if the statement has been previously
%% prepared on this connection, a cached statement handle may be returned.
%% @end
%% --------------------------------------
-spec prepare_cached( Connection :: erbi_connection(),
                      Query :: any() ) ->
                            { ok, erbi_statement() } | { error, any() }.
prepare_cached(_Connection,_Query) ->
    {error,"not implemented"}.

%% --------------------------------------
%% @doc Execute statement which does not return data.
%%
%% Returns count of records affected.
%% @end
%% --------------------------------------
-spec do( Connection :: erbi_connection(),
          Query :: any(),
          BindValues :: erbi_bind_values() ) ->
                { ok, erbi_row_count() } | { error, any() }.
do( Connection, Query, BindValues ) ->
    erbi_driver:call(Connection,{do,Query,BindValues}).

%% @doc
%%
%% Same as do(Connection,Query,[])
-spec do( Connection :: erbi_connection(),
          Query :: any() ) ->
                { ok, erbi_row_count() } | { error, any() }.
do( Connection, Query ) ->
    do( Connection, Query, [] ).

%% --------------------------------------
%% @doc Execute query with bind values and return all records as lists.
%%
%% Results are returned as a list of lists.  Each list contains the values,
%% in order for that row. This is a convenience method, equivalent to:
%% <pre>
%% Stmt = Connection:prepare( Query ),
%% Result = Stmt:fetchall_list( BindValues ),
%% Stmt:finish(),
%% Result
%% </pre>
%% @end
%% --------------------------------------
-spec selectall_list( Connection :: erbi_connection(),
                      Query :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, [[any()]] } | { error, any() }.
selectall_list( Connection, Query, BindValues ) ->
    prep_exec_and(fetchall_list,Connection,Query,BindValues).

%% @doc Execute and return lists.
%%
%% See selectall_list/3 for details.
-spec selectall_list( Connection :: erbi_connection(),
                      Query :: any() ) ->
                            { ok, [[any()]] } | { error, any() }.
selectall_list( Connection, Query ) ->
    selectall_list(Connection,Query,[]).

%% @doc Execute and return proplists.
%%
%% See selectall_proplist/3.
-spec selectall_proplist( Connection :: erbi_connection(),
                          Query :: any() ) ->
                            { ok, [[{atom(),any()}]] } | { error, any() }.
selectall_proplist( Connection, Query ) ->
    selectall_proplist(Connection,Query,[]).

%% @doc Execute query with bind values and return all records as proplists.
%%
%% Each returned record is a proplist; otherwise same as selectall_list/3.
-spec selectall_proplist( Connection :: erbi_connection(),
                          Query :: any(),
                          BindValues :: erbi_bind_values() ) ->
                                { ok, [[{atom(),any()}]] } | { error, any() }.
selectall_proplist( Connection, Query, BindValues ) ->
    prep_exec_and(fetchall_proplist,Connection,Query,BindValues).

%% @doc Execute and return dicts
%%
%% See selectall_dict/3.
-spec selectall_dict( Connection :: erbi_connection(),
                      Query :: any() ) ->
                            { ok, [dict:dict()] } | { error, any() }.
selectall_dict( Connection, Query ) ->
    selectall_dict(Connection,Query,[]).

%% @doc Execute query with bind values and return all records as dicts.
%%
%% Each record is returned as a dictionary created by the standard "dict" module.
%% See selectall_list/3 for more information.
-spec selectall_dict( Connection :: erbi_connection(),
                      Query :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, [dict:dict()] } | { error, any() }.
selectall_dict( Connection, Query, BindValues ) ->
    prep_exec_and(fetchall_dict,Connection,Query,BindValues).

%% --------------------------------------
%% @doc Execute Query and return one record.
%% Query is closed and remaining data discarded.
%%   Connection
%%   Query   - Query to execute
%%   BindValues  - [Optional] parameters.
%% @end
%% --------------------------------------
-spec selectrow_list( Connection :: erbi_connection(),
                      Query :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, [any()] } | { error, any() }.
selectrow_list( Connection, Query, BindValues ) ->
    prep_exec_and(fetchrow_list,Connection,Query,BindValues).

-spec selectrow_list( Connection :: erbi_connection(),
                      Query :: any() ) ->
                            { ok, [any()] } | { error, any() }.
selectrow_list( Connection, Query ) ->
    selectrow_list( Connection, Query, []).

-spec selectrow_proplist( Connection :: erbi_connection(),
                          Query :: any() ) ->
                            { ok, [{atom(),any()}] } | { error, any() }.
selectrow_proplist( Connection, Query ) ->
    selectrow_proplist( Connection, Query, [] ).

-spec selectrow_proplist( Connection :: erbi_connection(),
                          Query :: any(),
                          BindValues :: erbi_bind_values() ) ->
                                { ok, [{atom(),any()}] } | { error, any() }.
selectrow_proplist( Connection, Query, BindValues  ) ->
    prep_exec_and(fetchrow_proplist,Connection,Query,BindValues).


-spec selectrow_dict( Connection :: erbi_connection(),
                      Query :: any() ) ->
                            { ok, dict:dict() } | { error, any() }.
selectrow_dict( Connection, Query ) ->
    selectrow_dict( Connection, Query, [] ).

-spec selectrow_dict( Connection :: erbi_connection(),
                      Query :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, dict:dict() } | { error, any() }.
selectrow_dict( Connection, Query, BindValues  ) ->
    prep_exec_and(fetchrow_dict,Connection,Query,BindValues).

%% --------------------------------------
%% @doc Begins a transaction, or adds a save-point.
%%   Connection
%%   SavePoint  - [optional] name of Save-point
%% @end
%% --------------------------------------
-spec begin_work( Connection :: erbi_connection() ) -> ok | { error, any() }.
begin_work( Connection ) ->
    erbi_driver:call(Connection,begin_work).

-spec begin_work( Connection :: erbi_connection(),
                  SavePoint :: erbi_identifier() ) -> ok | { error, any() }.
begin_work( Connection, SavePoint ) ->
    erbi_driver:call(Connection,{begin_work,SavePoint}).

%% --------------------------------------
%% @doc Completes the current transaction; all changes are written to the database.
%%   Connection
%% @end
%% --------------------------------------
-spec commit( Connection :: erbi_connection() ) -> ok | { error, any() }.
commit( Connection ) ->
    erbi_driver:call(Connection,commit).

%% --------------------------------------
%% @doc Undoes all changes made during the transaction.
%% If savepoint is given, undoes changes after that savepoint.
%%   Connection
%%   SavePoint  - [optional]
%% @end
%% --------------------------------------
-spec rollback( Connection :: erbi_connection() ) -> ok | { error, any() }.
rollback( Connection ) ->
    erbi_driver:call(Connection,rollback).

-spec rollback( Connection :: erbi_connection(),
                SavePoint :: erbi_identifier() ) -> ok | { error, any() }.
rollback( Connection, SavePoint ) ->
    erbi_driver:call(Connection,{rollback,SavePoint}).

%% --------------------------------------
%% @doc Close database connection
%% @end
%% --------------------------------------
-spec disconnect( Connection :: erbi_connection() ) -> ok | { error, any() }.
disconnect( {erbi_connection,#conn{ pooled = true }} = Conn ) ->
    erbi_pool:checkin(Conn);
disconnect( {erbi_connection,#conn{pid=Pid, pooled = false}} ) ->
    gen_server:cast(Pid,disconnect).

%% --------------------------------------
%% @doc Call arbitrary driver function
%%
%% This function calls an arbitrary function in the driver module.
%% Similar to {@link erbi:call_driver/3} and {@link erbi_statement:call_driver/3},
%% this version prepends the driver's connection object to the argument list you supply,
%% and allows the driver to update its state information.
%%
%% See the documentation for your driver for what functions are available
%% and how to use them.
%% @end
%% --------------------------------------
-spec driver_call( Connection :: erbi_connection(),
                   Function :: atom(),
                   Args :: [any()] ) ->
                         any().
driver_call( Connection, Function, Args ) ->
    erbi_driver:call(Connection,{driver_call,Function,Args}).

%%==== Internals ====%%
prep_exec_and(FuncName,Connection,Query,BindValues) ->
    case prepare(Connection,Query) of
        {error,_}=E -> E;
        {ok,Statement} ->
            case erbi_statement:execute(Statement,BindValues) of
                {error,_}=E -> E;
                {ok,_Rows} ->
                    Ret = erbi_statement:FuncName(Statement),
                    erbi_statement:finish(Statement),
                    Ret
            end
    end.

