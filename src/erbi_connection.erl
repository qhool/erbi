%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(erbi_connection).

-opaque conn_private() :: #conn{}.
-export_type([conn_private/0]).

-export([prepare/2,
         prepare_cached/2,
         do/2, do/3,
         selectall_list/2, selectall_list/3,
         selectall_proplist/2, selectall_proplist/3,
         selectall_dict/2, selectall_dict/3,
         seletrow_list/2, seletrow_list/3,
         seletrow_proplist/2, seletrow_proplist/3,
         seletrow_dict/2, seletrow_dict/3,
         begin_work/1, begin_work/2,
         commit/1,
         rollback/1, rollback/2,
         disconnect/1
        ]).

-include("erbi.hrl").

%% --------------------------------------
%% @doc Prepare query/statement for execution.
%%
%% Parses statement and returns a handle which can be used to execute and retrieve rows.
%% @end
%% --------------------------------------
-spec prepare( Connection :: erbi_connection(),
               Statement :: any() ) -> 
                     { ok, erbi_statement() } | { error, any() }.
prepare(Connection,Statement) ->
    {error,"not implemented"}.

%% --------------------------------------
%% @doc Prepare with cacheing.
%%
%% This function is the same as prepare/2 except if the statement has been previously
%% prepared on this connection, a cached statement handle may be returned.
%% @end
%% --------------------------------------
-spec prepare_cached( Connection :: erbi_connection(),
                      Statement :: any() ) -> 
                            { ok, erbi_statement() } | { error, any() }.
prepare_cached(Connection,Statement) ->
    {error,"not implemented"}.

%% --------------------------------------
%% @doc Execute statement which does not return data.
%%
%% Returns count of records affected.
%% @end
%% --------------------------------------
-spec do( Connection :: erbi_connection(),
          Statement :: any(),
          BindValues :: erbi_bind_values() ) ->
                { ok, non_neg_integer() } | { error, any() }.
do( Connection, Statement, BindValues ) ->
    {error,"not implemented"}.

%% @doc
%%
%% Same as do(Connection,Statement,[])
-spec do( Connection :: erbi_connection(),
          Statement :: any() ) ->
                { ok, non_neg_integer() } | { error, any() }.
do( Connection, Statement ) ->
    {error,"not implemented"}.



%% --------------------------------------
%% @doc Execute query with bind values and return all records as lists.
%% 
%% Results are returned as a list of lists.  Each list contains the values, 
%% in order for that row. This is a convenience method, equivalent to:
%% <pre>
%% Stmt = Connection:prepare( Statement ),
%% Result = Stmt:fetchall_list( BindValues ),
%% Stmt:finish(),
%% Result
%% </pre>
%% @end
%% --------------------------------------
-spec selectall_list( Connection :: erbi_connection(),
                      Statement :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, [[any()]] } | { error, any() }.
selectall_list( Connection, Statement, BindValues ) ->
    {error,"not implemented"}.

%% @doc Execute and return lists.
%%
%% See selectall_list/3 for details.
-spec selectall_list( Connection :: erbi_connection(),
                      Statement :: any() ) ->
                            { ok, [[any()]] } | { error, any() }.
selectall_list( Connection, Statement ) ->
    {error,"not implemented"}.

%% @doc Execute and return proplists.
%%
%% See selectall_proplist/3.
-spec selectall_proplist( Connection :: erbi_connection(),
                          Statement :: any() ) ->
                            { ok, [[{atom(),any()}]] } | { error, any() }.
selectall_proplist( Connection, Statement ) ->
    {error,"not implemented"}.

%% @doc Execute query with bind values and return all records as proplists.
%%
%% Each returned record is a proplist; otherwise same as selectall_list/3.
-spec selectall_proplist( Connection :: erbi_connection(),
                          Statement :: any(),
                          BindValues :: erbi_bind_values() ) ->
                                { ok, [[{atom(),any()}]] } | { error, any() }.
selectall_proplist( Connection, Statement, BindValues ) ->
    {error,"not implemented"}.


%% @doc Execute and return dicts
%%
%% See selectall_dict/3.
-spec selectall_dict( Connection :: erbi_connection(),
                      Statement :: any() ) ->
                            { ok, [dict:dict()] } | { error, any() }.
selectall_dict( Connection, Statement ) ->
    {error,"not implemented"}.

%% @doc Execute query with bind values and return all records as dicts.
%%
%% Each record is returned as a dictionary created by the standard "dict" module.
%% See selectall_list/3 for more information.
-spec selectall_dict( Connection :: erbi_connection(),
                      Statement :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, [dict:dict()] } | { error, any() }.
selectall_dict( Connection, Statement, BindValues ) ->
    {error,"not implemented"}.

%% --------------------------------------
%% @doc Execute Statement and return one record.  
%% Statement is closed and remaining data discarded.
%%   Connection
%%   Statement   - Statement to execute
%%   BindValues  - [Optional] parameters.
%% @end
%% --------------------------------------
-spec selectrow_list( Connection :: erbi_connection(),
                      Statement :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, [any()] } | { error, any() }.
selectrow_list( Connection, Statement, BindValues ) ->
    {error,"not implemented"}.

-spec selectrow_list( Connection :: erbi_connection(),
                      Statement :: any() ) ->
                            { ok, [any()] } | { error, any() }.
selectrow_list( Connection, Statement ) ->
    {error,"not implemented"}.

-spec selectrow_proplist( Connection :: erbi_connection(),
                          Statement :: any() ) ->
                            { ok, [{atom(),any()}] } | { error, any() }.
selectrow_proplist( Connection, Statement ) ->
    {error,"not implemented"}.

-spec selectrow_proplist( Connection :: erbi_connection(),
                          Statement :: any(),
                          BindValues :: erbi_bind_values() ) ->
                                { ok, [{atom(),any()}] } | { error, any() }.
selectrow_proplist( Connection, Statement, BindValues  ) ->
    {error,"not implemented"}.

-spec selectrow_dict( Connection :: erbi_connection(),
                      Statement :: any() ) ->
                            { ok, dict:dict() } | { error, any() }.
selectrow_dict( Connection, Statement ) ->
    {error,"not implemented"}.

-spec selectrow_dict( Connection :: erbi_connection(),
                      Statement :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, dict:dict() } | { error, any() }.
selectrow_dict( Connection, Statement, BindValues  ) ->
    {error,"not implemented"}.

%% --------------------------------------
%% @doc Begins a transaction, or adds a save-point.
%%   Connection
%%   SavePoint  - [optional] name of Save-point
%% @end
%% --------------------------------------
-spec begin_work( Connection :: erbi_connection() ) -> ok | { error, any() }.
begin_work( Connection ) ->
    {error,"not implemented"}.

-spec begin_work( Connection :: erbi_connection(), 
                  SavePoint :: erbi_identifier() ) -> ok | { error, any() }.
begin_work( Connection, SavePoint ) ->
    {error,"not implemented"}.

%% --------------------------------------
%% @doc Completes the current transaction; all changes are written to the database.
%%   Connection
%% @end
%% --------------------------------------
-spec commit( Connection :: erbi_connection() ) -> ok | { error, any() }.
commit( Connection ) ->
    {error,"not implemented"}.

%% --------------------------------------
%% @doc Undoes all changes made during the transaction.
%% If savepoint is given, undoes changes after that savepoint.
%%   Connection
%%   SavePoint  - [optional] 
%% @end
%% --------------------------------------
-spec rollback( Connection :: erbi_connection() ) -> ok | { error, any() }.
rollback( Connection ) ->
    {error,"not implemented"}.

-spec rollback( Connection :: erbi_connection(),
                SavePoint :: erbi_identifier() ) -> ok | { error, any() }.
rollback( Connection, SavePoint ) ->
    {error,"not implemented"}.

%% --------------------------------------
%% @doc Close database connection
%% @end
%% --------------------------------------
-spec disconnect( Connection :: erbi_connection() ) -> ok | { error, any() }.
disconnect( Connection ) ->
    {error,"not implemented"}.


%%==== Internals ====%%
-record(conn,
        {}).
