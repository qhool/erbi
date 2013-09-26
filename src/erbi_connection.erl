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
%% @doc Prepare query/statement
%%   Connection  - Connection handle
%%   Statement   - Statement to parse
%% @end
%% --------------------------------------
-spec prepare( Connection :: erbi_connection(),
               Statement :: any() ) -> 
                     { ok, erbi_statement() } | { error, any() }.

%% --------------------------------------
%% @doc Same as prepare/2 except if the statement has been previously
%% prepared on this connection, a cached statement handle may be returned.
%%   Connection  - Connection handle
%%   Statement   - Statement to parse
%% @end
%% --------------------------------------
-spec prepare_cached( Connection :: erbi_connection(),
                      Statement :: any() ) -> 
                            { ok, erbi_statement() } | { error, any() }.

%% --------------------------------------
%% @doc Execute statement not returning data.
%%   Connection
%%   Statement    - Statement to execute
%%   BindValues   - [Optional] parameters to insert
%% Returns count of records affected.
%% @end
%% --------------------------------------
-spec do( Connection :: erbi_connection(),
          Statement :: any() ) ->
                { ok, non_neg_integer() } | { error, any() }.
-spec do( Connection :: erbi_connection(),
          Statement :: any(),
          BindValues :: erbi_bind_values() ) ->
                { ok, non_neg_integer() } | { error, any() }

%% --------------------------------------
%% @doc Execute Statement and return all results.
%%   Connection
%%   Statement   - Statement to execute
%%   BindValues  - [Optional] parameters.
%% @end
%% --------------------------------------
-spec selectall_list( Connection :: erbi_connection(),
                      Statement :: any() ) ->
                            { ok, [[any()]] } | { error, any() }.
-spec selectall_list( Connection :: erbi_connection(),
                      Statement :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, [[any()]] } | { error, any() }.

-spec selectall_proplist( Connection :: erbi_connection(),
                          Statement :: any() ) ->
                            { ok, [[{atom(),any()}]] } | { error, any() }.
-spec selectall_proplist( Connection :: erbi_connection(),
                          Statement :: any(),
                          BindValues :: erbi_bind_values() ) ->
                                { ok, [[{atom(),any()}]] } | { error, any() }.

-spec selectall_dict( Connection :: erbi_connection(),
                      Statement :: any() ) ->
                            { ok, [dict:dict()] } | { error, any() }.
-spec selectall_dict( Connection :: erbi_connection(),
                      Statement :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, [dict:dict()] } | { error, any() }.

%% --------------------------------------
%% @doc Execute Statement and return one record.  
%% Statement is closed and remaining data discarded.
%%   Connection
%%   Statement   - Statement to execute
%%   BindValues  - [Optional] parameters.
%% @end
%% --------------------------------------
-spec selectrow_list( Connection :: erbi_connection(),
                      Statement :: any() ) ->
                            { ok, [any()] } | { error, any() }.
-spec selectrow_list( Connection :: erbi_connection(),
                      Statement :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, [any()] } | { error, any() }.

-spec selectrow_proplist( Connection :: erbi_connection(),
                          Statement :: any() ) ->
                            { ok, [{atom(),any()}] } | { error, any() }.
-spec selectrow_proplist( Connection :: erbi_connection(),
                          Statement :: any(),
                          BindValues :: erbi_bind_values() ) ->
                                { ok, [{atom(),any()}] } | { error, any() }.

-spec selectrow_dict( Connection :: erbi_connection(),
                      Statement :: any() ) ->
                            { ok, dict:dict() } | { error, any() }.
-spec selectrow_dict( Connection :: erbi_connection(),
                      Statement :: any(),
                      BindValues :: erbi_bind_values() ) ->
                            { ok, dict:dict() } | { error, any() }.

%% --------------------------------------
%% @doc Begins a transaction, or adds a save-point.
%%   Connection
%%   SavePoint  - [optional] name of Save-point
%% @end
%% --------------------------------------
-spec begin_work( Connection :: erbi_connection() ) -> ok | { error, any() }.
-spec begin_work( Connection :: erbi_connection(), 
                  SavePoint :: erbi_identifier() ) -> ok | { error, any() }.
%% --------------------------------------
%% @doc Completes the current transaction; all changes are written to the database.
%%   Connection
%% @end
%% --------------------------------------
-spec commit( Connection :: erbi_connection() ) -> ok | { error, any() }.

%% --------------------------------------
%% @doc Undoes all changes made during the transaction.
%% If savepoint is given, undoes changes after that savepoint.
%%   Connection
%%   SavePoint  - [optional] 
%% @end
%% --------------------------------------
-spec rollback( Connection :: erbi_connection() ) -> ok | { error, any() }.
-spec rollback( Connection :: erbi_connection(),
                SavePoint :: erbi_identifier() ) -> ok | { error, any() }.

%% --------------------------------------
%% @doc Close database connection
%%   Connection
%% @end
%% --------------------------------------
-spec disconnect( Connection :: erbi_connection() ) -> ok | { error, any() }.


%%==== Internals ====%%
-record(conn,
        {}).
