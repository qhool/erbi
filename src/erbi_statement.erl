%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(erbi_statement).

-opaque stmt_private() :: #stmt{}.
-export_type([stmt_private/0]).

-export([]).

-include("erbi.hrl").

%% --------------------------------------
%% @doc Bind given parameters to this statement.
%% May be supplied as a positional list, or by name (contingent on driver support).
%%   Statement  - erbi statement handle
%%   BindValues - List of parameter values 
%% @end
%% --------------------------------------
-spec bind_params( Statement :: erbi_statement(),
                   Params :: erbi_bind_values() ) ->
                         ok | { error, any() }.
%% --------------------------------------
%% @doc Begin execution of this statement
%%   Statement
%%   BindValues - [optional] parameter values
%% @end
%% --------------------------------------
-spec execute( Statement :: erbi_statement() ) -> ok | { error, any() }.
-spec execute( Statement :: erbi_statement(),
               Params :: erbi_bind_values() ) ->
                     ok | { error, any() }.

%% --------------------------------------
%% @doc Fetch a single record from the result set.
%%   Statement
%% @end
%% --------------------------------------
-spec fetchrow_list( Statement :: erbi_statement() ) ->
                           { ok, [any()] } | { error, any() }.
-spec fetchrow_proplist( Statement :: erbi_statement() ) ->
                           { ok, [{atom(),any()}] } | { error, any() }.
-spec fetchrow_dict( Statement :: erbi_statement() ) ->
                           { ok, dict:dict() } | { error, any() }.

%% --------------------------------------
%% @doc Complete processing with this statement.
%% Closes associated cursor.
%%   Statement
%% @end
%% --------------------------------------
-spec finish( Statement :: erbi_statement() ) ->
                    ok | {error, any()}

%% --------------------------------------
%% @doc Fetch all remaining records and return a list.
%%   Statement
%% @end
%% --------------------------------------
-spec fetchall_list( Statement :: erbi_statement() ) ->
                           { ok, [[any()]] } | { error, any() }.
-spec fetchall_proplist( Statement :: erbi_statement() ) ->
                           { ok, [[{atom(),any()}]] } | { error, any() }.
-spec fetchall_dict( Statement :: erbi_statement() ) ->
                           { ok, [dict:dict()] } | { error, any() }.


%%==== Internals ====%%
-record(stmt,
        {}).
