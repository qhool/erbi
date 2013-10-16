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
%% Erbi statement handle functions: perform queries, fetch rows, etc. 
%% @end 
-module(erbi_statement).
-record(stmt,
        {}).
-opaque stmt_private() :: #stmt{}.
-export_type([stmt_private/0]).

-export([bind_params/2,
         execute/1, execute/2,
         fetchrow_list/1,
         fetchrow_proplist/1,
         fetchrow_dict/1,
         finish/1,
         fetchall_list/1,
         fetchall_proplist/1,
         fetchall_dict/1
        ]).

-include("erbi.hrl").

%% --------------------------------------
%% @doc Bind given parameters to this statement.
%% May be supplied as a positional list, or by name (contingent on driver support).
%% - Statement  - erbi statement handle
%% - BindValues - List of parameter values 
%% @end
%% --------------------------------------
-spec bind_params( Statement :: erbi_statement(),
                   Params :: erbi_bind_values() ) ->
                         ok | { error, any() }.
bind_params( Statement, Params ) ->
    { error, "not implemented" }.

%% --------------------------------------
%% @doc Begin execution of this statement
%% @end
%% --------------------------------------
-spec execute( Statement :: erbi_statement() ) -> {ok,erbi_row_count()} | { error, any() }.
execute( Statement ) ->
    { error, "not implemented" }.

-spec execute( Statement :: erbi_statement(),
               Params :: erbi_bind_values() ) ->
                     {ok,erbi_row_count()} | { error, any() }.
execute( Statement, Params ) ->
    { error, "not implemented" }.

%% --------------------------------------
%% @doc Fetch a single record from the result set.
%% @end
%% --------------------------------------
-spec fetchrow_list( Statement :: erbi_statement() ) ->
                           { ok, [any()] } | exhausted | { error, any() }.
fetchrow_list(Statement) ->
    { error, "not implemented" }.

-spec fetchrow_proplist( Statement :: erbi_statement() ) ->
                           { ok, [{atom(),any()}] } | exhausted | { error, any() }.
fetchrow_proplist(Statement) ->
    { error, "not implemented" }.

-spec fetchrow_dict( Statement :: erbi_statement() ) ->
                           { ok, dict() } | exhausted | { error, any() }.
fetchrow_dict(Statement) ->
    { error, "not implemented" }.

%% --------------------------------------
%% @doc Complete processing with this statement.
%% Closes associated cursor.
%% @end
%% --------------------------------------
-spec finish( Statement :: erbi_statement() ) ->
                    ok | {error, any()}.
finish(Statement) ->
    { error, "not implemented" }.


%% --------------------------------------
%% @doc 
%% Fetch all remaining records and return a list.
%% @end
%% --------------------------------------
-spec fetchall_list( Statement :: erbi_statement() ) ->
                           { ok, [[any()]] } | { error, any() }.
fetchall_list(Statement) ->
    { error, "not implemented" }.

-spec fetchall_proplist( Statement :: erbi_statement() ) ->
                           { ok, [[{atom(),any()}]] } | { error, any() }.
fetchall_proplist(Statement) ->
    { error, "not implemented" }.

-spec fetchall_dict( Statement :: erbi_statement() ) ->
                           { ok, [dict()] } | { error, any() }.
fetchall_dict(Statement) ->
    { error, "not implemented" }.



%%==== Internals ====%%

