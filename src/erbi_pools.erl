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
%% Wrap around module to hide away all details of poolboy interaction and
%% supervision that erbi provides for the pools.
%% @end
-module(erbi_pools).

-include("erbi.hrl").
-include("erbi_private.hrl").

-define(POOL_PROPS,[pool_name,pool_size,pool_max_overflow]).
-define(DEFAULT_CHECKOUT_TIMEOUT, 5000).

%% API
-export([
    start_pool/3,
    checkout/1,
    checkin/1,
    status/1,
    list_pool_names/0,
    scrape_pool_properties/1,
    validate_property/2,
    property_info/0
]).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec(start_pool(PoolName :: atom(), PoolArgs :: list(), WorkerArgs :: tuple()) ->
    {ok, Pid :: pid()} | {error, Reason :: term()}).
start_pool(PoolName, PoolArgs, WorkerArgs) ->
    case supervisor:start_child(
        erbi_sup,
        poolboy:child_spec(PoolName, [{name, {local, PoolName}},
            {worker_module, erbi_driver}] ++ PoolArgs, WorkerArgs)) of
        {ok, Pool} -> {ok, Pool};
        {error, {already_started,Pool}} -> {ok, Pool};
        {error, Reason} -> {error, Reason}
    end.

-spec checkout(PoolName :: atom() | string()) ->
    {ok, PooledConn :: erbi_connection()} | {error, Reason :: term()}.
checkout(PoolName) when is_atom(PoolName) orelse is_list(PoolName) ->
    case poolboy:checkout(ext_to_internal_name(PoolName), false, ?DEFAULT_CHECKOUT_TIMEOUT) of
        full -> {error, no_available_connections};
        Worker ->
            catch(erbi_driver:reset(Worker)),
            {ok, {erbi_connection, #conn{
                    pid = Worker,
                    pooled = true,
                    pool_name = PoolName }}}
    end.

-spec checkin(PooledConn :: erbi_connection()) -> ok.
checkin({erbi_connection, #conn{ pid = Worker,
                                 pooled = true,
                                 pool_name = PoolName }}) when is_atom(PoolName) orelse is_list(PoolName) ->
    catch(erbi_driver:reset(Worker)),
    poolboy:checkin(ext_to_internal_name(PoolName), Worker).

-spec list_pool_names() -> [atom()].
list_pool_names() ->
    [internal_to_ext_name(Id) || {Id,_,_,_} <- supervisor:which_children(erbi_sup)].

-spec status(PoolName :: atom) -> {State :: ready | full | overflow,
                                   NumberOfConnections :: integer(),
                                   NumberOfOverflowConnections :: integer(),
                                   NumberOfLeasedConnections :: integer()}.
status(PoolName) when is_atom(PoolName) ->
    poolboy:status(ext_to_internal_name(PoolName)).

-spec scrape_pool_properties(DataSource :: erbi_data_source()) ->
    {list(), erbi_data_source()}.
scrape_pool_properties(#erbi{ properties = Props } = DataSource) ->
    {L0, Props1} = proplists:split(Props, ?POOL_PROPS),
    L1 = lists:flatten(L0),
    L = case L1 =/= [] of
            true  -> erbi:normalize_properties(erbi_pools, L1);
            false -> []
        end,
    {L, DataSource#erbi{properties=Props1}};
scrape_pool_properties(Err) ->
    Err.

-spec validate_property( atom(), any() ) ->
    ok | {ok,[property()]} | {error,any()}.
validate_property(size, V) when is_list(V) ->
    {ok, [{size, list_to_integer(V)}]};
validate_property(name, V) when is_list(V) ->
    {ok, [{name, ext_to_internal_name(V)}]};
validate_property( max_overflow, V) when is_list(V) ->
    {ok, [{max_overflow, list_to_integer(V)}]};
validate_property( _,_ ) ->
    ok.

-spec property_info() -> [{atom(),any()}].
property_info()->
    [{aliases, [
        {pool_name,name},
        {pool_size,size},
        {pool_max_overflow,max_overflow}]},
     {defaults,[{size,5},{max_overflow,5}]},
     {required,[name]}
    ].

%%%===================================================================
%%% Helper functions
%%%===================================================================
internal_to_ext_name(PoolName) when is_atom(PoolName) ->
    internal_to_ext_name(atom_to_list(PoolName));
internal_to_ext_name("__erbi_pool_" ++ PoolName) ->
    list_to_atom(PoolName);
internal_to_ext_name(PoolName) when is_list(PoolName) ->
    list_to_atom(PoolName).

ext_to_internal_name(PoolName) when is_atom(PoolName) ->
    ext_to_internal_name(atom_to_list(PoolName));
ext_to_internal_name("__erbi_pool_" ++ _PoolName = FullName) ->
    list_to_atom(FullName);
ext_to_internal_name(PoolName) when is_list(PoolName) ->
    list_to_atom("__erbi_pool_" ++ PoolName).
