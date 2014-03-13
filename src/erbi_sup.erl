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
%% Creates top level supervisor for erbi pools. If pools are used in your application
%% you will have to fit this supervisor into the supervision tree of the host application.
%% Supervisor will be locally registered, so
%% @end

-module(erbi_sup).

-behaviour(supervisor).

-include("erbi.hrl").

%% API
-export([
    start_link/1
]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link(Pools :: list()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Pools) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Pools).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init(Pools) ->
    io:format("erbi_sup:init(~p)~n", [Pools]),
    PoolsSpecs = [pool_child_spec(Pool) || Pool <- Pools],
    {ok, {{one_for_one, 10, 10}, PoolsSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
pool_child_spec({DataSource, Username, Password}) when is_list(DataSource) ->
    DataSource1 = erbi:parse_data_source(DataSource),
    Module = erbi:get_driver_module(DataSource1),
    {PoolProps, DataSource2} = erbi_pool:scrape_pool_properties(DataSource1),
    case PoolProps =:= [] of
        true -> {error, no_pool_paramerts};
        false ->
            case erbi:normalize_data_source(Module,DataSource2) of
                {error,_} = E -> E;
                DataSource3 ->
                    Info = Module:driver_info(),
                    PoolName = proplists:get_value(name, PoolProps),
                    PoolArgs = [{name, {local, PoolName}},
                                {worker_module, erbi_driver}] ++ PoolProps,
                    WorkerArgs = {Module,Info,DataSource3,Username,Password},
                    poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
            end
    end;
pool_child_spec({DataSource}) ->
    pool_child_spec({DataSource, undefined, undefined}).
