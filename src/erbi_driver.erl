%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%
%% @private
%% @doc
%% This module defines the erbi_driver behaviour.
%% @end

-module(erbi_driver).
-export([behaviour_info/1]).

-include("erbi.hrl").

behaviour_info(callbacks) ->
    [{connect,1},{disconnect,1},
     {driver_info,0}
    ];
behaviour_info(_) ->
    undefined.
