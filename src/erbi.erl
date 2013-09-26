%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(erbi).
-export([connect/3]).

%% --------------------------------------
%% @doc Connect to a database
%%   Connect  - DB connect term or string "erbi:Driver:params"
%%   Username   
%%   Password
%% @end
%% --------------------------------------
-spec connect( ConnectDescriptor :: string() | list(tuple(atom(),any())),
               Username :: string(),
               Password :: string() ) -> 
                     { ok, erbi_connection() } | { error, any() }.                      

%%-------------
%% conn handle, stmt handle are records -- { erbi_connection, stuff... }
%% { erbi_statement, ... }
%% so you can do:
%% Conn = erbi:connect(...),
%% Statement = Conn:prepare(...),
%%
%% Return values:
