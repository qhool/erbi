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
%% Main erbi module.
%% @end 
-module(erbi).
-export([connect/3,parse_data_source/1]).
-include("erbi.hrl").

%% --------------------------------------
%% @doc Connect to a database.
%% 
%% <ul><li>Connect  - DB connect term or string "erbi:Driver:params"</li>
%% <li>Username</li>
%% <li>Password</li>
%% </ul>
%% @end
%% --------------------------------------
-spec connect( DataSource :: string() | erbi_data_source(),
               Username :: string() | atom() ,
               Password :: string() | atom() ) -> 
                     { ok, erbi_connection() } | { error, any() }.
connect( DataSource, UserName, Password ) ->                    
    { error, "not implemented" }.

%% --------------------------------------
%% @doc Parse data source string.
%%
%% Takes a data source descriptor in the form "erbi:driver:arg=val;arg=val[...]"
%% and returns an erbi_data_source() value.
%% @end
%% --------------------------------------
-spec parse_data_source( DataSource :: string() ) ->
                               { ok, erbi_data_source() } | { error, any() }.

parse_data_source( DataSource ) ->
    { error, "not implemented" }.

%% @headerfile "erbi.hrl"

%%==== Internals ====%%



