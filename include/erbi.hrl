%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%
%% Copyright 2013 Voalte Inc. <jburroughs@voalte.com>
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

-ifndef(ERBI_HRL).
-define(ERBI_HRL,true).
% Opaque connection handle
-type erbi_connection() :: {erbi_connection,any()}.

% Opaque statement handle
-type erbi_statement() :: {erbi_statement, any(), any()}.

-type erbi_bind_values() :: [erbi_bind_value()] | [erbi_bind_value_named()].
-type erbi_bind_value() :: erbi_bind_value_typed() | erbi_bind_value_untyped().
-type erbi_bind_value_typed() :: {erbi_value_type(), any()}.
-type erbi_bind_value_untyped() :: any().
-type erbi_bind_value_named() :: erbi_bind_value_named_typed() | erbi_bind_value_named_untyped().
-type erbi_bind_value_named_typed() :: {erbi_identifier(), erbi_value_type(), any()}.
-type erbi_bind_value_named_untyped() :: {erbi_identifier(), any()}.
-type erbi_identifier() :: atom() | string() | binary().
-type erbi_value_type() :: atom().
-type erbi_row_count() :: unknown | non_neg_integer().

-type property() :: atom() | {atom(),any()}.

-record(erbi,
        { driver :: atom(),
          properties = [] :: [property()],
          args = [] :: undefined | term() | [any()]
        }).
-type erbi_data_source() :: #erbi{}.
-type erbi_connect_tuple() :: {erbi,atom()} | {erbi,atom(),[property()]}.

-record(erbi_driver_info,
        { driver :: atom(),
          preparse_support :: boolean(),
          cursor_support :: boolean(),
          transaction_support :: boolean(),
          must_preparse = false :: boolean(),
          must_bind = false :: boolean(),
          multiple_bind = true :: boolean()
        }).

-type erbi_driver_info() :: #erbi_driver_info{}.

-endif.
