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

-ifndef(ERBI_DRIVER_HRL).
-define(ERBI_DRIVER_HRL,true).

-type erbdrv_call_status() :: ok | error | declined.
-type erbdrv_connection() :: any().
-type erbdrv_statement() :: any().

-type erbdrv_general_error() :: timeout | unauthorized.

-type erbdrv_connection_error() :: invalid_datasource |
                                   unknown_host | 
                                   connection_refused |
                                   communication_error |
                                   invalid_credentials |
                                   connection_lost.
                                   
-type erbdrv_statement_error() :: syntax_error |
                                  unknown_table |
                                  unknown_column |
                                  unknown_object.

-type erbdrv_execution_error() :: transaction_error | 
                                  insufficient_resources |
                                  statement_closed |
                                  connection_closed |
                                  no_more_rows.

-type erbdrv_error_code() :: erbdrv_general_error | erbdrv_connection_error() | 
                             erbdrv_statement_error() | erbdrv_execution_error().
                                   
-type erbdrv_error() :: { erbdrv_error_code(), any() }.
-type erbdrv_rows() :: list(list(any())).

-record(erbdrv_field,
        { name              :: string(),
          type              :: atom(),
          length            :: non_neg_integer(),
          precision         :: non_neg_integer()
        }).

-type erbdrv_field() :: #erbdrv_field{}.

-type erbdrv_columns() :: list(erbdrv_field()).

-record(erbdrv,
        { status = error    :: erbdrv_call_status(),
          conn              :: erbdrv_connection() | undefined | same,
          stmt              :: erbdrv_statement() | undefined | same,
          data              :: erbdrv_error() | erbdrv_columns() | erbdrv_rows() | 
                               { erbdrv_columns(), erbdrv_rows() }
        }).
-type erbdrv_return() :: #erbdrv{}.

-endif.
          