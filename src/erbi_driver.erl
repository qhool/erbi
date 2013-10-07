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
%% This module defines the erbi_driver behaviour, and provides
%% a private interface for use by drivers. 
%% @end

-module(erbi_driver).
-export([]).

-include("erbi.hrl").

-include("erbi_driver.hrl").

%%-----------------------------------------------
%% TOP LEVEL DRIVER CALLS
%%-----------------------------------------------

%% @doc Return an erbi_driver_info record
%% @end
-callback driver_info() -> erbi_driver_info().

%% @doc Establish a new connection to the database
%% @end
-callback connect( DS :: erbi_data_source(),
                   Username :: string(),
                   Password :: string() ) -> erbdrv_return().

%%-----------------------------------------------
%% CONNECTION LEVEL DRIVER CALLS
%%-----------------------------------------------

%% @doc tear-down existing database connection.
%%
%% Regardless of the return, all existing handles 
%% associated with this connection are assuemed to be invalid after this call.
%% @end
-callback disconnect( Connection :: erbdrv_connection() ) -> 
    ok | {error, erbdrv_error()}.

%% @doc begin transaction or create savepoint
%%
%% Optional Savepoint parameter should create a named savepoint, if supported.
%% @end
-callback begin_work( Connection :: erbdrv_connection() ) ->
    erbdrv_return().
-callback begin_work( Connection :: erbdrv_connection(),
                      Savepoint :: atom | string() ) ->
    erbdrv_return().

%% @doc undo changes
%%
%% Rollback to beginning of transaction, or to optional named savepoint.
%% @end
-callback rollback( Connection :: erbdrv_connection() ) -> 
    erbdrv_return().
-callback rollback( Connection :: erbdrv_connection(),
                    Savepoint :: atom | string() ) ->  
    erbdrv_return().

%% @doc commit changes
%%
%% Finalize current transaction.
%% @end
-callback commit( Connection :: erbdrv_connection() ) ->
    erbdrv_return().

%% @doc parse query/statement
%%
%% Should parse the supplied query and return a handle to the pre-parsed form.  
%% If the driver does not support pre-parsing, this function will not be called;
%% instead, the statement string will be cached until {@link execute/3} is called.
%%
%% Column info can be returned in data section.
%% @end
-callback prepare( Connection :: erbdrv_connection(), Query :: string() ) -> erbdrv_return().

%%-----------------------------------------------
%% STATEMENT LEVEL DRIVER CALLS
%%-----------------------------------------------

%% @doc bind parameters to a statement
%%
%% Will only be called if the driver supports parameter binding.
%% Otherwise, params will be passed to {@link execute/3}.
%% @end
-callback bind_params( Connection :: erbdrv_connection(),
                       Statement :: erbdrv_statement(),
                       Params :: erbi_bind_values() ) ->
    erbdrv_return().


%% @doc execute statement
%% 
%% Begin execution of given statement handle, returned from {@link prepare/2}.
%%
%% Should return a statement handle, and may return column info and/or 
%% any rows which are ready immediately. If no rows are ready, that's fine --
%% the intent is only to prevent the driver from needing to implement caching of complete rows.
%% 
%% If driver does not indicate support for pre-parsing, statement will
%% be a string.  If driver doesn't support cursors, data will be assumed to contain all
%% rows; Neither {@link fetch_rows/3} nor {@link finish/2} will be called.
-callback execute( Connection :: erbdrv_connection(),
                   Statement :: erbdrv_statement() | string(),
                   Params :: erbi_bind_values() ) ->
    erbdrv_return().

%% @doc retrieve rows from open cursor
%%
%% Amount indicates how many rows are desired; if it is 'one' driver should 
%% request the minimal amount from the database, but the actual amount returned may
%% be more than one; likewise, if it is 'all', request a maximal amount, but the driver is not
%% assumed to have returned all rows.
%% @end
-callback fetch_rows( Connection :: erbdrv_connection(),
                      Statement :: erbdrv_statement(),
                      Amount :: one | all ) ->
    erbdrv_return().

%% @doc close open cursor
%%
%% Will only be called on cursor-supporting drivers.
%% @end
-callback finish( Connection :: erbdrv_connection(),
                 Statement :: erbdrv_statement() ) ->
    erbdrv_return().

