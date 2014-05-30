%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%
%% @copyright 2013 Voalte Inc. <ccorral@voalte.com>
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
-module(erbdrv_temp).
-behaviour(erbi_driver).

-include("erbi.hrl").
-include("erbi_driver.hrl").

% erbi driver API
-export([driver_info/0,
         validate_property/2,
         property_info/0,
         parse_args/1,
         connect/3,
         disconnect/1,
         reset/1,
         begin_work/1,
         begin_work/2,
         rollback/1,
         rollback/2,
         commit/1,
         do/3,
         prepare/2,
         bind_params/3,
         execute/3,
         fetch_rows/3,
         finish/2
        ]).

-record(temp_connection,{
	  base_driver::atom(),
	  base_connection::erbdrv_connection()
			   }).

% erbi driver API implementation
-spec driver_info() -> erbi_driver_info().
driver_info()->
    #erbi_driver_info
        { driver = temp,
          preparse_support = true,
          cursor_support = true,
          transaction_support = true,
          must_preparse = true,
          must_bind = true,
          multiple_bind = false
          }.

-spec validate_property( atom(), any() ) ->
    ok | {ok,[property()]} | {error,any()}.
validate_property(base_driver,Driver) when is_list(Driver)->
    {ok,[{base_driver,list_to_atom(Driver)}]};
validate_property(init_files,[H|_]=FilesString) when not is_list(H)->
    FilesList=string:tokens(FilesString,","),
    {ok,[{init_files,FilesList}]};
validate_property(autoclean,Val) ->
    validate_property(auto_clean,Val);
validate_property(auto_clean,Val) when is_list(Val) ->
    case string:to_lower(Val) of
        "true" -> {ok,[{auto_clean,true}]};
        "false" -> {ok,[{auto_clean,false}]};
        "1" -> {ok,[{auto_clean,true}]};
        "0" -> {ok,[{auto_clean,false}]};
        _ -> {error,{invalid_datasource,{auto_clean,Val}}}
    end;
validate_property( _,_ ) ->
    ok.

-spec property_info() -> [{atom(),any()}].
property_info()->
    [
     {defaults,[{data_dir,undefined},
                {auto_clean,true}]},
     {required,[base_driver]}
    ].

-spec parse_args([any()]) ->
    declined | {error,any()} | term().
parse_args(_)->
    declined.

-spec connect( DS :: erbi_data_source(),
                   Username :: string(),
                   Password :: string() ) -> erbdrv_return().
connect(#erbi{driver = temp}=DataSource, Username0, Password0)->
    {BaseDriver,BaseDS0,DataDir} = erbi_temp_db:parse_temp_ds(DataSource),
    {BaseDS1,Username,Password} =
        case BaseDriver:get_temp_connect_data(BaseDS0,DataDir,Username0,Password0) of
            declined -> {BaseDS0,Username0,Password0};
            Any -> Any
        end,
    BaseDS=erbi:normalize_data_source(BaseDS1),
    #erbdrv{conn=BaseConnection}= BaseDriver:connect(BaseDS,Username,Password),

    TempConnection=#temp_connection{base_driver=BaseDriver,
				    base_connection=BaseConnection},
    #erbdrv
        {status = ok,
         conn = TempConnection
        }.

-spec disconnect( Connection :: erbdrv_connection() ) ->
    ok | {error, erbdrv_error()}.
disconnect(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection})->
    BaseDriver:disconnect(BaseConnection).

reset(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection})->
    BaseDriver:reset(BaseConnection).

-spec begin_work( Connection :: erbdrv_connection() ) ->
    erbdrv_return().
begin_work(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C)->
    wrap_ret(C,BaseDriver:begin_work(BaseConnection)).

-spec begin_work( Connection :: erbdrv_connection(),
                      Savepoint :: atom | string() ) ->
    erbdrv_return().
begin_work(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C,
	   Savepoint)->
    wrap_ret(C,BaseDriver:begin_work(BaseConnection,Savepoint)).

-spec rollback( Connection :: erbdrv_connection() ) ->
    erbdrv_return().
rollback(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C)->
    wrap_ret(C,BaseDriver:rollback(BaseConnection)).

-spec rollback( Connection :: erbdrv_connection(),
                    Savepoint :: atom | string() ) ->
    erbdrv_return().
rollback(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C,
	 Savepoint) ->
    wrap_ret(C,BaseDriver:rollback(BaseConnection,Savepoint)).

-spec commit( Connection :: erbdrv_connection() ) ->
    erbdrv_return().
commit(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C)->
    wrap_ret(C,BaseDriver:commit(BaseConnection)).

-spec do( Connection :: erbdrv_connection(),
              Query :: string(),
              Params ::  erbi_bind_values() ) ->
    erbdrv_return().
do(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C,
   Query,
   Params)->
    wrap_ret(C,BaseDriver:do(BaseConnection,Query,Params)).

-spec prepare( Connection :: erbdrv_connection(),
	       Query :: string() ) ->
		     erbdrv_return().
prepare(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C,Query) ->
    wrap_ret(C,BaseDriver:prepare(BaseConnection,Query)).

-spec bind_params( Connection :: erbdrv_connection(),
                       Statement :: erbdrv_statement(),
                       Params :: erbi_bind_values() ) ->
    erbdrv_return().
bind_params(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C,
	    Statement,
	    Params)->
    wrap_ret(C,BaseDriver:bind_params(BaseConnection,Statement,Params)).

-spec execute( Connection :: erbdrv_connection(),
                   Statement :: erbdrv_statement() | string(),
                   Params :: erbi_bind_values() ) ->
    erbdrv_return().
execute(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C,Statement,Params)->
    wrap_ret(C,BaseDriver:execute(BaseConnection,Statement,Params)).

-spec fetch_rows( Connection :: erbdrv_connection(),
                      Statement :: erbdrv_statement(),
                      Amount :: one | all ) ->
    erbdrv_return().
fetch_rows(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C,Statement,Amount)->
    wrap_ret(C,BaseDriver:fetch_rows(BaseConnection,Statement,Amount)).

-spec finish( Connection :: erbdrv_connection(),
                 Statement :: erbdrv_statement() ) ->
    erbdrv_return().
finish(#temp_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection}=C,Statement)->
    wrap_ret(C,BaseDriver:finish(BaseConnection,Statement)).


%% erbi drivers are allowed to update their connection state
%% this handles that
wrap_ret(TempConn,#erbdrv{conn=NewBaseConnection}=Ret) when NewBaseConnection =/= same ->
    NewTempConn = TempConn#temp_connection{base_connection=NewBaseConnection},
    Ret#erbdrv{conn=NewTempConn};
%% this case covers 'declined' and conn=same
wrap_ret(_TempConn,Ret) ->
    Ret.




