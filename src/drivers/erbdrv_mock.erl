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
-module(erbdrv_mock).
-behaviour(erbi_driver).

-include("erbi.hrl").
-include("erbi_driver.hrl").
-include_lib("epgsql/include/pgsql.hrl").
-export([driver_info/0,
         validate_property/2,
         property_info/0,
         parse_args/1,
         connect/3,
         disconnect/1,
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

-record(mock_connection,{
	  base_driver::atom(),
	  base_connection::erbdrv_connection()
			   }).

-define(base_driver_name(PList), proplists:get_value(base_driver,
						  PList) ).
-define(base_driver(PList), erbi:get_driver_module(
			      ?base_driver_name(PList))).

-define(DEFAULT_DATA_DIR,"/tmp_data/mock_db_data").

-spec driver_info() -> erbi_driver_info().
driver_info()->
    #erbi_driver_info
        { driver = mock,
          preparse_support = true,
          cursor_support = true,
          transaction_support = true,
          must_preparse = true,
          must_bind = true,
          multiple_bind = false
          }.

-spec validate_property( atom(), any() ) ->
    ok | {ok,[property()]} | {error,any()}.
validate_property(port,Port)->
     {ok,[{port,list_to_integer(Port)}]};
validate_property(base_driver,Driver)->
    {ok,[{base_driver,list_to_atom(Driver)}]};
validate_property(init_files,FilesString)->
    FilesList=string:tokens(FilesString,","),
    {ok,[{init_files,FilesList}]};
validate_property( _,_ ) ->
    ok.

-spec property_info() -> [{atom(),any()}].
property_info()->
    [
     {defaults,[{port,5433},
		{data_dir,code:get_path()++?DEFAULT_DATA_DIR}]},
     {required,[base_driver]}
     ].

-spec parse_args([any()]) ->
    declined | {error,any()} | term().
parse_args([])->
    declined.

-spec connect( DS :: erbi_data_source(),
                   Username :: string(),
                   Password :: string() ) -> erbdrv_return().
connect(#erbi{driver = mock, properties=PropList}=ErbiDriver, _Username, _Password)->
    BaseDriverName=?base_driver_name(PropList),
    BaseDriver=?base_driver(PropList),
    {BaseDriverPropList,MockUser,MockPassword}=BaseDriver:start_mocking(PropList),
    #erbdrv{conn=BaseConnection}=BaseDriver:connect(ErbiDriver#erbi{
						      driver = BaseDriverName,
						      properties=BaseDriverPropList},
						    MockUser,
						    MockPassword),
    MockConnection=#mock_connection{base_driver=BaseDriver,
				    base_connection=BaseConnection},
    #erbdrv
        {status = ok,
         conn = MockConnection
        }.

-spec disconnect( Connection :: erbdrv_connection() ) -> 
    ok | {error, erbdrv_error()}.
disconnect(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection})->
    BaseDriver:disconnect(BaseConnection).

-spec begin_work( Connection :: erbdrv_connection() ) ->
    erbdrv_return().
begin_work(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection})->
    BaseDriver:begin_work(BaseConnection).

-spec begin_work( Connection :: erbdrv_connection(),
                      Savepoint :: atom | string() ) ->
    erbdrv_return().
begin_work(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection},
	   Savepoint)->
    BaseDriver:begin_work(BaseConnection,Savepoint).

-spec rollback( Connection :: erbdrv_connection() ) -> 
    erbdrv_return().
rollback(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection})->
   BaseDriver:rollback(BaseConnection).

-spec rollback( Connection :: erbdrv_connection(),
                    Savepoint :: atom | string() ) ->  
    erbdrv_return().
rollback(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection},
	 Savepoint) ->
    BaseDriver:rollback(BaseConnection,Savepoint).
  
-spec commit( Connection :: erbdrv_connection() ) ->
    erbdrv_return().
commit(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection})->
    BaseDriver:commit(BaseConnection).
    
-spec do( Connection :: erbdrv_connection(),
              Query :: string(),
              Params ::  erbi_bind_values() ) ->
    erbdrv_return().
do(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection},
   Query,
   Params)->
    BaseDriver:do(BaseConnection,Query,Params).
    
-spec prepare( Connection :: erbdrv_connection(),
	       Query :: string() ) ->
		     erbdrv_return().
prepare(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection},Query) ->
    BaseDriver:prepare(BaseConnection,Query).

-spec bind_params( Connection :: erbdrv_connection(),
                       Statement :: erbdrv_statement(),
                       Params :: erbi_bind_values() ) ->
    erbdrv_return().
bind_params(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection},
	    Statement,
	    Params)->
    BaseDriver:bind_params(BaseConnection,Statement,Params).

-spec execute( Connection :: erbdrv_connection(),
                   Statement :: erbdrv_statement() | string(),
                   Params :: erbi_bind_values() ) ->
    erbdrv_return().
execute(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection},Statement,Params)->
    BaseDriver:execute(BaseConnection,Statement,Params).
    
-spec fetch_rows( Connection :: erbdrv_connection(),
                      Statement :: erbdrv_statement(),
                      Amount :: one | all ) ->
    erbdrv_return().
fetch_rows(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection},Statement,Amount)->
    BaseDriver:fetch_rows(BaseConnection,Statement,Amount).

-spec finish( Connection :: erbdrv_connection(),
                 Statement :: erbdrv_statement() ) ->
    erbdrv_return().
finish(#mock_connection{base_driver=BaseDriver,
		     base_connection=BaseConnection},Statement)->
    BaseDriver:finish(BaseConnection,Statement).

	       
