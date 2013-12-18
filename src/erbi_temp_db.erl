%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbi_temp_db).
-include("erbi.hrl").
-export([start/1,
         stop/1,
         parse_temp_ds/1,
         data_dir/1]).
%%-----------------------------------------------
%% TEMP DB DRIVER CALLS
%%-----------------------------------------------

%% @doc Provision and start temp database (driver callback)
%% 
%% Responsible for provisioning and starting a temporary database.
%% DataDir will already exist, and should be used to store any/all files required.
%% @end
-callback start_temp(ErbiDataSource::erbi_data_source(),DataDir::unicode:chardata())->
    ok.

%% @doc Shutdown temp database (driver callback)
%% 
%% Should shut down the temporary database in DataDir.
%% Do not delete files (other than pid/port/socket files); 
%% this is performed by erbi_tmp_db (or not; cleanup can be 
%% disabled using the auto_clean option).
%% @end
-callback stop_temp(ErbiDataSource::erbi_data_source(),DataDir::unicode:chardata())->
    ok.

%% @doc Get connection parameters for temp db
%%
%% Called to get modified connection parameters; arguments are the 'raw' connect parameters, 
%% plus the data dir:
%% <ul>
%%   <li>DataSource: constructed from the erbi:temp datasource.</li>
%%   <li>DataDir (as above)</li>
%%   <li>Username (unmodified)</li>
%%   <li>Password (unmodified)</li>
%% </ul>
%% Should return a tuple of {DataSource,Username,Password}.  DataSource will then be normalized, and
%% the values will be passed to your driver's connect/3 function.
%% @end
-callback get_temp_connect_data(ErbiDataSource::erbi_data_source(),
                                DataDir::unicode:chardata(),
                                Username::unicode:chardata(),
                                Password::unicode:chardata())->
    {ErbiDataSource::erbi_data_source(),
     unicode:chardata(),
     unicode:chardata()}
        | declined.

%% @doc start temp db
%%
%% Given an erbi 'temp' datasource in string or parsed form, creates and starts
%% the temp DB.
%% @end
-spec start(Datasource::unicode:chardata() | erbi_data_source())->
    ok.
start(DataSource)->
    {BaseDriver,BaseDS,DataDir} = parse_temp_ds(DataSource),
    erbi_temp_db_helpers:create_dir(DataDir),
    BaseDriver:start_temp(BaseDS,DataDir).

%% @doc start temp db
%%
%% Stops the temp db.  If the auto_clean paramater is true (the default) the data dir will also be deleted.
%% %% @end
-spec stop(Datasource::unicode:chardata() | erbi_data_source())->
    ok.
stop(DataSource)->
    {BaseDriver,#erbi{properties=Props}=BaseDS,DataDir} = parse_temp_ds(DataSource),
    ok = BaseDriver:stop_temp(BaseDS,DataDir),
    DoCleanup = proplists:get_value(auto_clean,Props),
    case DoCleanup of
        true ->
            erbi_temp_db_helpers:del_dir(DataDir),
            ok;
        _ -> ok
    end.

data_dir(DS)->
    {_,_,DataDir} = parse_temp_ds(DS),
    DataDir.

%----------------------------------------
% Erbi temp internal functions
%---------------------------------------- 
parse_temp_ds(#erbi{properties=Props,args=Args}=DataSource)->
    BaseDriver=proplists:get_value(base_driver,Props),
    BaseModule=erbi:get_driver_module(BaseDriver),
    DataDir =
        case proplists:get_value(data_dir,Props) of
            undefined ->
                HashPrefix= hash_ds(DataSource,20),
                atom_to_list(BaseDriver)++"_"++HashPrefix;
            DD -> 
                DD
        end,
    BaseDS = DataSource#erbi{driver=BaseDriver,
                             args=default_value(Args,[])},
    {BaseModule,BaseDS,filename:absname(DataDir)};
parse_temp_ds(StringDS) ->
    parse_temp_ds(erbi:normalize_data_source(StringDS)).

%this could be promoted to erbi or some other module later
%hash_ds(DS) ->
%    hash_ds(DS,all).
hash_ds(#erbi{driver = DriverName, properties=PropList, args=Args},Length)->
    Str = io_lib:format("erbi:~p:~p:~p",[DriverName,PropList,Args]),
    % / and + aren't incredibly filename-friendly
    Hash = 
        lists:map( fun($/) -> $.;
                      ($+) -> $_;
                      (X) -> X
                   end, binary_to_list(base64:encode(crypto:hash(md5,Str))) ),
    case Length of
        L when is_integer(L) andalso length(Hash) > L ->
            element(1,lists:split(20,Hash));
        _ ->
            Hash
    end.

default_value(undefined,Default)->
    Default;
default_value(Any,_) ->
    Any.

