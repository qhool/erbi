%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbi_temp_db).
-include("erbi.hrl").
-export([start/1,
         stop/1,
        get_normalized_base_data_source/5,
        get_data_dir_name/2]).

-callback start_temp(ErbiDataSource::erbi_data_source(),DataDir::unicode:chardata())->
    ok.


-callback stop_temp(ErbiDataSource::erbi_data_source(),DataDir::unicode:chardata())->
    ok.
    
-callback get_temp_connect_data(ErbiDataSource::erbi_data_source(),
                                Username::unicode:chardata(),
                                Password::unicode:chardata(),
                               DataDir::unicode:chardata())->
    {ErbiDataSource::erbi_data_source(),
     unicode:chardata(),
     unicode:chardata()}
        | declined.

-spec start(Datasource::unicode:chardata())->
    ok.
start(DataSource)->
   execute_on_driver(DataSource,fun(DriverModule,DS)->
                                        DriverModule:start(DS)
                                end).

-spec stop(Datasource::unicode:chardata())->
    ok.
stop(DataSource)->
   execute_on_driver(DataSource,fun(DriverModule,DS)->
                                        DriverModule:stop(DS)
                                end).


execute_on_driver(DataSource,Fun)->
   #erbi{driver=Driver} = NormDS = erbi:normalize_data_source(DataSource),
    DriverModule=erbi:get_driver_module(Driver),
    Fun(DriverModule,NormDS).

get_normalized_base_data_source(#erbi{properties=PropList, args=Args}=ErbiDriver,
                                Username,
                                Password,
                                BaseDriver,
                                BaseDriverName)->

    DataDir = get_data_dir_name(PropList,ErbiDriver),
    TmpDataSource = #erbi{
                       driver = BaseDriverName,
                       properties=PropList,
                       args=get_new_driver_args(Args)},

    {BaseDS,TmpUsername,TmpPasswd}
        =
        case BaseDriver:get_temp_connect_data(TmpDataSource,
                                              DataDir,
                                              Username,
                                              Password) of
            declined ->
                {TmpDataSource,Username,Password};
            Any->
                Any
        end,

    NormDataS=erbi:normalize_data_source(BaseDS),
    {NormDataS,TmpUsername,TmpPasswd}.

get_new_driver_args(undefined)->
    [];
get_new_driver_args(Any) ->
    Any.

get_data_dir_name(PropList,DataSource)->
    NormDs=erbi_temp_db_helpers:get_string_from_ds(DataSource),
    BaseDir = proplists:get_value(data_dir,PropList),
    Hash = binary_to_list(base64:encode(crypto:hash(md5,NormDs))),
    HashPrefix= if length(Hash) > 20 ->
                        element(1,lists:split(20,Hash));
                   true ->
                        Hash
                end,
    BaseDir++"/"++HashPrefix.

