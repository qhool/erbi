%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbi_temp_db).
-include("erbi.hrl").
-export([start/1,
         stop/1,
        get_normalized_base_data_source/5,
        add_data_dir/2]).

-callback start_temp(ErbiDataSource::erbi_data_source())->
    ok.


-callback stop_temp(ErbiDataSource::erbi_data_source())->
    ok.
    
-callback get_temp_connect_data(ErbiDataSource::erbi_data_source(),
                                Username::unicode:chardata(),
                                Password::unicode:chardata())->
    {ErbiDataSource::erbi_data_source(),
     unicode:chardata(),
     unicode:chardata()}.

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
    DDPropList=add_data_dir(PropList,ErbiDriver),

    {BaseDS,TmpUsername,TmpPasswd}
        = BaseDriver:get_temp_connect_data(#erbi{
                                              driver = BaseDriverName,
                                              properties=DDPropList,
                                              args=get_new_driver_args(Args)},
                                           Username,
                                           Password),

    NormDataS=erbi:normalize_data_source(BaseDS),
    {NormDataS,TmpUsername,TmpPasswd}.

get_new_driver_args(undefined)->
    [];
get_new_driver_args(Any) ->
    Any.



add_data_dir(PropList,DataSource)->
    NormDs=erbi_temp_db_helpers:get_string_from_ds(DataSource),
    BaseDir = proplists:get_value(data_dir,PropList),
    NewDir = get_data_dir_name(BaseDir,NormDs),
    lists:keyreplace(data_dir,1,PropList,{data_dir,NewDir}).

get_data_dir_name(BaseDir,DataSource)->
    Hash = binary_to_list(base64:encode(crypto:hash(md5,DataSource))),
    HashPrefix= if length(Hash) > 20 ->
                        element(1,lists:split(20,Hash));
                   true ->
                        Hash
                end,
    BaseDir++"/"++HashPrefix.

