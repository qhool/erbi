%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbdrv_epgsql_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").


%% connection_properties_test_()->
%%
%%     [?_assertEqual( #erbi{driver=epgsql,
%%                           properties=[{database,"test_db"},
%%                                       {host,"localhost"},
%%                                       {port,5432}
%%                                      ],
%%                           args=undefined},
%%                     ?debugVal(erbi:normalize_data_source( "erbi:epgsql:hostaddr=localhost;db=test_db" )) ),
%%      ?_assertEqual( #erbi{driver=epgsql,
%%                           properties=[{database,"test_db"},
%%                                       {host,"ahost"},
%%                                       {port,4444}
%%                                      ],
%%                           args=undefined},
%%                     ?debugVal(erbi:normalize_data_source( "erbi:epgsql:hostname=ahost;dbname=test_db;port=4444" )) )].
%%
%%
%% connect_epgsql_test_()->
%%     {setup,
%%      fun()->
%%              Config =?debugVal(erbi_test_util:config(epgsql)),
%%              Datasource=proplists:get_value(datasource,Config),
%%              User= proplists:get_value(user,Config),
%%              Pwd= proplists:get_value(password,Config),
%%              erbi_test_util:start_db_test(Datasource),
%%              {Datasource,User,Pwd}
%%      end,
%%      fun({Datasource,User,Pwd})->
%%              erbi_test_util:stop_db_test(Datasource)
%%      end,
%%      fun({Datasource,User,Pwd})->
%%              [ ?_test( { ok, _ } = ?debugVal(erbi:connect( Datasource, User, Pwd )) ),
%%                ?_test( { error, _ } = ?debugVal(erbi:connect( "erbi:epgsql:database=mydatabase", "", "" )) ),
%%                % epgsql driver does not require database name -- why add this restriction?
%%                %?_assertEqual( { error,
%%                %                 {invalid_datasource,
%%                %                  {missing_properties,[database]}} } ,
%%                %               ?debugVal(erbi:connect( "erbi:epgsql:", "postgres", "pass" )) ),
%%
%%                ?_test({error,{unknown_host,_}}= ?debugVal(erbi:connect( "erbi:epgsql:database=mydatabase;host=myhost", "postgres", "pass" )) )
%%              ]
%%      end
%%     }.


all_test_()->
    {setup,
     fun()->
             Config =erbi_test_util:config(epgsql),
             start_db(Config),
             Conn= connect(Config),
             {_,DataConfig}=erbi_test_util:dataset(erbdrv_epgsql),
             {Conn,Config,DataConfig}
     end,
     fun({_Conn,Config,_DataConfig})->
             Datasource=proplists:get_value(datasource,Config),
             erbi_test_util:stop_db_test(Datasource)
     end,

     fun({Conn,Config,DataConfig}) ->

             lists:flatten([
                            create_table(Conn,DataConfig),
                            erbi_transaction(Conn,Config,DataConfig),
                            erbi_selectall(Conn,DataConfig),
                            erbi_selectrow(Conn,DataConfig),
                            get_some_errors(Conn,Config,DataConfig, not_pooled),
                            null_value_test(Conn,DataConfig),
                            delete_table(Conn,DataConfig),
                            timestamp_test(Conn),
                            disconnect_epgsql(Conn)
                           ])
     end
    }.

all_with_pools_test_()->
    {setup,
     fun()->
             erbi:start(),
             Config =erbi_test_util:config(epgsql_pooled),
             start_db(Config),
             Conn= connect(Config),
             {_,DataConfig}=erbi_test_util:dataset(erbdrv_epgsql),
             {Conn,Config,DataConfig}
     end,
     fun({_Conn,Config,_DataConfig})->
             application:stop(erbi),
             Datasource=proplists:get_value(datasource,Config),
             erbi_test_util:stop_db_test(Datasource)
     end,

     fun({Conn,Config,DataConfig}) ->

             lists:flatten([
                            create_table(Conn,DataConfig),
                            erbi_transaction(Conn,Config,DataConfig),
                            erbi_selectall(Conn,DataConfig),
                            erbi_selectrow(Conn,DataConfig),
                            get_some_errors(Conn,Config,DataConfig, pooled),
                            null_value_test(Conn,DataConfig),
                            delete_table(Conn,DataConfig),
                            timestamp_test(Conn),
                            disconnect_epgsql(Conn)
                           ])
     end
    }.

delete_table(Conn,DataConfig)->
    DeleteTable=proplists:get_value(delete_table,DataConfig),
    ?_assertEqual({ok,unknown},?debugVal(erbi_connection:do(Conn,DeleteTable))).
create_table(Conn,DataConfig)->
    CreateTable=proplists:get_value(create_table,DataConfig),
    ?_assertEqual({ok,unknown},?debugVal(erbi_connection:do(Conn,CreateTable))).


erbi_transaction(Conn,Config,DataConfig)->
    {setup,
     fun()->
             OutsideTransConnect=connect(Config),

             [Data1,Data2]=generate_rows(2,proplists:get_value(data,DataConfig)),
             SelectAll=proplists:get_value(select_all,DataConfig),
             Insert=proplists:get_value(insert,DataConfig),
             DeleteAll=proplists:get_value(delete_all,DataConfig),
             {OutsideTransConnect,Data1,Data2,SelectAll,Insert,DeleteAll}
     end,
     fun({_OutsideTransConnect,_Data1,_Data2,_SelectAll,_Insert,DeleteAll})->
             erbi_connection:do(Conn,DeleteAll)
     end,
     fun({OutsideTransConnect,Data1,Data2,SelectAll,Insert,_DeleteAll})->
             [
              ?_assertEqual({ok,[]}, ?debugVal(erbi_connection:selectall_list(Conn,SelectAll))),
              ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn))),
              ?_assertEqual({ok,1},  ?debugVal(insert_data(Conn,Insert,Data1))),
              ?_assertEqual({ok,[Data1]}, ?debugVal(erbi_connection:selectall_list(Conn,SelectAll))),

              ?_assertEqual(ok , ?debugVal(erbi_connection:rollback(Conn))),
              ?_assertEqual({ok,[]}, ?debugVal(erbi_connection:selectall_list(Conn,SelectAll))),

              ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn))),
              ?_assertEqual({ok,1},  ?debugVal(insert_data(Conn,Insert,Data1))),
              ?_assertEqual({ok,[Data1]}, ?debugVal(erbi_connection:selectall_list(Conn,SelectAll))),

              ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn,"savepoint"))),
              ?_assertEqual({ok,1},  ?debugVal(insert_data(Conn,Insert,Data2))),
              ?_assertEqual({ok,[Data1,Data2]}, ?debugVal(erbi_connection:selectall_list(Conn,SelectAll))),

              ?_assertEqual({ok,[]},?debugVal(erbi_connection:selectall_list(OutsideTransConnect,SelectAll))),

              ?_assertEqual(ok , ?debugVal(erbi_connection:rollback(Conn,"savepoint"))),
              ?_assertEqual({ok,[Data1]}, ?debugVal(erbi_connection:selectall_list(Conn,SelectAll))),

              ?_assertEqual(ok , ?debugVal(erbi_connection:rollback(Conn))),
              ?_assertEqual({ok,[]}, ?debugVal(erbi_connection:selectall_list(Conn,SelectAll))),

              ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn))),
              ?_assertEqual({ok,1},  ?debugVal(insert_data(Conn,Insert,Data2))),

              ?_assertEqual(ok , ?debugVal(erbi_connection:commit(Conn))),

              ?_assertEqual({ok,[Data2]},?debugVal(erbi_connection:selectall_list(OutsideTransConnect,SelectAll)))

             ]
     end
    }.

insert_data(Conn,InsertQuery,Data)->
    erbi_connection:do(Conn,InsertQuery,Data).

populate_db(Conn,InsertQuery,DataList)->
    lists:map(fun(Elem)->
                      insert_data(Conn,InsertQuery,Elem) end,DataList).

erbi_selectall(Conn,DataConfig)->
    {setup,
     fun()->
             SelectAll=proplists:get_value(select_all,DataConfig),
             SelectMany=proplists:get_value(select_many_bind,DataConfig),
             SelectManyValue=proplists:get_value(select_many_bind_value,DataConfig),
             N=proplists:get_value(number_of_rows,DataConfig),
             Data=?debugVal(generate_rows(N,proplists:get_value(data,DataConfig))),
             Fields=proplists:get_value(fields,DataConfig),
             Insert=proplists:get_value(insert,DataConfig),
             populate_db(Conn,Insert,Data),
             DeleteAll=proplists:get_value(delete_all,DataConfig),
             {Data,Fields,SelectAll,SelectMany,SelectManyValue,DeleteAll}
     end,
     fun({_Data,_Fields,_SelectAll,_SelectMany,_SelectManyValue,DeleteAll})->
             erbi_connection:do(Conn,DeleteAll)
     end,
     fun({Data,Fields,SelectAll,SelectMany,SelectManyValue,_DeleteAll})->

             {ok,ResList}=?debugVal(erbi_connection:selectall_list(Conn,SelectAll)),
             {ok,ResProp}= ?debugVal(erbi_connection:selectall_proplist(Conn,SelectAll)),
             {ok,ResDict}= ?debugVal(erbi_connection:selectall_dict(Conn,SelectAll)),
             {ok,ResList2}= ?debugVal(erbi_connection:selectall_list(Conn,SelectMany,[SelectManyValue])),
             {ok,ResProp2}= ?debugVal(erbi_connection:selectall_proplist(Conn,SelectMany,[SelectManyValue])),
             {ok,ResDict2}= ?debugVal(erbi_connection:selectall_dict(Conn,SelectMany,[SelectManyValue])),


             [?_assert(equal_data_list(Data,ResList)),
              ?_assert(equal_data_proplist(Data,ResProp,Fields)),
              ?_assert(equal_data_dict(Data,ResDict,Fields)),
              ?_assert(equal_data_list(Data,ResList2)),
              ?_assert(equal_data_proplist(Data,ResProp2,Fields)),
              ?_assert(equal_data_dict(Data,ResDict2,Fields))
             ]
     end
    }.



erbi_selectrow(Conn,DataConfig)->
    {setup,
     fun()->
             SelectBind=proplists:get_value(select_one_bind,DataConfig),

             N=proplists:get_value(number_of_rows,DataConfig),
             Data=?debugVal(generate_rows(N,proplists:get_value(data,DataConfig))),
             Fields=proplists:get_value(fields,DataConfig),
             Insert=proplists:get_value(insert,DataConfig),
             populate_db(Conn,Insert,Data),
             DeleteAll=proplists:get_value(delete_all,DataConfig),
             {Data,Fields,SelectBind,N,DeleteAll}
     end,
     fun({_Data,_Fields,_SelectBind,_SelectOneBindValue,DeleteAll})->
             erbi_connection:do(Conn,DeleteAll)
     end,
     fun({Data,Fields,SelectBind,BValue,_DeleteAll})->

             {ok,RowList}= ?debugVal(erbi_connection:selectrow_list(Conn,SelectBind,[BValue])),
             {ok,RowProp}= ?debugVal(erbi_connection:selectrow_proplist(Conn,SelectBind,[BValue])),
             {ok,RowDict}= ?debugVal(erbi_connection:selectrow_dict(Conn,SelectBind,[BValue])) ,


             [
              ?_assert(equal_data_list([lists:nth(BValue,Data)],[RowList])),
              ?_assert(equal_data_proplist([lists:nth(BValue,Data)],[RowProp],Fields)),
              ?_assert(equal_data_dict([lists:nth(BValue,Data)],[RowDict],Fields)),
              ?_assertEqual(exhausted , ?debugVal(erbi_connection:selectrow_list(Conn,SelectBind,[BValue+3]))),
              ?_assertEqual(exhausted , ?debugVal(erbi_connection:selectrow_proplist(Conn,SelectBind,[BValue+3]))),
              ?_assertEqual(exhausted , ?debugVal(erbi_connection:selectrow_dict(Conn,SelectBind,[BValue+3])))

             ]
     end
    }.

get_some_errors(Conn,Config,DataConfig,Pooled)->
    {setup,
     fun()->
             SelectBind=proplists:get_value(select_one_bind,DataConfig),
             SelectMany=proplists:get_value(select_all,DataConfig),
             % return a closed connection to get an error
             TmpConn=connect(Config),
             erbi_connection:disconnect(TmpConn),
             {SelectBind,SelectMany,TmpConn}
     end,
     fun({SelectBind,SelectMany,TmpConn})->
             [?_test({error,{missing_parameter,_}}= ?debugVal(erbi_connection:selectrow_list(Conn,SelectBind,[]))),
              ?_test({error,{syntax_error,_}}=?debugVal(erbi_connection:do(Conn,"Insert into unknowntable (Id,val) values 1 ,2"))),
              ?_test({error,{unknown_table,_}}=?debugVal(erbi_connection:do(Conn,"Insert into unknowntable (Id,val) values (1 ,2)"))) |
              if_not_pooled(Pooled, ?_assertException(exit,{noproc,_},?debugVal(erbi_connection:selectall_list(TmpConn,SelectMany))))
             ]
     end
    }.


if_not_pooled(not_pooled, Item) ->
    [Item];
if_not_pooled(pooled, _Item) ->
    [].

disconnect_epgsql(Conn)->
    ?_assertEqual(ok, ?debugVal(erbi_connection:disconnect(Conn))).


connect(Config)->
    Datasource=proplists:get_value(datasource,Config),
    User= proplists:get_value(user,Config),
    Pwd= proplists:get_value(password,Config),
    element(2,erbi:connect( Datasource, User, Pwd )).

start_db(Config)->
    Datasource=proplists:get_value(datasource,Config),
    ok = erbi_test_util:start_db_test(Datasource).


equal_data_list(Data,List)->
    {NID,Res}=lists:foldl(fun(T,{NotInData,ResList})->
                                  case lists:member(T,ResList) of
                                      false->
                                          {[T|NotInData],ResList};
                                      true ->
                                          {NotInData,lists:delete(T,ResList)}
                                  end
                          end,
                          {[],Data},List),
    (NID==[])and(Res==[]).

equal_data_proplist(Data,List,Fields)->
    PropData=create_proplist(Data,Fields),
    equal_data_list(PropData,List).


create_proplist(Data,Fields)->
    lists:map(fun(Row)->
                      lists:zip(Fields,Row)
              end,Data).

equal_data_dict(Data,Res,Fields)->
    DataDict=?debugVal(create_dict(Data,Fields)),
    erbi_test_util:equal_rows_dict(DataDict,Res).

create_dict(Data,Fields) ->
    lists:map( fun(Row) ->
                       dict:from_list(lists:zip(Fields,Row))
               end, Data ).


generate_rows(N,Data)->
    lists:map(fun(Index)->
                      [Index|Data]
              end, lists:seq(1,N)).


timestamp_test(Conn)->
    {setup,
     fun()->
             erbi_connection:do(Conn,"CREATE TABLE tiempo_test (Id int, tiempo timestamp )"),
             []
     end,
     fun(_)->
             erbi_connection:do(Conn,"DROP TABLE tiempo_test")
     end,
     fun(_)->
             ?_assertEqual({ok,1}, ?debugVal(erbi_connection:do(Conn,"insert into tiempo_test (Id,tiempo) values ($1, $2)",[1,calendar:now_to_datetime(os:timestamp())])))
     end}.

null_value_test(Conn,DataConfig)->
    {setup,
     fun()->
             Insert=proplists:get_value(insert_id,DataConfig),
             SelectBind=proplists:get_value(select_one_bind,DataConfig),
             N=proplists:get_value(number_of_rows,DataConfig),
             insert_data(Conn,Insert,[N+7]),
             {SelectBind,N+7}
     end,
     fun({SelectBind,BindN})->
             ?_assertEqual({ok,[BindN,null]} ,?debugVal(erbi_connection:selectrow_list(Conn,SelectBind,[BindN])))
     end}.



connect_temp_epgsql_test_()->
    {setup,
     fun()->
             Config=erbi_test_util:config(epgsql_temp),
             Datasource1= "erbi:temp:base_driver=epgsql;" ++
                 ";init_files="++proplists:get_value(sql_init_files,Config,"") ++
                 ";data_dir="++proplists:get_value(data_dir,Config,""),
             Datasource2=Datasource1++"2",
             ok=erbi_temp_db:start(Datasource1),
             ok=erbi_temp_db:start(Datasource2),
             {Datasource1,Datasource2}
     end,
     fun({Datasource1,Datasource2})->
             ok=erbi_temp_db:stop(Datasource1),
             ok=erbi_temp_db:stop(Datasource2)

     end,
     fun({Datasource1,Datasource2})->
             [?_test({ok,_}=?debugVal(erbi:connect(Datasource1,undefined,undefined))),
              ?_test({ok,_}=?debugVal(erbi:connect(Datasource2,"","")))]
     end}.


driver_calls_temp_epgsql_test_()->
    {setup,
     fun()->
             Config=erbi_test_util:config(epgsql_temp),
             Datasource= "erbi:temp:base_driver=epgsql;data_dir="++
                 proplists:get_value(data_dir,Config,"")++
                 ";init_files="++proplists:get_value(sql_init_files,Config,""),

             ok=erbi_temp_db:start(Datasource),
             {ok,Conn}=erbi:connect(Datasource,"",""),
             {Datasource,Conn}
     end,
     fun({Datasource,_Conn})->
             ok=erbi_temp_db:stop(Datasource)
     end,
     fun({_Datasource,Conn})->
             [%initialized in scripts?
              ?_assertEqual({ok,[[{"id",0},{"name","Unknown"}],
                                 [{"id",1},{"name","This is a name"}]]},
                            ?debugVal(erbi_connection:selectall_proplist(Conn,"select * from test_temp"))),
                                                % Checking if calls are made to base driver corrctly,
                                                % Base driver smarts tested in driver test module
              ?_assertEqual({ok,unknown},
                            ?debugVal(erbi_connection:do(Conn,"CREATE TABLE test_temp2 (id bigserial,name text)"))),
              ?_assertEqual({ok,1},
                            ?debugVal(erbi_connection:do(Conn,"INSERT INTO test_temp2 (id, name) VALUES ($1, $2)",[2,"AnotherName"]))),
              ?_assertEqual({ok,[[{"id",2},
                                  {"name","AnotherName"}]]},
                            ?debugVal(erbi_connection:selectall_proplist(Conn,"select * from test_temp2"))),
              ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn))),
              ?_assertEqual(ok , ?debugVal(erbi_connection:rollback(Conn))),
              ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn,"savepoint"))),
              ?_assertEqual(ok , ?debugVal(erbi_connection:rollback(Conn,"savepoint"))),
              ?_assertEqual(ok , ?debugVal(erbi_connection:disconnect(Conn)))
             ]
     end}.

temp_epgsql_autoclean_on_start_test_()->
    [{setup,
     fun()->
             Config = erbi_test_util:config(epgsql_temp),
             Datasource = "erbi:temp:base_driver=epgsql;data_dir="++
                 proplists:get_value(data_dir,Config,""),
             erbi_temp_db:start(Datasource),
             Datasource
     end,
     fun(Datasource)->
        erbi_temp_db:stop(Datasource)
    end,
     fun(Datasource)->
             [{timeout, 500,?_assertEqual(ok, erbi_temp_db:start(Datasource))}, % Cleans previous instance and starts another
              ?_assertEqual(true, filelib:is_dir(erbi_temp_db:data_dir(Datasource)))]
     end},
     {setup,
     fun()->
             Config = erbi_test_util:config(epgsql_temp),
             Datasource = "erbi:temp:base_driver=epgsql;auto_clean=false;
                data_dir="++
                 proplists:get_value(data_dir,Config,""),
              erbi_temp_db:start(Datasource),
             Datasource
     end,
     fun(Datasource)->
        erbi_temp_db:stop(Datasource)
    end,
     fun(Datasource)->
             [?_assertException(error,{badmatch,{ok,{exit_status,1},undefined}}, erbi_temp_db:start(Datasource)), % Cleans previous instance and starts another
              ?_assertEqual(true, filelib:is_dir(erbi_temp_db:data_dir(Datasource)))]
     end}].
