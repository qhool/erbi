%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbdrv_epgsql_test).
-include_lib("eunit/include/eunit.hrl").
-include("erbi.hrl").

-define(CREATE_TABLE,"CREATE TABLE Persons (PersonID int, LastName varchar(255), FirstName varchar(255), Address varchar(255), City varchar(255))").
-define(SELECT_ALL_QUERY,"SELECT * FROM Persons").
-define(SELECT_BIND,"Select * from Persons p where p.PersonId= $1").
-define(INSERT,"insert into Persons (PersonId, LastName, FirstName, Address, City) values ($1, $2,$3,$4,$5 )").
-define(DELETE_TABLE,"drop table Persons").
-define(DATA1,[1, "Ruiz","Cris","Mierda","Coru"]).
-define(DATA2,[3, "Ruiz","Cris","Mierda","Coru"]).
-define(DATABASE,"mydatabase").
-define(USER,"postgres").
-define(PASSWORD,"a8pr3Nuq").


connect_epgsql_test_()->
    [ ?_test( { ok, _ } = ?debugVal(erbi:connect( "erbi:epgsql:database=mydatabase", "postgres", "a8pr3Nuq" )) ),
      ?_test( { error, _ } = ?debugVal(erbi:connect( "erbi:epgsql:database=mydatabase", "", "" )) ),
      ?_assertEqual( { error,
                {invalid_datasource,
                 {missing_properties,[database]}} } ,
                  ?debugVal(erbi:connect( "erbi:epgsql:", "postgres", "a8pr3Nuq" )) ),
      
      ?_test({error,{unknown_host,_}}= ?debugVal(erbi:connect( "erbi:epgsql:database=mydatabase;host=myhost", "postgres", "a8pr3Nuq" )) )
    ].


disconect_epgsql_test()->
     Conn= connect(),
     ok= ?debugVal(erbi_connection:disconnect(Conn)).

all_test_()->
     Conn=connect(),
    lists:flatten([
     ?_assertEqual({ok,unknown},?debugVal(erbi_connection:do(Conn,?CREATE_TABLE))),
    erbi_transaction(Conn),
     erbi_selectall(Conn),
     erbi_selectrow(Conn),             
    ?_assertEqual({ok,unknown},?debugVal(erbi_connection:do(Conn,?DELETE_TABLE)))
                   ]).




erbi_transaction(Conn)->
   OutsideTransConnect=connect(),
 
   [
    ?_assertEqual({ok,[]}, ?debugVal(erbi_connection:selectall_list(Conn,?SELECT_ALL_QUERY))), 
    ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn))),
    ?_assertEqual({ok,unknown},  ?debugVal(insert_data(Conn,?INSERT,?DATA1))),
    ?_assertEqual({ok,[?DATA1]}, ?debugVal(erbi_connection:selectall_list(Conn,?SELECT_ALL_QUERY))),
    
    ?_assertEqual(ok , ?debugVal(erbi_connection:rollback(Conn))),
    ?_assertEqual({ok,[]}, ?debugVal(erbi_connection:selectall_list(Conn,?SELECT_ALL_QUERY))),

    ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn))),
    ?_assertEqual({ok,unknown},  ?debugVal(insert_data(Conn,?INSERT,?DATA1))),
    ?_assertEqual({ok,[?DATA1]}, ?debugVal(erbi_connection:selectall_list(Conn,?SELECT_ALL_QUERY))),
     
    ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn,"savepoint"))),
    ?_assertEqual({ok,unknown},  ?debugVal(insert_data(Conn,?INSERT,?DATA2))),
    ?_assertEqual({ok,[?DATA1,?DATA2]}, ?debugVal(erbi_connection:selectall_list(Conn,?SELECT_ALL_QUERY))),

    ?_assertEqual({ok,[]},?debugVal(erbi_connection:selectall_list(OutsideTransConnect,?SELECT_ALL_QUERY))), 
    
    ?_assertEqual(ok , ?debugVal(erbi_connection:rollback(Conn,"savepoint"))),
    ?_assertEqual({ok,[?DATA1]}, ?debugVal(erbi_connection:selectall_list(Conn,?SELECT_ALL_QUERY))),
     
    ?_assertEqual(ok , ?debugVal(erbi_connection:rollback(Conn))),
    ?_assertEqual({ok,[]}, ?debugVal(erbi_connection:selectall_list(Conn,?SELECT_ALL_QUERY))),

    ?_assertEqual(ok , ?debugVal(erbi_connection:begin_work(Conn))),
    ?_assertEqual({ok,unknown},  ?debugVal(insert_data(Conn,?INSERT,?DATA2))),
    
    ?_assertEqual(ok , ?debugVal(erbi_connection:commit(Conn))),

    ?_assertEqual({ok,[?DATA2]},?debugVal(erbi_connection:selectall_list(OutsideTransConnect,?SELECT_ALL_QUERY)))
    
   ].

insert_data(Conn,InsertQuery,Data)->
     erbi_connection:do(Conn,InsertQuery,Data).              

%% populate_db(Conn,InsertQuery,DataList)->
%%     lists:map(
%%       insert_data(Conn,InsertQuery,DataList),DataList).

erbi_selectall(Conn)->
    [?_test({ok,_}= ?debugVal(erbi_connection:selectall_list(Conn,?SELECT_ALL_QUERY))),
     ?_test({ok,_}= ?debugVal(erbi_connection:selectall_proplist(Conn,?SELECT_ALL_QUERY))),
     ?_test({ok,_}= ?debugVal(erbi_connection:selectall_dict(Conn,?SELECT_ALL_QUERY))) ].

erbi_selectrow(Conn)->
    [?_test(exhausted= ?debugVal(erbi_connection:selectrow_list(Conn,?SELECT_BIND,[3]))),
     ?_test({ok,_}= ?debugVal(erbi_connection:selectrow_proplist(Conn,?SELECT_BIND,[3]))),
     ?_test({ok,_}= ?debugVal(erbi_connection:selectrow_dict(Conn,?SELECT_BIND,[3]))) ].


connect()->
    element(2,erbi:connect( "erbi:epgsql:database=mydatabase", "postgres", "a8pr3Nuq" )).                       


