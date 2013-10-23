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
-define(DATA,[123, 'Ruiz','Cris','Mierda','Coru']).
-define(DATA2,[3, 'Ruiz','Cris','Mierda','Coru']).
epgsql_dataset()->
     case file:consult("test/drivers/epgsql_test.dat") of
        {ok,Terms} ->
            Terms;
        {error,Reason} ->
            {error,Reason}
    end.

erbi_connect_epgsql_test_()->
    [ ?_test( { ok, _ } = ?debugVal(erbi:connect( "erbi:epgsql:database=mydatabase", "postgres", "a8pr3Nuq" )) ),
      ?_test( { error, _ } = ?debugVal(erbi:connect( "erbi:epgsql:database=mydatabase", "", "" )) ),
      ?_test( { error,
                {invalid_datasource,
                 {missing_properties,[database]}} } =
                  ?debugVal(erbi:connect( "erbi:epgsql:param=param", "postgres", "a8pr3Nuq" )) ),
      ?_test({error,{unknown_host,_}}= ?debugVal(erbi:connect( "erbi:epgsql:database=mydatabase;host=myhost", "postgres", "a8pr3Nuq" )) )
    ].


erbi_disconect_epgsql_test()->
    Conn= connect(),
    ok= ?debugVal(erbi_connection:disconnect(Conn)).

erbi_transaction_epgsql_test()->
    Conn=connect(),
    ok = ?debugVal(erbi_connection:begin_work(Conn)),
    ok = ?debugVal(erbi_connection:rollback(Conn)),
    ok = ?debugVal(erbi_connection:begin_work(Conn,"savepoint")),
    ok = ?debugVal(erbi_connection:rollback(Conn,"savepoint")),
    ok = ?debugVal(erbi_connection:rollback(Conn)),
    ok = ?debugVal(erbi_connection:commit(Conn)).

create_table_test()->
 Conn=connect(),
 ?debugVal(create_table(Conn)), 
 {error,{invalid_datasource,_}}=?debugVal(do(Conn,?CREATE_TABLE,[])).

erbi_prepare_epgsql_test()->
  Conn=connect(),
  ?debugVal(erbi_connection:prepare(Conn,?SELECT_BIND)).


    
erbi_bind_get_epgsql_test() ->
    Conn=connect(),
    {ok,Stmt}=erbi_connection:prepare(Conn,?SELECT_BIND),
    ?debugVal(erbi_statement:bind_params(Stmt,[3])).
                                

%% populate_epgsql_test()->
%%     {ok,A}=?debugVal(pgsql:connect("localhost","postgres","a8pr3Nuq", [{database,"mydatabase"}])),
%%     {ok, Statement} =?debugVal( pgsql:parse(A, [], "insert into Persons (PersonId, LastName, FirstName, Address, City) values ($1, $2,$3,$4,$5 )",[])),
%%     ok = pgsql:bind(A, Statement, [],[1, 'Ruiz','Cris','Mierda','Coru']),
%%     ?debugVal(pgsql:execute(A, Statement, [], 1)),
%%     select_epgsql(A).

%% select_epgsql(A)->
%%    {ok, Statement} = pgsql:parse(A, [], "SELECT * FROM Persons", []),
%%  ok = pgsql:bind(A, Statement, [], []),
%%    ?debugVal(pgsql:execute(A, Statement, [], 1)).
   

populate_db_test()->
    Conn=connect(),
    do(Conn,?INSERT,?DATA),
    prepare_bind_execute(Conn,?INSERT,?DATA2),
    List=erbi_select(Conn),
    length(List)==2.

erbi_statement_fetch_all_test()->
    Conn= connect(),
    Stmt=?debugVal(prepare_bind_execute(Conn,?SELECT_ALL_QUERY,[])),
    ?debugVal(erbi_statement:fetchall_list(Stmt)).

erbi_selectall_test_()->
     Conn= connect(),
    [?_test({ok,_}= ?debugVal(erbi_connection:selectall_list(Conn,?SELECT_ALL_QUERY))),
     ?_test({ok,_}= ?debugVal(erbi_connection:selectall_proplist(Conn,?SELECT_ALL_QUERY))),
     ?_test({ok,_}= ?debugVal(erbi_connection:selectall_dict(Conn,?SELECT_ALL_QUERY))) ].

erbi_selectrow_test_()->
      Conn= connect(),
    [?_test({ok,_}= ?debugVal(erbi_connection:selectrow_list(Conn,?SELECT_BIND,[3]))),
     ?_test({ok,_}= ?debugVal(erbi_connection:selectrow_proplist(Conn,?SELECT_BIND,[3]))),
     ?_test({ok,_}= ?debugVal(erbi_connection:selectrow_dict(Conn,?SELECT_BIND,[3]))) ].


prepare_bind_execute(Conn,Query,Data)->    
    {ok,Stmt} =?debugVal( erbi_connection:prepare(Conn,Query)),
   ?debugVal(erbi_statement:bind_params(Stmt,Data)),
    ?debugVal({ok,_}= erbi_statement:execute(Stmt)),
    Stmt.

erbi_select(Conn)->
    {ok,List}= erbi_connection:selectall_list(Conn,?SELECT_ALL_QUERY) ,
    List.

finish_statement(Stmt)->    
    erbi_statement:finish(Stmt).

create_table(Conn)->
    Stmt=prepare_bind_execute(Conn,?CREATE_TABLE,[]),
    erbi_statement:finish(Stmt).

delete_table()->
    %% Stmt=?debugVal(prepare_bind_execute(Conn,?DELETE_TABLE,[])),
    %% ?debugVal(erbi_statement:finish(Stmt)).
    {ok,A}=?debugVal(pgsql:connect("localhost","postgres","a8pr3Nuq", [{database,"mydatabase"}])),
    ?debugVal(pgsql:squery(A, ?DELETE_TABLE)).
    
do(Conn,Query,Data)->
    erbi_connection:do(Conn,Query,Data).

connect()->
    element(2,erbi:connect( "erbi:epgsql:database=mydatabase", "postgres", "a8pr3Nuq" )).                       

delete_table_test()->
 Conn=connect(),
 ?debugVal(delete_table()).
