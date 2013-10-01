

# Module erbi_connection #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



Functions for manipulating erbi connection state, and db convenience functions.
Copyright (c) 2013 Voalte Inc. <jburroughs@voalte.com>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


<a name="types"></a>

## Data Types ##




### <a name="type-conn_private">conn_private()</a> ###


__abstract datatype__: `conn_private()`




### <a name="type-erbi_bind_value">erbi_bind_value()</a> ###



<pre><code>
erbi_bind_value() = <a href="#type-erbi_bind_value_typed">erbi_bind_value_typed()</a> | <a href="#type-erbi_bind_value_untyped">erbi_bind_value_untyped()</a>
</code></pre>





### <a name="type-erbi_bind_value_named">erbi_bind_value_named()</a> ###



<pre><code>
erbi_bind_value_named() = <a href="#type-erbi_bind_value_named_typed">erbi_bind_value_named_typed()</a> | <a href="#type-erbi_bind_value_named_untyped">erbi_bind_value_named_untyped()</a>
</code></pre>





### <a name="type-erbi_bind_value_named_typed">erbi_bind_value_named_typed()</a> ###



<pre><code>
erbi_bind_value_named_typed() = {<a href="#type-erbi_identifier">erbi_identifier()</a>, <a href="#type-erbi_value_type">erbi_value_type()</a>, any()}
</code></pre>





### <a name="type-erbi_bind_value_named_untyped">erbi_bind_value_named_untyped()</a> ###



<pre><code>
erbi_bind_value_named_untyped() = {<a href="#type-erbi_identifier">erbi_identifier()</a>, any()}
</code></pre>





### <a name="type-erbi_bind_value_typed">erbi_bind_value_typed()</a> ###



<pre><code>
erbi_bind_value_typed() = {<a href="#type-erbi_value_type">erbi_value_type()</a>, any()}
</code></pre>





### <a name="type-erbi_bind_value_untyped">erbi_bind_value_untyped()</a> ###



<pre><code>
erbi_bind_value_untyped() = any()
</code></pre>





### <a name="type-erbi_bind_values">erbi_bind_values()</a> ###



<pre><code>
erbi_bind_values() = [<a href="#type-erbi_bind_value">erbi_bind_value()</a>] | [<a href="#type-erbi_bind_value_named">erbi_bind_value_named()</a>]
</code></pre>





### <a name="type-erbi_connection">erbi_connection()</a> ###



<pre><code>
erbi_connection() = {erbi_connection, <a href="erbi_connection.md#type-conn_private">erbi_connection:conn_private()</a>}
</code></pre>





### <a name="type-erbi_identifier">erbi_identifier()</a> ###



<pre><code>
erbi_identifier() = atom() | string() | binary()
</code></pre>





### <a name="type-erbi_statement">erbi_statement()</a> ###



<pre><code>
erbi_statement() = {erbi_statement, <a href="erbi_statement.md#type-stmt_private">erbi_statement:stmt_private()</a>}
</code></pre>





### <a name="type-erbi_value_type">erbi_value_type()</a> ###



<pre><code>
erbi_value_type() = atom()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#begin_work-1">begin_work/1</a></td><td>Begins a transaction, or adds a save-point.</td></tr><tr><td valign="top"><a href="#begin_work-2">begin_work/2</a></td><td></td></tr><tr><td valign="top"><a href="#commit-1">commit/1</a></td><td>Completes the current transaction; all changes are written to the database.</td></tr><tr><td valign="top"><a href="#disconnect-1">disconnect/1</a></td><td>Close database connection.</td></tr><tr><td valign="top"><a href="#do-2">do/2</a></td><td><p></p>Same as do(Connection,Statement,[]).</td></tr><tr><td valign="top"><a href="#do-3">do/3</a></td><td>Execute statement which does not return data.</td></tr><tr><td valign="top"><a href="#prepare-2">prepare/2</a></td><td>Prepare query/statement for execution.</td></tr><tr><td valign="top"><a href="#prepare_cached-2">prepare_cached/2</a></td><td>Prepare with cacheing.</td></tr><tr><td valign="top"><a href="#rollback-1">rollback/1</a></td><td>Undoes all changes made during the transaction.</td></tr><tr><td valign="top"><a href="#rollback-2">rollback/2</a></td><td></td></tr><tr><td valign="top"><a href="#selectall_dict-2">selectall_dict/2</a></td><td>Execute and return dicts.</td></tr><tr><td valign="top"><a href="#selectall_dict-3">selectall_dict/3</a></td><td>Execute query with bind values and return all records as dicts.</td></tr><tr><td valign="top"><a href="#selectall_list-2">selectall_list/2</a></td><td>Execute and return lists.</td></tr><tr><td valign="top"><a href="#selectall_list-3">selectall_list/3</a></td><td>Execute query with bind values and return all records as lists.</td></tr><tr><td valign="top"><a href="#selectall_proplist-2">selectall_proplist/2</a></td><td>Execute and return proplists.</td></tr><tr><td valign="top"><a href="#selectall_proplist-3">selectall_proplist/3</a></td><td>Execute query with bind values and return all records as proplists.</td></tr><tr><td valign="top"><a href="#selectrow_dict-2">selectrow_dict/2</a></td><td></td></tr><tr><td valign="top"><a href="#selectrow_dict-3">selectrow_dict/3</a></td><td></td></tr><tr><td valign="top"><a href="#selectrow_list-2">selectrow_list/2</a></td><td></td></tr><tr><td valign="top"><a href="#selectrow_list-3">selectrow_list/3</a></td><td>Execute Statement and return one record.</td></tr><tr><td valign="top"><a href="#selectrow_proplist-2">selectrow_proplist/2</a></td><td></td></tr><tr><td valign="top"><a href="#selectrow_proplist-3">selectrow_proplist/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="begin_work-1"></a>

### begin_work/1 ###


<pre><code>
begin_work(Connection::<a href="#type-erbi_connection">erbi_connection()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


Begins a transaction, or adds a save-point.
Connection
SavePoint  - [optional] name of Save-point
<a name="begin_work-2"></a>

### begin_work/2 ###


<pre><code>
begin_work(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, SavePoint::<a href="#type-erbi_identifier">erbi_identifier()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>



<a name="commit-1"></a>

### commit/1 ###


<pre><code>
commit(Connection::<a href="#type-erbi_connection">erbi_connection()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


Completes the current transaction; all changes are written to the database.
Connection
<a name="disconnect-1"></a>

### disconnect/1 ###


<pre><code>
disconnect(Connection::<a href="#type-erbi_connection">erbi_connection()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


Close database connection
<a name="do-2"></a>

### do/2 ###


<pre><code>
do(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, non_neg_integer()} | {error, any()}
</code></pre>

<br></br>




Same as do(Connection,Statement,[])
<a name="do-3"></a>

### do/3 ###


<pre><code>
do(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, non_neg_integer()} | {error, any()}
</code></pre>

<br></br>



Execute statement which does not return data.


Returns count of records affected.
<a name="prepare-2"></a>

### prepare/2 ###


<pre><code>
prepare(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, <a href="#type-erbi_statement">erbi_statement()</a>} | {error, any()}
</code></pre>

<br></br>



Prepare query/statement for execution.


Parses statement and returns a handle which can be used to execute and retrieve rows.
<a name="prepare_cached-2"></a>

### prepare_cached/2 ###


<pre><code>
prepare_cached(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, <a href="#type-erbi_statement">erbi_statement()</a>} | {error, any()}
</code></pre>

<br></br>



Prepare with cacheing.


This function is the same as prepare/2 except if the statement has been previously
prepared on this connection, a cached statement handle may be returned.
<a name="rollback-1"></a>

### rollback/1 ###


<pre><code>
rollback(Connection::<a href="#type-erbi_connection">erbi_connection()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


Undoes all changes made during the transaction.
If savepoint is given, undoes changes after that savepoint.
Connection
SavePoint  - [optional]
<a name="rollback-2"></a>

### rollback/2 ###


<pre><code>
rollback(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, SavePoint::<a href="#type-erbi_identifier">erbi_identifier()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>



<a name="selectall_dict-2"></a>

### selectall_dict/2 ###


<pre><code>
selectall_dict(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, [dict()]} | {error, any()}
</code></pre>

<br></br>



Execute and return dicts


See selectall_dict/3.
<a name="selectall_dict-3"></a>

### selectall_dict/3 ###


<pre><code>
selectall_dict(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, [dict()]} | {error, any()}
</code></pre>

<br></br>



Execute query with bind values and return all records as dicts.


Each record is returned as a dictionary created by the standard "dict" module.
See selectall_list/3 for more information.
<a name="selectall_list-2"></a>

### selectall_list/2 ###


<pre><code>
selectall_list(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, [[any()]]} | {error, any()}
</code></pre>

<br></br>



Execute and return lists.


See selectall_list/3 for details.
<a name="selectall_list-3"></a>

### selectall_list/3 ###


<pre><code>
selectall_list(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, [[any()]]} | {error, any()}
</code></pre>

<br></br>



Execute query with bind values and return all records as lists.


Results are returned as a list of lists.  Each list contains the values,
in order for that row. This is a convenience method, equivalent to:

```

  Stmt = Connection:prepare( Statement ),
  Result = Stmt:fetchall_list( BindValues ),
  Stmt:finish(),
  Result
```

<a name="selectall_proplist-2"></a>

### selectall_proplist/2 ###


<pre><code>
selectall_proplist(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, [[{atom(), any()}]]} | {error, any()}
</code></pre>

<br></br>



Execute and return proplists.


See selectall_proplist/3.
<a name="selectall_proplist-3"></a>

### selectall_proplist/3 ###


<pre><code>
selectall_proplist(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, [[{atom(), any()}]]} | {error, any()}
</code></pre>

<br></br>



Execute query with bind values and return all records as proplists.


Each returned record is a proplist; otherwise same as selectall_list/3.
<a name="selectrow_dict-2"></a>

### selectrow_dict/2 ###


<pre><code>
selectrow_dict(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, dict()} | {error, any()}
</code></pre>

<br></br>



<a name="selectrow_dict-3"></a>

### selectrow_dict/3 ###


<pre><code>
selectrow_dict(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, dict()} | {error, any()}
</code></pre>

<br></br>



<a name="selectrow_list-2"></a>

### selectrow_list/2 ###


<pre><code>
selectrow_list(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, [any()]} | {error, any()}
</code></pre>

<br></br>



<a name="selectrow_list-3"></a>

### selectrow_list/3 ###


<pre><code>
selectrow_list(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, [any()]} | {error, any()}
</code></pre>

<br></br>


Execute Statement and return one record.
Statement is closed and remaining data discarded.
Connection
Statement   - Statement to execute
BindValues  - [Optional] parameters.
<a name="selectrow_proplist-2"></a>

### selectrow_proplist/2 ###


<pre><code>
selectrow_proplist(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, [{atom(), any()}]} | {error, any()}
</code></pre>

<br></br>



<a name="selectrow_proplist-3"></a>

### selectrow_proplist/3 ###


<pre><code>
selectrow_proplist(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, [{atom(), any()}]} | {error, any()}
</code></pre>

<br></br>



