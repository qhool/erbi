

# Module erbi_connection #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-conn_private">conn_private()</a> ###


__abstract datatype__: `conn_private()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#begin_work-1">begin_work/1</a></td><td><p>Begins a transaction, or adds a save-point.
Connection
SavePoint  - [optional] name of Save-point</p>.</td></tr><tr><td valign="top"><a href="#begin_work-2">begin_work/2</a></td><td></td></tr><tr><td valign="top"><a href="#commit-1">commit/1</a></td><td><p>Completes the current transaction; all changes are written to the database.
Connection</p>.</td></tr><tr><td valign="top"><a href="#disconnect-1">disconnect/1</a></td><td><p>Close database connection</p>.</td></tr><tr><td valign="top"><a href="#do-2">do/2</a></td><td>

<pre><tt>Same as do(Connection,Statement,[])</tt></pre>
.</td></tr><tr><td valign="top"><a href="#do-3">do/3</a></td><td><p>Execute statement which does not return data.</p>


<pre><tt>Returns count of records affected.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#prepare-2">prepare/2</a></td><td><p>Prepare query/statement for execution.</p>


<pre><tt>Parses statement and returns a handle which can be used to execute and retrieve rows.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#prepare_cached-2">prepare_cached/2</a></td><td><p>Prepare with cacheing.</p>


<pre><tt>This function is the same as prepare/2 except if the statement has been previously
prepared on this connection, a cached statement handle may be returned.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#rollback-1">rollback/1</a></td><td><p>Undoes all changes made during the transaction.
If savepoint is given, undoes changes after that savepoint.
Connection
SavePoint  - [optional]</p>.</td></tr><tr><td valign="top"><a href="#rollback-2">rollback/2</a></td><td></td></tr><tr><td valign="top"><a href="#selectall_dict-2">selectall_dict/2</a></td><td><p>Execute and return dicts</p>


<pre><tt>See selectall_dict/3.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#selectall_dict-3">selectall_dict/3</a></td><td><p>Execute query with bind values and return all records as dicts.</p>


<pre><tt>Each record is returned as a dictionary created by the standard "dict" module.
See selectall_list/3 for more information.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#selectall_list-2">selectall_list/2</a></td><td><p>Execute and return lists.</p>


<pre><tt>See selectall_list/3 for details.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#selectall_list-3">selectall_list/3</a></td><td><p>Execute query with bind values and return all records as lists.</p>


<pre><tt>Results are returned as a list of lists.  Each list contains the values,
in order for that row. This is a convenience method, equivalent to:
<pre>
Stmt = Connection:prepare( Statement ),
Result = Stmt:fetchall_list( BindValues ),
Stmt:finish(),
Result
</pre></tt></pre>
.</td></tr><tr><td valign="top"><a href="#selectall_proplist-2">selectall_proplist/2</a></td><td><p>Execute and return proplists.</p>


<pre><tt>See selectall_proplist/3.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#selectall_proplist-3">selectall_proplist/3</a></td><td><p>Execute query with bind values and return all records as proplists.</p>


<pre><tt>Each returned record is a proplist; otherwise same as selectall_list/3.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#selectrow_dict-2">selectrow_dict/2</a></td><td></td></tr><tr><td valign="top"><a href="#selectrow_dict-3">selectrow_dict/3</a></td><td></td></tr><tr><td valign="top"><a href="#selectrow_list-2">selectrow_list/2</a></td><td></td></tr><tr><td valign="top"><a href="#selectrow_list-3">selectrow_list/3</a></td><td><p>Execute Statement and return one record.
Statement is closed and remaining data discarded.
Connection
Statement   - Statement to execute
BindValues  - [Optional] parameters.</p>.</td></tr><tr><td valign="top"><a href="#selectrow_proplist-2">selectrow_proplist/2</a></td><td></td></tr><tr><td valign="top"><a href="#selectrow_proplist-3">selectrow_proplist/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="begin_work-1"></a>

### begin_work/1 ###


<pre><code>
begin_work(Connection::<a href="#type-erbi_connection">erbi_connection()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


<p>Begins a transaction, or adds a save-point.
Connection
SavePoint  - [optional] name of Save-point</p>

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


<p>Completes the current transaction; all changes are written to the database.
Connection</p>

<a name="disconnect-1"></a>

### disconnect/1 ###


<pre><code>
disconnect(Connection::<a href="#type-erbi_connection">erbi_connection()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


<p>Close database connection</p>

<a name="do-2"></a>

### do/2 ###


<pre><code>
do(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, non_neg_integer()} | {error, any()}
</code></pre>

<br></br>




<pre><tt>Same as do(Connection,Statement,[])</tt></pre>


<a name="do-3"></a>

### do/3 ###


<pre><code>
do(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, non_neg_integer()} | {error, any()}
</code></pre>

<br></br>


<p>Execute statement which does not return data.</p>


<pre><tt>Returns count of records affected.</tt></pre>


<a name="prepare-2"></a>

### prepare/2 ###


<pre><code>
prepare(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, <a href="#type-erbi_statement">erbi_statement()</a>} | {error, any()}
</code></pre>

<br></br>


<p>Prepare query/statement for execution.</p>


<pre><tt>Parses statement and returns a handle which can be used to execute and retrieve rows.</tt></pre>


<a name="prepare_cached-2"></a>

### prepare_cached/2 ###


<pre><code>
prepare_cached(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, <a href="#type-erbi_statement">erbi_statement()</a>} | {error, any()}
</code></pre>

<br></br>


<p>Prepare with cacheing.</p>


<pre><tt>This function is the same as prepare/2 except if the statement has been previously
prepared on this connection, a cached statement handle may be returned.</tt></pre>


<a name="rollback-1"></a>

### rollback/1 ###


<pre><code>
rollback(Connection::<a href="#type-erbi_connection">erbi_connection()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


<p>Undoes all changes made during the transaction.
If savepoint is given, undoes changes after that savepoint.
Connection
SavePoint  - [optional]</p>

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


<p>Execute and return dicts</p>


<pre><tt>See selectall_dict/3.</tt></pre>


<a name="selectall_dict-3"></a>

### selectall_dict/3 ###


<pre><code>
selectall_dict(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, [dict()]} | {error, any()}
</code></pre>

<br></br>


<p>Execute query with bind values and return all records as dicts.</p>


<pre><tt>Each record is returned as a dictionary created by the standard "dict" module.
See selectall_list/3 for more information.</tt></pre>


<a name="selectall_list-2"></a>

### selectall_list/2 ###


<pre><code>
selectall_list(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, [[any()]]} | {error, any()}
</code></pre>

<br></br>


<p>Execute and return lists.</p>


<pre><tt>See selectall_list/3 for details.</tt></pre>


<a name="selectall_list-3"></a>

### selectall_list/3 ###


<pre><code>
selectall_list(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, [[any()]]} | {error, any()}
</code></pre>

<br></br>


<p>Execute query with bind values and return all records as lists.</p>


<pre><tt>Results are returned as a list of lists.  Each list contains the values,
in order for that row. This is a convenience method, equivalent to:
<pre>
Stmt = Connection:prepare( Statement ),
Result = Stmt:fetchall_list( BindValues ),
Stmt:finish(),
Result
</pre></tt></pre>


<a name="selectall_proplist-2"></a>

### selectall_proplist/2 ###


<pre><code>
selectall_proplist(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any()) -&gt; {ok, [[{atom(), any()}]]} | {error, any()}
</code></pre>

<br></br>


<p>Execute and return proplists.</p>


<pre><tt>See selectall_proplist/3.</tt></pre>


<a name="selectall_proplist-3"></a>

### selectall_proplist/3 ###


<pre><code>
selectall_proplist(Connection::<a href="#type-erbi_connection">erbi_connection()</a>, Statement::any(), BindValues::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; {ok, [[{atom(), any()}]]} | {error, any()}
</code></pre>

<br></br>


<p>Execute query with bind values and return all records as proplists.</p>


<pre><tt>Each returned record is a proplist; otherwise same as selectall_list/3.</tt></pre>


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


<p>Execute Statement and return one record.
Statement is closed and remaining data discarded.
Connection
Statement   - Statement to execute
BindValues  - [Optional] parameters.</p>

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



