

# Module erbi_statement #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-stmt_private">stmt_private()</a> ###


__abstract datatype__: `stmt_private()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bind_params-2">bind_params/2</a></td><td>Bind given parameters to this statement.</td></tr><tr><td valign="top"><a href="#execute-1">execute/1</a></td><td>Begin execution of this statement.</td></tr><tr><td valign="top"><a href="#execute-2">execute/2</a></td><td></td></tr><tr><td valign="top"><a href="#fetchall_dict-1">fetchall_dict/1</a></td><td></td></tr><tr><td valign="top"><a href="#fetchall_list-1">fetchall_list/1</a></td><td>
Fetch all remaining records and return a list.</td></tr><tr><td valign="top"><a href="#fetchall_proplist-1">fetchall_proplist/1</a></td><td></td></tr><tr><td valign="top"><a href="#fetchrow_dict-1">fetchrow_dict/1</a></td><td></td></tr><tr><td valign="top"><a href="#fetchrow_list-1">fetchrow_list/1</a></td><td>Fetch a single record from the result set.</td></tr><tr><td valign="top"><a href="#fetchrow_proplist-1">fetchrow_proplist/1</a></td><td></td></tr><tr><td valign="top"><a href="#finish-1">finish/1</a></td><td>Complete processing with this statement.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bind_params-2"></a>

### bind_params/2 ###


<pre><code>
bind_params(Statement::<a href="#type-erbi_statement">erbi_statement()</a>, Params::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


Bind given parameters to this statement.
May be supplied as a positional list, or by name (contingent on driver support).
- Statement  - erbi statement handle
- BindValues - List of parameter values
<a name="execute-1"></a>

### execute/1 ###


<pre><code>
execute(Statement::<a href="#type-erbi_statement">erbi_statement()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


Begin execution of this statement
<a name="execute-2"></a>

### execute/2 ###


<pre><code>
execute(Statement::<a href="#type-erbi_statement">erbi_statement()</a>, Params::<a href="#type-erbi_bind_values">erbi_bind_values()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>



<a name="fetchall_dict-1"></a>

### fetchall_dict/1 ###


<pre><code>
fetchall_dict(Statement::<a href="#type-erbi_statement">erbi_statement()</a>) -&gt; {ok, [dict()]} | {error, any()}
</code></pre>

<br></br>



<a name="fetchall_list-1"></a>

### fetchall_list/1 ###


<pre><code>
fetchall_list(Statement::<a href="#type-erbi_statement">erbi_statement()</a>) -&gt; {ok, [[any()]]} | {error, any()}
</code></pre>

<br></br>



Fetch all remaining records and return a list.
<a name="fetchall_proplist-1"></a>

### fetchall_proplist/1 ###


<pre><code>
fetchall_proplist(Statement::<a href="#type-erbi_statement">erbi_statement()</a>) -&gt; {ok, [[{atom(), any()}]]} | {error, any()}
</code></pre>

<br></br>



<a name="fetchrow_dict-1"></a>

### fetchrow_dict/1 ###


<pre><code>
fetchrow_dict(Statement::<a href="#type-erbi_statement">erbi_statement()</a>) -&gt; {ok, dict()} | {error, any()}
</code></pre>

<br></br>



<a name="fetchrow_list-1"></a>

### fetchrow_list/1 ###


<pre><code>
fetchrow_list(Statement::<a href="#type-erbi_statement">erbi_statement()</a>) -&gt; {ok, [any()]} | {error, any()}
</code></pre>

<br></br>


Fetch a single record from the result set.
<a name="fetchrow_proplist-1"></a>

### fetchrow_proplist/1 ###


<pre><code>
fetchrow_proplist(Statement::<a href="#type-erbi_statement">erbi_statement()</a>) -&gt; {ok, [{atom(), any()}]} | {error, any()}
</code></pre>

<br></br>



<a name="finish-1"></a>

### finish/1 ###


<pre><code>
finish(Statement::<a href="#type-erbi_statement">erbi_statement()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


Complete processing with this statement.
Closes associated cursor.
