

# Module erbi #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td>Connect to a database.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="connect-3"></a>

### connect/3 ###


<pre><code>
connect(ConnectDescriptor::string() | [{atom(), any()}], Username::string(), Password::string()) -&gt; {ok, <a href="#type-erbi_connection">erbi_connection()</a>} | {error, any()}
</code></pre>

<br></br>



Connect to a database.


* Connect  - DB connect term or string "erbi:Driver:params"

* Username

* Password


