

# Module erbi #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)




<pre><tt>== ERBI ERlang unified dataBase Interface ==
This package intends to provide a simple database interface for erlang code,
making it possible to move existing code to a different database engine with
at worst only a change of any embedded queries.  It is strongly inspired by Perl's excellent
DBI.pm</tt></pre>
.
__Version:__ Sep 26 2013 17:23:17

__Authors:__ Josh Burroughs ([`jburroughs@voalte.com`](mailto:jburroughs@voalte.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td><p>Connect to a database.
<ul>
<li>Connect  - DB connect term or string "erbi:Driver:params"</li>
<li>Username</li>
<li>Password</li>
</ul></p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="connect-3"></a>

### connect/3 ###


<pre><code>
connect(ConnectDescriptor::string() | [{atom(), any()}], Username::string(), Password::string()) -&gt; {ok, <a href="#type-erbi_connection">erbi_connection()</a>} | {error, any()}
</code></pre>

<br></br>


<p>Connect to a database.
<ul>
<li>Connect  - DB connect term or string "erbi:Driver:params"</li>
<li>Username</li>
<li>Password</li>
</ul></p>

