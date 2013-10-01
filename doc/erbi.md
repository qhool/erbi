

# Module erbi #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



Main erbi module.
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




### <a name="type-erbi_connection">erbi_connection()</a> ###



<pre><code>
erbi_connection() = {erbi_connection, <a href="erbi_connection.md#type-conn_private">erbi_connection:conn_private()</a>}
</code></pre>





### <a name="type-erbi_data_source">erbi_data_source()</a> ###



<pre><code>
erbi_data_source() = #erbi{}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td>Connect to a database.</td></tr><tr><td valign="top"><a href="#parse_data_source-1">parse_data_source/1</a></td><td>Parse data source string.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="connect-3"></a>

### connect/3 ###


<pre><code>
connect(DataSource::string() | <a href="#type-erbi_data_source">erbi_data_source()</a>, Username::string() | atom(), Password::string() | atom()) -&gt; {ok, <a href="#type-erbi_connection">erbi_connection()</a>} | {error, any()}
</code></pre>

<br></br>



Connect to a database.


* Connect  - DB connect term or string "erbi:Driver:params"

* Username

* Password


<a name="parse_data_source-1"></a>

### parse_data_source/1 ###


<pre><code>
parse_data_source(DataSource::string()) -&gt; {ok, <a href="#type-erbi_data_source">erbi_data_source()</a>} | {error, any()}
</code></pre>

<br></br>



Parse data source string.


Takes a data source descriptor in the form "erbi:driver:arg=val;arg=val[...]"
and returns an erbi_data_source() value.
