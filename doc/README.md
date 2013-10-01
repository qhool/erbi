

# ERBI - ERlang dataBase Interface #

__Authors:__ Josh Burroughs ([`jburroughs@voalte.com`](mailto:jburroughs@voalte.com)).


This package intends to provide a simple database interface for erlang code,
making it possible to move existing code to a different database engine with 
at worst only a change of any embedded queries.  It is strongly inspired by Perl's 
excellent DBI.pm

The public interface of ERBI is divided into three modules:



<dt>erbi</dt>




<dd>The starting point: create a connection and anything done without an active connection.</dd>




<dt>erbi_connection</dt>




<dd>Transaction control, statement preparation, all-in-one convenience functions, disconnect.</dd>




<dt>erbi_statement</dt>




<dd>Manipulating prepared statements - bind parameters, execute, fetch rows.</dd>




## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="erbi.md" class="module">erbi</a></td></tr>
<tr><td><a href="erbi_connection.md" class="module">erbi_connection</a></td></tr>
<tr><td><a href="erbi_statement.md" class="module">erbi_statement</a></td></tr></table>

