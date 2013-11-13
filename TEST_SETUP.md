Running Tests
=============
You must edit test.config to supply the correct connection parameters for testing database drivers.


Neo4j
-----
If you do not want to run the neo4j tests which require a database connection, simply comment out the two 'datasource' lines in the neo4j section.

If you obtain the latest neo4j community-edition from http://www.neo4j.org/download, unpack and run:

 $ bin/neo4j start

This will start the neo4j database on the default port, with no access controls -- tests should run as is.  Uncomment 'transactional_write' and 'nontransactional_write' to perform tests which modify the database.

Postgres:
---------
It is currently not possible to skip the postgres tests.

epgsql does not support unix-domain socket connections (the joy of not being able to call c functions), so postgres must be configure to listen on a tcp port.  You must supply a user which has permissions to create & delete tables.  Put hostname and port into the connect string if not using default values.  It is recommended that you create a database for these tests.