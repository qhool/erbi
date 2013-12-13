Using temp database
=================
Temp erbi temp database is  used to wrap an erbi driver, and add to it the functionality of
launching and removing an autocongigured temporary instance of the underlying database.
Specially used for testing purposes.

This driver provides two external API functions:
- erbi_temp_db:start/1 : Takes a data source descriptor and startst the temporary database instance acording
to the data provided.
- erbi_temp_db:stop/1 : Takes the data source used to start the instance, stops that instance and removes any
 data related to the temporary instance.

These functions are intended to be called in test setup/cleanup.

The data source must have the format "erbi:temp:Parameters:Args", where the specific parameters
 for this driver are:
- base_driver : Required parameter that must contain the name of the driver whose temporary 
instance will be created.
- data_dir : Base directory where the data of the temporary instance will be stored.
If it is not supplied, default is code:get_path/0.
- init_files : Comma separated path files, that can contain any instruction to
initialize the db (schema, data..). The current working directory will be used as base directory for the relative paths.
For example, while executing common tests, base directory will be *APP_ROOT_DIR/test/logs/ct.TIMESTAMP/*, while with eunit
the base dir will be *APP_ROOT_DIR/test/.eunit/*.
- bin_dir : Directory where to find database binaries. If not provided the base driver 
should try to find them automatically, searching searching in some known paths and in system "PATH".
- Any other parameter needed by the base driver. However, depending on the base driver
 implementation, these parameters can be overwriten in the temp initialization.


Wraping neo4j driver
--------------------
Erbi Neo4j driver has been adapted to be used in temporary mode.
A separated instance of neo4j will be started in a port between 7475 and 8475.
No neo4j specific parameters are necessary. However, the endpoint parameter can be added to modify the default one.

Wraping epgsql driver
---------------------
Erbi epgsql driver has been adapted to be used in temporary mode. 
The temporary epgsql driver initialization will start a separated instance of postgres, 
that will be listening in a port chosen by the driver in the interval between 5433 and 5533. 
The data used by this postgres instance will be stored in a subdirectory of "data_dir" parameter.

To use temp driver with epgsql, data source does not need any parameters but those specific to temp driver.
However, the following parameter can be added:
- database : If this parameters is provided, driver assumes that this database is created in the initialization scripts.
If is not provided connections will be perform to a default database.

Also, while using temp drivers with epgsql, it is not necessary to provide an username
 and a password to connect to database as it is configures in *trust* mode.
If an username is provided is must be created in the initialization scripts, if it is not provided,
connections will be performed using the current user.

Example of temp data source that wraps epgsql driver:

	"erbi:temp:base_driver=epgsql;data_dir=data_dir/;init_files=dbschema/schema.sql,dbschema/data.sql"
	"erbi:temp:base_driver=epgsql;data_dir=data_dir/;init_files=dbschema/schema.sql,dbschema/ref-data.sql;bin_dir=/usr/local/postgres/bin"
  
