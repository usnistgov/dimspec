/*=============================================================================
	Description
		Create a database structure to hold results for non-targeted 
		analysis (NTA) high-resolution accurate-mass mass spectrometry 
		(HRAM-MS) experiments using the schema defined in this directory.
		
	Status
		Development
		
	LastUpdate
		2022-03-31
		
	Support
		For information or support, contact the development team at
			- NIST PFAS Program	PFAS@nist.gov
			- Jared M. Ragland	jared.ragland@nist.gov	*author
			- Benjamin J. Place	benjamin.place@nist.gov
			
	Dependencies
		sqlite3 (https://sqlite.org/version3.html)
		
	Usage
		Run this script from the terminal to build the schema. It is 
		recommended to run from the project directory as
		
			sqlite3 nist_nta_dev.sqlite
			.read config/build.sql
		
	Details
		Build files are located in the "config" directory and local
		paths will need to be referenced appropriately, which may
		require modifications to this script. This mostly applies to 
		the commands in this introductory section and at the end of 
		this script file where default tables are populated.
		
		This will create the schema on your local machine for local
		population.	Example data (a full demonstration version will be
		provided in the future) can be read in with
		
			.read config/populate_demo.sql
		
		See the build history after build with
			
			SELECT * FROM version_history;
			
		By default, this uses the write ahead log (WAL) mode to increase
		read speeds	(https://sqlite.org/wal.html), with the default page
		limit (1000 pages). Simply set those here, or change them 
		manually with the CLI if your use case requires a different
		journal mode or page limit. 
		
=============================================================================*/

PRAGMA journal_mode=WAL;
/* Reference node */
.read config/sql_nodes/reference.sql
.read config/sql_nodes/reference_data.sql
/* Method node */
.read config/sql_nodes/methods.sql
/* Analyte node */
.read config/sql_nodes/analyte.sql
/* Data node */
.read config/sql_nodes/data.sql
/* Contributors node */
.read config/sql_nodes/contributors.sql
.read config/sql_nodes/contributors_data.sql
/* Logging node - optional, comment out to skip */
.read config/sql_nodes/logging.sql
/* .read config/sql_nodes/auto_logs.sql */
/* Additional scripts to run - comment out to skip */
.read config/sql_nodes/auto_triggers.sql
.read config/sql_nodes/auto_views.sql
PRAGMA foreign_keys=on;
insert into logs (category, description, bundle, effect, affects_table, executed_from) values ('build', 'build DIMSpec database', 'install', 4, 'all', 'script');
