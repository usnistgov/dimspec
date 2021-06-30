/*====================================================================================================
Description:	Build a database sketch to hold results for non-targeted analysis high-resolution 
				accurate-mass mass spectrometry (NTA-HRAM-MS) experiments.
Status:			Development version
LastUpdate:		2021-06-03
Support:		For information or support, contact the development team at
					- NIST PFAS Program	PFAS@nist.gov
					- Jared M. Ragland	jared.ragland@nist.gov	*author
					- Benjamin J. Place	benjamin.place@nist.gov
Dependencies:	sqlite3
Usage:			Run this script from the terminal to create a sketch of the SQLite database. It is 
				recommended to run from the project directory as
				
					sqlite3 nist_nta_dev.sqlite
					.read config/build.sql
				
Details:		Build files are located in the "config" directory and subdirectories and local 
				paths will need to be referenced appropriately, which may require modifications 
				to this script. This mostly applies to the commands in this introductory 
				section and at the end of this script file where default tables are populated.
				
				This will create the schema on your local machine for local population.
				Example data (a full demonstration version will be provided in the future) can 
				be read in with
				
					.read config/demo_data.sql
					
				See the build history after build with
					
					SELECT * FROM view_history;
					
				By default, this uses the write ahead log (WAL) mode (https://sqlite.org/wal.html)
				to increase read speeds, with the default page limit (1000 pages). Simply set those
				here, or change them manually with the CLI if your use case requires a different
				journal mode or page limit. 
					
====================================================================================================*/

PRAGMA journal_mode=WAL;
/* Reference node */
.read config/sql_nodes/reference.SQL
.read config/sql_nodes/reference_data.SQL 
/* Method node */
.read config/sql_nodes/methods.SQL 
/* Contributors node */
.read config/sql_nodes/contributors.SQL 
.read config/sql_nodes/contributors_data.SQL 
/* Analyte node */
.read config/sql_nodes/analyte.SQL 
/* Data node */
.read config/sql_nodes/data.SQL
/* Logging node */
.read config/sql_nodes/logging.SQL
/* .read config/sql_nodes/auto_logs.SQL */
