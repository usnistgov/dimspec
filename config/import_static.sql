/*=============================================================================
	Run this file in sqlite3 to load basic table data into the database using:
	
		.read import_static.sql
	
	You may also directly import these files into the appropriate tables using 
	your SQLite IDE of choice; CSVs are named for their associated tables.
	The OS file separator is assumed to be the "forward slash" ("/") character; 
	if this is inappropriate for your OS, you should globally replace it within 
	this script as appropriate.
=============================================================================*/

.import --csv --skip 1 config/data/elements.csv elements
.import --csv --skip 1 config/data/isotopes.csv isotopes
.import --csv --skip 1 config/data/source_types.csv source_types