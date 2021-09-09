/*=============================================================================
	Run this file in sqlite3 to load data commonly shared across projects into 
	the database schema using:
	
		.read config/populate_common.sql
	
	Note this will first remove all data within the referenced tables!
	You may also directly import these files into the appropriate tables using 
	your SQLite IDE of choice; CSVs are named for their associated tables.
	The OS file separator is assumed to be the "forward slash" ("/") character; 
	if this is inappropriate for your OS, you should globally replace it within 
	this script as appropriate.
=============================================================================*/

DELETE FROM norm_ionization;
.import --csv --skip 1 config/data/norm_ionization.csv norm_ionization
DELETE FROM norm_source_types;
.import --csv --skip 1 config/data/norm_source_types.csv norm_source_types
DELETE FROM norm_vendors;
.import --csv --skip 1 config/data/norm_vendors.csv norm_vendors
DELETE FROM norm_qc_methods_name;
.import --csv --skip 1 config/data/norm_qc_methods_name.csv norm_qc_methods_name
DELETE FROM norm_qc_methods_reference;
.import --csv --skip 1 config/data/norm_qc_methods_reference.csv norm_qc_methods_reference
