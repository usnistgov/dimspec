/*=============================================================================
	Run this file in sqlite3 to load demonstration data into the database 
	schema using:
	
		.read demo_data.sql
		
	Note this will first remove all data within the referenced tables!
	You may also directly import these files into the appropriate tables using 
	your SQLite IDE of choice; CSVs are named for their associated tables.
	The OS file separator is assumed to be the "forward slash" ("/") character; 
	if this is inappropriate for your OS, you should globally replace it within 
	this script as appropriate.
=============================================================================*/

/* To build in console, use each of the following as needed. */

delete from solvents;
delete from solvent_aliases;
delete from solvent_mix;
delete from compounds;
delete from mobile_phases;
delete from sample_classes;
delete from samples;
delete from vendors;
delete from ms_method;
.import --csv --skip 1 config/demo/solvents.csv solvents 
.import --csv --skip 1 config/demo/solvent_aliases.csv solvent_aliases
.import --csv --skip 1 config/demo/solvent_mix.csv solvent_mix
.import --csv --skip 1 config/demo/compounds.csv compounds
.import --csv --skip 1 config/demo/mobile_phases.csv mobile_phases
.import --csv --skip 1 config/demo/sample_classes.csv sample_classes
.import --csv --skip 1 config/demo/samples.csv samples
.import --csv --skip 1 config/demo/vendors.csv vendors
.import --csv --skip 1 config/demo/ms_method.csv ms_method
