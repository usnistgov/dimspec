/*=============================================================================
	Run this file in sqlite3 to load validation list data into the database 
	schema using:
	
		.read populate_validation_lists.sql
	
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
.import --csv --skip 1 config/data/pfas/solvents.csv solvents 
.import --csv --skip 1 config/data/pfas/solvent_aliases.csv solvent_aliases
.import --csv --skip 1 config/data/pfas/solvent_mix.csv solvent_mix
.import --csv --skip 1 config/data/pfas/compounds.csv compounds
.import --csv --skip 1 config/data/pfas/mobile_phases.csv mobile_phases
.import --csv --skip 1 config/data/pfas/sample_classes.csv sample_classes
.import --csv --skip 1 config/data/pfas/samples.csv samples
.import --csv --skip 1 config/data/pfas/vendors.csv vendors
.import --csv --skip 1 config/data/pfas/ms_method.csv ms_method
