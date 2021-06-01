/*=============================================================================
	Run this file in sqlite3 to load basic data regarding PFAS into the 
	database schema using:
	
		.read config/setup_pfas.sql
	
	Note this will first remove all data within the referenced tables!
	You may also directly import these files into the appropriate tables using 
	your SQLite IDE of choice; CSVs are named for their associated tables.
	The OS file separator is assumed to be the "forward slash" ("/") character; 
	if this is inappropriate for your OS, you should globally replace it within 
	this script as appropriate.
=============================================================================*/

/* To build in console, use each of the following as needed. */

delete from compounds;
.import --csv --skip 1 config/data/pfas/compounds.csv compounds
delete from mobile_phases;
.import --csv --skip 1 config/data/pfas/mobile_phases.csv mobile_phases
delete from ms_descriptions;
.import --csv --skip 1 config/data/pfas/ms_descriptions.csv ms_descriptions
delete from ms_methods;
.import --csv --skip 1 config/data/pfas/ms_methods.csv ms_methods
delete from norm_ms_types;
.import --csv --skip 1 config/data/pfas/norm_ms_types.csv norm_ms_types
delete from norm_solvents;
.import --csv --skip 1 config/data/pfas/norm_solvents.csv norm_solvents 
delete from norm_sample_classes;
.import --csv --skip 1 config/data/pfas/norm_sample_classes.csv norm_sample_classes
delete from samples;
.import --csv --skip 1 config/data/pfas/samples.csv samples
delete from solvent_aliases;
.import --csv --skip 1 config/data/pfas/solvent_aliases.csv solvent_aliases
delete from solvent_mixes;
.import --csv --skip 1 config/data/pfas/solvent_mixes.csv solvent_mixes
--delete from vendors;
--.import --csv --skip 1 config/data/norm_vendors.csv norm_vendors
