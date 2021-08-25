/*=============================================================================
	Run this file in sqlite3 to load demonstration data into the database 
	schema using:
	
		.read config/demo_data.sql
		
	Note this will first remove all data within the referenced tables!
	You may also directly import these files into the appropriate tables using 
	your SQLite IDE of choice; CSVs are named for their associated tables.
	The OS file separator is assumed to be the "forward slash" ("/") character; 
	if this is inappropriate for your OS, you should globally replace it within 
	this script as appropriate.
=============================================================================*/

/* To build in console, use each of the following as needed. */

delete from norm_source_types;
.import --csv --skip 1 config/data/norm_source_types.csv norm_source_types
delete from compounds;
.import --csv --skip 1 config/data/demo/compounds.csv compounds
delete from compound_aliases;
.import --csv --skip 1 config/data/demo/compound_aliases.csv compound_aliases
delete from compound_alias_references;
.import --csv --skip 1 config/data/demo/compound_alias_references.csv compound_alias_references
delete from mobile_phases;
.import --csv --skip 1 config/data/demo/mobile_phases.csv mobile_phases
delete from ms_descriptions;
.import --csv --skip 1 config/data/demo/ms_descriptions.csv ms_descriptions
delete from ms_methods;
.import --csv --skip 1 config/data/demo/ms_methods.csv ms_methods
delete from norm_ms_types;
.import --csv --skip 1 config/data/demo/norm_ms_types.csv norm_ms_types
delete from norm_solvents;
.import --csv --skip 1 config/data/demo/norm_solvents.csv norm_solvents 
delete from norm_sample_classes;
.import --csv --skip 1 config/data/demo/norm_sample_classes.csv norm_sample_classes
delete from affiliations;
.import --csv --skip 1 config/data/demo/affiliations.csv affiliations
delete from contributors;
.import --csv --skip 1 config/data/demo/contributors.csv contributors
delete from samples;
.import --csv --skip 1 config/data/demo/samples.csv samples
delete from solvent_aliases;
.import --csv --skip 1 config/data/demo/solvent_aliases.csv solvent_aliases
delete from solvent_mixes;
.import --csv --skip 1 config/data/demo/solvent_mixes.csv solvent_mixes