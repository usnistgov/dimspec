/*=============================================================================
	Run this file in sqlite3 to load demonstration data into the database 
	schema using:
	
		.read config/populate_demo.sql
		
	Note this will first remove all data within the referenced tables!
	You may also directly import these files into the appropriate tables using 
	your SQLite IDE of choice; CSVs are named for their associated tables.
	The OS file separator is assumed to be the "forward slash" ("/") character; 
	if this is inappropriate for your OS, you should globally replace it within 
	this script as appropriate.
=============================================================================*/

/* To build in console, use each of the following as needed. */

DELETE FROM solvent_mixes;
DELETE FROM solvent_mix_collections;
DELETE FROM solvent_aliases;
DELETE FROM mobile_phases;
DELETE FROM norm_solvents;
DELETE FROM samples;
DELETE FROM norm_sample_classes;
DELETE FROM norm_ms_types;
DELETE FROM ms_methods;
DELETE FROM ms_descriptions;
.read config/populate_common.sql
.read config/sql_nodes/contributors_data.sql
DELETE FROM compounds;
DELETE FROM compound_categories;
DELETE FROM compound_alias_references;
DELETE FROM compound_aliases;
DELETE FROM conversion_software_linkage;
DELETE FROM conversion_software_settings;
.import --csv --skip 1 config/data/demo/norm_solvents.csv norm_solvents
.import --csv --skip 1 config/data/demo/norm_sample_classes.csv norm_sample_classes
.import --csv --skip 1 config/data/demo/norm_ms_types.csv norm_ms_types
.import --csv --skip 1 config/data/demo/ms_methods.csv ms_methods
.import --csv --skip 1 config/data/demo/ms_descriptions.csv ms_descriptions
.import --csv --skip 1 config/data/demo/affiliations.csv affiliations
.import --csv --skip 1 config/data/demo/contributors.csv contributors
.import --csv --skip 1 config/data/demo/solvent_mix_collections.csv solvent_mix_collections
.import --csv --skip 1 config/data/demo/mobile_phases.csv mobile_phases
.import --csv --skip 1 config/data/demo/solvent_aliases.csv solvent_aliases
.import --csv --skip 1 config/data/demo/solvent_mixes.csv solvent_mixes
.import --csv --skip 1 config/data/demo/compound_categories.csv compound_categories
.import --csv --skip 1 config/data/demo/compounds.csv compounds
.import --csv --skip 1 config/data/demo/compound_alias_references.csv compound_alias_references
.import --csv --skip 1 config/data/demo/compound_aliases.csv compound_aliases
.import --csv --skip 1 config/data/demo/conversion_software_linkage.csv conversion_software_linkage
.import --csv --skip 1 config/data/demo/conversion_software_settings.csv conversion_software_settings
.import --csv --skip 1 config/data/demo/samples.csv samples