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
DELETE FROM norm_ionization_units;
.import --csv --skip 1 config/data/norm_ionization_units.csv norm_ionization_units
DELETE FROM norm_source_types;
.import --csv --skip 1 config/data/norm_source_types.csv norm_source_types
DELETE FROM norm_ms_types;
.import --csv --skip 1 config/data/norm_ms_types.csv norm_ms_types
DELETE FROM norm_chromatography_types;
.import --csv --skip 1 config/data/norm_chromatography_types.csv norm_chromatography_types
DELETE FROM norm_polarity_types;
.import --csv --skip 1 config/data/norm_polarity_types.csv norm_polarity_types
DELETE FROM norm_fragment_generation_type;
.import --csv --skip 1 config/data/norm_fragment_generation_type.csv norm_fragment_generation_type
DELETE FROM norm_fragmentation_types;
.import --csv --skip 1 config/data/norm_fragmentation_types.csv norm_fragmentation_types
DELETE FROM norm_ms_n_types;
.import --csv --skip 1 config/data/norm_ms_n_types.csv norm_ms_n_types
DELETE FROM norm_column_chemistries;
.import --csv --skip 1 config/data/norm_column_chemistries.csv norm_column_chemistries
DELETE FROM norm_column_positions;
.import --csv --skip 1 config/data/norm_column_positions.csv norm_column_positions
DELETE FROM norm_vendors;
.import --csv --skip 1 config/data/norm_vendors.csv norm_vendors
DELETE FROM norm_ce_desc;
.import --csv --skip 1 config/data/norm_ce_desc.csv norm_ce_desc
DELETE FROM norm_ce_units;
.import --csv --skip 1 config/data/norm_ce_units.csv norm_ce_units
DELETE FROM norm_qc_methods_name;
.import --csv --skip 1 config/data/norm_qc_methods_name.csv norm_qc_methods_name
DELETE FROM norm_qc_methods_reference;
.import --csv --skip 1 config/data/norm_qc_methods_reference.csv norm_qc_methods_reference
DELETE FROM norm_ion_states;
.import --csv --skip 1 config/data/norm_ion_states.csv norm_ion_states
