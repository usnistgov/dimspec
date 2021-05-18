/*
	Run this file in sqlite3 to load demonstration data into the database schema using:
		.read demo_data.sql
	You may also directly import these files into the appropriate tables using your SQLite IDE of choice; CSVs are named for their associated tables.
*/

/* To build in console, use*/
.import --csv --skip 1 demo/solvents.csv solvents 
.import --csv --skip 1 demo/solvent_aliases.csv solvent_aliases
.import --csv --skip 1 demo/solvent_mix.csv solvent_mix
.import --csv --skip 1 demo/compounds.csv compounds
.import --csv --skip 1 demo/mobile_phases.csv mobile_phases
.import --csv --skip 1 demo/sample_classes.csv sample_classes
.import --csv --skip 1 demo/samples.csv samples
.import --csv --skip 1 demo/vendors.csv vendors
.import --csv --skip 1 demo/ms_method.csv ms_method

