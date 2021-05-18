/*
	Run this file in sqlite3 to load demonstration data into the database schema using:
		.read demo_data.sql
	You may also directly import these files into the appropriate tables using your SQLite IDE of choice; CSVs are named for their associated tables.
	
*/

/* To build in console, use*/
.import --csv --skip 1 solvents.csv solvents 
.import --csv --skip 1 solvent_aliases.csv solvent_aliases
.import --csv --skip 1 solvent_fractions.csv solvent_fractions
.import --csv --skip 1 solvent_mix.csv solvent_mix

