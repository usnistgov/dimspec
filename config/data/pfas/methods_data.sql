/*====================================================================================================
Description:	Data population for methods node including normalization tables for NIST 
				high-resolution-accurate-mass spectrometric database for non-target analysis (HRAM-NTA).
Status:			Development version
LastUpdate:		2021-06-15
Support:		For information or support, contact the development team at
					- NIST PFAS Program	PFAS@nist.gov
					- Jared M. Ragland	jared.ragland@nist.gov	*author
					- Benjamin J. Place	benjamin.place@nist.gov
Dependencies:	sqlite3
Usage:			Run this script from the terminal to create a sketch of the SQLite database. It is 
				recommended to run from the project directory as
				
					sqlite3 nist_nta_dev.sqlite
					.read config/sql_nodes/methods.sql
					.read config/sql_nodes/methods_data.sql
				
				Node build files are located in the "config/sql_nodes" directory and serve to allow 
				for modular construction and reuse. Local paths will need to be referenced 
				appropriately, which may require modifications to scripts references this script. 
				
====================================================================================================*/

DELETE FROM norm_ionization;
.import --csv --skip 1 config/data/norm_ionization.csv norm_ionization

/*magicsplit*/

DELETE FROM norm_source_types;
.import --csv --skip 1 config/data/norm_source_types.csv norm_source_types

/*magicsplit*/

DELETE FROM norm_vendors;
.import --csv --skip 1 config/data/norm_vendors.csv norm_vendors

/*magicsplit*/

DELETE FROM norm_qc_methods_reference;
.import --csv --skip 1 config/data/norm_qc_methods_reference.csv norm_qc_methods_reference

/*magicsplit*/

DELETE FROM norm_qc_methods_name;
.import --csv --skip 1 config/data/norm_qc_methods_name.csv norm_qc_methods_name

/*magicsplit*/

DELETE FROM norm_solvents;
.import --csv --skip 1 config/data/pfas/norm_solvents.csv norm_solvents

/*magicsplit*/

DELETE FROM solvent_aliases;
.import --csv --skip 1 config/data/pfas/solvent_aliases.csv solvent_aliases

/*magicsplit*/

DELETE FROM solvent_mixes;
.import --csv --skip 1 config/data/pfas/solvent_mixes.csv solvent_mixes

/*magicsplit*/

DELETE FROM mobile_phases;
.import --csv --skip 1 config/data/pfas/mobile_phases.csv mobile_phases

/*magicsplit*/
