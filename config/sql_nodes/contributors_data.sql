
/*====================================================================================================
Description:	Data population for contributors node including normalization tables for NIST 
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
					.read config/sql_nodes/contributors.sql
					.read config/sql_nodes/contributors_data.sql
				
				Node build files are located in the "config/sql_nodes" directory and serve to allow 
				for modular construction and reuse. Local paths will need to be referenced 
				appropriately, which may require modifications to scripts references this script. 
				
====================================================================================================*/

DELETE FROM contributors;
/*magicsplit*/
DELETE FROM affiliations;
/*magicsplit*/
INSERT OR IGNORE INTO affiliations
	VALUES (1, "system");
/*magicsplit*/
INSERT OR IGNORE INTO contributors
		(id, username, affiliation)
	VALUES (1, "sys", 1);
/*magicsplit*/

