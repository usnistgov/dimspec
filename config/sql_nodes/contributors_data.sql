/*=============================================================================
	Description
		Set up basic contributor information for a "system" user for the NIST 
		high-resolution accurate-mass spectrometry (HRAM-MS-NTA) spectral 
		database. This is part of the standard build workflow.
	Status
		Development
	LastUpdate
		2022-03-31
	Support
		For information or support, contact the development team at
			- NIST PFAS Program	PFAS@nist.gov
			- Jared M. Ragland	jared.ragland@nist.gov	*author
			- Benjamin J. Place	benjamin.place@nist.gov
	Dependencies
		sqlite3
	Usage
		Run this script from the terminal to populate the contributors and 
		affiliations tables with a "system" user only. It is recommended to run 
		from the project directory as
		
			sqlite3 nist_nta_dev.sqlite
			.read config/sql_nodes/contributors_data.sql
		
	Details
		Node build files are located in the "config/sql_nodes" directory and 
		serve to allow for modular construction and reuse. Local paths will 
		need to be referenced appropriately, which may require modifications 
		to scripts referencing this script.
		
=============================================================================*/

DELETE FROM contributors;
DELETE FROM affiliations;
INSERT OR IGNORE INTO affiliations VALUES (1, "system", NULL);
INSERT OR IGNORE INTO contributors (id, username, affiliation) VALUES (1, "sys", 1);
