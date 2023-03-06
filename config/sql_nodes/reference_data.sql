/*=============================================================================
	Description
		Clear and populate reference data regarding elements and isotopes to 
		support non-targeted analysis (NTA) high-resolution accurate-mass mass 
		spectrometry (HRAM-MS) experiments using the schema defined in 
		this directory.
		
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
		sqlite3 (https://sqlite.org/version3.html)
		
	Usage
		Run this file in sqlite3 to populate data commonly shared across
		projects into the database using:
		
			sqlite3 nist_nta_dev.sqlite
			.read config/sql_nodes/reference_data.sql
		
		Note this will first remove all data within the referenced tables!
		You may also directly import these files into the appropriate 
		tables using your SQLite IDE of choice.
		
	Details
		Data files are located in the "config/data" directory and local
		paths will need to be referenced appropriately, which may
		require modifications to this script; CSVs are named for their 
		associated tables. The OS file separator is assumed to be the 
		"forward slash" ("/") character; if this is inappropriate for 
		your OS, you should globally replace it within this script as
		appropriate.
		
=============================================================================*/

DELETE FROM elements;
.import --csv --skip 1 config/data/elements.csv elements
DELETE FROM isotopes;
.import --csv --skip 1 config/data/isotopes.csv isotopes
