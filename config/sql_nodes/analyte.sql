/*====================================================================================================
Description:	Analyte node for NIST high-resolution-accurate-mass spectrometric database for 
				non-target analysis (HRAM-NTA).
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
					.read config/sql_nodes/analyte.sql
				
Details:		Node build files are located in the "config/sql_nodes" directory and serve to allow
				for modular construction and reuse. Local paths will need to be referenced 
				appropriately, which may require modifications to scripts referencing this script.
				
				The comment "magicsplit" is present to provide a hook for external processing,
				allowing for direct building via R or Python when the CLI is unavailable. 
				
				Data are not available in the "config/sql_nodes" directory but should instead be
				populated directly from those applicable to the current project, if any. Examples 
				are provided in the "config/demo" directory.
				
====================================================================================================*/

/* Normalization Tables */

	CREATE TABLE IF NOT EXISTS norm_fragment_generation_type
		/* Normalization table for fragmenet generation source type */
	(
		id
			INTEGER PRIMARY KEY,
		name
			TEXT NOT NULL,
			/* one of "in silico" or "empirical" */
		/* Constraints */
		CHECK (name IN ("in silico", "empirical"))
	);
	/*magicsplit*/

/* Tables */

	CREATE TABLE IF NOT EXISTS compounds
		/* Controlled list of chemical compounds with attributable analytical data. */
	(
		id
			INTEGER PRIMARY KEY,
		category
			INTEGER,
			/* foreign key to compound_categories */
		name
			TEXT NOT NULL,
			/* name of compound, uncontrolled */
		obtained_from
			TEXT NOT NULL,
			/* DOI/Link of compound structure's source */
		source_type
			INTEGER NOT NULL,
			/* one-letter character indicating type of source */
		additional
			TEXT,
			/* additional information, as submitted */
		local_pos
			INTEGER NOT NULL DEFAULT 0,
			/* number of atoms with positive charges, derived */
		local_neg
			INTEGER NOT NULL DEFAULT 0,
			/* number of atoms with negative charges, derived */
		formula
			TEXT NOT NULL,
			/* elemental formula, derived */
		fixedmass
			REAL NOT NULL,
			/* exact mass of compound, derived */
		netcharge
			INTEGER NOT NULL DEFAULT 0,
			/* total formal charge of compound, derived */
		inspected_by
			TEXT,
			/* user inspection id */
		inspected_on
			TEXT,
			/* timestamp at which this compound was recorded as inspected (YYYY-MM-DD HH:MM:SS UTC) */
		/* Check constraints */
		CHECK (local_pos >= 0),
		CHECK (local_neg >= 0),
		CHECK (formula GLOB Replace(Hex(ZeroBlob(Length(formula))), '00', '[A-Za-z0-9]')),
		CHECK (inspected_on == strftime("%Y-%m-%d %H:%M:%S", inspected_on)),
		/* Foreign key relationships */
		FOREIGN KEY (source_type) REFERENCES norm_source_types(id) ON UPDATE CASCADE,
		FOREIGN KEY (category) REFERENCES compound_categories(id) ON UPDATE CASCADE
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS compound_aliases
		/* List of alternate names or identifiers for compounds */
	(
		compound_id
			INTEGER,
			/* foreign key to compounds */
		reference
			INTEGER,
			/* foreign key to compound_alias_references*/
		alias
			TEXT NOT NULL,
			/* Text name of the alias for a compound */
		/* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (compound_id) REFERENCES compounds(id) ON UPDATE CASCADE,
		FOREIGN KEY (reference) REFERENCES compound_alias_references(id) ON UPDATE CASCADE
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS compound_alias_references
		/* Normalization table for compound alias sources (e.g. CAS, DTXSID, INCHI, etc.) */
	(
		id
			INTEGER PRIMARY KEY,
		name
			TEXT NOT NULL
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS compound_categories
		/* Normalization table for self-hierarchical chemical classes of compounds. */
	(
		id
			INTEGER PRIMARY KEY,
		name
			TEXT NOT NULL,
			/* name of the class */
		subclass_of
			INTEGER,
			/* self referential to compound_categories */
		/* Foreign key relationships */
		FOREIGN KEY (subclass_of) REFERENCES compound_categories(id)
	);
	/*magicsplit*/
	
	CREATE TABLE IF NOT EXISTS compound_fragments
		/* Bidirectional linkage table to tie peaks and compounds to their confirmed and annotated fragments. */
	(
		peak_id
			INTEGER NOT NULL,
			/* foreign key to peaks */
		compound_id
			INTEGER NOT NULL,
			/* foreign key to compounds */
		fragment_id
			INTEGER NOT NULL,
			/* foreign key to fragments */
		/* Foreign key relationships */
		FOREIGN KEY (peak_id) REFERENCES peaks(id),
		FOREIGN KEY (compound_id) REFERENCES compounds(id),
		FOREIGN KEY (fragment_id) REFERENCES fragments(id)
	);
	/*magicsplit*/
	
	CREATE TABLE IF NOT EXISTS fragments
		/* Potential annotated fragment ions that are attributed to one or more mass spectra. */
	(
		id
			INTEGER PRIMARY KEY,
		mz
			REAL NOT NULL,
			/* m/z value for specific fragment, derived */
		formula
			TEXT NOT NULL,
			/* elemental formula for specific fragment, user submitted */
		description
			TEXT NOT NULL,
			/*  */
		charge
			INTEGER NOT NULL,
			/* charge of specific fragment,derived */
		radical
			TEXT,
			/* TRUE/FALSE: the fragment contains a radical electron, user submitted */
		smiles
			TEXT,
			/* smiles structure of fragment ion, can be NULL, user submitted */
		/* Constraints */
		CHECK (charge IN (-1, 1)),
		CHECK (radical IN (0, 1)),
		CHECK (formula GLOB Replace(Hex(ZeroBlob(Length(formula))), '00', '[A-Za-z0-9]'))
	);
	/*magicsplit*/
	
	CREATE TABLE IF NOT EXISTS fragment_sources
		/* Citation information about a given fragment to hold multiple identifications (e.g. one in silico and two empirical). */
	(
		fragment_id
			INTEGER NOT NULL,
			/* foreign key to fragments */
		generated_by
			INTEGER NOT NULL,
			/* foreign key to norm_fragment_generation_type */
		citation
			TEXT NOT NULL,
			/* DOI, etc. */
		/* Foreign key relationships */
		FOREIGN KEY (fragment_id) REFERENCES fragments(id),
		FOREIGN KEY (generated_by) REFERENCES norm_fragment_generation_type(id)
	);
	/*magicsplit*/

/* Views */

	CREATE VIEW IF NOT EXISTS view_compound_fragments AS
		/* Fragments associated with compounds. */
		SELECT c.id, c.formula AS compound, f.formula AS fragments, f.mz
			FROM compounds c
			INNER JOIN compound_fragments cf ON c.id = cf.compound_id
			INNER JOIN fragments f ON cf.fragment_id = f.id
			ORDER BY mz ASC;
	/*magicsplit*/
	
	CREATE VIEW IF NOT EXISTS view_fragment_count AS
		/* Number of fragments associated with compounds. */
		SELECT c.name, c.formula AS compound, COUNT(f.formula) AS n_fragments
			FROM compounds c
			INNER JOIN compound_fragments cf ON c.id = cf.compound_id
			INNER JOIN fragments f ON cf.fragment_id = f.id
			GROUP BY compound
			ORDER BY n_fragments DESC;
	/*magicsplit*/
	
	CREATE VIEW IF NOT EXISTS compound_url AS
		/* Combine information from the compounds table to form a URL link to the resource. */
		SELECT c.id, c.name AS compound,
			ca.alias,
			car.name as ref_type,
			CASE 
				WHEN car.name == "DTXSID"
					THEN "https://comptox.epa.gov/dashboard/dsstoxdb/results?search="||ca.alias 
				WHEN car.name == "DTXCID"
					THEN "https://comptox.epa.gov/dashboard/dsstoxdb/results?search="||ca.alias
				WHEN car.name == "PUBCHEMID"
					THEN "https://pubchem.ncbi.nlm.nih.gov/compound/"||ca.alias 
				WHEN car.name == "CASRN"
					THEN "https://commonchemistry.cas.org/detail?cas_rn="||ca.alias 
				WHEN car.name == "INCHIKEY"
					THEN "https://www.google.com/search?q="||ca.alias 
				WHEN car.name == "INCHI"
					THEN "https://www.google.com/search?q="||ca.alias 
				WHEN car.name == "SMILES"
					THEN "https://www.google.com/search?q=canonical+SMILES+"||
						REPLACE(ca.alias , "#", "%23")
				WHEN c.obtained_from IS NOT NULL 
					THEN c.obtained_from
				ELSE
					"(not available)"
			END AS link
			FROM compounds c
			INNER JOIN compound_aliases ca ON c.id = ca.compound_id
			INNER JOIN compound_alias_references car ON ca.reference = car.id;
	/*magicsplit*/

/* Triggers */
