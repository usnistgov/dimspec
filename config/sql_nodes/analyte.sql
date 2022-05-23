/*=============================================================================
	Description
		Analyte node schema definition for the NIST high-resolution
		accurate-mass spectrometry spectral database (HRAM-MS-NTA). This node 
		contains information relevant to analytical targets (e.g. compounds 
		and their measured fragments from HRAM-MS experiments).
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
		Run this script from the terminal to create a sketch of the SQLite 
		database. It is recommended to run from the project directory as
		
			sqlite3 nist_nta_dev.sqlite
			.read config/sql_nodes/analyte.sql
		
	Details
		Node build files are located in the "config/sql_nodes" directory and 
		serve to allow for modular construction and reuse. Local paths will 
		need to be referenced appropriately, which may require modifications 
		to scripts referencing this script.
		
		The comment "magicsplit" is present to provide a hook for external 
		processing,	allowing for direct building via R or Python when the CLI 
		is unavailable. 
		
		Data are not available in the "config/sql_nodes" directory but should 
		instead be populated directly from those applicable to the current 
		project, if any. Examples are provided in the "config/data" directory 
		and subdirectories; population scripts are available as 
		"config/populate_X.sql" files.
		
=============================================================================*/
/* Tables */
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_source_types
		/* Validation list of source types to be used in the compounds TABLE. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL,
			/* full name of the source type */
		acronym
			TEXT NOT NULL,
			/* (single) letter acronym for the source type */
		definition
			TEXT NOT NULL
			/* definition of the source type */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_ion_states
		/* Normalization table for the measured ion state as comared with the molecular ion. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* state of the found ion with common mass spectrometric adjuncts/losses/charge */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS compound_categories
		/* Normalization table for self-hierarchical chemical classes of compounds. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL,
			/* name of the class */
		subclass_of
			INTEGER,
			/* self referential to compound_categories */
		/* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (subclass_of) REFERENCES compound_categories(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_analyte_alias_references
		/* Normalization table for compound alias sources (e.g. CAS, DTXSID, INCHI, etc.) */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL,
			/* name of the source for the compound alias */
		description
			TEXT NOT NULL,
			/* text describing the reference name/acronym */
		reference
			TEXT
			/* reference URL for the alias */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS compounds
		/* Controlled list of chemical compounds with attributable analytical data. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		category
			INTEGER,
			/* foreign key to compound_categories */
		name
			TEXT NOT NULL,
			/* name of compound, uncontrolled */
		obtained_from
			TEXT,
			/* DOI/Link of compound structure's source */
		source_type
			INTEGER NOT NULL,
			/* foreign key to norm_source_types */
		additional
			TEXT,
			/* additional information, as submitted */
		local_positive
			INTEGER NOT NULL DEFAULT 0,
			/* number of atoms with positive charges, derived */
		local_negative
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
			INTEGER,
			/* user inspection id */
		inspected_on
			TEXT,
			/* timestamp at which this compound was recorded as inspected (YYYY-MM-DD HH:MM:SS UTC) */
		/* Check constraints */
		CHECK (local_positive >= 0),
		CHECK (local_negative >= 0),
		CHECK (formula GLOB Replace(Hex(ZeroBlob(Length(formula))), '00', '[A-Za-z0-9]')),
		CHECK (inspected_on IS NULL OR inspected_on == strftime("%Y-%m-%d %H:%M:%S", inspected_on)),
		/* Foreign key relationships */
		FOREIGN KEY (source_type) REFERENCES norm_source_types(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (category) REFERENCES compound_categories(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (inspected_by) REFERENCES contributors(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS compound_aliases
		/* List of alternate names or identifiers for compounds */
	(
		compound_id
			INTEGER NOT NULL
			/* foreign key to compounds */
		alias_type
			INTEGER NOT NULL,
			/* foreign key to norm_analyte_alias_references */
		alias
			TEXT NOT NULL,
			/* Text name of the alias for a compound */
		/* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (compound_id) REFERENCES compounds(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (alias_type) REFERENCES norm_analyte_alias_references(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS compound_fragments
		/* Bidirectional linkage table to tie peaks and compounds to their confirmed and annotated fragments. */
	(
		peak_id
			INTEGER,
			/* foreign key to peaks */
		compound_id
			INTEGER,
			/* foreign key to compounds */
		fragment_id
			INTEGER,
			/* foreign key to fragments */
		/* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (peak_id) REFERENCES peaks(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (compound_id) REFERENCES compounds(id) ON UPDATE CASCADE ON DELETE SET NULL,
		FOREIGN KEY (fragment_id) REFERENCES fragments(id) ON UPDATE CASCADE ON DELETE CASCADE
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS annotated_fragments
		/* Potential annotated fragment ions that are attributed to one or more mass spectra. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		mz
			REAL NOT NULL,
			/* m/z value for specific fragment, derived */
		fragment_id
			INTEGER NOT NULL,
			/* smiles structure of fragment ion, can be NULL, user submitted */
		/* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (fragment_id) REFERENCES norm_fragments(id) ON UPDATE CASCADE ON DELETE CASCADE
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS fragment_inspections
		/* Fragment inspections by users for ions that are attributed to one or more mass spectra. */
	(
	  annotated_fragment_id
	    INTEGER,
	    /* foreign key to fragments table */
		user_note
			TEXT,
			/* user-supplied description of the fragment */
		inspected_by
			INTEGER,
			/* user inspection id */
		inspected_on
			TEXT,
			/* timestamp at which this compound was recorded as inspected (YYYY-MM-DD HH:MM:SS UTC) */
		/* Check constraints */
		CHECK (inspected_on == strftime("%Y-%m-%d %H:%M:%S", inspected_on)),
		/* Foreign key relationships */
		FOREIGN KEY (inspected_by) REFERENCES contributors(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (annotated_fragment_id) REFERENCES annotated_fragments(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_fragments
	  /* Normalization list of annotated fragments */
	(
	  id
	    INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		fixedmass
		  REAL,
		  /* fixed molecular formula, generally generated from either rcdk or RDKit */
		netcharge
		  INTEGER,
		  /* net ionic charge for this fragment */
		formula
			TEXT NOT NULL,
			/* elemental formula for specific fragment, user submitted */
		radical
			INTEGER,
			/* TRUE/FALSE: the fragment contains a radical electron, user submitted */
		smiles
			TEXT,
			/* smiles structure of fragment ion, can be NULL, user submitted */
		/* Check constraints */
		UNIQUE(fixedmass, netcharge, formula, radical, smiles),
		CHECK (radical IN (0, 1)),
		CHECK (formula GLOB Replace(Hex(ZeroBlob(Length(formula))), '00', '[A-Za-z0-9]'))
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS fragment_aliases
		/* List of alternate names or identifiers for compounds */
	(
		fragment_id
			INTEGER,
			/* foreign key to compounds */
		alias_type
			INTEGER,
			/* foreign key to norm_analyte_alias_references */
		alias
			TEXT NOT NULL,
			/* Text name of the alias for a compound */
		/* Check constraints */
		UNIQUE (fragment_id, alias_type, alias),
		/* Foreign key relationships */
		FOREIGN KEY (fragment_id) REFERENCES norm_fragments(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (alias_type) REFERENCES norm_analyte_alias_references(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS fragment_sources
		/* Citation information about a given fragment to hold multiple identifications (e.g. one in silico and two empirical). */
	(
		annotated_fragments_id
			INTEGER NOT NULL,
			/* foreign key to fragments */
		generation_type
			INTEGER NOT NULL,
			/* foreign key to norm_generation_type */
		citation
			TEXT NOT NULL,
			/* DOI, etc. */
		/* Check constraints */
		UNIQUE (annotated_fragments_id, generation_type, citation),
		/* Foreign key relationships */
		FOREIGN KEY (annotated_fragments_id) REFERENCES annotated_fragments(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (generation_type) REFERENCES norm_generation_type(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
/* Views */
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_compound_fragments AS
		/* Fragments associated with compounds. */
		SELECT
			c.id,
				/* compounds.id field */
			c.formula AS compound,
				/* compounds.formula field */
			nf.formula AS fragments,
				/* fragments.formula field */
			af.mz
				/* fragments.mz field */
		FROM compounds c
		INNER JOIN compound_fragments cf ON c.id = cf.compound_id
		INNER JOIN annotated_fragments af ON cf.fragment_id = af.id
		INNER JOIN norm_fragments nf ON af.fragment_id = nf.id
		ORDER BY mz ASC;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_fragment_count AS
		/* Number of fragments associated with compounds. */
		SELECT
			c.name,
				/* compounds.name field */
			c.formula AS compound,
				/* compounds.formula field */
			COUNT(DISTINCT(nf.formula)) AS n_fragments
				/* distinct number of fragments associated with this compound as the count of associated fragments.formula */
		FROM compounds c
		INNER JOIN compound_fragments cf ON c.id = cf.compound_id
		INNER JOIN annotated_fragments af ON cf.fragment_id = af.id
		INNER JOIN norm_fragments nf ON af.fragment_id = nf.id
		GROUP BY compound
		ORDER BY n_fragments DESC;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS compound_url AS
		/* Combine information from the compounds table to form a URL link to the resource. */
		SELECT
			c.id,
				/* compound identifier */
			c.name AS compound,
				/* compound name */
			car.name as ref_type,
				/* compound alias reference name */
			ca.alias,
				/* compound alias */
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
					THEN "https://www.google.com/search?q=INCHIKEY+"||ca.alias
				WHEN car.name == "INCHI"
					THEN "https://www.google.com/search?q="||ca.alias
				WHEN car.name == "SMILES"
					THEN "https://www.google.com/search?q=canonical+SMILES+"||
						REPLACE(ca.alias , "#", "%23")
				WHEN c.obtained_from IS NOT NULL 
					THEN c.obtained_from
				ELSE
					"https://www.google.com/search?q="||ca.alias
			END AS link
				/* URL link to the alias ID source */
		FROM compounds c
		INNER JOIN compound_aliases ca ON c.id = ca.compound_id
		INNER JOIN norm_analyte_alias_references car ON ca.alias_type = car.id;
	/*magicsplit*/
/* Triggers */
	/*magicsplit*/
	CREATE TRIGGER nullify_blank_compounds_inspected_by
		AFTER INSERT ON compounds
		WHEN NEW.inspected_by = ''
		BEGIN
			UPDATE compounds SET
				inspected_by = NULL,
				inspected_on = NULL
			WHERE ROWID = NEW.ROWID;
		END;
	/*magicsplit*/
	/* none */
