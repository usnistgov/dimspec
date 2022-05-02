/*=============================================================================
	Description
		Reference data node schema definition for the NIST high-resolution
		accurate-mass spectrometry spectral database (HRAM-MS-NTA). This node 
		contains information relevant to tracking when a given instance of the 
		database was built, and reference information for chemical elements
		and isotopes useful for chemoinformatic and mass spectral evaluation.
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
			.read config/sql_nodes/reference.sql
		
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
	CREATE TABLE IF NOT EXISTS config
	  /* Installation code to facilitate widespread usage. */
	(
	  id
	    INTEGER PRIMARY KEY,
	    /* primary key */
	  code
	    TEXT NOT NULL,
	    /* random installation number */
	  name
	    TEXT,
	    /* name for this installation */
	  build_date
	    TEXT,
	    /* build date for this installation */
		/* Check constraints */
		CHECK (id = 0),
		CHECK (build_date == strftime("%Y-%m-%d %H:%M:%S", build_date))
		/* Foreign key relationships */
	);
	/*magicsplit*/
	INSERT INTO config VALUES (
	  0, hex(randomblob(8)), null, datetime('now', 'utc')
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS elements
		/* Normalization list of periodic table elements 1-118. */
	(
		atomic_number
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* periodic table atomic number (e.g. 2) */
		symbol
			TEXT NOT NULL UNIQUE,
			/* periodic table symbol (e.g. "He") */
		common_name
			TEXT NOT NULL UNIQUE
			/* periodic table common name (e.g. "Helium") */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS isotopes
		/* Elemental isotope abundance ratios for comparison and deconvolution. */
	(
		atomic_number
			INTEGER NOT NULL,
			/* periodic TABLE atomic number (e.g. 2) */
		exact_mass
			REAL NOT NULL,
			/* exact atomic mass (e.g. 4.00260325413) */
		abundance
			REAL NOT NULL,
			/* isotopic abundance of exact_mass (e.g. 0.99999866) */
		/* Check constraints */
		CHECK (abundance BETWEEN 0 AND 1),
		CHECK (exact_mass > 0),
		/* Foreign key relationships */
		FOREIGN KEY (atomic_number) REFERENCES elements(atomic_number) ON UPDATE CASCADE ON DELETE CASCADE
	);
	/*magicsplit*/	

/* Views */
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_element_isotopes AS
	/* A view of all elemental isotopes and their relative abundances joining reference tables "elements" and "isotopes". */
  	SELECT
  		e.atomic_number,
  			/* elemental atomic number */
  		e.symbol,
  			/* periodic table symbol */
  		cast(round(exact_mass) as int)||symbol as isotope,
  			/* "human readable" isotopic notation */
  		e.common_name AS "element",
  			/* element common name */
  		i.exact_mass,
  			/* element nominal exact mass */
  		i.abundance
  			/* relative "natural" isotopic abundance */
  	FROM isotopes i
  	INNER JOIN elements e ON i.atomic_number = e.atomic_number;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_exact_masses AS
	/* Exact monoisotopic masses for elements at their highest abundance */
	  SELECT
	  	symbol,
	  		/* element symbol */
	    element,
  			/* element common name */
	    exact_mass
  			/* monisotopic nominal exact mass */
	  FROM (
	    SELECT
	    	symbol,
	      element,
	      exact_mass,
	      abundance,
	      MAX(abundance) OVER (PARTITION BY atomic_number) AS q01
	      FROM view_element_isotopes
	  )
	  WHERE abundance = q01;
	/*magicsplit*/

/* Triggers */
	/*magicsplit*/
	/* none */
