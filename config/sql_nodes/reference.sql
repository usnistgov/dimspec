/*====================================================================================================
Description:	Reference node including normalization tables for NIST high-resolution-accurate-mass 
				spectrometric database for non-target analysis (HRAM-NTA).
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
					.read config/sql_nodes/reference.sql
				
Details:		Node build files are located in the "config/sql_nodes" directory and serve to allow
				for modular construction and reuse. Local paths will need to be referenced 
				appropriately, which may require modifications to scripts referencing this script.
				
				The comment "magicsplit" is present to provide a hook for external processing,
				allowing for direct building via R or Python when the CLI is unavailable. 
				
====================================================================================================*/

/* Tables */

	/* - Reference tables */
	
	CREATE TABLE IF NOT EXISTS elements
		/* Normalization list of periodic table elements 1-118. */
	(
		atomic_number
			INTEGER PRIMARY KEY,
			/* periodic table atomic number (e.g. 2) */
		symbol
			TEXT NOT NULL UNIQUE,
			/* periodic table symbol (e.g. "He") */
		common_name
			TEXT NOT NULL UNIQUE
			/* periodic table common name (e.g. "Helium") */
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
		/* Constraints */
		CHECK (abundance BETWEEN 0 AND 1),
		CHECK (exact_mass > 0),
		/* Foreign key relationships */
		FOREIGN KEY (atomic_number) REFERENCES elements(atomic_number) ON UPDATE CASCADE
	);
	/*magicsplit*/	
	
/* Views */

	CREATE VIEW IF NOT EXISTS view_element_isotopes AS
	/* A view of all elemental isotopes and their relative abundances joining reference tables "elements" and "isotopes". */
	SELECT
		e.atomic_number,
		e.symbol,
		cast(round(exact_mass) as int)||symbol as isotope,
		e.common_name AS "element",
		i.exact_mass,
		i.abundance
	FROM isotopes i
	INNER JOIN elements e ON i.atomic_number = e.atomic_number;
	/*magicsplit*/

/* Triggers */

