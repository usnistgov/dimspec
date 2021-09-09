/*====================================================================================================
Description:	Analytical methods node for NIST high-resolution-accurate-mass spectrometric database 
				for non-target analysis (HRAM-NTA).
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
				
Details:		Node build files are located in the "config/sql_nodes" directory and serve to allow
				for modular construction and reuse. Local paths will need to be referenced 
				appropriately, which may require modifications to scripts referencing this script.
				
				The comment "magicsplit" is present to provide a hook for external processing,
				allowing for direct building via R or Python when the CLI is unavailable. 
				
====================================================================================================*/

/* Tables */

	/* - Normalization Tables */

	CREATE TABLE IF NOT EXISTS norm_ionization
		/* Normalization table for mass spectrometer ionization source types */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		acronym
			TEXT NOT NULL UNIQUE,
			/* validation list of ionization source acronyms */
		name
			TEXT NOT NULL UNIQUE
			/* validation list of ionization source names */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS norm_solvents
		/* Mobile phase solvent list: controlled. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* IUPAC name for mobile phase norm_solvents */
		tech
			TEXT NOT NULL,
			/* controlled vocabulary for separation system, one of "GC" or "LC" */
		/* Check constraints */
		CHECK (tech IN ("GC", "LC"))
		/* Foreign key relationships */
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS norm_source_types
		/* Validation list of source types to be used in the compounds TABLE. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		abbreviation
			TEXT NOT NULL,
			/* (single) letter abbreviation for the source type */
		st_type
			TEXT NOT NULL,
			/* full name of the source type */
		definition
			TEXT NOT NULL
			/* definition of the source type */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS norm_vendors
		/* Normalization TABLE holding commercial instrument vendor information. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* company name */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS norm_qc_methods_reference
		/* Normalization table for quality control reference types. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		value
			TEXT NOT NULL
			/* type of QC reference */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS norm_qc_methods_name
		/* Normalization table for quality control types. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* type of QC method */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	
	CREATE TABLE IF NOT EXISTS norm_ms_types
		/* Normalization table for mass spectrometer types. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* type of the mass analyzer */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS solvent_mix_collections
		/* An intermediary identification table linking mobile_phases and solvent_mixes */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* short hand name of the mixture */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS mobile_phases
		/* Description of mobile phases used during a chromatographic separation. */
	(
		methods_id
			INTEGER NOT NULL,
			/* foreign key to methods */
		carrier
			INTEGER NOT NULL,
			/* informal foreign key to solvent_mixes */
		additive
			TEXT,
			/* buffer/salt/acid addition to mobile phase */
		duration
			REAL,
			/* time duration mobile phase was applied */
		duration_units
			TEXT DEFAULT "minutes",
			/* time duration units, constrained to one of "second" or "minutes" */
		/* Check constraints */
		CHECK (duration_units IN ("seconds", "minutes")),
		CHECK (duration > 0),
		/* Foreign key relationships */
		FOREIGN KEY (methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
		FOREIGN KEY (carrier) REFERENCES solvent_mix_collections(id) ON UPDATE CASCADE
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS ms_descriptions
		/* Full description of all mass spectrometer types used for a given entry in the ms_methods TABLE. */
	(
		ms_methods_id
			INTEGER NOT NULL,
			/* foreign key to ms_methods */
		ms_types_id
			INTEGER NOT NULL,
			/* foreign key to norm_ms_types */
		/* Check constraints */
		UNIQUE(ms_methods_id, ms_types_id),
		/* Foreign key relationships */
		FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
		FOREIGN KEY (ms_types_id) REFERENCES norm_ms_types(id) ON UPDATE CASCADE ON DELETE CASCADE
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS ms_methods
		/* Mass spectrometer method settings. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		ionization
			INTEGER,
			/* ionization mode (ESI, APCI, EI, etc.) */
		voltage
			REAL,
			/* ionization voltage/Current (depending on mode) */
		polarity
			TEXT,
			/* ionization polarity (negative, positive, or negative/positive) */
		ce_value
			TEXT,
			/* value for collision energy, normally a number but can be a range */
		ce_desc
			TEXT,
			/* description/context of the collision energy value (normalized, stepped, range, etc.) */
		ms_vendor
			INTEGER,
			/* vendor of the mass spectrometer; FOREIGN KEY to norm_vendors */
		has_qc_method
			INTEGER NOT NULL, 
			/* (0, 1) boolean: does the experiment have a QC method in place */
		citation
			TEXT,
			/* citation for the experimental method */
		/* Check constraints */
		CHECK (polarity IN ('negative', 'positive', 'negative/positive')),
		CHECK (has_qc_method IN (0, 1)),
		/* Foreign key relationships */
		FOREIGN KEY (ms_vendor) REFERENCES norm_vendors(id) ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED,
		FOREIGN KEY (ionization) REFERENCES norm_ionization(id) ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS conversion_software_linkage
		/* Linkage table keeping conversion software setting id groupings in line. These IDs are used to link tables conversion_software_settings and samples. This must be incremented prior to adding new rows in conversion_software_settings, and the new ID used in both conversion_software_settings(id) and samples(software_conversion_settings_id). */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT
			/* automatically populated with each call to keep settings together */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS conversion_software_settings
		/* Settings specific to the software package used to preprocess raw data. */
	(
		linkage_id
			INTEGER,
			/* foreign key to msconvert_settings_linkage */
		software_name
			TEXT NOT NULL DEFAULT "msconvert",
			/* name of the software used to convert data */
		setting_name
			TEXT NOT NULL,
			/* name of the software setting */
		setting_value
			TEXT NOT NULL,
			/* value of the software setting */
		/* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (linkage_id) REFERENCES conversion_software_linkage(id)
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS qc_methods
		/* References to quality control (QC) methods used to vet experimental results */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		ms_methods_id
			INTEGER NOT NULL,
			/* foreign key to ms_methods */
		name
			INTEGER,
			/* the type of QC performed; controlled vocabulary must be one of "Mass Analyzer Calibration", "External Standard Verification", "Internal Standard Verification", or "Matrix Standard Verification" */
		reference
			INTEGER,
			/* the category of the QC method; controlled vocabulary must be one of "SOP (Internal)", "SOP (External/Published)", or "Manuscript" */
		reference_text
			TEXT,
			/* free text entry pointing to a description of the QC method, whether a DOI, SOP reference, or manual description */
		/* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
		FOREIGN KEY (name) REFERENCES norm_qc_methods_name(id) ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED,
		FOREIGN KEY (reference) REFERENCES norm_qc_methods_reference(id) ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS solvent_aliases
		/* List of common aliases for each entry in TABLE norm_solvents */
	(
		solvent_id
			INTEGER NOT NULL,
			/* foreign key to norm_solvents */
		alias
			TEXT NOT NULL UNIQUE,
			/* human meaningful name(s) associated with a solvent */
		/* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (solvent_id) REFERENCES norm_solvents(id) ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS solvent_mixes
		/* Mobile phase solvent mixture for a given elution method */
	(
		mix_id
			INTEGER NOT NULL,
			/* mixture identifier to gather discrete components; foreign key to solvent_mix_collections */
		component
			INTEGER NOT NULL,
			/* foreign key to solvent_fractions */
		fraction
			REAL NOT NULL,
			/* amount fraction amount of this solvent in the mixture, contrained from 0 - 1 */
		/* Check constraints */
		CHECK (fraction BETWEEN 0 AND 1),
		/* Foreign key relationships */
		FOREIGN KEY (mix_id) REFERENCES solvent_mix_collections(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (component) REFERENCES norm_solvents(id) ON UPDATE CASCADE
	);
	/*magicsplit*/

/* Data */

	/* Normalization tables should be populated as appropriate for the project. Examples are given in "config/data" directory and may be imported from there or by running "config/demo_data.sql" */

/* Views */

	CREATE VIEW IF NOT EXISTS view_mass_analyzers AS
		/* View all mass analyzers used in methods */
		SELECT
			msd.ms_methods_id,
				/* mass spec method id */
			ms.name
				/* mass spectrometer type used in this method */
		FROM ms_descriptions msd
		INNER JOIN norm_ms_types ms ON ms.id = msd.ms_types_id;
	/*magicsplit*/

	CREATE VIEW IF NOT EXISTS view_mobile_phase AS
		/* View complete mobile phase used in a mixture */
		SELECT
			sm.mix_id,
				/* solvent mix id */
			s.name AS solvent,
				/* solvent name */
			sm.fraction
				/* solvent fraction in this mix */
		FROM solvent_mixes sm
		INNER JOIN norm_solvents s ON s.id = sm.component;
	/*magicsplit*/

	CREATE VIEW IF NOT EXISTS view_method AS
		/* View mass spectrometer information and method settings */
		SELECT
			nv.name,
			/* Vendor name */
			REPLACE(GROUP_CONCAT(nmt.name), ",", " ") AS detector,
			/* Mass spectrometer type */
			msm.polarity,
			/* Polarity setting */
			ni.acronym,
			/* Ionization type */
			msm.voltage AS "Voltage (V)",
			/* Ionization energy in volts */
			msm.ce_value AS "Collision Energy (eV)",
			/* Collision energy in electron volts */
			msm.ce_desc AS "Collision Energy Description"
			/* Collision energy description */
		FROM ms_methods msm 
		JOIN norm_ionization ni ON msm.ionization = ni.id
		JOIN norm_vendors nv ON msm.ms_vendor = nv.id
		JOIN ms_descriptions msd ON msm.id = msd.ms_methods_id
		JOIN norm_ms_types nmt ON msd.ms_types_id = nmt.id
		GROUP BY msm.id;
	/*magicsplit*/

/* Triggers */

	CREATE TRIGGER IF NOT EXISTS ms_methods_vendors
		/* When adding a record to ms_methods, ensure the value for ms_vendor matches its normalization table. */
		AFTER INSERT ON ms_methods
		WHEN NEW.ms_vendor NOT IN (SELECT id FROM norm_vendors)
	BEGIN
		INSERT INTO norm_vendors (name)
			VALUES (NEW.ms_vendor);
		UPDATE ms_methods
			SET ms_vendor = (SELECT id FROM norm_vendors WHERE name = NEW.ms_vendor)
			WHERE ROWID = NEW.ROWID;
	END;
	/*magicsplit*/

	CREATE TRIGGER IF NOT EXISTS insert_ms_descriptions
		/* When adding a record to ms_descriptions tying an ms_methods record to a mass spectrometer type, ensure the value matches its normalization table. */
		AFTER INSERT ON ms_descriptions
		WHEN NEW.ms_types_id NOT IN (SELECT id FROM norm_ms_types)
	BEGIN
		INSERT INTO norm_ms_types (name)
			VALUES (NEW.ms_types_id);
		UPDATE ms_descriptions
			SET ms_types_id = (SELECT id FROM norm_ms_types WHERE name = NEW.ms_types_id)
			WHERE ROWID = NEW.ROWID;
	END;
	/*magicsplit*/