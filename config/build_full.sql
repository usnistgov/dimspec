/*====================================================================================================
Description:	Build a database sketch to hold results for non-targeted analysis high-resolution 
				accurate-mass mass spectrometry (NTA-HRAM-MS) experiments.
Status:			Development version
LastUpdate:		2021-06-03
Support:		For information or support, contact the development team at
					- NIST PFAS Program	PFAS@nist.gov
					- Jared M. Ragland	jared.ragland@nist.gov	*author
					- Benjamin J. Place	benjamin.place@nist.gov
Dependencies:	sqlite3
Usage:			Run this script from the terminal to create a sketch of the SQLite database. It is 
				recommended to run from the project directory as
				
					sqlite3 nist_nta_dev.sqlite
					.read config/build.sql
				
				Note that build files are mostly located in the "config" directory and local 
				paths will need to be referenced appropriately, which may require modifications 
				to this script. This mostly applies to the commands in this introductory 
				section and at the end of this script file where default tables are populated.
				
				This will create the schema on your local machine for local population.
				Example data (a full demonstration version will be provided in the future) can 
				be read in with
				
					.read config/demo_data.sql
					
				See the build history after build with
					
					SELECT * from view_history;
					
====================================================================================================*/

PRAGMA journal_mode=WAL;

/* - Create tables */
/* -- Reference node begins */

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

/* --- Normalization tables */

CREATE TABLE IF NOT EXISTS norm_ionization
	/* Normalization table for mass spectrometer ionization source types */
(
	id
		INTEGER PRIMARY KEY,
	acronym
		TEXT NOT NULL UNIQUE,
		/* validation list of ionization source acronyms */
	name
		TEXT NOT NULL UNIQUE
		/* validation list of ionization source names */
);
/*magicsplit*/

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

CREATE TABLE IF NOT EXISTS norm_log_execution_from
	/* Normalization table for log(executed_from) */
(
	id
		INTEGER PRIMARY KEY,
	value
		TEXT NOT NULL UNIQUE,
	CHECK (value IN ("trigger", "application", "console", "other"))
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS norm_ms_types
	/* Normalization table for mass spectrometer types. */
(
	id
		INTEGER PRIMARY KEY,
	name
		TEXT NOT NULL UNIQUE
		/* type of the mass analyzer */
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS norm_qc_methods_reference
	/* Normalization table for quality control reference types. */
(
	id
		INTEGER PRIMARY KEY,
	value
		TEXT NOT NULL
		/* type of QC reference */
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS norm_qc_methods_name
	/* Normalization table for quality control types. */
(
	id
		INTEGER PRIMARY KEY,
	name
		TEXT NOT NULL UNIQUE
		/* type of QC method */
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS norm_sample_classes
	/* Normalization TABLE linking to samples to hold controlled vocabulary. */
(
	id
		INTEGER PRIMARY KEY,
	name
		TEXT NOT NULL UNIQUE
		/* name of the sample class */
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS norm_solvents
	/* Mobile phase solvent list: controlled. */
(
	id
		INTEGER PRIMARY KEY,
	name
		TEXT NOT NULL UNIQUE,
		/* IUPAC name for mobile phase norm_solvents */
	tech
		TEXT NOT NULL,
		/* controlled vocabulary for separation system, one of "GC" or "LC" */
	/* Constraints */
	CHECK (tech IN ("GC", "LC"))
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS norm_source_types
	/* Validation list of source types to be used in the compounds TABLE. */
(
	id
		INTEGER PRIMARY KEY,
	abbreviation
		TEXT NOT NULL,
		/* (single) letter abbreviation for the source type */
	st_type
		TEXT NOT NULL,
		/* full name of the source type */
	definition
		TEXT NOT NULL
		/* definition of the source type */
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS norm_vendors
	/* Normalization TABLE holding commercial instrument vendor information. */
(
	id
		INTEGER PRIMARY KEY,
	name
		TEXT NOT NULL UNIQUE
		/* company name */
);
/*magicsplit*/

/* -- Reference node ends */

/* -- Analyte node begins */

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
	inchi
		TEXT,
		/* inchi structure of compound, as submitted */
	obtained_from
		TEXT NOT NULL,
		/* DOI/Link of compound structure's source */
	source_type
		INTEGER NOT NULL,
		/* one-letter character indicating type of source */
	additional
		TEXT,
		/* additional information, as submitted */
	smiles
		TEXT,
		/* smiles structure of compound, derived */
	inchikey
		TEXT,
		/* inchikey structure of compound, derived */
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
	dtxsid
		TEXT,
		/* dtxsid identifier */
	dtxcid
		TEXT,
		/* dtxcid identifier */
	casrn
		TEXT,
		/* CAS registry number */
	pubchemid
		TEXT,
		/* PubChem identifier */
	inspectedby
		TEXT,
		/* user inspection id */
	inspectedon
		TEXT,
		/* timestamp at which this compound was recorded as inspected (YYYY-MM-DD HH:MM:SS UTC) */
	/* Check constraints */
	CHECK (local_pos >= 0),
	CHECK (local_neg >= 0),
	CHECK (formula GLOB Replace(Hex(ZeroBlob(Length(formula))), '00', '[A-Za-z0-9]')),
	/* Foreign key relationships */
	FOREIGN KEY (source_type) REFERENCES norm_source_types(id) ON UPDATE CASCADE,
	FOREIGN KEY (category) REFERENCES compound_categories(id) ON UPDATE CASCADE
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS compound_categories
	/* Chemical class category of compounds. */
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
	/* Linkage TABLE to tie compounds to their confirmed and annotated fragments. */
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
	inchi
		TEXT,
		/* InChI representation of fragment ion, can be NULL, user submitted */
	inchikey
		TEXT,
		/* InChIKey representation of fragment ion, can be NULL, user submitted */
	/* Constraints */
	CHECK (charge IN (-1, 1)),
	CHECK (radical IN (0, 1)),
	CHECK (formula GLOB Replace(Hex(ZeroBlob(Length(formula))), '00', '[A-Za-z0-9]'))
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS fragment_sources
	/* Citation information about a given fragment to hold multiple identifications (e.g. one in silico and two empirical) */
(
	fragment_id
		INTEGER NOT NULL,
		/* foreign key to fragments */
	generated_from
		INTEGER NOT NULL,
		/* foreign key to norm_fragment_generation_type */
	citation
		TEXT NOT NULL,
		/* DOI, etc. */
	/* Foreign key relationships */
	FOREIGN KEY (fragment_id) REFERENCES fragments(id),
	FOREIGN KEY (generated_from) REFERENCES norm_fragment_generation_type(id)
);
/*magicsplit*/

/* -- Analyte node ends */

/* -- Logging node begins */

CREATE TABLE IF NOT EXISTS version_history
	/* Versions of this database and its associated data or application */
(
	version
		REAL NOT NULL,
		/* version number */
	affects
		TEXT NOT NULL,
		/* which aspect is affected, constrained to one of "data", "schema", or "application" */
	description
		TEXT NOT NULL,
		/* plain text description of the change */
	active_date
		TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
		/* timestamp for when the action was executed */
	/* Constraints */
	CHECK (affects IN ("data", "schema", "application")),
	CHECK (active_date==strftime("%Y-%m-%d %H:%M:%S", active_date))
	/* Foreign keys */
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS log
	/* Placeholder for logs */
(
	id
		INTEGER PRIMARY KEY,
	category
		TEXT NOT NULL,
		/* categorical grouping of the action */
	description
		TEXT,
		/* plain text description if any */
	effect
		TEXT NOT NULL,
		/* type of database update, constraint list as one of "INSERT" "UPDATE", "DELETE", or "schema" */
	affects_table
		TEXT NOT NULL,
		/* table reference name for the table that was changed */
	affects_ids
		INTEGER,
		/* id associated with records changed or updated  */
	executed_by
		INTEGER NOT NULL DEFAULT 1,
		/* foreign key to contributors */
	executed_from
		INTEGER NOT NULL DEFAULT 3,
		/* constraint list as one of "trigger", "application", "console", or "other" */
	executed_on
		TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
		/* timestamp for when the action was executed */
	new_vals
		TEXT,
		/* new values, if any */
	old_vals
		TEXT,
		/* prior values, if any */
	/* Constraints */
	CHECK (executed_on==strftime("%Y-%m-%d %H:%M:%S", executed_on)),
	CHECK (effect IN ("INSERT", "UPDATE", "DELETE", "schema")),
	/* Foreign keys */
	FOREIGN KEY (executed_from) REFERENCES norm_log_execution_from(id)
);
/*magicsplit*/

/* -- Logging node ends */

/* -- Persons node begins */

CREATE TABLE IF NOT EXISTS contributors
	/* Placeholder for contributors */
(
	id
		INTEGER PRIMARY KEY,
	first_name
		TEXT NOT NULL,
		/* user's preferred first name */
	last_name
		TEXT NOT NULL,
		/* user's preferred last name */
	affiliation
		INTEGER NOT NULL,
		/* user's professional affiliation (see table affiliations) */
	orcid
		TEXT,
		/* user's ORCID number, if available */
	orcid_url
		TEXT GENERATED ALWAYS AS ("https://orcid.org/" || orcid) VIRTUAL,
		/* calculated column to provide a link to a user's ORCID id profile */
	/* Constraints */
	CHECK (orcid GLOB('[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]')),
		/* Ensures ORCID follows formatting requirement as of 2021-06-07
	/* Foreign key relationships */
	FOREIGN KEY (affiliation) REFERENCES affiliations(id) ON UPDATE CASCADE
);

/*magicsplit*/

CREATE TABLE IF NOT EXISTS affiliations
	/* Normalization table for user affiliations */
(
	id
		INTEGER PRIMARY KEY,
	name
		TEXT NOT NULL UNIQUE
		/* name of professional affiliation */
);

/*magicsplit*/

/* -- Persons node ends */

/* -- Method node begins */

CREATE TABLE IF NOT EXISTS mobile_phases
	/* Description of mobile phases used during a chromatographic separation. */
(
	methods_id
		INTEGER NOT NULL,
		/* foreign key to methods */
	carrier
		INTEGER NOT NULL,
		/* foreign key to solvent_mixes */
	additive
		TEXT,
		/* buffer/salt/acid addition to mobile phase */
	duration
		REAL,
		/* time duration mobile phase was applied */
	duration_units
		TEXT DEFAULT "minutes",
		/* time duration units, constrained to one of "second" or "minutes" */
	/* Constraints */
	CHECK (duration_units IN ("seconds", "minutes")),
	CHECK (duration > 0),
	/* Foreign key relationships */
	FOREIGN KEY (methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
	FOREIGN KEY (carrier) REFERENCES solvent_mixes(mix_id) ON UPDATE CASCADE
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
	/* Constraints */
	UNIQUE(ms_methods_id, ms_types_id),
	/* Foreign key relationships */
	FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
	FOREIGN KEY (ms_types_id) REFERENCES norm_ms_types(id) ON UPDATE CASCADE
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS ms_methods
	/* Mass spectrometer method settings. */
(
	id
		INTEGER PRIMARY KEY,
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
	/* Constraints */
	CHECK (polarity IN ('negative', 'positive', 'negative/positive')),
	CHECK (has_qc_method IN (0, 1)),
	/* Foreign key relationships */
	FOREIGN KEY (ms_vendor) REFERENCES norm_vendors(id) ON UPDATE CASCADE,
	FOREIGN KEY (ionization) REFERENCES norm_ionization(id) ON UPDATE CASCADE
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS msconvert_settings
	/* Settings specific to the msconvert program. Automatically populated by calls to insert to view_msconvert_settings */
(
	setting
		TEXT,
		/* settings file expression */
	id
		INTEGER
		/* automatically populated with each call to keep settings together */
	/* Constraints */
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS qc_methods
	/* References to quality control (QC) methods used to vet experimental results */
(
	id
		INTEGER PRIMARY KEY,
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
	/* Constraints */
	/* Foreign key relationships */
	FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
	FOREIGN KEY (name) REFERENCES norm_qc_methods_name(id),
	FOREIGN KEY (reference) REFERENCES norm_qc_methods_reference(id)
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS solvent_aliases
	/* List of common aliases for each entry in TABLE norm_solvents	*/
(
	solvent_id
		INTEGER NOT NULL,
		/* foreign key to norm_solvents */
	alias
		TEXT NOT NULL UNIQUE,
		/* human meaningful name(s) associated with a solvent */
	/* Foreign key relationships */
	FOREIGN KEY (solvent_id) REFERENCES norm_solvents(id) ON UPDATE CASCADE
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS solvent_mixes
	/*	Mobile phase solvent mixture for a given elution method	*/
(
	mix_id
		INTEGER NOT NULL,
		/* mixture identifier to gather discrete components */
	component
		INTEGER NOT NULL,
		/* foreign key to solvent_fractions */
	fraction
		REAL NOT NULL,
		/* amount fraction amount of this solvent in the mixture, contrained from 0 - 1 */
	/* Foreign key relationships */
	FOREIGN KEY (component) REFERENCES norm_solvents(id) ON UPDATE CASCADE,
	CHECK (fraction BETWEEN 0 AND 1)
);
/*magicsplit*/

/* -- Method node ends */

/* -- Data node begins */

CREATE TABLE IF NOT EXISTS ms1data
	/* Mass spectral data derived from experiments on a compound by compound basis. Emperical isotopic pattern. */
(
	id
		INTEGER PRIMARY KEY,
	peak_id
		INTEGER NOT NULL,
		/* foreign key to peaks */
	scantime
		REAL NOT NULL,
		/* scan time of spectrum */
	ms1_data
		TEXT NOT NULL,
		/* locator/actual MS1 fragmentation data */
	contributor
		INTEGER,
		/* contributor for these data */
	/* Constraints */
	CHECK (scantime > 0),
	/* Foreign key relationships */
	FOREIGN KEY (peak_id) REFERENCES peaks(id) ON UPDATE CASCADE,
	FOREIGN KEY (contributor) REFERENCES contributors(id) ON UPDATE CASCADE
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS ms1_spectra
	/* Retained mass spectra associated with ms1data. */
(
	ms1data_id
		INTEGER NOT NULL,
		/* foreign key to ms1data */
	mz
		REAL NOT NULL,
		/* mass to charge ratio */
	intensity
		REAL NOT NULL,
		/* signal intensity */
	/* Constraints */
	CHECK (mz > 0),
	CHECK (intensity > 0),
	/* Foreign key relationships */
	FOREIGN KEY (ms1data_id) REFERENCES ms1data(id)
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS ms2data
	/* Mass spectral data associated with ms2data for a given peak. */
(
	id
		INTEGER PRIMARY KEY,
	peak_id
		INTEGER NOT NULL,
		/* foreign key to peaks */
	scantime
		REAL NOT NULL,
		/* scan time of spectrum */
	ms2_data
		TEXT NOT NULL,
		/* NIST INTERNAL REFERENCE locator for raw MS2 fragmentation data */
	contributor
		INTEGER,
		/* contributor for these data */
	/* Constraints */
	CHECK (scantime > 0),
	/* Foreign key relationships */
	FOREIGN KEY (peak_id) REFERENCES peaks(id) ON UPDATE CASCADE,
	FOREIGN KEY (contributor) REFERENCES contributors(id) ON UPDATE CASCADE
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS ms2_spectra
	/* Retained mass spectra associated with ms2data. */
(
	ms2data_id
		INTEGER NOT NULL,
		/* foreign key to ms2data */
	mz
		REAL NOT NULL,
		/* mass to charge ratio */
	intensity
		REAL NOT NULL,
		/* signal intensity */
	/* Constraints */
	CHECK (mz > 0),
	CHECK (intensity > 0),
	/* Foreign key relationships */
	FOREIGN KEY (ms2data_id) REFERENCES ms2data(id)
);
/*magicsplit*/

/* -- Data node ends */

CREATE TABLE IF NOT EXISTS peaks
	/* Peaks (or features) identified within the results from a sample. */
(
	id
		INTEGER PRIMARY KEY,
	sample_id
		INTEGER NOT NULL,
		/* foreign key to samples */
	measured_mz1
		REAL NOT NULL,
		/* precursor ion mass to charge ratio (constrained to positive numbers) */
	peak_timestamp
		TEXT NOT NULL,
		/* timestamp for the peak (YYYY-MM-DD HH:MM:SS UTC) */
	charge
		INTEGER NOT NULL,
		/* ion charge state (constrained to -1 ["negative"] or 1 ["positive"]) */
	rt_start
		REAL NOT NULL,
		/* peak retention time start point (constrained to positive numbers) */
	rt_center
		REAL NOT NULL,
		/* peak retention time centroid, (derived constrained to positive numbers) */
	rt_end
		REAL NOT NULL,
		/* peak retention time end point (constrained to positive numbers) */
	/* Constraints */
	CHECK (measured_mz1 > 0),
	CHECK (charge IN (-1, 1)),
	CHECK (rt_start > 0),
	CHECK (rt_center > 0),
	CHECK (rt_end > 0),
	/* Foreign key relationships */
	FOREIGN KEY (sample_id) REFERENCES samples(id) ON UPDATE CASCADE
);
/*magicsplit*/

CREATE TABLE IF NOT EXISTS samples
	/* Samples from which analytical data are derived. What goes into an analytical instrument.	*/
(
	id
		INTEGER PRIMARY KEY,
	name
		TEXT NOT NULL,
		/* user-defined name of the sample */
	sample_class_id
		INTEGER NOT NULL,
		/* foreign key to norm_sample_classes */
	SOURCE
		TEXT,
		/* citation for the sample source */
	data_generator
		TEXT NOT NULL,
		/* generator of data for this sample */
	msconvert_settings_id
		INTEGER,
		/* settings for the msconvert program used to generate data from this sample */
	ms_methods_id
		INTEGER,
		/* foreign key to methods */
	/* Foreign key relationships */
	FOREIGN KEY (sample_class_id) REFERENCES norm_sample_classes(id) ON UPDATE CASCADE,
	FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
	FOREIGN KEY (msconvert_settings_id) REFERENCES msconvert_settings(id) ON UPDATE CASCADE
);
/*magicsplit*/

/* - Create views */

CREATE VIEW IF NOT EXISTS view_element_isotopes AS
	SELECT e.atomic_number, e.symbol, e.common_name AS element, i.exact_mass, i.abundance
		FROM isotopes i
		INNER JOIN elements e ON i.atomic_number = e.atomic_number;
/*magicsplit*/

CREATE VIEW IF NOT EXISTS view_mass_analyzers AS
	SELECT msd.ms_methods_id, ms.name
		FROM ms_descriptions msd
		INNER JOIN norm_ms_types ms ON ms.id = msd.ms_types_id;
/*magicsplit*/

CREATE VIEW IF NOT EXISTS view_mobile_phase AS
	SELECT sm.mix_id, s.name AS solvent, sm.fraction
		FROM solvent_mixes sm
		INNER JOIN norm_solvents s ON s.id = sm.component;
/*magicsplit*/

CREATE VIEW IF NOT EXISTS view_compound_fragments AS
	SELECT c.id, c.formula AS compound, f.formula AS fragments, f.mz
		FROM compounds c
		INNER JOIN compound_fragments cf ON c.id = cf.compound_id
		INNER JOIN fragments f ON cf.fragment_id = f.id
		ORDER BY mz ASC;
/*magicsplit*/

CREATE VIEW IF NOT EXISTS view_fragment_count AS
	SELECT c.name, c.formula AS compound, COUNT(f.formula) AS n_fragments
		FROM compounds c
		INNER JOIN compound_fragments cf ON c.id = cf.compound_id
		INNER JOIN fragments f ON cf.fragment_id = f.id
		GROUP BY compound
		ORDER BY n_fragments DESC;
/*magicsplit*/
	
CREATE VIEW IF NOT EXISTS view_contributors AS
	/* Readable version of the contributors table that can be expanded with
	 * counts of contributions from various places. */
	SELECT 
		c.first_name || " " || c.last_name AS name,
		a.name AS affiliation,
		c.orcid_url AS ORCID
	FROM contributors c
	JOIN affiliations a
	ON c.affiliation = a.id;
/*magicsplit*/

CREATE VIEW IF NOT EXISTS compound_url AS
	/* Combine information from the compounds table to form a URL link to the resource */
	SELECT id,
		name,
		CASE 
			WHEN dtxsid IS NOT NULL AND NOT dtxsid IN ("NA", "")
				THEN "https://comptox.epa.gov/dashboard/dsstoxdb/results?search="||dtxsid 
			WHEN dtxcid IS NOT NULL AND NOT dtxcid IN ("NA", "")
				THEN "https://comptox.epa.gov/dashboard/dsstoxdb/results?search="||dtxcid
			WHEN pubchemid IS NOT NULL AND NOT pubchemid IN ("NA", "")
				THEN "https://pubchem.ncbi.nlm.nih.gov/compound/"||pubchemid
			WHEN casrn IS NOT NULL AND NOT casrn IN ("NA", "")
				THEN "https://commonchemistry.cas.org/detail?cas_rn="||casrn
			WHEN inchikey IS NOT NULL AND NOT inchikey IN ("NA", "")
				THEN "https://www.google.com/search?q="||inchikey
			WHEN inchi IS NOT NULL AND NOT inchi IN ("NA", "")
				THEN "https://www.google.com/search?q="||inchi
			WHEN smiles IS NOT NULL AND NOT smiles IN ("NA", "")
				THEN "https://www.google.com/search?q=canonical+SMILES+"||
					REPLACE(smiles, "#", "%23")
			WHEN obtained_from IS NOT NULL 
				THEN obtained_from
			ELSE
				"(not available)"
		END AS link
		FROM compounds;
/*magicsplit*/

/* - Triggers */
	
CREATE TRIGGER IF NOT EXISTS new_contributor
	/* When creating a new contributor, redirect to allow using affiliations(name) 
	 * instead of affiliations(id), but still allow for direct use of 
	 * affiliations(id). */
	AFTER INSERT ON contributors
	WHEN NEW.affiliation NOT IN (SELECT id FROM affiliations)
BEGIN
	INSERT OR IGNORE INTO affiliations (name)
		VALUES (NEW.affiliation);
	UPDATE OR IGNORE contributors
		SET affiliation = (SELECT id FROM affiliations WHERE name = NEW.affiliation)
		WHERE ROWID = NEW.ROWID;
END;
/*magicsplit*/

/* - Insert any required limited data. */

INSERT OR IGNORE INTO affiliations VALUES(1, "system");
/*magicsplit*/

INSERT OR IGNORE INTO contributors (id, first_name, last_name, affiliation) VALUES(1, "auto", "log", 1);
/*magicsplit*/

/* - Import standard data tables */

.import --csv --skip 1 config/data/elements.csv elements

/*magicsplit*/

.import --csv --skip 1 config/data/isotopes.csv isotopes

/*magicsplit*/

.import --csv --skip 1 config/data/norm_fragment_generation_type.csv norm_fragment_generation_type

/*magicsplit*/

.import --csv --skip 1 config/data/norm_ionization.csv norm_ionization

/*magicsplit*/

.import --csv --skip 1 config/data/norm_source_types.csv norm_source_types

/*magicsplit*/

.import --csv --skip 1 config/data/norm_vendors.csv norm_vendors

/*magicsplit*/

.import --csv --skip 1 config/data/norm_log_execution_from.csv norm_log_execution_from

/*magicsplit*/

.import --csv --skip 1 config/data/norm_qc_methods_reference.csv norm_qc_methods_reference

/*magicsplit*/

.import --csv --skip 1 config/data/norm_qc_methods_name.csv norm_qc_methods_name

/*magicsplit*/
