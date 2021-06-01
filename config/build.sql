/*====================================================================================================
Description:	Build a database sketch to hold results for non-targeted analysis high-resolution 
				accurate-mass mass spectrometry (NTA-HRAM-MS) experiments.
Status:			Development version
LastUpdate:		2021-05-28
Support:		For information or support, contact the development team at
					- NIST PFAS Program	PFAS@nist.gov
					- Jared M. Ragland	jared.ragland@nist.gov	*author
					- Benjamin J. Place	benjamin.place@nist.gov
Dependencies:	sqlite3
Usage:			Run this script from the terminal to CREATE a sketch of the SQLite database. It is 
				recommended to run from the project directory as
				
					sqlite3 nist_pfas_nta_dev
					.read config/build.sql
				
				Note that build files are mostly located in the "config" directory and local 
				paths will need to be referenced appropriately, which may require modifications 
				to this script. This mostly applies to the commands in this introductory 
				section and at the end of this script file where DEFAULT tables are populated.
				
				This will CREATE the schema on your local machine for local population.
				Example data (a full demonstration version will be provided in the future) can 
				be read in with
				
					.read config/demo_data.sql
					
				See the version history after build with
					
					SELECT * from log where category = "history" and affects = "build.sql"
					
====================================================================================================*/

/* Create tables */

/* -Reference node begins */

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

/* --Normalization tables */

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

CREATE TABLE IF NOT EXISTS norm_ms_types
	/* Normalization table for mass spectrometer types. */
(
	id	INTEGER PRIMARY KEY,
	name	TEXT NOT NULL UNIQUE	/* type of the mass analyzer */
);

CREATE TABLE IF NOT EXISTS norm_sample_classes
	/* Normalization TABLE linking to samples to hold controlled vocabulary. */
(
	id	INTEGER PRIMARY KEY,
	name	TEXT NOT NULL UNIQUE	/* name of the sample class */
);

CREATE TABLE IF NOT EXISTS norm_solvents
	/* Mobile phase solvent list: controlled. */
(
	id	INTEGER PRIMARY KEY,
	name	TEXT NOT NULL UNIQUE,	/* IUPAC name for mobile phase norm_solvents */
	tech	TEXT NOT NULL,	/* controlled vocabulary for separation system, one of "GC" or "LC" */
	/* Constraints */
	CHECK (tech IN ("GC", "LC"))
);

CREATE TABLE IF NOT EXISTS norm_source_types
	/* Validation list of source types to be used in the compounds TABLE. */
(
	id	INTEGER PRIMARY KEY,
	abbreviation	TEXT NOT NULL,	/* (single) letter abbreviation for the source type */
	st_type	TEXT NOT NULL,	/* full name of the source type */
	definition	TEXT NOT NULL	/* definition of the source type */
);

CREATE TABLE IF NOT EXISTS norm_vendors
	/* Normalization TABLE holding commercial instrument vendor information. */
(
	id	INTEGER PRIMARY KEY,
	name	TEXT NOT NULL UNIQUE	/* company name */
);

/* -Reference node ends */

/* -Analyte node begins */

CREATE TABLE IF NOT EXISTS compounds
	/* Controlled list of chemical compounds with attributable analytical data. */
(
	id
		INTEGER PRIMARY KEY,
	category
		INTEGER,
		/* FOREIGN KEY TO compound_categories */
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

CREATE TABLE IF NOT EXISTS compound_categories
	/* Chemical class category of compounds. */
(
	id INTEGER PRIMARY KEY,
	name	TEXT NOT NULL,	/* name of the class */
	subclass_of INTEGER, /* self referential to compound_categories */
	/* Foreign key relationships */
	FOREIGN KEY (subclass_of) REFERENCES compound_categories(id)
);

CREATE TABLE IF NOT EXISTS compound_fragments
	/* Linkage TABLE to tie compounds to their confirmed and annotated fragments. */
(
	peak_id INTEGER NOT NULL, /* FOREIGN KEY to peaks */
	compound_id INTEGER NOT NULL, /* FOREIGN KEY to compounds */
	fragment_id INTEGER NOT NULL, /* FOREIGN KEY to fragments */
	/* Foreign key relationships */
	FOREIGN KEY (peak_id) REFERENCES peaks(id),
	FOREIGN KEY (compound_id) REFERENCES compounds(id),
	FOREIGN KEY (fragment_id) REFERENCES fragments(id)
);

CREATE TABLE IF NOT EXISTS fragments
	/* Potential annotated fragment ions that are attributed to one or more mass spectra. */
(
	id	INTEGER PRIMARY KEY,
	mz	REAL NOT NULL,	/* m/z value for specific fragment, derived */
	formula	TEXT NOT NULL,	/* elemental formula for specific fragment, user submitted */
	description	TEXT NOT NULL,	/*  */
	charge	INTEGER NOT NULL,	/* charge of specific fragment,derived */
	radical	INTEGER NOT NULL,	/* TRUE/FALSE: the fragment contains a radical electron, user submitted */
	smiles	TEXT,	/* smiles structure of fragment ion, can be NULL, user submitted */
	inchi	TEXT,	/* InChI representation of fragment ion, can be NULL, user submitted */
	inchikey	TEXT,	/* InChIKey representation of fragment ion, can be NULL, user submitted */
	/* Constraints */
	CHECK (charge IN (-1, 1)),
	CHECK (radical IN (0, 1)),
	CHECK (formula GLOB Replace(Hex(ZeroBlob(Length(formula))), '00', '[A-Za-z0-9]'))
);

CREATE TABLE IF NOT EXISTS fragment_sources
	/* Citation information about a given fragment to hold multiple identifications (e.g. one in silico and two empirical) */
(
	fragment_id	INTEGER NOT NULL,	/* FOREIGN KEY to fragments */
	generated_from	INTEGER NOT NULL,	/* FOREIGN KEY to norm_fragment_generation_type */
	citation	TEXT NOT NULL,	/* DOI, etc. */
	/* Foreign key relationships */
	FOREIGN KEY (fragment_id) REFERENCES fragments(id),
	FOREIGN KEY (generated_from) REFERENCES norm_fragment_generation_type(id)
);

CREATE TABLE IF NOT EXISTS log
	/* Placeholder for logs */
(
	id	INTEGER PRIMARY KEY,
	of_type TEXT,
	action TEXT,
	affects_table TEXT,
	executed_from TEXT,
	executed_on TEXT DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS mobile_phases
	/* Description of mobile phases used during a chromatographic separation. */
(
	methods_id	INTEGER NOT NULL,	/* FOREIGN KEY to methods */
	carrier	INTEGER NOT NULL,	/* FOREIGN KEY to solvent_mixes */
	additive	TEXT,	/* buffer/salt/acid addition to mobile phase */
	duration	REAL,	/* time duration mobile phase was applied */
	duration_units	TEXT DEFAULT "minutes",	/* time duration units, constrained to one of "second" or "minutes" */
	/* Constraints */
	CHECK (duration_units IN ("seconds", "minutes")),
	CHECK (duration > 0),
	/* Foreign key relationships */
	FOREIGN KEY (methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
	FOREIGN KEY (carrier) REFERENCES solvent_mixes(mix_id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS ms1data
	/* Mass spectral data derived from experiments on a compound by compound basis. Emperical isotopic pattern. */
(
	id 	INTEGER PRIMARY KEY,
	peak_id	INTEGER NOT NULL, 	/* FOREIGN KEY to peaks */
	scantime 	REAL NOT NULL, 	/* scan time of spectrum */
	ms1_data 	TEXT NOT NULL, 	/* locator/actual MS1 fragmentation data */
	contributor	TEXT,	/* contributor for these data */
	/* Constraints */
	CHECK (scantime > 0),
	/* Foreign key relationships */
	FOREIGN KEY (peak_id) REFERENCES peaks(id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS ms1_spectra
	/* Retained mass spectra associated with ms1data. */
(
	ms1data_id	INTEGER NOT NULL,	/* FOREIGN KEY to ms1data */
	mz	REAL NOT NULL,	/* mass to charge ratio */
	intensity	REAL NOT NULL,	/* signal intensity */
	/* Constraints */
	CHECK (mz > 0),
	CHECK (intensity > 0),
	/* Foreign key relationships */
	FOREIGN KEY (ms1data_id) REFERENCES ms1data(id)
);

CREATE TABLE IF NOT EXISTS ms2data
	/* Mass spectral data associated with ms2data for a given peak. */
(
	id 	INTEGER PRIMARY KEY,
	peak_id	INTEGER NOT NULL, 	/* FOREIGN KEY to peaks */
	scantime 	REAL NOT NULL, 	/* scan time of spectrum */
	ms2_data 	TEXT NOT NULL, 	/* NIST INTERNAL REFERENCE locator for raw MS2 fragmentation data */
	/* Constraints */
	CHECK (scantime > 0),
	/* Foreign key relationships */
	FOREIGN KEY (peak_id) REFERENCES peaks(id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS ms2_spectra
	/* Retained mass spectra associated with ms2data. */
(
	ms2data_id	INTEGER NOT NULL,	/* FOREIGN KEY to ms2data */
	mz	REAL NOT NULL,	/* mass to charge ratio */
	intensity	REAL NOT NULL,	/* signal intensity */
	/* Constraints */
	CHECK (mz > 0),
	CHECK (intensity > 0),
	/* Foreign key relationships */
	FOREIGN KEY (ms2data_id) REFERENCES ms2data(id)
);

CREATE TABLE IF NOT EXISTS msconvert_settings
	/* Settings specific to the msconvert program. */
(
	setting TEXT,
	id		INTEGER
	/* Constraints */
	-- CHECK (id > 0 AND id > (select max(id) from msconvert_settings))	/* ensure id is new on insertion */
);

/*
DEPRECATED
CREATE TABLE IF NOT EXISTS msconvert_settings
	/* Settings specific to the msconvert program.
(
	id		INTEGER PRIMARY KEY,
	mzML	INTEGER NOT NULL DEFAULT 1,	/* TRUE/FALSE: whether to use the mzML format
	zlib	INTEGER NOT NULL DEFAULT 1,	/* TRUE/FALSE; whether to use the zlib setting
	inten64	INTEGER NOT NULL DEFAULT 1,	/* TRUE/FALSE; whether to use the inten64 setting
	filter_peak_pick	TEXT NOT NULL DEFAULT "peakPicking vendor msLevel=1-",	/* peak picking setting
	filter_threshold	TEXT NOT NULL DEFAULT "threshold absolute 1 most-intense 1-", /* threshold setting
	/* Constraints
	UNIQUE(mzML, zlib, inten64, filter_peak_pick, filter_threshold),
	CHECK (id > 0),	/* ensure id is an unsigned int
	CHECK (mzML IN (0, 1)),	/* ensure mzML is boolean
	CHECK (zlib IN (0, 1)),	/* ensure zlib is boolean
	CHECK (inten64 IN (0, 1))	/* ensure inten64 is boolean
);
*/

CREATE TABLE IF NOT EXISTS ms_descriptions
	/* Full description of all mass spectrometer types used for a given entry in the ms_methods TABLE. */
(
	ms_methods_id	INTEGER NOT NULL,	/* FOREIGN KEY to ms_methods */
	ms_types_id	INTEGER NOT NULL,	/* FOREIGN KEY to norm_ms_types */
	/* Constraints */
	UNIQUE(ms_methods_id, ms_types_id),
	/* Foreign key relationships */
	FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
	FOREIGN KEY (ms_types_id) REFERENCES norm_ms_types(id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS ms_methods
	/* Mass spectrometer method settings. */
(
	id	INTEGER PRIMARY KEY,
	ionization	INTEGER, 	/* ionization mode (ESI, APCI, EI, etc.) */
	voltage 	REAL, 	/* ionization voltage/Current (depending on mode) */
	polarity 	TEXT, 	/* ionization polarity (negative, positive, or negative/positive) */
	ce_value 	TEXT, 	/* value for collision energy, normally a number but can be a range */
	ce_desc 	TEXT, 	/* description/context of the collision energy value (normalized, stepped, range, etc.) */
	ms_vendor 	INTEGER,	/* vendor of the mass spectrometer; FOREIGN KEY to norm_vendors */
	has_qc_method	INTEGER NOT NULL, 	/* (0, 1) boolean: does the experiment have a QC method in place */
	citation 	TEXT, 	/* citation for the experimental method */
	/* Constraints */
	CHECK (polarity IN ('negative', 'positive', 'negative/positive')),
	CHECK (has_qc_method IN (0, 1)),
	/* Foreign key relationships */
	FOREIGN KEY (ms_vendor) REFERENCES norm_vendors(id) ON UPDATE CASCADE,
	FOREIGN KEY (ionization) REFERENCES norm_ionization(id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS peaks
	/* Peaks (or features) identified within the results from a sample. */
(
	id 	INTEGER PRIMARY KEY,
	sample_id	INTEGER NOT NULL, 	/* FOREIGN KEY to samples */
	measured_mz1 	REAL NOT NULL, 	/* precursor ion mass to charge ratio (constrained to positive numbers) */
	peak_timestamp	TEXT NOT NULL,	/* timestamp for the peak (YYYY-MM-DD HH:MM:SS UTC) */
	charge 	INTEGER NOT NULL, 	/* ion charge state (constrained to -1 ["negative"] or 1 ["positive"]) */
	rt_start 	REAL NOT NULL, 	/* peak retention time start point (constrained to positive numbers) */
	rt_center 	REAL NOT NULL, 	/* peak retention time centroid, (derived constrained to positive numbers) */
	rt_end 	REAL NOT NULL, 	/* peak retention time end point (constrained to positive numbers) */
	/* Constraints */
	CHECK (measured_mz1 > 0),
	CHECK (charge IN (-1, 1)),
	CHECK (rt_start > 0),
	CHECK (rt_center > 0),
	CHECK (rt_end > 0),
	/* Foreign key relationships */
	FOREIGN KEY (sample_id) REFERENCES samples(id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS qc_methods
	/* References to quality control (QC) methods used to vet experimental results */
(
	id	INTEGER PRIMARY KEY,
	ms_methods_id	INTEGER NOT NULL,	/* FOREIGN KEY to ms_methods */
	name	TEXT NOT NULL,	/* the type of QC performed; controlled vocabulary must be one of "Mass Analyzer Calibration", "External Standard Verification", "Internal Standard Verification", or "Matrix Standard Verification" */
	reference	TEXT,	/* the category of the QC method; controlled vocabulary must be one of "SOP (Internal)", "SOP (External/Published)", or "Manuscript" */
	reference_text	TEXT,	/* free text entry pointing to a description of the QC method, whether a DOI, SOP reference, or manual description */
	/* Constraints */
	CHECK ("reference" IN ("SOP (Internal)", "SOP (External/Published)", "Manuscript")),
	CHECK ("type" IN ("Mass Analyzer Calibration", "External Standard Verification", "Internal Standard Verification", "Matrix Standard Verification")),
	/* Foreign key relationships */
	FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS samples
	/* Samples from which analytical data are derived. What goes into an analytical instrument.	*/
(
	id	INTEGER PRIMARY KEY,
	name	TEXT NOT NULL, 	/* user-defined name of the sample */
	sample_class_id	INTEGER NOT NULL, 	/* FOREIGN KEY to norm_sample_classes */
	source 	TEXT, 	/* citation for the sample source */
	data_generator	TEXT NOT NULL,	/* generator of data for this sample */
	msconvert_settings_id	INTEGER,	/* settings for the msconvert program used to generate data from this sample */
	ms_methods_id	INTEGER,	/* FOREIGN KEY to methods */
	/* Foreign key relationships */
	FOREIGN KEY (sample_class_id) REFERENCES norm_sample_classes(id) ON UPDATE CASCADE,
	FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
	FOREIGN KEY (msconvert_settings_id) REFERENCES msconvert_settings(id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS solvent_aliases
	/* List of common aliases for each entry in TABLE norm_solvents	*/
(
	solvent_id	INTEGER NOT NULL,	/* FOREIGN KEY to norm_solvents */
	alias	TEXT NOT NULL UNIQUE,	/* human meaningful name(s) associated with a solvent */
	/* Foreign key relationships */
	FOREIGN KEY (solvent_id) REFERENCES norm_solvents(id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS solvent_mixes
	/*	Mobile phase solvent mixture for a given elution method	*/
(
	mix_id	INTEGER NOT NULL,	/* mixture identifier to gather discrete components */
	component	INTEGER NOT NULL,	/* FOREIGN KEY to solvent_fractions */
	fraction	REAL NOT NULL,	/* amount fraction amount of this solvent in the mixture, contrained from 0 - 1 */
	/* Foreign key relationships */
	FOREIGN KEY (component) REFERENCES norm_solvents(id) ON UPDATE CASCADE,
	CHECK (fraction BETWEEN 0 AND 1)
);

/* Create views */

CREATE VIEW IF NOT EXISTS view_element_isotopes AS
	SELECT e.atomic_number, e.symbol, e.common_name AS element, i.exact_mass, i.abundance
		FROM isotopes i
		INNER JOIN elements e ON i.atomic_number = e.atomic_number;

CREATE VIEW IF NOT EXISTS view_mass_analyzers AS
	SELECT msd.ms_methods_id, ms.name
		FROM ms_descriptions msd
		INNER JOIN norm_ms_types ms ON ms.id = msd.ms_types_id;

CREATE VIEW IF NOT EXISTS view_mobile_phase AS
	SELECT sm.mix_id, s.name AS solvent, sm.fraction
		FROM solvent_mixes sm
		INNER JOIN norm_solvents s ON s.id = sm.component;

CREATE VIEW IF NOT EXISTS view_compound_fragments AS
	SELECT c.id, c.formula AS compound, f.formula AS fragments, f.mz
		FROM compounds c
		INNER JOIN compound_fragments cf ON c.id = cf.compound_id
		INNER JOIN fragments f ON cf.fragment_id = f.id
		ORDER BY mz ASC;

CREATE VIEW IF NOT EXISTS view_fragment_count AS
	SELECT c.name, c.formula AS compound, COUNT(f.formula) AS n_fragments
		FROM compounds c
		INNER JOIN compound_fragments cf ON c.id = cf.compound_id
		INNER JOIN fragments f ON cf.fragment_id = f.id
		GROUP BY compound
		ORDER BY n_fragments DESC;
		
CREATE VIEW IF NOT EXISTS view_msconvert_settings AS
	/* Hack around to insert new records to msconvert_settings with an automatic index.	See trigger import_msconvert_settings. */
	SELECT * FROM msconvert_settings;

/* Triggers */

CREATE TRIGGER IF NOT EXISTS import_msconvert_settings
INSTEAD OF INSERT
ON view_msconvert_settings
BEGIN
	INSERT INTO msconvert_settings
	VALUES (
		NEW.setting,
		(IFNULL((SELECT MAX(id) + 1 FROM msconvert_settings), 1))
	);
END;

/* Insert any required limited data. */


/* Import standard data tables */

.import --csv --skip 1 config/data/elements.csv elements
.import --csv --skip 1 config/data/isotopes.csv isotopes
.import --csv --skip 1 config/data/norm_fragment_generation_type.csv norm_fragment_generation_type
.import --csv --skip 1 config/data/norm_ionization.csv norm_ionization
.import --csv --skip 1 config/data/norm_source_types.csv norm_source_types
.import --csv --skip 1 config/data/norm_vendors.csv norm_vendors
	