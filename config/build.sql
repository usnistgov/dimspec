/*=============================================================================
NIST database to hold results of non-targeted analysis high-resolution 
accurate-mass mass spectrometry (NTA-HRAM-MS) experiments.

					** Development version **
					
For information or support, contact the development team at
	- NIST PFAS Program		PFAS@nist.gov
	- Jared M. Ragland		jared.ragland@nist.gov
	- Benjamin J. Place		benjamin.place@nist.gov
===============================================================================
Use this script to create a sketch of the SQLite database. Building in this 
manner requires sqlite3 to be installed. Run from the project directory with

	sqlite3 nist_pfas_nta_dev
	.read config/build.sql

Note that build files are mostly located in the "config" directory and local 
paths will need to be referenced appropriately, which may require modifications 
to this script. This mostly applies to the commands in this introductory 
section and at the end of this script file where default tables are populated.

This will create the schema on your local machine for local population.
Example data (a full demonstration version will be provided in the future) can 
be read in with

	.read config/demo_data.sql
	
=============================================================================*/

/* Create tables */

create table if not exists elements
	/*
		Elemental isotope abundance ratios for comparison and deconvolution.
	*/
(
	atomic_number	INTEGER PRIMARY KEY,	-- periodic table atomic number (e.g. 2)
	symbol			TEXT NOT NULL UNIQUE,	-- periodic table symbol (e.g. "He")
	common_name		TEXT NOT NULL UNIQUE	-- periodic table common name (e.g. "Helium")
);

create table if not exists isotopes
	/*
		Elemental isotope abundance ratios for comparison and deconvolution.
	*/
(
	atomic_number	INTEGER,			-- periodic table atomic number (e.g. 2)
	exact_mass		REAL NOT NULL,		-- exact atomic mass (e.g. 4.00260325413)
	abundance		REAL NOT NULL,		-- isotopic abundance of exact_mass (e.g. 0.99999866)
	FOREIGN KEY (atomic_number)
		REFERENCES elements(atomic_number)
		ON UPDATE CASCADE,
	CHECK (abundance BETWEEN 0 AND 1)
);

create table if not exists source_types
	/*
		Validation list of source types to be used in the compounds table.
	*/
(
	id				INTEGER PRIMARY KEY,
	abbreviation	TEXT NOT NULL,		-- (single) letter abbreviation for the source type
	st_type			TEXT NOT NULL,		-- full name of the source type
	definition		TEXT NOT NULL		-- definition of the source type
);

create table if not exists compounds
	/*
		Controlled list of chemical compounds with attributable analytical data.
	*/
(
	id 				INTEGER PRIMARY KEY,
	name 			TEXT NOT NULL,		-- name of compound, uncontrolled
	inchi 			TEXT NOT NULL,		-- inchi structure of compound, as submitted
	source 			TEXT NOT NULL,		-- DOI/Link of compound structure's source
	source_type 	INTEGER NOT NULL,	-- one-letter character indicating type of source
	additional 		TEXT, 				-- additional information, as submitted
	smiles 			TEXT NOT NULL, 		-- smiles structure of compound, derived
	inchikey 		TEXT NOT NULL, 		-- inchikey structure of compound, derived
	local_pos 		INTEGER NOT NULL DEFAULT 0,	-- number of atoms with positive charges, derived
	local_neg 		INTEGER NOT NULL DEFAULT 0,	-- number of atoms with negative charges, derived
	formula 		TEXT NOT NULL, 				-- elemental formula, derived
	fixedmass 		REAL NOT NULL, 				-- exact mass of compound, derived
	netcharge 		INTEGER NOT NULL DEFAULT 0,	-- total formal charge of compound, derived
	dtxsid 			TEXT, 				-- dtxsid identifier
	dtxcid 			TEXT, 				-- dtxcid identifier
	casrn 			TEXT, 				-- CAS registry number
	pubchemid 		TEXT, 				-- PubChem identifier
	inspectedby 	TEXT, 				-- user inspection id
	inspectedon		TEXT,				-- timestamp at which this compound was recorded as inspected (YYYY-MM-DD HH:MM:SS UTC)
	CHECK (local_pos >= 0),
	CHECK (local_neg >= 0),
	FOREIGN KEY (source_type)
		REFERENCES source_types(id)
		ON UPDATE CASCADE
);

create table if not exists solvents
	/*
		Mobile phase solvent list: controlled.
	*/
(
	id				INTEGER PRIMARY KEY,
	name			TEXT UNIQUE			-- IUPAC name for mobile phase solvents
);

create table if not exists solvent_aliases
	/*
		List of common aliases for each entry in table solvents
	*/
(
	solvent_id		INTEGER,			-- foreign key to the solvents table
	alias			TEXT UNIQUE,		-- human meaningful name(s) associated with a solvent
	FOREIGN KEY (solvent_id)
		REFERENCES solvents(id)
		ON UPDATE CASCADE
);

create table if not exists solvent_mix
	/*
		Mobile phase solvent mixture for a given elution method
	*/
(
	mix_id			INTEGER,			-- mixture identifier to gather discrete components
	component		INTEGER,			-- foreign key to solvent_fractions
	fraction		REAL NOT NULL,		-- amount fraction amount of this solvent in the mixture, contrained from 0 - 1
	FOREIGN KEY (component)
		REFERENCES solvents(id)
		ON UPDATE CASCADE,
	CHECK (fraction BETWEEN 0 AND 1)
);

create table if not exists vendors
	/*
		Normalization table holding commercial instrument vendor information.
	*/
(
	id				INTEGER PRIMARY KEY,
	name			TEXT NOT NULL		-- company name
);

create table if not exists ms_method
	/*
		Mass spectrometer method settings.
	*/
(
	id				INTEGER PRIMARY KEY,
	ionization		TEXT, 				-- ionization mode (ESI, APCI, EI, etc.)
	voltage 		REAL, 				-- ionization voltage/Current (depending on mode)
	polarity 		TEXT, 				-- ionization polarity (negative, positive, or negative/positive)
	ce_value 		TEXT, 				-- value for collision energy, normally a number but can be a range
	ce_desc 		TEXT, 				-- description/context of the collision energy value (normalized, stepped, range, etc.)
	ms_vendor 		INTEGER,			-- vendor of the mass spectrometer
	ms_type 		TEXT, 				-- type of mass analyzers (QTOF, Q-ORBITRAP, etc.)
	qc_method 		INTEGER, 			-- (0, 1) boolean: does the experiment have a QC method in place
	qc_type 		TEXT, 				-- category of QC analysis (TBD)
	source 			TEXT, 				-- citation for the experimental method
	FOREIGN KEY (ms_vendor)
		REFERENCES vendors(id)
		ON UPDATE CASCADE,
	CHECK (polarity IN ('negative', 'positive', 'negative/positive')),
	CHECK (qc_method IN (0, 1))
);

create table if not exists mobile_phases
	/*
		Description of mobile phases used during a chromatographic separation.
	*/
(
	method_id		INTEGER,			-- foreign key to methods
	carrier			INTEGER,			-- foreign key to solvent_mix
	additive		TEXT,				-- buffer/salt/acid addition to mobile phase
	duration		REAL,				-- time duration mobile phase was applied
	duration_units	TEXT DEFAULT "minutes",	-- time duration units, constrained to one of "second" or "minutes"
	FOREIGN KEY (method_id)
		REFERENCES methods(id)
		ON UPDATE CASCADE,
	FOREIGN KEY (carrier)
		REFERENCES solvent_mix(mix_id)
		ON UPDATE CASCADE,
	CHECK (duration_units IN ("seconds", "minutes")),
	CHECK (duration > 0)
);

create table if not exists sample_classes
	/*
		Normalization table linking to samples to hold controlled vocabulary.
	*/
(
	id				INTEGER PRIMARY KEY,
	name			TEXT NOT NULL UNIQUE	-- name of the sample class
);

create table if not exists samples
	/*
		Samples from which analytical data are derived. What goes into an analytical instrument.
	*/
(
	id				INTEGER PRIMARY KEY,
	name			TEXT, 				-- user-defined name of the sample
	sample_class	INTEGER, 			-- foreign key to sample_classes
	source 			TEXT, 				-- citation for the sample source
	data_generator	TEXT,				-- generator of data for this sample
	msconvert_settings INTEGER,			-- settings for the msconvert program used to generate data from this sample
	FOREIGN KEY (sample_class)
		REFERENCES sample_classes(id)
		ON UPDATE CASCADE,
	FOREIGN KEY (msconver_settings)
		REFERENCES msconvert_settings(id)
		ON UPDATE CASCADE
);

create table if not exists peaks
	/*
		Mass spectral data derived from experiments on a compound by compound basis. Emperical isotopic pattern.
	*/
(
	id 				INTEGER PRIMARY KEY,
	compound_id		INTEGER, 			-- foreign key to compounds
	exp_id			INTEGER, 			-- foreign key to methods
	sample_id		INTEGER, 			-- foreign key to samples
	measured_mz1 	REAL NOT NULL, 		-- precursor ion m/z
	peak_timestamp	TEXT,				-- timestamp for the peak (YYYY-MM-DD HH:MM:SS UTC)
	charge 			INTEGER NOT NULL, 	-- ion charge state (-1, 1)
	rt_start 		REAL NOT NULL, 		-- peak retention time start point
	rt_center 		REAL NOT NULL, 		-- peak retention time centroid, derived
	rt_end 			REAL NOT NULL, 		-- peak retention time end point
	FOREIGN KEY (compound_id)
		REFERENCES compounds(id)
		ON UPDATE CASCADE,
	FOREIGN KEY (exp_id)
		REFERENCES methods(id)
		ON UPDATE CASCADE,
	FOREIGN KEY (sample_id)
		REFERENCES samples(id)
		ON UPDATE CASCADE,
	CHECK (charge IN (-1, 1))
);

create table if not exists ms1data
	/*
		Mass spectral data derived from experiments on a compound by compound basis. Emperical isotopic pattern.
	*/
(
	id 				INTEGER PRIMARY KEY,
	peak_id			INTEGER, 			-- foreign key to peaks
	scantime 		REAL NOT NULL, 		-- scan time of spectrum
	ms1_data 		TEXT NOT NULL, 		-- locator/actual MS1 isotope data
	contributor		TEXT,				-- contributor for these data
	FOREIGN KEY (peak_id)
		REFERENCES peaks(id)
		ON UPDATE CASCADE
);

create table if not exists ms2data
	/*
		Mass spectral data associated with ms2data; fragment spectra for an entry in ms1data.
	*/
(
	id 				INTEGER PRIMARY KEY,
	peak_id			INTEGER, 			-- foreign key to peaks
	scantime 		REAL NOT NULL, 		-- scan time of spectrum
	ms2_data 		TEXT NOT NULL, 		-- locator/actual MS2 fragmentation data
	FOREIGN KEY (peak_id)
		REFERENCES peaks(id)
		ON UPDATE CASCADE
);

create table if not exists fragments
	/*
		Potential annotated fragment ions that are attributed to one or more mass spectra.
	*/
(
	id 				INTEGER PRIMARY KEY,
	mz 				REAL NOT NULL, 		-- m/z value for specific fragment, derived
	formula 		TEXT NOT NULL, 		-- elemental formula for specific fragment, user submitted
	charge 			INTEGER NOT NULL, 	-- charge of specific fragment,derived
	radical 		INTEGER NOT NULL, 	-- TRUE/FALSE: the fragment contains a radical electron, user submitted
	smiles	 		TEXT, 				-- smiles structure of fragment ion, can be NULL, user submitted
	source 			TEXT NOT NULL DEFAULT 'user', 	-- citation/source for fragment identity, "user" is an option
	CHECK (charge IN (-1, 1)),
	CHECK (radical IN (0, 1))
);

create table if not exists fragment_ms1data_linkage
	/*
		Experimental error associated with a fragment_table(fragment_id) and data_table(data_id)
	*/
(
	fragment_id 	INTEGER NOT NULL, 	-- foreign key to fragment_table
	data_id 		INTEGER NOT NULL, 	-- foreign key to ms1data
	mz_error 		REAL NOT NULL, 		-- measured mass error from fragment_table$MZ, derived
	FOREIGN KEY (fragment_id)
		REFERENCES fragments(id)
		ON UPDATE CASCADE,
	FOREIGN KEY (data_id)
		REFERENCES ms1data(id)
		ON UPDATE CASCADE
);

create table if not exists msconvert_settings
	/*
		Settings specific to the msconvert program.
	*/
(
	id				INTEGER UNIQUE,
	mzML			INTEGER NOT NULL DEFAULT 1,	-- TRUE/FALSE: whether to use the mzML format
	zlib			INTEGER NOT NULL DEFAULT 1,	-- TRUE/FALSE; whether to use the zlib setting
	inten64			INTEGER NOT NULL DEFAULT 1,	-- TRUE/FALSE; whether to use the inten64 setting
	filter_peak_pick	TEXT NOT NULL DEFAULT "peakPicking vendor msLevel=1-",	-- peak picking setting
	filter_threshold	TEXT NOT NULL DEFAULT "threshold absolute 1 most-intense 1-", -- threshold setting
	UNIQUE(mzML, zlib, inten64, filter_peak_pick, filter_threshold),
	CHECK (id > 0),								-- ensure id is an unsigned int
	CHECK (mzML IN (0, 1)),						-- ensure mzML is boolean
	CHECK (zlib IN (0, 1)),						-- ensure zlib is boolean
	CHECK (inten64 IN (0, 1))					-- ensure inten64 is boolean
);

INSERT OR IGNORE INTO msconvert_settings(id) VALUES (1);

/* Create views */

create view if not exists element_isotopes as
	select e.atomic_number, e.symbol, e.common_name, i.exact_mass, i.abundance
	from elements e
	join isotopes i on e.atomic_number = i.atomic_number;

create view if not exists view_mobile_phase AS
	select sm.mix_id, s.name as solvent, sm.fraction from solvent_mix sm
		left join solvents s on s.id = sm.component;

/* Import default data tables */

.import --csv --skip 1 config/data/elements.csv elements
.import --csv --skip 1 config/data/isotopes.csv isotopes
.import --csv --skip 1 config/data/source_types.csv source_types
		