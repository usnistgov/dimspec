/*
create database if not exists nist_pfas_nta_dev;
	-- Database to hold results of non-targeted analyses mass spectrometry experiments.
	-- To build, open a new database in SQLite in this directory
		sqlite3 nist_pfas_nta_dev
		.read build.sql
	-- Example data can be read in with
		.read demo_data.sql
*/

create table if not exists elements
	/*
		Elemental isotope abundance ratios for comparison and deconvolution.
	*/
(
	atomic_number	INTEGER PRIMARY KEY,	-- periodic table atomic number (e.g. 2)
	symbol			TEXT NOT NULL,		-- periodic table symbol (e.g. "He")
	common_name		TEXT NOT NULL		-- periodic table common name (e.g. "Helium")
);

create table if not exists isotopes
	/*
		Elemental isotope abundance ratios for comparison and deconvolution.
	*/
(
	atomic_number	INTEGER,			-- periodic table atomic number (e.g. 2)
	exact_mass		REAL NOT NULL,		-- exact atomic mass (e.g. 4.00260325413)
	abundance		REAL NOT NULL,		-- isotopic abundance of exact_mass (e.g. 0.99999866)
	FOREIGN KEY (atomic_number) REFERENCES elements(atomic_number),
	CHECK (abundance BETWEEN 0 AND 1)
);

create table if not exists compounds
	/*
		Controlled list of chemical compounds with attributable analytical data.
	*/
(
	id 				INTEGER PRIMARY KEY,
	name 			TEXT, 				-- name of compound, uncontrolled
	inchi 			TEXT, 				-- inchi structure of compound, as submitted
	source 			TEXT, 				-- DOI/Link of compound structure's source
	source_type 	TEXT, 				-- one-letter character indicating type of source
	additional 		TEXT, 				-- additional information, as submitted
	smiles 			TEXT, 				-- smiles structure of compound, derived
	inchikey 		TEXT, 				-- inchikey structure of compound, derived
	local_pos 		INTEGER, 			-- number of atoms with positive charges, derived
	local_neg 		INTEGER, 			-- number of atoms with negative charges, derived
	formula 		TEXT, 				-- elemental formula, derived
	fixedmass 		REAL, 				-- exact mass of compound, derived
	netcharge 		INTEGER, 			-- total formal charge of compound, derived
	dtxsid 			TEXT, 				-- dtxsid identifier
	dtxcid 			TEXT, 				-- dtxcid identifier
	casrn 			TEXT, 				-- CAS registry number
	pubchemid 		TEXT, 				-- PubChem identifier
	inspectedby 	TEXT 				-- user inspection id
);

create table if not exists solvents
	/*
		mobile phase solvent list: controlled.
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
	FOREIGN KEY (solvent_id) REFERENCES solvents(id)
);

create table if not exists solvent_mix
	/*
		Mobile phase solvent mixture for a given elution method
	*/
(
	mix_id			INTEGER,			-- mixture identifier to gather discrete components
	component		INTEGER,			-- foreign key to solvent_fractions
	fraction		REAL NOT NULL,		-- amount fraction amount of this solvent in the mixture, contrained from 0 - 1
	FOREIGN KEY (component) REFERENCES solvents(id),
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
	FOREIGN KEY (ms_vendor) REFERENCES vendors(id),
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
	duration		REAL NOT NULL,		-- time duration mobile phase was applied
	duration_units	TEXT NOT NULL DEFAULT "minute", -- time duration units, constrained to one of "second" or "minute"
	FOREIGN KEY (method_id) REFERENCES methods(id),
	FOREIGN KEY (carrier) REFERENCES solvent_mix(mix_id),
	CHECK (duration_units IN ("second", "minute"))
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
		Samples from which analytical data are derived.
	*/
(
	id				INTEGER PRIMARY KEY,
	name			TEXT, 				-- user-defined name of the sample
	sample_class	INTEGER, 			-- foreign key to sample_classes
	source 			TEXT, 				-- citation for the sample source
	FOREIGN KEY (sample_class) REFERENCES sample_classes(id)
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
	FOREIGN KEY (compound_id) REFERENCES compounds(id),
	FOREIGN KEY (exp_id) REFERENCES methods(id),
	FOREIGN KEY (sample_id) REFERENCES samples(id),
	CHECK (charge IN (-1, 1))
);

create table if not exists ms1data
	/*
		Mass spectral data derived from experiments on a compound by compound basis. Emperical isotopic pattern.
	*/
(
	id 				INTEGER PRIMARY KEY,
	peak_id			INTEGER, 			-- foreign key to ms1data
	scantime 		REAL NOT NULL, 		-- scan time of spectrum
	ms1_data 		TEXT NOT NULL, 		-- locator/actual MS1 isotope data
	FOREIGN KEY (peak_id) REFERENCES peaks(id)
);

create table if not exists ms2data
	/*
		Mass spectral data associated with ms2data; fragment spectra for an entry in ms1data.
	*/
(
	id 				INTEGER PRIMARY KEY,
	peak_id			INTEGER, 			-- foreign key to ms1data
	scantime 		REAL NOT NULL, 		-- scan time of spectrum
	ms2_data 		TEXT NOT NULL, 		-- locator/actual MS2 fragmentation data
	FOREIGN KEY (peak_id) REFERENCES peaks(id)
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
	source 			TEXT DEFAULT 'user' NOT NULL, 	-- citation/source for fragment identity, "user" is an option
	CHECK (charge IN (-1, 1)),
	CHECK (radical IN (0, 1))
);

create table if not exists fragment_ms1data_linkage
	/*
		Experimental error associated with a fragment_table(fragment_id) and data_table(data_id)
	*/
(
	fragment_id 	INTEGER NOT NULL, 	-- foreign key to fragment_table
	data_id 		INTEGER NOT NULL, 	-- foreign key to data_table
	mz_error 		REAL NOT NULL, 		-- measured mass error from fragment_table$MZ, derived
	FOREIGN KEY (fragment_id) REFERENCES fragments(id),
	FOREIGN KEY (data_id) REFERENCES ms1data(id)
);

.import --csv --skip 1 data/elements.csv elements
.import --csv --skip 1 data/isotopes.csv isotopes

create view if not exists element_isotopes as
	select e.atomic_number, e.symbol, e.common_name, i.exact_mass, i.abundance
	from elements e
	join isotopes i on e.atomic_number = i.atomic_number;

create view if not exists view_mobile_phase AS
	select sm.mix_id, s.name as solvent, sm.fraction from solvent_mix sm
		left join solvents s on s.id = sm.component;
		