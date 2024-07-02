/*=============================================================================
	Description
		Methods node schema definition for the NIST high-resolution
		accurate-mass spectrometry spectral database (HRAM-MS-NTA). This node 
		contains information relevant to methodological descriptions of HRAM-MS 
		experiments (e.g. chromatographic and mass spectrometer settings and 
		properties, and quality control data reported for those methods).
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
			.read config/sql_nodes/methods.sql
		
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
	CREATE TABLE IF NOT EXISTS norm_ionization
		/* Normalization table for mass spectrometer ionization source types */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* validation list of ionization source names */
		acronym
			TEXT NOT NULL UNIQUE,
			/* validation list of ionization source acronyms */
		import_text
		  TEXT UNIQUE
		  /* NTA MRT controlled vocabulary expression */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_carriers
		/* Mobile phase carrier list: controlled. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* IUPAC name for mobile phase norm_carriers */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_additives
		/* carrier additives list: controlled. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* IUPAC name for carrier additives */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_vendors
		/* Normalization table holding commercial instrument vendor information. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* company name */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_ce_desc
		/* Normalization table for collision energy description. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* type of CE */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_ce_units
		/* Normalization table for collision energy units. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* collision energy units */
		abbreviation
		  TEXT
			/* collision energy unit abbreviation, if any */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_voltage_units
		/* Normalization table for ionization energy units. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* ionization energy units */
		abbreviation
			TEXT NOT NULL UNIQUE
			/* ionization energy units acronym */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_ms_types
		/* Normalization table for mass spectrometer types. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* type of the mass analyzer */
		acronym
			TEXT NOT NULL UNIQUE
			/* common acronym for the mass spectrometer type */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_ms_n_types
		/* Normalization table for types of ms_n experiments. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* type of the mass analyzer */
		acronym
			TEXT UNIQUE,
			/* common acronym for the mass spectrometer type */
		import_text
		  TEXT UNIQUE
		  /* NTA MRT controlled vocabulary expression */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_fragmentation_types
		/* Normalization table for fragmentation type. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* type of fragmentation */
		acronym
			TEXT NOT NULL UNIQUE
			/* common acronym for the fragmentation type */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_polarity_types
		/* Normalization table for ionization polarity. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* type of the polarity, controlled vocabulary */
		/* Check constraints */
		CHECK (name IN ('negative', 'positive', 'negative/positive'))
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_chromatography_types
		/* Normalization table for chromatography types. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* type of chromatography */
		acronym
			TEXT NOT NULL UNIQUE
			/* common acronym for chromatographic type (e.g. LC, GC) */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_column_chemistries
		/* Normalization table for chromatographic column type. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* column chemistry used */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_column_positions
		/* Normalization table for chromatographic column position */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* column position name */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_duration_units
	  /* Normalization table for mobile phase duration units */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* duration units full text */
		abbreviation
			TEXT NOT NULL UNIQUE
			/* duration units abbreviation */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_additive_units
	  /* Normalization table for mobile phase additive units */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* duration units full text */
		abbreviation
			TEXT NOT NULL UNIQUE
			/* duration units abbreviation */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_flow_units
	  /* Normalization table for mobile phase flow rate units */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* duration units full text */
		abbreviation
			TEXT NOT NULL UNIQUE
			/* duration units abbreviation */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS carrier_mix_collections
		/* An intermediary identification table linking mobile_phases and carrier_mixes */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT
			/* short hand name of the mixture */
		/* Check constraints */
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS mobile_phases
		/* Description of mobile phases used during a chromatographic separation. */
	(
		ms_methods_id
			INTEGER NOT NULL,
			/* foreign key to methods */
		sample_id
			INTEGER,
			/* foreign key to samples */
		peak_id
			INTEGER,
			/* foreign key to peaks */
		carrier_mix_collection_id
			INTEGER NOT NULL,
			/* foreign key to carrier_mixes */
		flow
			REAL,
			/* flow rate of carrier described in carrier_mix_collection_id */
		flow_units
			INTEGER,
			/* foreign key to norm_flow_units */
		duration
			REAL,
			/* time duration mobile phase was applied */
		duration_units
			INTEGER,
			/* foreign key to norm_duration_units */
		/* Check constraints */
		CHECK (duration > 0),
		/* Foreign key relationships */
		FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (sample_id) REFERENCES samples(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (peak_id) REFERENCES peaks(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (carrier_mix_collection_id) REFERENCES carrier_mix_collections(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (flow_units) REFERENCES norm_flow_units(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (duration_units) REFERENCES norm_duration_units(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS ms_descriptions
		/* Full description of all mass spectrometer types used for a given entry in ms_methods. */
	(
		ms_methods_id
			INTEGER NOT NULL,
			/* foreign key to ms_methods */
		ms_types_id
			INTEGER NOT NULL,
			/* foreign key to norm_ms_types */
		vendor_id
			INTEGER NOT NULL,
			/* foreign key to norm_vendors */
		vendor_model
		  TEXT,
		  /* free text entry for mass spectrometry system model */
		reference
		  TEXT,
		  /* resolvable reference for this mass spectrometry model, e.g. a URL to the product */
		/* Check constraints */
		UNIQUE(ms_methods_id, ms_types_id, vendor_id),
		/* Foreign key relationships */
		FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (ms_types_id) REFERENCES norm_ms_types(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (vendor_id) REFERENCES norm_vendors(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS chromatography_descriptions
		/* Full description of all chromatography types used for a given entry in ms_methods. */
	(
		ms_methods_id
			INTEGER NOT NULL,
			/* foreign key to ms_methods */
		chromatography_types_id
			INTEGER NOT NULL,
			/* foreign key to norm_chromatography_types */
		system_vendor_id
		  INTEGER,
		  /* foreign key to norm_vendors */
		system_vendor_model
		  TEXT,
		  /* free text entry for chromatography system model */
		column_chemistry_id
			INTEGER NOT NULL,
			/* foreign key to norm_column_chemistries */
		column_position_id
			INTEGER NOT NULL,
			/* foreign key to norm_column_positions */
		column_vendor_id
			INTEGER NOT NULL,
			/* foreign key to norm_vendors */
		column_vendor_model
		  TEXT,
		  /* free text entry for chromatography column model */
		internal_diameter
		  REAL,
		  /* internal diameter of this column in micrometers */
		column_length
		  REAL,
		  /* column length in centimeters (LC or CE) or meters (GC) */
		particle_diameter
		  REAL,
		  /* diameter of particles for packed columns (applicable to LC or CE columns only) */
		citation
		  TEXT,
		  /* citation for this chromatography method */
		/* Check constraints */
		UNIQUE(ms_methods_id, chromatography_types_id, column_chemistry_id, column_position_id, column_vendor_id),
		/* Foreign key relationships */
		FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (column_chemistry_id) REFERENCES norm_column_chemistries(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (chromatography_types_id) REFERENCES norm_chromatography_types(id) ON UPDATE CASCADE ON DELETE RESTRICT
		FOREIGN KEY (column_position_id) REFERENCES norm_column_positions(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (system_vendor_id) REFERENCES norm_vendors(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (column_vendor_id) REFERENCES norm_vendors(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS ms_methods
		/* Mass spectrometer method settings. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		ionization
			INTEGER,
			/* ionization mode (ESI, APCI, EI, etc.); foreign key to norm_ionization */
		voltage
			REAL,
			/* ionization voltage/current (depending on mode) */
		voltage_units
			INTEGER,
			/* foreign key to norm_voltage_units */
		polarity
			INTEGER NOT NULL,
			/* ionization polarity (negative, positive, or negative/positive); foreign key to norm_polarity */
		ce_value
			TEXT,
			/* value for collision energy, normally a number but can be a range */
		ce_units
			INTEGER NOT NULL,
			/* collision energy units; foreign key to norm_ce_units */
		ce_desc
			INTEGER NOT NULL,
			/* description/context of the collision energy value (normalized, stepped, range, etc.); foreign key to norm_ce_desc */
		fragmentation
			INTEGER NOT NULL,
			/* fragmentation type; foreign key to norm_fragmentation_types */
		ms2_type
			INTEGER,
			/* type of data acquisition for MS2 experiment; foreign key to norm_ms_n_types */
		has_qc_method
			INTEGER NOT NULL, 
			/* constrained to (0, 1) boolean: does the experiment have a QC method in place */
		citation
			TEXT,
			/* citation for the experimental method */
		/* Check constraints */
		CHECK (has_qc_method IN (0, 1))
		/* Foreign key relationships */
		FOREIGN KEY (ionization) REFERENCES norm_ionization(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (voltage_units) REFERENCES norm_voltage_units(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (polarity) REFERENCES norm_polarity_types(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (ce_desc) REFERENCES norm_ce_desc(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (ce_units) REFERENCES norm_ce_units(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (fragmentation) REFERENCES norm_fragmentation_types(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (ms2_type) REFERENCES norm_ms_n_types(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS carrier_aliases
		/* List of common aliases for each entry in TABLE norm_carriers */
	(
		carrier_id
			INTEGER NOT NULL,
			/* foreign key to norm_carriers */
		alias
			TEXT NOT NULL UNIQUE,
			/* human meaningful name(s) associated with a carrier */
		/* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (carrier_id) REFERENCES norm_carriers(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS additive_aliases
		/* List of common aliases for each entry in norm_additives */
	(
		additive_id
			INTEGER NOT NULL,
			/* foreign key to norm_additives */
		alias
			TEXT NOT NULL UNIQUE,
			/* human meaningful name(s) associated with an additive */
		/* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (additive_id) REFERENCES norm_additives(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS carrier_mixes
		/* Mobile phase carrier mixture for a given elution method */
	(
		mix_id
			INTEGER NOT NULL,
			/* mixture identifier to gather discrete components; foreign key to carrier_mix_collections */
		component
			INTEGER NOT NULL,
			/* foreign key to norm_carriers */
		fraction
			REAL,
			/* amount fraction of this carrier in the mixture, constrained from 0 - 1 */
		/* Check constraints */
		CHECK (fraction BETWEEN 0 AND 1),
		/* Foreign key relationships */
		FOREIGN KEY (mix_id) REFERENCES carrier_mix_collections(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (component) REFERENCES norm_carriers(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS carrier_additives
		/* Mobile phase additives mixture for a given carrier mix collection */
	(
		mix_id
			INTEGER NOT NULL,
			/* mixture identifier to gather discrete additives; foreign key to carrier_mix_collections */
		component
			INTEGER NOT NULL,
			/* foreign key to norm_additives */
		amount
			REAL,
			/* amount fraction amount of this carrier in the mixture, contrained from 0 - 1 */
		units
		  INTEGER,
		  /* additive units, foreign key to norm_additive_units */
		/* Check constraints */
		CHECK (amount > 0),
		/* Foreign key relationships */
		FOREIGN KEY (mix_id) REFERENCES carrier_mix_collections(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (component) REFERENCES norm_additives(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (units) REFERENCES norm_additive_units(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
/* Data */
	/*magicsplit*/
	/* Normalization tables should be populated as appropriate for the project. Examples are given in "config/data" directory and may be imported from there or by running "config/demo_data.sql" */
	/*magicsplit*/
/* Views */
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_mass_analyzers AS
		/* View all mass analyzers used in methods */
		SELECT
			msd.ms_methods_id,
				/* mass spectrometric method id */
			ms.acronym,
				/* mass spectrometer acronym */
			ms.name
				/* mass spectrometer type used in this method */
		FROM ms_descriptions msd
		INNER JOIN norm_ms_types ms ON ms.id = msd.ms_types_id;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_chromatography_types AS
		/* View all chromatography types in methods */
		SELECT
			cd.ms_methods_id,
				/* mass spec method id */
			nct.acronym,
				/* chromatographic type acronym */
			nct.name
				/* chromatographic type used in this method */
		FROM chromatography_descriptions cd
		INNER JOIN norm_chromatography_types nct ON nct.id = cd.chromatography_types_id;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_carrier_mix AS
		/* View complete mobile phase used in a mixture */
		SELECT
			sm.mix_id AS "mix_id",
				/* carrier mix id */
			ns.name AS "component",
				/* carrier name */
			CASE WHEN sm.fraction IS NULL THEN "" ELSE sm.fraction * 100 END "amount",
				/* carrier fraction in this mix */
			CASE WHEN sm.fraction IS NULL THEN "" ELSE "percent" END "unit_name",
			  /* full name of carrier amount unit */
			CASE WHEN sm.fraction IS NULL THEN "" ELSE "%" END "unit"
			  /* carrier unit abbreviation */
		FROM carrier_mixes sm
		LEFT JOIN norm_carriers ns ON ns.id = sm.component;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_carrier_additives AS
		/* View complete mobile phase used in a mixture */
		SELECT
			sa.mix_id AS "mix_id",
				/* carrier mix id */
			na.name AS "component",
				/* additive name */
			CASE WHEN sa.amount IS NULL THEN "" ELSE sa.amount END "amount",
				/* additive amount in this mix */
			CASE WHEN sa.amount IS NULL THEN "" ELSE nau.name END "unit_name",
			  /* full name of additive units */
			CASE WHEN sa.amount IS NULL THEN "" ELSE nau.abbreviation END "unit"
			  /* additive units abbreviation */
		FROM carrier_additives sa
		LEFT JOIN norm_additive_units nau ON sa.units = nau.id
		LEFT JOIN norm_additives na ON sa.component = na.id;
	/*magicsplit*/
  CREATE VIEW IF NOT EXISTS view_carrier_mix_collection AS
		/* Tabular view of carrier mix components by mixture ID */
  	SELECT
  	  smc.id,
  	    /* carrier mix collection id */
  	  smc.name,
  	    /* carrier mix user-supplied name */
  	  vst.component,
  	    /* carrier mix component name */
  	  vst.component_type,
  	    /* carrier mix component type, whether a carrier or an additive */
  	  vst.amount,
  	    /* carrier mix component amount */
  	  vst.unit
  	    /* carrier mix component units associated with the amount */
  		FROM carrier_mix_collections smc JOIN
  			(SELECT
  				*,
  				"carrier" AS "component_type"
  				FROM view_carrier_mix vsm
  			UNION
  			SELECT
  				*,
  				"additive" AS "component_type"
  				FROM view_carrier_additives vsa
  			) AS vst
  		ON mix_id = smc.id
  		ORDER BY id, component_type DESC, amount DESC;
  /*magicsplit*/
  CREATE VIEW IF NOT EXISTS view_mobile_phase_narrative AS
    /* A print convenience view creating a narrative from the elution profile of each ms_methods_id, with one row for each profile stage. */
    SELECT
    	mp.ms_methods_id,
    	  /* MS Methods ID, foreign key to ms_methods.id */
    	mp.sample_id,
    	  /* Sample ID, foreign key to samples.id */
		mp.peak_id,
		  /* Peak ID, foreign key to peaks.id */
    	CASE ifnull(carriers, "") WHEN "" THEN "" ELSE carriers END ||
    		CASE ifnull(additives, "") WHEN "" THEN "" ELSE
    			CASE ifnull(carriers, "") WHEN "" THEN "" ELSE " with " END
    			|| additives || " amendment" END ||
    		CASE ifnull(mp.duration, "") WHEN "" THEN "" ELSE " for " || mp.duration END ||
    		CASE ifnull(ndu.abbreviation, "") WHEN "" THEN "" ELSE " " || ndu.abbreviation END ||
    		CASE ifnull(mp.flow, "") WHEN "" THEN "" ELSE " at " || mp.flow END ||
    		CASE ifnull(nfu.abbreviation, "") WHEN "" THEN "" ELSE " " || nfu.abbreviation END
    		AS narrative
    		/* Summary narrative of this elution profile */
  	FROM 
  		mobile_phases mp
  		LEFT JOIN norm_duration_units ndu ON mp.duration_units = ndu.id 
  		LEFT JOIN norm_flow_units nfu ON mp.flow_units = nfu.id
		LEFT JOIN(
			SELECT
				CASE ifnull(mix_id, "") WHEN "" THEN mix2 ELSE mix_id END AS mix_id,
				carriers,
				additives
			FROM
				(SELECT
		  	    	mix_id as mix2,
		  		    REPLACE(REPLACE(group_concat(vsa.component || " (" || vsa.amount || " " || vsa.unit || ")"), ",", " / "), " ( )", "") AS "additives"
		  		    FROM view_carrier_additives vsa
		  	        GROUP BY vsa.mix_id)
		  	    LEFT JOIN
		  	    (SELECT
		  	    	mix_id,
		  		    REPLACE(REPLACE(group_concat(vsm.component || " (" || vsm.amount || " " || vsm.unit || ")"), ",", " / "), " ( )", "") AS "carriers"
		  		    FROM view_carrier_mix vsm    
		  	        GROUP BY vsm.mix_id)
	  	    	ON mix_id = mix2
	  	) ON mp.carrier_mix_collection_id = mix_id;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_detectors AS
		/* Convenience view to build view_method_as by providing a single character string for detectors used in this method */
		SELECT
			msd.ms_methods_id,
				/* ms_descriptions id */
			REPLACE(group_concat(name), ",", " ") AS "detectors"
				/* concatenated list of detectors */
		FROM
			ms_descriptions msd
			JOIN norm_ms_types nmt ON msd.ms_types_id = nmt.id
		GROUP BY msd.ms_methods_id;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_column_chemistries AS
		/* Convenience view to build view_method_as by providing a single character string for column chemistries used in this method */
		SELECT 
			cd.ms_methods_id,
				/* ms_descriptions id */
			REPLACE(group_concat(DISTINCT(ncc.name || " " || ncp.name || " column")), ",", ' with ') AS "columns"
				/* concatenated list of column chemistries used */
		FROM
			chromatography_descriptions cd
			JOIN norm_column_positions ncp ON cd.column_position_id = ncp.id 
			JOIN norm_column_chemistries ncc ON cd.column_chemistry_id = ncc.id
		GROUP BY cd.ms_methods_id;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_separation_types AS
		/* Convenience view to build view_method_as by providing a single character string for chromatography type */
		SELECT
			cd.ms_methods_id,
				/* chromatography_descriptions id */
			REPLACE(GROUP_CONCAT(DISTINCT(nv.name)), ",", " x ") AS "chrom_vendor",
				/* chromatography system vendor */
			REPLACE(GROUP_CONCAT(DISTINCT(cd.system_vendor_model)), ",", " x ") AS "chrom_model",
				/* chromatography system vendor */
			ctv.acronym AS "chrom_type"
				/* chromatography type (e.g. LC, GC, etc.) */
		FROM chromatography_descriptions cd 
		LEFT JOIN norm_chromatography_types ct ON cd.chromatography_types_id = ct.id
		LEFT JOIN(
			SELECT ms_methods_id, replace(GROUP_CONCAT(acronym), ",", " x ") AS "acronym"
			FROM chromatography_descriptions ncd
			LEFT JOIN norm_chromatography_types nct ON ncd.chromatography_types_id = nct.id
			WHERE column_position_id = 2
			GROUP BY ms_methods_id
		) ctv ON cd.ms_methods_id = ctv.ms_methods_id
		LEFT JOIN norm_vendors nv ON cd.system_vendor_id = nv.id
		GROUP BY cd.ms_methods_id;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_method AS
		/* View mass spectrometer information and method settings */
		SELECT
			msm.id,
			/* Method id */
			vst.chrom_vendor AS "chromatography_system_vendor",
			/* Chromatograhic system vendor */
			vst.chrom_model AS "chromatograph_model",
			/* Chromatographic system model */
			vst.chrom_type AS "chromatographic_type",
			/* Chromatographic separation type name */
			nv.name AS "mass_spectrometer_vendor",
			/* Vendor name */
			msd.vendor_model AS "mass_spectrometer_model",
			/* Mass spectrometer system model */
			vd.detectors AS "detector",
			/* Mass spectrometer type */
			vcc.columns AS "columns",
			/* Chromatographic columns used in this method */ 
			nft.acronym AS "fragmentation_acronym",
			/* Mass spectrometer fragmentation type acronym */
			nft.name AS "fragmentation_name",
			/* Mass spectrometer fragmentation type */
			pt.name AS "polarity",
			/* Polarity setting */
			ni.acronym AS "ionization",
			/* Ionization type */
			msm.voltage || " " || niu.name AS "voltage",
			/* Ionization energy */
			msm.ce_value|| " " || ncu.name  AS "collision_energy",
			/* Collision energy in electron volts */
			ncd.name AS "collision_energy_description"
			/* Collision energy description */
		FROM ms_methods msm 
		LEFT JOIN norm_ionization ni ON msm.ionization = ni.id
		LEFT JOIN ms_descriptions msd ON msm.id = msd.ms_methods_id
		LEFT JOIN norm_vendors nv ON msd.vendor_id = nv.id
		LEFT JOIN view_detectors vd ON msm.id = vd.ms_methods_id
		LEFT JOIN norm_polarity_types pt ON pt.id = msm.polarity
		LEFT JOIN chromatography_descriptions cd ON msm.id = cd.ms_methods_id
		LEFT JOIN view_separation_types vst ON msm.id = vst.ms_methods_id
		LEFT JOIN view_column_chemistries vcc ON msm.id = vcc.ms_methods_id
		LEFT JOIN norm_voltage_units niu ON msm.voltage_units = niu.id
		LEFT JOIN norm_ce_units ncu ON msm.ce_units = ncu.id
		LEFT JOIN norm_fragmentation_types nft ON msm.fragmentation = nft.id
		LEFT JOIN norm_ce_desc ncd ON msm.ce_desc = ncd.id
		GROUP BY msm.id;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_method_narrative AS
	  /* Collapses the contents of view_method into a single narrative string by ID */
		SELECT
			id AS "Method ID",
			/* primary key */
			"Measured by " ||
			chromatographic_type ||
				" (" || chromatography_system_vendor || ") " || 
			detector ||
				" MS (" || mass_spectrometer_vendor || "), separated by " ||
			columns ||
				" in " ||
			polarity ||
				" " ||
			ionization ||
				" mode at " ||
			voltage ||
				" and " ||
			collision_energy_description ||
				" fragmentation by " ||
			fragmentation_acronym ||
				" (" || fragmentation_name || ")" ||
				" at " ||
			collision_energy ||
				"."
				AS "Narrative"
			/* narrative string collapsed into readable form from view_method */
		FROM view_method;
	/*magicsplit*/
/* Triggers */
	/*magicsplit*/
	/* none */
