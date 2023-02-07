/*=============================================================================
	Description
		Data node schema definition for the NIST high-resolution
		accurate-mass spectrometry spectral database (HRAM-MS-NTA). This node 
		contains information relevant to analytical results (e.g. descriptions 
		of samples, chromatographic peaks derived from them, resulting mass 
		spectra, and descriptive information to describe those results).
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
			.read config/sql_nodes/data.sql
		
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
	CREATE TABLE IF NOT EXISTS norm_generation_type
		/* Normalization table for fragment generation source type */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL,
			/* one of "in silico" or "empirical" */
		/* Check constraints */
		CHECK (name IN ("in silico", "empirical"))
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_sample_classes
		/* Normalization table linking to samples to hold controlled vocabulary. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* name of the sample class */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_ion_states
		/* Normalization table for the measured ion state as compared with the molecular ion. */
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
	CREATE TABLE IF NOT EXISTS norm_peak_confidence
  	/* Normalization levels for peak identification confidence */
  (
  	id
  		INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  		/* primary key */
  	level1
  		TEXT,
  		/* primary level of confidence */
  	level2
  		TEXT,
  		/* confidence sublevel */
  	confidence
  		TEXT UNIQUE,
  		/* description of the confidence level */
  	import_text
  		TEXT UNIQUE
  		/* customized import format expression of the confidence level */
  	/* Check constraints */
  	/* Foreign key relationships */
  );
  /*magicsplit*/
	CREATE TABLE IF NOT EXISTS sample_aliases
		/* Alternative names by which this sample may be identified e.g. laboratory or repository names, external reference IDs, URIs, etc. */
	(
	  sample_id
	    INTEGER NOT NULL,
	    /* foreign key to samples */
	  alias
	    TEXT NOT NULL,
	    /* reference alias for the sample */
	  reference
	    TEXT,
	    /* source of the name, e.g. external database pointer or PID */
	  /* Check constraints */
		/* Foreign key relationships */
		FOREIGN KEY (sample_id) REFERENCES samples(id) ON UPDATE CASCADE ON DELETE CASCADE
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS samples
		/* Samples from which analytical data are derived. What goes into an analytical instrument. Deleting a contributor from the contributors table will also remove their data from the system. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		mzml_name
		  TEXT NOT NULL,
		  /* user-defined name of mzML file used to represent the sample */
		description
			TEXT NOT NULL,
			/* user-defined description of the sample */
		sample_class_id
			INTEGER NOT NULL,
			/* foreign key to norm_sample_classes */
		source_citation
			TEXT,
			/* citation for the sample source */
		sample_contributor
			TEXT NOT NULL,
			/* generator of data for this sample */
		generation_type
			INTEGER NOT NULL,
			/* data generated by, foreign key to norm_generation_type */
		generated_on
		  TEXT NOT NULL,
		  /* datetime the raw data file was generated in UTC */
		ms_methods_id
			INTEGER,
			/* foreign key to ms_methods */
		sample_solvent
		  INTEGER,
		  /* foreign key to norm_solvents */
		/* Check constraints */
		CHECK (generated_on IS strftime("%Y-%m-%d %H:%M:%S", generated_on)),
		/* Foreign key relationships */
		FOREIGN KEY (sample_class_id) REFERENCES norm_sample_classes(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (sample_contributor) REFERENCES contributors(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (generation_type) REFERENCES norm_generation_type(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (sample_solvent) REFERENCES norm_carriers(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS conversion_software_peaks_linkage
		/* Linkage reference tying peaks with the conversion software settings used to generate them. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		generated_on
			TEXT NOT NULL UNIQUE,
			/* timestamp of the sample generation to tie in with samples */
		/* Check constraints */
		CHECK (generated_on IS strftime("%Y-%m-%d %H:%M:%S", generated_on))
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS conversion_software_settings
		/* Settings specific to the software package used to preprocess raw data. */
	(
		linkage_id
			INTEGER,
			/* foreign key to conversion_software_peaks_linkage */
		setting_value
			TEXT NOT NULL,
			/* value of the software setting */
		/* Check constraints */
		UNIQUE (linkage_id, setting_value),
		/* Foreign key relationships */
		FOREIGN KEY (linkage_id) REFERENCES conversion_software_peaks_linkage(id) ON UPDATE CASCADE ON DELETE CASCADE
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS peaks
		/* Peaks (or features) identified within the results from a sample. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		sample_id
			INTEGER NOT NULL,
			/* foreign key to samples */
		conversion_software_peaks_linkage_id
			INTEGER NOT NULL,
			/* foreign key to conversion_software_peaks_linkage */
		num_points
		  INTEGER NOT NULL,
		  /* number of points collected across this peak */
		precursor_mz
			REAL NOT NULL,
			/* precursor ion mass to charge ratio (constrained to positive numbers) */
		ion_state
			INTEGER NOT NULL,
			/* ion state (e.g. [M]+, [M+H]+, etc.); foreign key to norm_ion_states */
		rt_start
			REAL NOT NULL,
			/* peak retention time start point (constrained to positive numbers) */
		rt_centroid
			REAL NOT NULL,
			/* peak retention time centroid (derived, constrained to positive numbers) */
		rt_end
			REAL NOT NULL,
			/* peak retention time end point (constrained to positive numbers) */
		identification_confidence
		  INTEGER,
		  /* confidence in this peak's identification */
		/* Check constraints */
		CHECK (precursor_mz > 0),
		CHECK (rt_start >= 0),
		CHECK (rt_centroid > 0),
		CHECK (rt_end > 0),
		/* Foreign key relationships */
		FOREIGN KEY (sample_id) REFERENCES samples(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (conversion_software_peaks_linkage_id) REFERENCES conversion_software_peaks_linkage(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (ion_state) REFERENCES norm_ion_states(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (identification_confidence) REFERENCES norm_peak_confidence(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS ms_data
		/* Mass spectral data derived from experiments on a compound by compound basis. Empirical isotopic pattern. */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		peak_id
			INTEGER NOT NULL,
			/* foreign key to peaks */
		ms_n
			INTEGER NOT NULL,
			/* MS type, 1...n */
		scantime
			REAL NOT NULL,
			/* scan time of spectrum */
		base_ion
			REAL,
			/* measured mass to charge ratio of the precursor ion, if 'null' the requested precursor was not found at this scantime */
		base_int
			REAL NOT NULL,
			/* measured intensity of the base_ion, if 0 the requested precursor was not found at this scantime */
		measured_mz
			TEXT NOT NULL,
			/* mass to charge ratios measured in this spectrum */
		measured_intensity
			TEXT NOT NULL,
			/* intensities associated with measured_mz in a 1:1 relationship. if persisted, may be entered into table ms_spectra */
		/* Check constraints */
		CHECK (scantime >= 0),
		CHECK (base_int >= 0),
		CHECK (ms_n > 0 AND ms_n < 9),
		UNIQUE (peak_id, ms_n, scantime, base_ion, base_int, measured_mz, measured_intensity),
		/* Foreign key relationships */
		FOREIGN KEY (peak_id) REFERENCES peaks(id) ON UPDATE CASCADE ON DELETE CASCADE
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS ms_spectra
		/* Retained mass spectra associated with ms1data, unencoded from ms_data.measured_mz and .measured_intensity respectively. */
	(
		ms_data_id
			INTEGER NOT NULL,
			/* foreign key to ms_data */
		mz
			REAL NOT NULL,
			/* mass to charge ratio */
		intensity
			REAL NOT NULL,
			/* signal intensity */
		/* Check constraints */
		CHECK (mz > 0),
		CHECK (intensity >= 0),
		UNIQUE (ms_data_id, mz, intensity),
		/* Foreign key relationships */
		FOREIGN KEY (ms_data_id) REFERENCES ms_data(id) ON UPDATE CASCADE ON DELETE CASCADE
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS qc_data
	  /* Detailed quality control data data as assessed by expert review (long format). */
	(
  	sample_id
			INTEGER NOT NULL,
			/* foreign key to samples */
		parameter
		  TEXT NOT NULL,
		  /* QC parameter class */
		name
		  TEXT NOT NULL,
		  /* QC aspect, e.g. "reportedformula") */
		value
		  TEXT NOT NULL,
		  /* Value associated with parameter and name */
		/* Check constraints */
		UNIQUE (sample_id, parameter, name),
		/* Foreign key relationships */
		FOREIGN KEY (sample_id) REFERENCES samples(id) ON UPDATE CASCADE ON DELETE CASCADE
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS opt_ums_params
	  /* table of optimal parameters for uncertainty mass spectra */
	(
  	peak_id
  	  INTEGER NOT NULL,
  	  /* foreign key to peaks(id) */
  	mslevel
  	  INTEGER NOT NULL,
  	  /* mslevel for the optimal parameters */
  	correl
  	  REAL,
  	  /* optimal correlation limit setting */
  	ph
  	  REAL,
  	  /* optimal peak height setting */
  	freq
  	  REAL,
  	  /* optimal observational frequency setting */
  	n
  	  INTEGER,
  	  /* number of scans using optimal settings */
  	masserror
  	  REAL,
  	  /* mass error setting used for optimal settings */
  	minerror
  	  REAL,
  	  /* minimum mass error setting used for optimal settings */
  	/* Check constraints */
  	 UNIQUE (peak_id, mslevel),
  	/* Foreign key relationships */
  	FOREIGN KEY (peak_id) REFERENCES peaks(id) ON UPDATE CASCADE ON DELETE CASCADE
	);
	/*magicsplit*/
/* Views */
	/*magicsplit*/
  CREATE VIEW IF NOT EXISTS view_samples AS 
  		/* View of "samples" normalized by "norm_sample_classes", "norm_generation_type", and "norm_carriers". */ 
  	SELECT 	
  		s.id AS id, 
  			/* Direct use column 'id' from table 'samples'. */ 	
  		s.mzml_name AS mzml_name, 
  			/* Direct use column 'mzml_name' from table 'samples'. */ 	
  		s.description AS description, 
  			/* Direct use column 'description' from table 'samples'. */ 	
  		nsc.name AS sample_class_id, 
  			/* Normalized value column 'name' from table 'nsc'. */ 	
  		s.source_citation AS source_citation, 
  			/* Direct use column 'source_citation' from table 'samples'. */ 	
  		s.sample_contributor AS sample_contributor, 
  			/* Direct use column 'sample_contributor' from table 'samples'. */ 	
  		ngt.name AS generation_type, 
  			/* Normalized value column 'name' from table 'norm_generation_type'. */ 	
  		s.generated_on AS generated_on, 
  			/* Direct use column 'generated_on' from table 'samples'. */ 	
  		s.ms_methods_id AS ms_methods_id, 
  			/* Direct use column 'ms_methods_id' from table 'samples'. */ 	
  		nc.name AS sample_solvent
  			/* Normalized value column 'name' from table 'norm_carriers'. */ 
  	FROM samples s
  	LEFT JOIN norm_sample_classes nsc ON s.sample_class_id = nsc.id 
  	LEFT JOIN norm_generation_type ngt ON s.generation_type = ngt.id 
  	LEFT JOIN norm_carriers nc ON s.sample_solvent = nc.id;
	/*magicsplit*/
  CREATE VIEW IF NOT EXISTS view_sample_narrative AS 
	  /* Collapses the contents of view_samples and view_contributors into a single narrative string by ID */
  	SELECT 
  		vs.id AS "Sample ID",
  		 /* Sample PK ID */
  		"These " ||
  		vs.generation_type ||
  		" data from " ||
  		CASE
  			WHEN substr(vs.sample_class_id, 1, 1) IN ('a','e','i','o','u')
  				THEN "an "
  			ELSE "a "
  		END ||
  		vs.sample_class_id ||
  		" in " ||
  		vs.sample_solvent ||
  		" were provided by " ||
  		vc.name ||
  		" (" ||
  		vc.contact ||
  		" - " ||
  		vc.pid_url ||
  		") of " ||
  		vc.affiliation ||
  		' and described as "' ||
  		vs.description ||
  		'" in file "' ||
  		vs.mzml_name ||
  		'"' ||
  		CASE 
  			WHEN source_citation IS NULL 
  				THEN ""
  			ELSE source_citation
  		END ||
  		". Data were generated on " ||
  		vs.generated_on ||
  		" using the mass spectrometry method ID " ||
  		vs.ms_methods_id ||
  		"."
  			AS "Narrative"
  		/* narrative string collapsed into readable form from view_samples and view_contributors */
  	FROM view_samples vs
  	LEFT JOIN view_contributors vc ON vs.sample_contributor = vc.id;
	/*magicsplit*/
  CREATE VIEW IF NOT EXISTS view_peaks AS 
  		/* View of "peaks" with text values displayed from normalization tables. */ 
  	SELECT 	
  		p.id AS id, 
  			/* Direct use column 'id' from table 'p'. */ 	
  		p.sample_id AS sample_id, 
  			/* Direct use column 'sample_id' from table 'p'. */ 	
  		p.num_points AS num_points, 
  			/* Direct use column 'num_points' from table 'p'. */ 	
  		p.precursor_mz AS precursor_mz, 
  			/* Direct use column 'precursor_mz' from table 'p'. */ 	
  		nis.name AS ion_state, 
  			/* Normalized value column 'name' from table 'nis'. */ 	
  		p.rt_start AS rt_start, 
  			/* Direct use column 'rt_start' from table 'p'. */ 	
  		p.rt_centroid AS rt_centroid, 
  			/* Direct use column 'rt_centroid' from table 'p'. */ 	
  		p.rt_end AS rt_end, 
  			/* Direct use column 'rt_end' from table 'p'. */ 	
  		iif(npc.level1 = "",
  		  confidence,
  		  "Level " ||
  		  npc.level1 ||
  		  npc.level2 ||
  		  " - " ||
  		  upper(substr(npc.confidence, 1, 1)) ||
  		  lower(substr(npc.confidence, 2))
  		  )
  		  AS confidence
  			/* Narrative form of confidence from table 'norm_peak_confidence'. */ 
  	FROM peaks p
  	LEFT JOIN norm_ion_states nis ON p.ion_state = nis.id 
  	LEFT JOIN norm_peak_confidence npc ON p.identification_confidence = npc.id;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS peak_data AS
		/* View raw peak data for a specific peak */
		SELECT
			p.id AS peak_id,
				/* internal peak id */
			msd.id AS ms_data_id,
				/* internal id of ms_data */
			"MS" || msd.ms_n as ms_n,
			  /* mass spectral layer, e.g. MS1, MS2, ... MSn */
			p.precursor_mz,
				/* peak precursor ion */
			msd.base_int,
				/* measured mass of precursor_mz */
			msd.scantime,
				/* ms scantime for this spectrum */
			msd.measured_mz,
				/* mass to charge ratios */
			msd.measured_intensity
				/* measured signal intensities */
		FROM ms_data msd
		INNER JOIN peaks p ON msd.peak_id = p.id;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS peak_spectra AS
		/* View archived and verified peak spectra for a specific peak */
		SELECT
			pd.peak_id,
				/* internal peak id */
			pd.precursor_mz,
				/* peak precursor ion */
			pd.scantime,
				/* ms scantime for this spectrum */
			mss.mz,
				/* mass to charge ratio */
			mss.intensity
				/* measured signal intensity */
		FROM peak_data pd
		INNER JOIN ms_spectra mss ON pd.ms_data_id = mss.ms_data_id;
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_masserror AS 
    /* Get the mass error information for all peaks */
    SELECT 
      p.id AS peak_id, 
      /* Foreign key to peaks.id */
      s.id AS sample_id, 
      /* Foreign key to samples.id */
      p.precursor_mz AS precursor_mz, 
      /* Precursor mass of the peak */
      qcd.value
      /* msaccuracy value from qc_data */
    FROM qc_data qcd
    INNER JOIN samples s ON qcd.sample_id = s.id
    INNER JOIN peaks p ON s.id = p.sample_id
    WHERE qcd.name = "msaccuracy"
    GROUP BY p.id;
	/*magicsplit*/
/* Triggers */
	/*magicsplit*/
	/* none */
