/*====================================================================================================
Description:	Analytical results (data) node for NIST high-resolution-accurate-mass spectrometric
				database for non-target analysis (HRAM-NTA).
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
					.read config/sql_nodes/data.sql
				
Details:		Node build files are located in the "config/sql_nodes" directory and serve to allow
				for modular construction and reuse. Local paths will need to be referenced 
				appropriately, which may require modifications to scripts referencing this script.
				
				The comment "magicsplit" is present to provide a hook for external processing,
				allowing for direct building via R or Python when the CLI is unavailable. 
				
====================================================================================================*/

/* Tables */
	
	CREATE TABLE IF NOT EXISTS norm_sample_classes
		/* Normalization table linking to samples to hold controlled vocabulary. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* name of the sample class */
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS samples
		/* Samples from which analytical data are derived. What goes into an analytical instrument. Deleting a contributor from the contributors table will also remove their data from the system. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL,
			/* user-defined name of the sample */
		sample_class_id
			INTEGER NOT NULL,
			/* foreign key to norm_sample_classes */
		source_citation
			TEXT,
			/* citation for the sample source */
		sample_contributor
			TEXT NOT NULL,
			/* generator of data for this sample */
		generated_on
		  TEXT NOT NULL,
		  /* datetime the raw data file was generated in UTC */
		software_conversion_settings_id
			INTEGER,
			/* settings for the msconvert program used to generate data from this sample */
		ms_methods_id
			INTEGER,
			/* foreign key to methods */
		/* Check constraints */
		CHECK (generated_on == strftime("%Y-%m-%d %H:%M:%S", generated_on))
		/* Foreign key relationships */
		FOREIGN KEY (sample_class_id) REFERENCES norm_sample_classes(id) ON UPDATE CASCADE,
		FOREIGN KEY (ms_methods_id) REFERENCES ms_methods(id) ON UPDATE CASCADE,
		FOREIGN KEY (sample_contributor) REFERENCES contributors(id) ON UPDATE CASCADE ON DELETE CASCADE,
		FOREIGN KEY (software_conversion_settings_id) REFERENCES conversion_software_linkage(id) ON UPDATE CASCADE
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS peaks
		/* Peaks (or features) identified within the results from a sample. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		sample_id
			INTEGER NOT NULL,
			/* foreign key to samples */
		precursor_mz
			REAL NOT NULL,
			/* precursor ion mass to charge ratio (constrained to positive numbers) */
		charge
			INTEGER NOT NULL,
			/* ion charge state (constrained to -1 ["negative"] or 1 ["positive"]) */
		rt_start
			REAL NOT NULL,
			/* peak retention time start point (constrained to positive numbers) */
		rt_centroid
			REAL NOT NULL,
			/* peak retention time centroid (derived, constrained to positive numbers) */
		rt_end
			REAL NOT NULL,
			/* peak retention time end point (constrained to positive numbers) */
		/* Check constraints */
		CHECK (precursor_mz > 0),
		CHECK (charge IN (-1, 1)),
		CHECK (rt_start >= 0),
		CHECK (rt_centroid > 0),
		CHECK (rt_end > 0),
		/* Foreign key relationships */
		FOREIGN KEY (sample_id) REFERENCES samples(id) ON UPDATE CASCADE
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS ms_data
		/* Mass spectral data derived from experiments on a compound by compound basis. Emperical isotopic pattern. */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
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
		/* Foreign key relationships */
		FOREIGN KEY (peak_id) REFERENCES peaks(id) ON UPDATE CASCADE
	);
	/*magicsplit*/

	CREATE TABLE IF NOT EXISTS ms_spectra
		/* Retained mass spectra associated with ms1data, unencoded from ms_data.measured_mz and .measured_intensity respectively. */
	(
		ms_data_id
			INTEGER NOT NULL,
			/* foreign key to ms1data */
		mz
			REAL NOT NULL,
			/* mass to charge ratio */
		intensity
			REAL NOT NULL,
			/* signal intensity */
		/* Check constraints */
		CHECK (mz > 0),
		CHECK (intensity >= 0),
		/* Foreign key relationships */
		FOREIGN KEY (ms_data_id) REFERENCES ms_data(id)
	);
	/*magicsplit*/

/* Views */

	CREATE VIEW IF NOT EXISTS peak_data AS
		/* View raw peak data for a specific peak */
		SELECT
			p.id AS peak_id,
				/* internal peak id */
			msd.id AS id,
				/* internal id of ms_data */
			p.precursor_mz,
				/* peak precursor ion */
			msd.base_int,
				/* measured mass of precursor_mz */
			msd.scantime,
				/* ms scantime for this spectrum */
			msd.measured_mz AS m_z,
				/* mass to charge ratios */
			msd.measured_intensity AS intensity
				/* measured signal intensities */
		FROM ms_data msd
		INNER JOIN peaks p ON msd.peak_id = p.id;

	CREATE VIEW IF NOT EXISTS peak_spectra AS
		/* View archived and verified peak spectra for a specific peak */
		SELECT
			ps.peak_id,
				/* internal peak id */
			ps.precursor_mz,
				/* peak precursor ion */
			ps.scantime,
				/* ms scantime for this spectrum */
			mss.mz,
				/* mass to charge ratio */
			mss.intensity
				/* measured signal intensity */
		FROM peak_data ps
		INNER JOIN ms_spectra mss ON ps.id = mss.ms_data_id;

/* Triggers */
