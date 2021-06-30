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
			INTEGER PRIMARY KEY,
		name
			TEXT NOT NULL UNIQUE
			/* name of the sample class */
	);
	/*magicsplit*/
	
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

/* Views */

/* Triggers */
