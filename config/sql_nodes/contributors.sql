/*=============================================================================
	Description
		Contributors node schema definition for the NIST high-resolution
		accurate-mass spectrometry spectral database (HRAM-MS-NTA). This node 
		contains information relevant to identifying contributors, similar to 
		a "users" table.
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
			.read config/sql_nodes/contributors.sql
		
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
	CREATE TABLE IF NOT EXISTS contributors
		/* Contact information for individuals contributing data to this database */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		username
			TEXT NOT NULL UNIQUE,
			/* verified username */
		contact
			TEXT,
			/* contact information, preferred as an email address, but is not restricted */
		first_name
			TEXT,
			/* user's preferred first name */
		last_name
			TEXT,
			/* user's preferred last name */
		affiliation
			INTEGER NOT NULL,
			/* user's professional affiliation, foreign key to affiliations */
		orcid
			TEXT DEFAULT 'null',
			/* user's ORCID number, if available */
		/* Check constraints */
		CHECK (orcid
			GLOB('[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9X]')
			OR (orcid in ('NULL', 'null', 'NA', 'na', ''))
		),
		/* Foreign key relationships */
		FOREIGN KEY (affiliation) REFERENCES affiliations(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS affiliations
		/* Normalization table for contributor.affiliation */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* name of professional affiliation */
		PID
		  TEXT
		  /* URL resolvable persistent identifier for affiliation organization, typically a ROR (https://ror.org/) or GRID (https://grid.ac/) identifier */
	);
		/* Check constraints */
		/* Foreign key relationships */
	/*magicsplit*/
/* Views */
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_contributors AS
		/* Readable version of the contributors table that can be expanded with counts of contributions from various places. */
		SELECT 
			c.id,
				/* contributor primary key */
			c.first_name || " " || c.last_name AS name,
				/* concatenation of contributors.first_name and .last_name fields */
			a.name AS affiliation,
				/* contributor affiliation */
			c.contact,
				/* contributor contact information */
			case
				when c.orcid is null then a.PID
				else ("https://orcid.org/" || c.orcid)
			end AS pid_url,
				/* contributors.orcid or affiliations.PID as a hyperlink to their persistent identifier page */
			count(s.sample_contributor) as samples_contributed
				/* number of samples provided by this contributor */
		FROM contributors c
		JOIN affiliations a
			ON c.affiliation = a.id
		JOIN samples s
			ON c.id = s.sample_contributor
		WHERE NOT c.id = 1
		GROUP BY c.id;
	/*magicsplit*/
/* Triggers */
	/*magicsplit*/
	CREATE TRIGGER IF NOT EXISTS new_contributor
		/* When creating a new contributor, redirect to allow using affiliations(name) instead of affiliations(id), but still allow for direct use of affiliations(id). */
		AFTER INSERT ON contributors
		WHEN NEW.affiliation NOT IN (SELECT id FROM affiliations)
		BEGIN
			INSERT OR IGNORE INTO affiliations (name)
				VALUES (NEW.affiliation);
			UPDATE OR IGNORE contributors
				SET affiliation = (SELECT id FROM affiliations WHERE UPPER(name) = UPPER(NEW.affiliation))
				WHERE ROWID = NEW.ROWID;
		END;
	/*magicsplit*/
	CREATE TRIGGER IF NOT EXISTS ensure_null_orcid
		/* When creating a new contributor, ensure allowed nullable ORCID values are stored as NULL. */
		AFTER INSERT ON contributors
		WHEN NEW.orcid IN ("NA", "", "NULL", "null", "na")
		BEGIN
			UPDATE contributors 
				SET orcid = NULL
				WHERE ROWID = NEW.ROWID;
		END;
	/*magicsplit*/
	CREATE TRIGGER IF NOT EXISTS affiliation_update
		/* When updating a contributor's affiliation with an affiliation that does not currently exist, add the affiliation and update the id appropriately, but still allow for direct use of affiliations(id). */
		AFTER UPDATE ON contributors
		WHEN NEW.affiliation NOT IN (SELECT id FROM affiliations)
		BEGIN
			INSERT OR IGNORE INTO affiliations (name)
				VALUES (NEW.affiliation);
			UPDATE contributors
				SET affiliation = (SELECT id FROM affiliations WHERE name = NEW.affiliation)
				WHERE ROWID = NEW.ROWID;
		END;
	/*magicsplit*/
