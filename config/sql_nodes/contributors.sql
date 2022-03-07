/*====================================================================================================
Description:	Contributors node for NIST high-resolution-accurate-mass spectrometric database for 
				non-target analysis (HRAM-NTA).
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
					.read config/sql_nodes/contributors.sql
				
				Node build files are located in the "config/sql_nodes" directory and serve to allow
				for modular construction and reuse. Local paths will need to be referenced 
				appropriately, which may require modifications to scripts referencing this script. 
				
====================================================================================================*/

/* Tables */
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS contributors
		/* Contact information for individuals contributing data to this database */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
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
		)
		CHECK (length(contact) = length(replace(replace(replace(replace(replace(replace(contact, ";", ""), "delete", ""), "select", ""), "alter", ""), "drop", ""), "update", ""))),
		CHECK (length(first_name) = length(replace(replace(replace(replace(replace(replace(first_name, ";", ""), "delete", ""), "select", ""), "alter", ""), "drop", ""), "update", ""))),
		CHECK (length(last_name) = length(replace(replace(replace(replace(replace(replace(last_name, ";", ""), "delete", ""), "select", ""), "alter", ""), "drop", ""), "update", ""))),
		CHECK (length(username) = length(replace(replace(replace(replace(replace(replace(username, ";", ""), "delete", ""), "select", ""), "alter", ""), "drop", ""), "update", ""))),
		/* Foreign key relationships */
		FOREIGN KEY (affiliation) REFERENCES affiliations(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS affiliations
		/* Normalization table for contributor.affiliation */
	(
		id
			INTEGER PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE
			/* name of professional affiliation */
	);
		/* Check constraints */
		/* Foreign key relationships */
	/*magicsplit*/
/* Views */
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS view_contributors AS
		/* Readable version of the contributors table that can be expanded with counts of contributions from various places. */
		SELECT 
			c.first_name || " " || c.last_name AS name,
				/* concatenation of contributors.first_name and .last_name fields */
			a.name AS affiliation,
				/* contributor affiliation */
			c.contact,
				/* contributor contact information */
			("https://orcid.org/" || c.orcid) AS orcid_url,
				/* contributors.orcid as a hyperlink to their ORCID page */
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
