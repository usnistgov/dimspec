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

	CREATE TABLE IF NOT EXISTS contributors
		/* Placeholder for contributors */
	(
		id
			INTEGER PRIMARY KEY,
			/* Primary key */
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
			TEXT,
			/* user's ORCID number, if available */
		orcid_url
			TEXT GENERATED ALWAYS AS ("https://orcid.org/" || orcid) VIRTUAL,
			/* calculated column to provide a link to a user's ORCID id profile */
		/* Check constraints */
		CHECK (orcid GLOB('[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]')),
			/* Ensures ORCID follows formatting requirement as of 2021-06-07 */
		CHECK (length(contact) = length(replace(replace(replace(replace(replace(replace(contact, ";", ""), "delete", ""), "select", ""), "alter", ""), "drop", ""), "update", ""))),
		/* Foreign key relationships */
		FOREIGN KEY (affiliation) REFERENCES affiliations(id) ON UPDATE CASCADE
	); /*magicsplit*/

	CREATE TABLE IF NOT EXISTS affiliations
		/* Normalization table for user affiliations */
	(
		id
			INTEGER PRIMARY KEY,
			/* Primary key */
		name
			TEXT NOT NULL UNIQUE
			/* name of professional affiliation */
	); /*magicsplit*/

	/* Views */
		
	CREATE VIEW IF NOT EXISTS view_contributors AS
		/* Readable version of the contributors table that can be expanded with counts of contributions from various places. */
		SELECT 
			c.first_name || " " || c.last_name AS name,
			a.name AS affiliation,
			c.orcid_url AS ORCID
		FROM contributors c
		JOIN affiliations a
		ON c.affiliation = a.id
		WHERE NOT c.id = 1; /*magicsplit*/

/* Triggers */
		
	CREATE TRIGGER IF NOT EXISTS new_contributor
		/* When creating a new contributor, redirect to allow using affiliations(name) instead of affiliations(id), but still allow for direct use of affiliations(id). */
		AFTER INSERT ON contributors
		WHEN NEW.affiliation NOT IN (SELECT id FROM affiliations)
	BEGIN
		INSERT OR IGNORE INTO affiliations (name)
			VALUES (NEW.affiliation);
		UPDATE OR IGNORE contributors
			SET affiliation = (SELECT id FROM affiliations WHERE name = NEW.affiliation)
			WHERE ROWID = NEW.ROWID;
	END; /*magicsplit*/
