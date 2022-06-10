/*=============================================================================
	Description
		[OPTIONAL] Logging node schema definition for the NIST high-resolution 
		accurate-mass spectrometry spectral database (HRAM-MS-NTA). This node 
		contains logs as defined by any added triggers, applications, or other 
		avenues as well as a version history for this database. Its primary 
		purpose is to support application development and provide user support 
		for longer running implementations. It is not used as of v0.9 of the 
		NIST HRMS-MS database for NTA.
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
			.read config/sql_nodes/logging.sql
		
	Details
		Node build files are located in the "config/sql_nodes" directory and 
		serve to allow for modular construction and reuse. Local paths will 
		need to be referenced appropriately, which may require modifications 
		to scripts referencing this script.
		
		The comment "magicsplit" is present to provide a hook for external 
		processing,	allowing for direct building via R or Python when the CLI 
		is unavailable. 
		
		Data are added here as a convenience for the normalization tables 
		"norm_log_executed_from" and "norm_log_effect" rather than imported.
		
=============================================================================*/

/* Tables */
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_log_executed_from
		/* Normalization table for logs(executed_from) */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* plain text source for this log entry */
		/* Check constraints */
		CHECK (name IN ("trigger", "application", "console", "script", "other"))
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS norm_log_effect
		/* Normalization table for logs(effect) */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		name
			TEXT NOT NULL UNIQUE,
			/* plain text of the type of database action */
		/* Check constraints */
		CHECK (name IN ("INSERT", "UPDATE", "DELETE", "schema", "insert", "update", "delete"))
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS version_history
		/* Versions of this database and its associated data or application */
	(
		version
			REAL NOT NULL,
			/* version number */
		affects
			TEXT NOT NULL,
			/* which aspect is affected, constrained to one of "data", "schema", or "application" */
		description
			TEXT NOT NULL,
			/* plain text description of the change */
		active_as_of
			TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
			/* timestamp for when the action was executed */
		/* Check constraints */
		CHECK (affects IN ("data", "schema", "application")),
		CHECK (active_as_of == strftime("%Y-%m-%d %H:%M:%S", active_as_of))
		/* Foreign key relationships */
	);
	/*magicsplit*/
	CREATE TABLE IF NOT EXISTS logs
		/* Placeholder for logs */
	(
		id
			INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
			/* primary key */
		category
			TEXT NOT NULL,
			/* categorical grouping of the action */
		bundle
			INTEGER NOT NULL,
			/* bundle id for bulk actions */
		description
			TEXT,
			/* plain text description if any */
		effect
			INTEGER NOT NULL,
			/* foreign key to norm_log_effect as the type of database update */
		affects_table
			TEXT NOT NULL,
			/* table reference name for the table that was changed, not constrained */
		affects_ids
			INTEGER,
			/* id associated with records changed or updated */
		executed_by
			INTEGER NOT NULL DEFAULT 1,
			/* foreign key to contributors */
		executed_from
			INTEGER NOT NULL DEFAULT 1,
			/* constraint list as one of "console", "script", "trigger", "application", or "other" */
		executed_on
			TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
			/* timestamp for when the action was executed */
		new_vals
			TEXT,
			/* new values, if any */
		old_vals
			TEXT,
			/* prior values, if any */
		/* Check constraints */
		CHECK (executed_on==strftime("%Y-%m-%d %H:%M:%S", executed_on)),
		/* Foreign key relationships */
		FOREIGN KEY (effect) REFERENCES norm_log_effect(id) ON UPDATE CASCADE ON DELETE RESTRICT,
		FOREIGN KEY (executed_from) REFERENCES norm_log_executed_from(id) ON UPDATE CASCADE ON DELETE RESTRICT
	);
	/*magicsplit*/
/* Data */
	/*magicsplit*/
	INSERT OR IGNORE INTO norm_log_executed_from
		VALUES
			(1, "console"),
			(2, "script"),
			(3, "trigger"),
			(4, "application"),
			(5, "other");
	/*magicsplit*/
	INSERT OR IGNORE INTO norm_log_effect
		VALUES
			(1, "INSERT"),
			(2, "UPDATE"),
			(3, "DELETE"),
			(4, "schema");
	/*magicsplit*/
/* Views */
	/*magicsplit*/
	CREATE VIEW IF NOT EXISTS log_summaries AS
		/* Human readable logs */
		SELECT
			l.category,
				/* database action category */
			l.description,
				/* database log description */
			nle.name AS action,
				/* normalized database action name */
			count(l.affects_ids) AS rows_affected,
				/* number of rows affected by this action */
			l.affects_table,
				/* table affected by this action */
			strftime("%Y-%m-%d", l.executed_on) AS on_day
				/* date on which this action was executed */
		FROM logs l
		INNER JOIN norm_log_effect nle ON l.effect = nle.id
		GROUP BY affects_table, on_day;
	/*magicsplit*/
/* Triggers */
	/*magicsplit*/
	/* none */
