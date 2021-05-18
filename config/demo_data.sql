/*
	Run this file in sqlite3 to load demonstration data into the database schema using:
		.read demo_data.sql
	Note this will force the current database mode to csv. If you use another mode, change back afterward.
*/

.import solvents.csv solvents --csv
.import solvent_aliases.csv solvent_aliases --csv
