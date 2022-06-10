/*=============================================================================
	Description
		Clear and populate data for poly- and perfluoroalkyl substances (PFAS) 
		to support non-targeted analysis (NTA) high-resolution accurate-mass 
		mass spectrometry (HRAM-MS) experiments using the schema defined in 
		this directory.
		
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
		sqlite3 (https://sqlite.org/version3.html)
		
	Usage
		Run this file in sqlite3 to populate data into the database using:
		
			.read config/populate_pfas.sql
		
		Note this will first remove all data within the referenced tables!
		You may also directly import these files into the appropriate 
		tables using your SQLite IDE of choice.
		
	Details
		Data files are located in the "config/data/pfas" directory and 
		local paths will need to be referenced appropriately, which may
		require modifications to this script; CSVs are named for their 
		associated tables. The OS file separator is assumed to be the 
		"forward slash" ("/") character; if this is inappropriate for 
		your OS, you should globally replace it within this script as
		appropriate.
		
=============================================================================*/

/* To build in console, use each of the following as needed. */

DELETE FROM carrier_mixes;
DELETE FROM carrier_additives;
DELETE FROM carrier_mix_collections;
DELETE FROM carrier_aliases;
DELETE FROM additive_aliases;
DELETE FROM mobile_phases;
DELETE FROM norm_carriers;
DELETE FROM norm_additives;
DELETE FROM norm_sample_classes;
DELETE FROM instrument_properties;
DELETE FROM ms_methods;
DELETE FROM ms_descriptions;
.read config/populate_common.sql
DELETE FROM contributors;
DELETE FROM affiliations;
DELETE FROM chromatography_descriptions;
DELETE FROM compound_categories;
DELETE FROM compound_aliases;
DELETE FROM compounds;
DELETE FROM conversion_software_settings;
DELETE FROM conversion_software_peaks_linkage;
DELETE FROM norm_peak_confidence;
DELETE FROM samples;
DELETE FROM norm_fragments;
INSERT INTO norm_analyte_alias_references (name, description, reference) VALUES ("NIST Suspect List", "Identifying number in the NIST Suspect List", "https://data.nist.gov/od/id/mds2-2387"), ("XICLISTID", "ID number for the original XIC List", "https://data.nist.gov/od/id/mds2-2387");
.import --csv --skip 1 config/data/pfas/norm_carriers.csv norm_carriers
.import --csv --skip 1 config/data/pfas/norm_additives.csv norm_additives
.import --csv --skip 1 config/data/pfas/norm_sample_classes.csv norm_sample_classes
.import --csv --skip 1 config/data/pfas/ms_methods.csv ms_methods
.import --csv --skip 1 config/data/pfas/instrument_properties.csv instrument_properties
.import --csv --skip 1 config/data/pfas/ms_descriptions.csv ms_descriptions
.import --csv --skip 1 config/data/pfas/chromatography_descriptions.csv chromatography_descriptions
.import --csv --skip 1 config/data/pfas/affiliations.csv affiliations
.import --csv --skip 1 config/data/pfas/contributors.csv contributors
.import --csv --skip 1 config/data/pfas/carrier_mix_collections.csv carrier_mix_collections
.import --csv --skip 1 config/data/pfas/carrier_aliases.csv carrier_aliases
.import --csv --skip 1 config/data/pfas/additive_aliases.csv additive_aliases
.import --csv --skip 1 config/data/pfas/carrier_mixes.csv carrier_mixes
.import --csv --skip 1 config/data/pfas/carrier_additives.csv carrier_additives
.import --csv --skip 1 config/data/pfas/compound_categories.csv compound_categories
.import --csv --skip 1 config/data/pfas/compounds.csv compounds
.import --csv --skip 1 config/data/pfas/compound_aliases.csv compound_aliases
.import --csv --skip 1 config/data/pfas/conversion_software_peaks_linkage.csv conversion_software_peaks_linkage
.import --csv --skip 1 config/data/pfas/conversion_software_settings.csv conversion_software_settings
.import --csv --skip 1 config/data/pfas/samples.csv samples
.import --csv --skip 1 config/data/pfas/mobile_phases.csv mobile_phases
.import --csv --skip 1 config/data/pfas/norm_peak_confidence.csv norm_peak_confidence
.import --csv --skip 1 config/data/pfas/norm_fragments.csv norm_fragments
.import --csv --skip 1 config/data/pfas/fragment_aliases.csv fragment_aliases
.import --csv --skip 1 config/data/pfas/annotated_fragments.csv annotated_fragments
