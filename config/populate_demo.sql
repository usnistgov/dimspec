/*=============================================================================
	Description
		Clear and populate a demonstration version of data to support 
		non-targeted analysis (NTA) high-resolution accurate-mass mass 
		spectrometry (HRAM-MS) experiments using the schema defined in 
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
		Run this file in sqlite3 to populate example data into the 
		database using:
		
			.read config/populate_demo.sql
		
		Note this will first remove all data within the referenced tables!
		You may also directly import these files into the appropriate 
		tables using your SQLite IDE of choice.
		
	Details
		Data files are located in the "config/data/demo" directory and 
		local paths will need to be referenced appropriately, which may
		require modifications to this script; CSVs are named for their 
		associated tables. The OS file separator is assumed to be the 
		"forward slash" ("/") character; if this is inappropriate for 
		your OS, you should globally replace it within this script as
		appropriate.
		
=============================================================================*/

/* To build in console, use each of the following as needed. */

DELETE FROM carrier_mixes;
DELETE FROM carrier_mix_collections;
DELETE FROM carrier_additives;
DELETE FROM carrier_aliases;
DELETE FROM additive_aliases;
DELETE FROM mobile_phases;
DELETE FROM norm_carriers;
DELETE FROM norm_additives;
DELETE FROM norm_peak_confidence;
DELETE FROM samples;
DELETE FROM norm_sample_classes;
DELETE FROM instrument_properties;
DELETE FROM ms_methods;
DELETE FROM ms_descriptions;
.read config/populate_common.sql
DELETE FROM contributors;
DELETE FROM affiliations;
DELETE FROM chromatography_descriptions;
DELETE FROM compounds;
DELETE FROM compound_categories;
DELETE FROM compound_aliases;
DELETE FROM conversion_software_settings;
DELETE FROM conversion_software_peaks_linkage;
.import --csv --skip 1 config/data/demo/norm_carriers.csv norm_carriers
.import --csv --skip 1 config/data/demo/norm_additives.csv norm_additives
.import --csv --skip 1 config/data/demo/norm_sample_classes.csv norm_sample_classes
.import --csv --skip 1 config/data/demo/ms_methods.csv ms_methods
.import --csv --skip 1 config/data/demo/instrument_properties.csv instrument_properties
.import --csv --skip 1 config/data/demo/ms_descriptions.csv ms_descriptions
.import --csv --skip 1 config/data/demo/chromatography_descriptions.csv chromatography_descriptions
.import --csv --skip 1 config/data/demo/affiliations.csv affiliations
.import --csv --skip 1 config/data/demo/contributors.csv contributors
.import --csv --skip 1 config/data/demo/carrier_mix_collections.csv carrier_mix_collections
.import --csv --skip 1 config/data/demo/carrier_aliases.csv carrier_aliases
.import --csv --skip 1 config/data/demo/additive_aliases.csv additive_aliases
.import --csv --skip 1 config/data/demo/carrier_mixes.csv carrier_mixes
.import --csv --skip 1 config/data/demo/carrier_additives.csv carrier_additives
.import --csv --skip 1 config/data/demo/compound_categories.csv compound_categories
.import --csv --skip 1 config/data/demo/compounds.csv compounds
.import --csv --skip 1 config/data/demo/compound_aliases.csv compound_aliases
.import --csv --skip 1 config/data/demo/conversion_software_peaks_linkage.csv conversion_software_peaks_linkage
.import --csv --skip 1 config/data/demo/conversion_software_settings.csv conversion_software_settings
.import --csv --skip 1 config/data/demo/samples.csv samples
.import --csv --skip 1 config/data/demo/mobile_phases.csv mobile_phases
.import --csv --skip 1 config/data/demo/norm_peak_confidence.csv norm_peak_confidence
