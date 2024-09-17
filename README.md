# Database Infrastructure for Mass Spectrometry (DIMSpec)

## About

&emsp;Welcome to the Database Infrastructure for Mass Spectrometry project. This project is the result of work from the <a href="https://www.nist.gov">National Institute of Standards and Technology's</a> <a href="https://www.nist.gov/mml/csd">Chemical Sciences Division</a>, part of the <a href="https://www.nist.gov/mml">Material Measurement Laboratory</a>. We seek to provide a comprehensive portable database toolkit supporting non-targeted analysis of high resolution mass spectrometry experiments for exposure-based analyte targets (e.g. per- and polyfluorinated alkyl substances (PFAS)) including descriptive metadata for analytical instrument method, quality analysis, and samples. If you would like to get involved, or just to keep track of the project, please give this repository a watch or star, or send an email to <a href="mailto:pfas@nist.gov?subject=DIMSpec%20Interest">pfas@nist.gov</a> to receive updates.

## Latest News

<strong>2024 September</strong> (@jmr-nist-gov)</summary>

&emsp;Lots of news this month as we work toward the next release. Many of these updates are thanks to users beginning to use DIMSpec "in the wild", partly in association with the recently completed NIST PFAS Non-Targeted Analysis Interlaboratory Study. Many thanks to all the participants; the final report is working its way through development now and should be available by the end of 2024. Keep reading below for the highlights, or click into the details fold to get the nitty gritty:
1. The compound aliases list in the database has been expanded to include a large number of PubChem ID references (~2,600).
1. Certain views in the database were refined to fix small display issues and typos.
1. Several small bugs have been fixed throughout.
1. Thanks to contributors sending in data, the DIMSpec-QC application has been expanded to larger use cases supporting higher throughput, and will now handle any number of mzML and sample JSON files more easily. (The <a href="https://mastering-shiny.org/action-transfer.html#server">Shiny upload limit</a> still applies.)
1. We've added a section in this README for Frequently Asked Questions under "DIMSpec User Tips and FAQs" below.

<details style="padding-left: 15px;">
<summary>Click here for more detail.</summary>

- Swapped the installation order of the API and RDKit as both rely on `reticulate` and on certain Linux-based systems, the previous order was causing an attempt at double installation. Thanks to [robertyoung3](https://github.com/robertyoung3) for finding this one.
- Fixed an issue where fresh installs may be missing ALL required packages, causing none to be installed.
- Fixed an issue where changing the isolation window can cause a crash in MSMatch.
- Database updates specific to DIMSpec for PFAS:
  - The view `view_separation_types` was updated to fix a display error which would sometimes indicate e.g. "LC x LC" when a guard column was used. An SQL script is available to directly update this on your local copy if you are comfortable using it.
  - The view `view_methods` was updated to fix a typo.
  - The compound alias list was expanded with two common acronyms and 2,628 newly aligned PubChem IDs to improve cross referencing. Additionally, several aliases have been split out for more clarity where they were previously concatenated mistakenly with a comma.
  - Contact <a href="mailto:pfas@nist.gov?subject=DIMSpec%20for%20PFAS%20Update%20Script%20Request">pfas@nist.gov</a> to request CSV and SQL files to provide local patches or, if your local copy has not been modified, download the current "dimspec_nist_pfas.sqlite" database file from GitHub and replace your current copy. Make certain there are no active connections to the database prior to replacing it in this manner. The version hosted on data.nist.gov will be updated during the next release cycle for that platform, date TBD but hopefully by the end of the year.
- Fixed issues and changes in DIMSpec-QC:
  - Thanks to [schwtr02](https://github.com/schwtr02) and the USEPA for contributing more spectra use cases for us to test. These improvements wouldn't have been possible without additional test cases.
  - Multiple .mzML files now load and match correctly with sample JSONs, with some limited flexibility to catch misnaming. It is still recommended that references to mzML files in sample JSONs from the NTA-MRT exactly match the mzML file name.
  - Added a pop up message indicating a possible name mismatch between .mzML files and JSON files.
  - A crash occured if no MS2 scans fit the defined search window or if certain QA checks could not be performed.
  - DIMSpec-QC now resolves multiple collision energies when data were collected by sequentially-stepped-collision-energy experiments (e.g. 10, 30, 50 eV). This results in multiple import files being produced for a single compound. This approach does duplicate the MS1 spectra to keep them tied to the same CE-specific fragmentation spectra for matching purposes, but each is assigned a different ms_methods ID to represent the collision energy that applies to those spectra; sample relationships are maintained. This is not ideal as data are duplicated, but enables multiple collision energies within one experiment without a schema change. This should be an edge case for most uses.
  - The Export Data page has been removed in favor of a direct export button on the Quality Review page.
- Updated project function `nist_shinyalert` to catch an issue with `shinyalert` v3.0.0 (this has been fixed in v3.1.0) and providing `tagList` arguments to pop up modal text and moved it (along with `valid_file_format` and `complete_form_entry`) to file `shiny_helpers.R` rather than individually in `app_functions.R` for each shiny application.
</details>

----

<strong>2024 May</strong> (@jmr-nist-gov)

&emsp;A [paper describing this project](https://doi.org/10.1021/jasms.4c00073) has been published in the Journal of the American Society for Mass Spectrometry. It is freely available until November 2024 as an ACS Editor's Choice selection.
> Ragland, J. M.; Place, B. J. A Portable and Reusable Database Infrastructure for Mass Spectrometry, and Its Associated Toolkit (The DIMSpec Project). J. Am. Soc. Mass Spectrom. 2024. https://doi.org/10.1021/jasms.4c00073.

----

<strong>2024 February</strong> (@jmr-nist-gov)

&emsp;A [video tutorial series](https://www.nist.gov/programs-projects/and-polyfluoroalkyl-substances-pfas/research/reference-data-and-tools/dimspec) is now available for DIMSpec, discussing download and setup, file conversion to .mzML, and using the MSMatch application.

----

<strong>2024 January</strong> (@jmr-nist-gov)

&emsp;The DIMSpec project was featured as part of the SERDP Webinar Series on December 7, 2024. A recording of that webinar, the first half of which is dedicated to DIMSpec is now [available](https://www.serdp-estcp.mil/toolsandtraining/details/a50f42a3-db4a-4857-9b63-0ff4266a47b3/forensic-methods-for-pfas-source-tracking-and-allocation).

----

<details style="padding-left: 15px;">
<summary><strong>Older news items (click to expand)</strong></summary>
<details style="padding-left: 15px; padding-top: 10px;">
<summary><strong>2024 May</strong> (@jmr-nist-gov)</summary>

&emsp;The MSMatch application has been updated to fix a typo on the landing page, fix a bug preventing isolation widths above 4 Da on the data input page (mostly applicable to SWATH experiments), and prevent certain edge conditions from resulting in unrenderable tables. Additionally, the DIMSpec-QC application and underlying functions in `gather_qc.R` and `elementalcomposition.R` have received some quality of life improvements to preferentially interact with the API, if available, rather than a local database connection.
</details>
<details>
<summary><strong>2024 February (addl)</strong> (@jmr-nist-gov)</summary>

&emsp;Minor changes to the [quick install guide](https://pages.nist.gov/dimspec/docs/quick_install.pdf) were made to clarify some language, especially in regards to what is actually required versus recommended versus suggested, and under which circumstances those apply.

&emsp;A bug was fixed in the `molecule_picture` function where invalid filenames were produced from InChI (and other) strings. Invalid filename characters are now substituted with descriptive characters for these; the result is that filenames no longer match 1:1 with molecular notation in many cases, though most SMILES strings should remain intact. Also, use of the `show` argument should be more intuitive and will now display the resulting picture in the system viewer.

&emsp;These changes will be included in the next release, but can be downloaded directly from the current repository.
</details>
<details>
<summary><strong>2023 December</strong> (@jmr-nist-gov)</summary>

&emsp;This update provides quality of life improvements and minor bug fixes in MSMatch, and supports certain functionality issues related to package versioning when installed on R v4.3 as of Nov 2023. If you are running with R v4.1 and certain package combinations, you may run into an issue with logging and receive a console message regarding `log_formatter`. If so, turn off logging by setting `LOGGING_ON <- FALSE` in the `config/env_log.txt` file or update your packages. Furthermore, this update (a) fixes certain instances with alert messages failing to render, (b) fixes a rare issue with uncertainty calculation inheriting NaN values, (c) adds support for advanced settings on the match uncertainty evaluation tool, and (d) fixes the location of alert messages which could occasionally run past the bottom of the browser.
</details>
<details>
<summary><strong>2023 July</strong> (@jmr-nist-gov)</summary>

&emsp;DIMSpec has been updated to its first release candidate version. Changes include schema tightening for annotated fragments and PFAS data updates including consistency updates to analyte nomenclature including aliases, and other minor bug fixes.
</details>
</details>

## DIMSpec User Tips and FAQ

Along with this version, we've collected some tips and tricks from users, along with answers to some frequently asked questions. Normally this would go in a separate document, but we're keeping it here for visibility until it becomes larger.

<details style="padding-left: 15px;">
<summary>Tips From the Bench</summary>

- In MSMatch, toggling on advanced features provides controls to fine tune the search parameters. This can provide much deeper matching options especially for fragment annotation.
- When using DIMSpec-QC, the name of the .mzML file must match, <strong>exactly</strong>, the name of the file reference in the Sample JSON file(s). It is not advisable to rename your .mzML files after creating the sample JSON file with the NTA-MRT without also renaming the `sample{name = , ...}` property in the JSON file (usually this is on line 2).
</details>
<details style="padding-left: 15px;">
<summary>Frequently Asked Questions</summary>
<details style="padding-left: 15px; padding-top: 10px;">
<summary>Why am I getting errors related to Python versions?</summary>

This is usually indicated by an error message along the lines of

> "The requested version of Python ('...') cannot be used, as another version of Python ('...') has already been initialized. Please restart the R session if you need to attach reticulate to a different version of Python."

Reticulate may bind an R session to an environment other than `nist_dimspec` when opening the project if you have multiple environments available. This can occur when an object in the R environment points to a reticulate environment or object that has become disconnected from the current R session, usually by closing out of the project and saving a .RData file which is then loaded automatically the next time the project is opened.

Generally, this means the project was started with an R environment that already includes a pointer bound to a reticulate environment; by default this is the `rdk` object in a DIMSpec session. To remove this error, remove `rdk` (e.g. `rm(rdk)` at the console) and close and restart R or RStudio, choosing to save the environment with your data objects. Opening the project again should exhibit normal behavior. To prevent this, ensure removal of Python-bound objects (e.g. `rdk`) prior to saving data when you exit R or RStudio. A future version of `close_up_shop` will remove any residual python-bound objects to prevent this from happening.
</details>
<details style="padding-left: 15px;">
<summary>Why am I getting package errors?</summary>

This is usually indicated by an error message along the lines of `Error in library(x) : there is no package called 'x'`.

This can occur when you run the `compliance.R` script when a package has failed to install properly. Install the package manually (i.e. `install.packages(x)` at the console) and ensure that it installs correctly, dealing with any dependencies that may occur as guided by console messages. Once it has installed correctly, running the `compliance.R` script again should solve the issue.

It is difficulty to say why any given package has failed to load, but usually it is a matter of what is and is not installed locally on a computer, which can depend on other aspects such as security limitations, version incompatibility, or dependency availability. Error messages in the console, such as that listed above, will tell you exactly which package is unavailable, though other dependencies may be the underlying cause.
</details>
<details style="padding-left: 15px;">
<summary>Does DIMSpec include functions for X data processing step in non-targeted analysis (NTA)?</summary>

Categorically, no, as DIMSpec is not a standalone project for non-targeted analysis. This includes functionality such as baseline correction, peak alignment, etc. DIMSpec is a toolkit for building, populating, and using an opinionated data container supporting NTA and is intended to complement a wide variety of existing NTA workflows rather than replace them.
</details>
</details>

## Motivation

&emsp;In analytical chemistry, the objective of **non-targeted analysis (NTA)** is to detect and identify unknown (generally organic) compounds using a combination of advanced analytical instrumentation (e.g. high-resolution mass spectrometry) and computational tools. For NTA using mass spectrometry, the use of reference libraries containing fragmentation mass spectra of known compounds is essential to successfully identifying unknown compounds in complex mixtures. However, due to the diversity of vendors of mass spectrometers and mass spectrometry software, it is difficult to easily share mass spectral data sets between laboratories using different instrument vendor software packages while maintaining the quality and detail of complex data and metadata that makes the mass spectra commutable and useful. Additionally, this diversity can also alter fragmentation patterns as instrument engineering and method settings can differ between analyses.

&emsp;This report describes a set of tools developed in the NIST Chemical Sciences Division to provide a database infrastructure for the management and use of NTA data and associated metadata. In addition, as part of a NIST-wide effort to make data more Findable, Accessible, Interoperable, and Reusable ([FAIR](https://www.go-fair.org/)), the database and affiliated tools were designed using only open-source resources that can be easily shared and reused by researchers within and outside of NIST. The information provided in this report includes guidance for the setup, population, and use of the database and its affiliated analysis tools. This effort has been primarily supported by the Department of Defense Strategic Environmental Research and Development Program ([DOD-SERDP](https://serdp-estcp.org/)), project number [ER20-1056](https://www.serdp-estcp.org/projects/details/a0bb4198-02cd-44b9-9e73-9ef916e7f7e0/er20-1056-project-overview). As that project focuses on per- and polyfluoroalkyl substances (PFAS), DIMSpec is distributed with mass spectra including compounds on the [NIST Suspect List of Possible PFAS](https://data.nist.gov/od/id/mds2-2387) as collected using the [Non-Targeted Analysis Method Reporting Tool](https://github.com/usnistgov/NISTPFAS/tree/main/methodreportingtool).

## Features

- Portable and reusable database infrastructure for linking sample and method details to high resolution mass spectrometry data.
- Easily extendable schema for new data extensions or views.
- Open source from inception to delivery using only R, python, and SQLite.
- Application programming interface (API) support using the [plumber](https://www.rplumber.io/index.html) framework.
- Web applications for exploration and data processing, including a template web application to quickly build new GUI functionality using the [shiny](https://shiny.rstudio.com) framework.
- Development support through flexible logging and function argument validation frameworks.
- Includes curated high resolution mass spectra for 132 per- and polyfluorinated alkyl substances from over 100 samples using ESI-, ESI+, and APCI- detection methods (as of 2023-03-16). The DIMSpec for PFAS database is provided here as an example, and is published on the NIST Public Data Repository at https://doi.org/10.18434/mds2-2905. If you use the DIMSpec for PFAS database, please cite both this repository and that file.

## Getting Started

While the only hard requirement for using DIMSpec is R version 4.1 or later (packages will be installed as part of the installation compliance script, though users on Windows systems should also install RTools), to get the most out of DIMSpec users may want to include other software such as (but in no way limited to):

- Java (with bit architecture matching that of R)
- MSConvert >= 3.0.21050 (from ProteoWizard)
- SQLite >= v3.32.0
- Mini/Anaconda w/ Python >= 3.8 (if not already installed, R will install it as part of the compliance script, though advanced users may want to explicitly install this themselves)

Note: <strong>As of the [December 2023 release](https://github.com/usnistgov/dimspec/releases/tag/v1.0.2-202312), use of R v4.3 is encouraged as support for older versions of R will sunset in 2024.</strong>

To get started in most cases from a blank slate:

1. Ensure R v4.1+ is installed ([download](https://www.r-project.org/))
1. Download the project by forking this repository or downloading the [zip file](https://github.com/usnistgov/dimspec/archive/refs/heads/main.zip).
   - If using Windows, ensure RTools ([download](https://cran.r-project.org/bin/windows/Rtools/)) matching your R version is installed to build certain packages.
1. Run the compliance script, which should install everything needed for the project.
   - The easiest way is to load the project using RStudio ([download](https://posit.co/download/rstudio-desktop/)).
     - Open RStudio and click "File" > "Open Project..." and navigate to the location where you downloaded the project.
     - Either open the file at "R/compliance.R" from the "Files" pane and click the "Source" button or enter the command `source(file.path("R", "compliance"))` in the console pane.
   - If not using RStudio, open an R terminal at the project directory (or `setwd(file.path("path", "to", "project")`) and enter the command `source(file.path("R", "compliance"))`.
   - The first installation typically takes around half an hour from start to finish, depending on the speed of your internet connection and computer.

A [quick guide](https://pages.nist.gov/dimspec/docs/quick_install.pdf) is available describing the install process.

For evaluation and distribution purposes, DIMSpec is distributed with a populated database of [per- and polyfluorinated alkyl substances (PFAS)](https://doi.org/10.18434/mds2-2905), but supporting functionality is present to easily create new databases. This enables DIMSpec to support multiple efforts simultaneously as research needs require.

## Guides and Documentation

For a full description of the project and its different aspects, please see the [DIMSpec User Guide](https://pages.nist.gov/dimspec/docs/index.html).

A series of Quick Guides have been made available focusing on various aspects of the project.

- [DIMSpec Quick Guide - Installation](https://pages.nist.gov/dimspec/docs/quick_install.pdf)
- [DIMSpec Quick Guide - Plumber](https://pages.nist.gov/dimspec/docs/quick_plumber.pdf)
- [DIMSpec Quick Guide - Web Applications](https://pages.nist.gov/dimspec/docs/quick_apps.pdf)
- [DIMSpec Quick Guide - Advanced Use](https://pages.nist.gov/dimspec/docs/quick_advanced.pdf)
- [DIMSpec Quick Guide - Importing Data](https://pages.nist.gov/dimspec/docs/quick_import.pdf)
- [File Conversion using msconvert](https://pages.nist.gov/dimspec/docs/file_convert.pdf)

In addition, a series of [short video tutorials](https://www.nist.gov/programs-projects/and-polyfluoroalkyl-substances-pfas/research/reference-data-and-tools/dimspec) are available discussing certain topics.

- Download and installation
- mzML conversion of instrument data files
- Import files and process on MSMatch
- Library searching and data mining
- Fragmenation searching and data mining

## Links

Several links can provide additional contextual information about this project. If any of the resource links below are broken, <a href="mailto:pfas@nist.gov?subject=DIMSpec%20Documentation%20Unavailable">please report them</a> so we may address it. The user guide is also available in running DIMSpec sessions using the `user_guide()` function which will load a local version of the user guide if the web version is unavailable or your computer is offline.

- [PFAS Program at the US National Institute of Standards and Technology](https://www.nist.gov/programs-projects/and-polyfluoroalkyl-substances-pfas) 
- [DoD SERDP Progam Project ER20-1056](https://www.serdp-estcp.org/projects/details/a0bb4198-02cd-44b9-9e73-9ef916e7f7e0/er20-1056-project-overview#:~:text=ER20-1056%20Objective%20The%20use%20of%20spectral%20libraries%20is,per-%20and%20polyfluoroalkyl%20substances%20%28PFAS%29%20in%20environmental%20samples.) 
- [NIST Suspect List of Possible PFAS](https://github.com/usnistgov/NISTPFAS/blob/main/suspectlist) 
- [NIST Method Reporting Tool for Non-Targeted Analysis (NTA MRT)](https://github.com/usnistgov/NISTPFAS/blob/main/methodreportingtool) 
- [Database Infrastructure for Mass Spectrometry - Per- and Polyfluoroalkyl Substances](https://data.nist.gov/od/id/mds2-2905)

## Contacting Us

If you have any issues with any portion of the repository, please feel free to contact the NIST PFAS program at <a href="mailto:pfas@nist.gov?subject=DIMSpec%20Inquiry">pfas@nist.gov</a> directly or post an issue in the repository itself.

The main contributors to this project from NIST were members of the <a href="https://www.nist.gov/mml">Material Measurement Laboratory's</a> <a href="https://www.nist.gov/mml/csd">Chemical Sciences Division</a>:
1. Jared M. Ragland <a href="https://orcid.org/0000-0002-8055-2432"><img src="https://avatars.githubusercontent.com/u/1122775?s=200&v=4" alt="orcid icon with link" width="15"></a> ([@jmr-nist-gov](https://github.com/jmr-nist-gov)) (<a href="mailto:=jared.ragland@nist.gov?subject=DIMSpec%20Inquiry">email</a>) (<a href="https://www.nist.gov/people/jared-ragland">staff page</a>) (<a href="https://www.nist.gov/mml/csd/chemical-informatics-group">Chemical Informatics Group</a>)
1. Benjamin J. Place <a href="https://orcid.org/0000-0003-0953-5215"><img src="https://avatars.githubusercontent.com/u/1122775?s=200&v=4" alt="orcid icon with link" width="15"></a> ([@benjaminplace](https://github.com/benjaminplace)) (<a href="mailto:=benjamin.place@nist.gov?subject=DIMSpec%20Inquiry">email</a>) (<a href="https://www.nist.gov/people/benjamin-place">staff page</a>) (<a href="https://www.nist.gov/mml/csd/organic-chemical-metrology">Organic Chemical Metrology Group</a>)

## Contributing

NIST projects are provided as a public service, and we always appreciate feedback and contributions. If you have a contribution, feel free to fork this project, open a PR, or start a discussion. The authors hope this effort spurs further innovations in the NTA open data space for environmental mass spectrometry.

## Disclaimer

> Certain commercial equipment, instruments, software, or materials are identified in this documentation in order to specify the experimental procedure adequately. Such identification is not intended to imply recommendation or endorsement by the National Institute of Standards and Technology, nor is it intended to imply that the materials or equipment identified are necessarily the best available for the purpose.

> This work is provided by NIST as a public service and is expressly provided "AS IS." Please see the [license statement](LICENSE.md) for details.

## Funding Source

The work included in this repository has been funded in large part by the Department of Defense's [Strategic Environmental Research and Development Program (SERDP)](https://serdp-estcp.mil/), project number [ER20-1056](https://serdp-estcp.org/projects/details/a0bb4198-02cd-44b9-9e73-9ef916e7f7e0/er20-1056-project-overview).
