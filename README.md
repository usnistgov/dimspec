# Database Infrastructure for Mass Spectrometry (DIMSpec)

## About

Welcome to the Database Infrastructure for Mass Spectrometry project. This project is the result of work from the National Institute of Standards and Technology's Material Measurement Laboratory, Chemical Sciences Division. We seek to provide a comprehensive portable database toolkit supporting non-targeted analysis of high resolution mass spectrometry experiments for exposure-based analyte targets (e.g. per- and polyfluorinated alkyl substances (PFAS)) including descriptive metadata for analytical instrument method, quality analysis, and samples. If you would like to get involved, or just to keep track of the project, please give this repository a watch or star, or send an email to <a href="mailto:pfas@nist.gov?subject=DIMSpec%20Interest">pfas@nist.gov</a> to receive updates.

## Latest News

2023 December (@jmr-nist-gov) - This update provides quality of life improvements and minor bug fixes in MSMatch, and supports certain functionality issues related to package versioning when installed on R v4.3 as of Nov 2023. If you are running with R v4.1 and certain package combinations, you may run into an issue with logging and receive a console message regarding `log_formatter`. If so, turn off logging by setting `LOGGING_ON <- FALSE` in the `config/env_log.txt` file or update your packages. Furthermore, this update (a) fixes certain instances with alert messages failing to render, (b) fixes a rare issue with uncertainty calculation inheriting NaN values, (c) adds support for advanced settings on the match uncertainty evaluation tool, and (d) fixes the location of alert messages which could occasionally run past the bottom of the browser.

2023 July (@jmr-nist-gov) - DIMSpec has been updated to its first release candidate version. Changes include schema tightening for annotated fragments and PFAS data updates including consistency updates to analyte nomenclature including aliases, and other minor bug fixes.

## Motivation

In analytical chemistry, the objective of **non-targeted analysis (NTA)** is to detect and identify unknown (generally organic) compounds using a combination of advanced analytical instrumentation (e.g. high-resolution mass spectrometry) and computational tools. For NTA using mass spectrometry, the use of reference libraries containing fragmentation mass spectra of known compounds is essential to successfully identifying unknown compounds in complex mixtures. However, due to the diversity of vendors of mass spectrometers and mass spectrometry software, it is difficult to easily share mass spectral data sets between laboratories using different instrument vendor software packages while maintaining the quality and detail of complex data and metadata that makes the mass spectra commutable and useful. Additionally, this diversity can also alter fragmentation patterns as instrument engineering and method settings can differ between analyses.

This report describes a set of tools developed in the NIST Chemical Sciences Division to provide a database infrastructure for the management and use of NTA data and associated metadata. In addition, as part of a NIST-wide effort to make data more Findable, Accessible, Interoperable, and Reusable ([FAIR](https://www.go-fair.org/)), the database and affiliated tools were designed using only open-source resources that can be easily shared and reused by researchers within and outside of NIST. The information provided in this report includes guidance for the setup, population, and use of the database and its affiliated analysis tools. This effort has been primarily supported by the Department of Defense Strategic Environmental Research and Development Program ([DOD-SERDP](https://serdp-estcp.org/)), project number [ER20-1056](https://www.serdp-estcp.org/projects/details/a0bb4198-02cd-44b9-9e73-9ef916e7f7e0/er20-1056-project-overview). As that project focuses on per- and polyfluoroalkyl substances (PFAS), DIMSpec is distributed with mass spectra including compounds on the [NIST Suspect List of Possible PFAS](https://data.nist.gov/od/id/mds2-2387) as collected using the [Non-Targeted Analysis Method Reporting Tool](https://github.com/usnistgov/NISTPFAS/tree/main/methodreportingtool).

## Features

- Portable and reusable database infrastructure for linking sample and method details to high resolution mass spectrometry data.
- Easily extendable schema for new data extensions or views.
- Open source from inception to delivery using only R, python, and SQLite.
- Application programming interface (API) support using the [plumber](https://www.rplumber.io/index.html) framework.
- Web applications for exploration and data processing, including a template web application to quickly build new GUI functionality using the [shiny](https://shiny.rstudio.com) framework.
- Development support through flexible logging and function argument validation frameworks.
- Includes curated high resolution mass spectra for 133 per- and polyfluorinated alkyl substances from over 100 samples using ESI-, ESI+, and APCI- detection methods (as of 2023-03-16). The DIMSpec for PFAS database is provided here as an example, and is published on the NIST Public Data Repository at https://doi.org/10.18434/mds2-2905. If you use the DIMSpec for PFAS database, please cite both this repository and that file.

## Getting Started

While the only hard requirement for using DIMSpec is R version 4.1 or later (packages will be installed as part of the installation compliance script, though users on Windows systems should also install RTools), to get the most out of DIMSpec users may want to include other software such as (but in no way limited to):

- Java (with bit architecture matching that of R)
- MSConvert >= 3.0.21050 (from ProteoWizard)
- SQLite >= v3.32.0
- Mini/Anaconda w/ Python >= 3.8 (if not already installed, R will install it as part of the compliance script, though advanced users may want to explicitly install this themselves)

Note: As of the [December 2023 release](https://github.com/usnistgov/dimspec/releases/tag/v1.0.2-202312), use of R v4.3 is encouraged as support for older versions of R will sunset in 2024.

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

In addition, a series of video tutorials will also be made available in the near future.

- Download and installation (not yet available)
- Import files and process on MSMatch
  - Waters Instruments (not yet available)
  - All Other Instruments (not yet available)
- Library searching and data mining (not yet available)
- Fragmenation searching and data mining (not yet available)
- mzmine conversion (not yet available)

## Links

Several links can provide additional contextual information about this project. If any of the resource links below are broken, <a href="mailto:pfas@nist.gov?subject=DIMSpec%20Documentation%20Unavailable">please report them</a> so we may address it.

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

The work included in this repository has been funded in large part by the Department of Defense's Strategic Environmental Research and Development Program (SERDP), project [number ER20-1056](https://serdp-estcp.org/projects/details/a0bb4198-02cd-44b9-9e73-9ef916e7f7e0/er20-1056-project-overview).
