# Database Infrastructure for Mass Spectrometry (DIMSpec)

## About

Welcome to the home page of the Database Infrastructure for Mass Spectrometry project.

## Motivation

## Features

- Portable and reusable database infrastructure for linking sample and method details to high resolution mass spectrometry data.
- Easily extendable schema for new data extensions or views.
- Open source from inception to delivery using only R, python, and SQLite.
- Application programming interface (API) support using the [plumber](https://www.rplumber.io/index.html) framework.
- Web applications for exploration and data processing, including a template web application to quickly build new GUI functionality using the [shiny](https://shiny.rstudio.com) framework.
- Development support through flexible logging and function argument validation frameworks.
- Includes curated high resolution mass spectra for 133 per- and polyfluorinated alkyl substances from over 100 samples using ESI-, ESI+, and APCI- detection methods.

## Getting Started

While the only hard requirement for using DIMSpec is R version 4.1 or later (packages will be installed as part of the installation compliance script, though users on Windows systems should also install RTools), to get the most out of DIMSpec other software users may want to include are (but may not be limited to):
- Java (with bit architecture matching that of R)
- MSConvert >= 3.0.21050 (from ProteoWizard)
- SQLite >= v3.32.0
- Mini/Anaconda w/ Python >= 3.8 (if not already installed, R will install it as part of the compliance script, though advanced users may want to explicitly install this themselves)

To get started in most cases from a blank slate, ensure R v4.1+ is installed ([download](https://www.r-project.org/)) and download the project by cloning this repository or downloading the [zip file](https://github.com/usnistgov/dimspec/archive/refs/heads/main.zip). If using Windows, you will need RTools ([download](https://cran.r-project.org/bin/windows/Rtools/)) to install certain packages. The only other step is to run the compliance script, which will install everything needed for the project. The easiest way is to load the project using RStudio ([download](https://github.com/usnistgov/dimspec.git)). Whether using RStudio or not, run `source(file.path("R", "compliance"))` in an R console opened at the project directory. The first installation typically takes around half an hour from start to finish, depending on the speed of your internet connection and computer.

For evaluation and distribution purposes, DIMSpec is distributed with a populated database of per- and polyfluorinated alkyl substances (PFAS), but supporting functionality is present to easily create new databases. This enables DIMSpec to support multiple efforts simultaneously as research needs require.

## Guides and docs/dimspec_user_guide

For a full description of the project and its different aspects, please see the [DIMSpec User Guide](https://usnistgov.github.io/dimspec/dimspec_user_guide).

A series of Quick Guides have been made available focusing on various aspects of the project.

- [DIMSpec Quick Guide - Installation](https://usnistgov.github.io/dimspec/docs/dimspec_user_guide/quick_install.pdf)
- [DIMSpec Quick Guide - Plumber](https://usnistgov.github.io/dimspec/docs/dimspec_user_guide/quick_plumber.pdf)
- [DIMSpec Quick Guide - Web Applications](https://usnistgov.github.io/dimspec/docs/dimspec_user_guide/quick_apps.pdf)
- [DIMSpec Quick Guide - Advanced Use](https://usnistgov.github.io/dimspec/docs/dimspec_user_guide/quick_advanced.pdf)
- [DIMSpec Quick Guide - Importing Data](https://usnistgov.github.io/dimspec/docs/dimspec_user_guide/quick_import.pdf)
- [File Conversion using msconvert](https://usnistgov.github.io/dimspec/docs/dimspec_user_guide/file_convert.pdf)

In addition, a series of video tutorials are also available.

- [Import files and process on MSMatch (non Waters)]()
- [Import files and process on MSMatch (Waters)]()
- [Library searching and data mining]()
- [Fragmenation searching and data mining]()
- [(optional) Download and install the R program]()
- [(optional) mzmine conversion]()

## Links

Several links can provide additional contextual information about this project.

- [PFAS Program at the US National Institute of Standards and Technology](https://www.nist.gov/programs-projects/and-polyfluoroalkyl-substances-pfas) 
- [DoD SERDP Progam Project ER20-1056](https://www.serdp-estcp.org/projects/details/a0bb4198-02cd-44b9-9e73-9ef916e7f7e0/er20-1056-project-overview#:~:text=ER20-1056%20Objective%20The%20use%20of%20spectral%20libraries%20is,per-%20and%20polyfluoroalkyl%20substances%20%28PFAS%29%20in%20environmental%20samples.) 
- [NIST Suspect List of Possible PFAS](https://github.com/usnistgov/NISTPFAS/blob/main/suspectlist) 
- [NIST Method Reporting Tool for Non-Targeted Analysis (NTA MRT)](https://github.com/usnistgov/NISTPFAS/blob/main/methodreportingtool) 

## Contacting Us

If you have any issues with any portion of the repository, please feel free to contact the NIST PFAS program at <a href="mailto:=pfas@nist.gov?subject=DIMSpec%20Inquiry">pfas@nist.gov</a> directly or post an issue in the repository itself.

The main contributors to this project from NIST were members of the <a href="https://www.nist.gov/mml">Material Measurement Laboratory's</a> <a href="https://www.nist.gov/mml/csd">Chemical Sciences Division</a>:
1. Jared M. Ragland, Chemical Informatics Group <a href="https://orcid.org/0000-0002-8055-2432"><img src="https://orcid.org/0000-0003-0953-5215" alt="orcid icon" width="13"></a> (<a href="mailto:=jared.ragland@nist.gov?subject=DIMSpec%20Inquiry">email</a> | <a href="https://www.nist.gov/people/jared-ragland">staff page</a> | <a href="https://www.nist.gov/mml/csd/chemical-informatics-group">group page</a>)
1. Benjamin J. Place, Organic Chemical Metrology Group <a href="https://orcid.org/0000-0003-0953-5215"><img src="https://orcid.org/0000-0003-0953-5215" alt="orcid icon" width="13"></a> (<a href="mailto:=benjamin.place@nist.gov?subject=DIMSpec%20Inquiry">email</a> | <a href="https://www.nist.gov/people/benjamin-place">staff page</a> | <a href="https://www.nist.gov/mml/csd/organic-chemical-metrology">group page</a>)

## Contributing

NIST projects are provided as a public service, and we always appreciate feedback and contributions. If you have a contribution, feel free to fork this project, open a PR, or start a discussion. The authors hope this effort spurs further innovations in the NTA open data space for mass spectrometry.

## Disclaimer

> Certain commercial equipment, instruments, software, or materials are identified in this documentation in order to specify the experimental procedure adequately. Such identification is not intended to imply recommendation or endorsement by the National Institute of Standards and Technology, nor is it intended to imply that the materials or equipment identified are necessarily the best available for the purpose.

> This work is provided by NIST as a public service and is expressly provided "AS IS." Please see the [license statement](License.md) for details.

## Funding Source

The data included in this repository has been funded in part by the Department of Defense's Strategic Environmental Research and Development Program (SERDP), project number ER20-1056.
