# Maritimes-SUMMER-Atlas
Repository for the Maritimes Region Summer RV survey atlas

The contents of this repository are as follows:

* the file Atlas-main.R is the main script that will 
  * perform database queries to access the Summer RV survey data and perform a number of analytical steps necessary, handled by the file data-and-stats.R
  * generate figures for the species of interest, handled by the file figures.R
  * assemble a Technical Report with contents of the analyses, handled by the file tech-report.R

* folder Data-extract: R scripts that perform different data extractions and data analyses
* folder Figures-data: where output text files of the different analyses are stored
* folder Figures-code: R scripts to generate the figures
* folder Figures-actual: bitmap files of the different figures are stored
* folder Report-generation: R scripts to generate the files required by the Technical Report
* TechReport-EN: a folder for the Technical Report using [csasdown](https://github.com/pbs-assess/csasdown)

The repository does not included the actual data files and figures, it contains the tools necessary to replicate the Technical Report. 

You will need appropriate credentials to access the RV survey database in order to perform the data extractions.
