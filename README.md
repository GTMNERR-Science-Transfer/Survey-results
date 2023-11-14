## GTMNERR Science Transfer Project
**Geraldine Klarenberg, Kristie Perez, Nikki Dix, Nia Morales, Shirley Baker**

The Guana Tolomato Matanzas (GTM) NERR in northeast Florida includes multiple water resources where both physical access and access to data is in demand by a wide variety of user groups across several systems and unique habitat types. The needs of these users vary widely and extend beyond the information currently accessible through the NERRS System-wide Monitoring Program (SWMP) for which data is available through the Centralized Data Management Office. 

This project will develop an open data science product to provide access to non-SWMP ecological data sets and visualizations for the Guana River Estuary through an interactive web-based dashboard, expanding engagement to include the community. 

The project was kickstarted with a virtual meeting (Nov 2022), followed by a participatory in-person follow-up meeting (Dec 2022). A survey was distributed to obtain information about users' needs, both in terms of data sets as well as dashboard features.

This repository has R scripts pertaining to the survey (numbered 1 through 7). They:
1. Download responses of survey respondents,
2. Download and reorganize questions and answers to create a unified metadata file,
3. Deidentify and clean data, creating subsets of question groups, and
4. Create exploratory and summative figures and tables for each question group. These outputs are saved in the folder "8_results"

Eventually all this information was analyzed and consolidated into a final report, which can be found in the folder "9_FINAL_REPORT". The file "survey_results_Aug2023.html" is the final report, it will open in a browser. The script/code that generates this file is in "survey_results_Aug2023.Rmd", an RMarkdown file. The files "survey_results_Aug2023_appendix.html" and "survey_results_Aug2023_appendix.Rmd" contain some additional graphs for subgroups of survey questions.

### NOTE!
This RProject uses `renv` to ensure a reproducible environment. If you are unfamiliar with this approach, please read https://rstudio.github.io/renv/articles/renv.html. Specifically note the following: *"Now when one of your collaborators opens this project, renv will automatically bootstrap itself, downloading and installing the appropriate version of renv. It will also ask them if they want to download and install all the packages it needs by running `renv::restore()`."*

Make sure you access the code by opening the .Rproject file!

**This code was written with R version 4.3.1**