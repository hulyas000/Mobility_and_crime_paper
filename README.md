This repository contains the R code for the second empirical chapter of my dissertation project, as well as for the corresponding journal paper based on that chapter. 
The research investigates the relationship between mobility patterns and crime dynamics, using police recorded crime data and Google Mobility Community Reports.
                                                                                                                                                                                                                                                                                                                                         
**crime_mobility_RQ2.Rmd** is main analysis file used for the paper and the second empirical dissertation chapter. It includes the full modeling workflow, data preparation, and estimation procedures. 
Note that not all figures produced by this script appear in the paper or the dissertation; some visualizations were included only in the paper for clarity and conciseness. 

**mobility_crime_models.Rmd** is earlier version of the analysis. It includes additional control variables drawn from census data. 
However, because these controls do not vary over the observation period, they were excluded from the final models that use fixed effects (which eliminate time-invariant covariates).
This version is retained for reference and transparency. 

**police_data_handling_function.R** contains functions for processing and cleaning the raw police data downloaded from [data.police.uk](https://data.police.uk). 
These scripts handle data standardization, spatial alignment, and variable preparation for subsequent analysis.                                                                                                          |
