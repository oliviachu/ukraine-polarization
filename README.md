## Code for "The micro-dynamics of spatial polarization: A model and an application to survey data from Ukraine"

This code repository accompanies the paper "The micro-dynamics of spatial polarization: A model and an application to survey data from Ukraine", by Olivia J. Chu, Jonathan F. Donges, Graeme B. Robertson, and Grigore Pop-Eleches. It includes raw data and the functions and scripts used in our adaptive voter model (AVM) simulations. The replication data and scripts used in our regression analyses can be found [here](https://doi.org/10.7910/DVN/Q4HFRS). 

The files in this repository are organized into four folders, described below:

* ***raw_data***: this folder contains a .csv file of all the raw data gathered from the survey respondents 

* ***data_inputs***: this folder contains:
	* a .csv file of the latitude and longitude coordinates of the respondents with uniform low and medium noise added
	* .csv and corresponding .mat files of the initial data-informed adjacency matrices (for the geographically uninformed (GU) model and geographically informed model) used in the simulations whose results are reflected in the figures in this paper
	* a .csv and corresponding .mat file of the first 50 closest geographic neighbors of each respondent (using the instantiation of the geographically-informed initial adjacency matrix as described above)
	* a .csv and corresponding .mat file of the respondents' identifying number, 2012 EU-integration opinion, and 2015 EU-integration opinion 
 
* ***scripts***: this folder contains:
	* an R script used to process the raw data, impute missing data, and create the initial adjacency matrices
	* Matlab scripts for the geographically uninformed (GU) and geographically informed (GI) models 

* ***final_ops***: this folder contains a .csv file of the model-predicted final opinions (for the results presented in this paper), sorted by respondent ID number 




