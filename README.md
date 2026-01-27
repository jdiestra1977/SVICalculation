Files in R code are used to calculate the adapted CDC Social Vulnerability Index (SVI) that I have adapted from this link: https://github.com/azh2/Social-Vulnerability-R. 

functionForSVI.R - Contains all code to extract and process data, using tidycensus, from the American Community Survey (ACS). Function getVariables() get relevant variable for SVI calculation. Function rankingAndSvi() aggregate and process data to calculate the SVI.

examples.R - Contains code showing examples of how to use functions to calculate SVI at different geographic levels.
