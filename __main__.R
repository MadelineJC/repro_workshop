# Point is to successively call different scripts that do different tasks, such that everything is happening in one file

#############
#############
# This main file calls and runs all subsequent R files in this analysis
#############
#############
# AUTHOR:
# DATE: 
#############
#############

# set-up ==============================
library(here)

# data wrangling ==============================

# This script reads in iris data and performs some simple data wrangling, then writes out the files needed for model-fitting
source(here("./code/data-wrangling.R"))

# model fitting ==============================

# This script reads in appropriate csv files and fits a shitty glm, then makes model predictions in and out of sample data
rm(list = ls()) # To remove extraneous objects created in data wrangling (we re-read them in in next file)
source(here("./code/model-fitting.R"))

# model visualization ==============================

# This script reads in two data frames and creates and saves some simple plots
rm(list = ls())
source(here("./code/visualization.R"))