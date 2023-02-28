rm(list = ls())

# Setup folder structure
if (!dir.exists("./01_raw-data")) {dir.create("./01_raw-data")}
if (!dir.exists("./02_data")) {dir.create("./02_data")}
if (!dir.exists("./03_output")) {dir.create("./03_output")}
if (!dir.exists("./04_figures-and-tables")) {dir.create("./04_figures-and-tables")}

## # ----------------------Acquiring the raw data -------------------------- #
## 1. You can find the data used on www.measuredhs.com :
## Data usage is free of charge for scientific usage. Please apply for "Nigeria 2013 - survey data" and "Nigeria 2013 - GPS data".
## 2. You are interested in downloading "Children's recode - NGKR6ADT.ZIP" from here
# https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGKR6ADT.zip&Tp=1&Ctry_Code=NG&surv_id=438&dm=1&dmode=nm
## and the SHP file set with subnational boundaries from here (for selection aid see also "Finding_correct_shapefile.png")
# https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=NG
## 3. Then place the following files in the folder "01_raw-data" :
## -"NGKR6AFL.DTA"
## -"sdr_subnational_boundaries.cpg
## -"sdr_subnational_boundaries.dbf"
## -"sdr_subnational_boundaries.prj"
## -"sdr_subnational_boundaries.sbn"
## -"sdr_subnational_boundaries.sbx"
## -"sdr_subnational_boundaries.shp"
## -"sdr_subnational_boundaries.shp.xml"
## -"sdr_subnational_boundaries.shx"
## # ----------------------------------------------------------------------- #

# Setting all packages to the versions used for this analysis. Please confirm prompt in console with "y"! 
# This is temporary only for this particular R session. 
renv::restore() 

if (!require("foreign")) {install.packages("foreign")}
if (!require("car")) {install.packages("car")}
if (!require("BayesX")) {install.packages("BayesX")}
if (!require("sdPrior")) {install.packages("sdPrior")}
if (!require("MASS")) {install.packages("MASS")}
if (!require("parallel")) {install.packages("parallel")}
if (!require("mboost")) {install.packages("mboost")}
if (!require("ggplot2")) {install.packages("ggplot2")}
if (!require("kableExtra")) {install.packages("kableExtra")}
if (!require("dplyr")) {install.packages("dplyr")}

cluster <- T
cores <- ifelse(cluster, 20, 1)
BXpath <- "" # Enter path to your local BayesX entity

# ## 
# User linear option and add choice of cross validation
# ##


# cleanse raw data
source("./00_Data-preprocessing.R")
# prepare data for estimation with BayesX, add spatial information
source("./01_Data-preparation.R")
# elicitate prior and create batches for BayesX
source("./02_Prior-elic-and-BX-batch-creation.R")

# <<< Warning: potentially needs manual execution if run from Windows OS or if BayesX is only available under sudo in Linux >>> 
# run BayesX 
source("./03_Run-BayesX.R")

# run mboost
source("./03_Run-mboost.R")
# read out results for tables and plots
source("./04_read-out-results.R")
# print tables and plot figures from results
source("./05_Print-tables-and-plot-results.R")
