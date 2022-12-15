# Nigeria
This code is meant for those who would like to replicate the analysis run on the DHS data set on childhood malnutrition in Nigeria using the NBPSS prior for effect selection in Bayesian structured additive quantile regression available from the current developer version of BayseX and comparing it to mboost (R package for gradient boosting).
## Data
1. The data is publicly available on www.measuredhs.com : Data usage is free of charge for scientific usage. Please apply for "Nigeria 2013 - survey data" and "Nigeria 2013 - GPS data".  
2. To get to the data you need download "Children's recode - NGKR6ADT.ZIP" from here 
**https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGKR6ADT.zip&Tp=1&Ctry_Code=NG&surv_id=438&dm=1&dmode=nm**
and the SHP file set with subnational boundaries from here (for selection aid see also "Finding_correct_shapefile.png")
**https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=NG**
3. Then place the following files in the folder "01_raw-data":
-"NGKR6AFL.DTA"
-"sdr_subnational_boundaries.cpg  
-"sdr_subnational_boundaries.dbf  
-"sdr_subnational_boundaries.prj  
-"sdr_subnational_boundaries.sbn  
-"sdr_subnational_boundaries.sbx  
-"sdr_subnational_boundaries.shp  
-"sdr_subnational_boundaries.shp.xml  
-"sdr_subnational_boundaries.shx  

## Analysis
Run the file "Run-code.R". Potentially adjust it for running it on a cluster server and definitely adjust the location your local BayesX is run from. The script will automatically call all the other scripts sequentially and generate the necessary output. 
