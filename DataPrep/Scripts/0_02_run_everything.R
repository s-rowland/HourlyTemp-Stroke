# Run Everything
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Prepare Weather Data
# 2: Prepare Health Data 
# 3: Assign Exposures

####**************
#### N: Notes ####
####**************

# This is a super script, to run everything. 

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the analysis is beginning 
StartTime_0_02 <- Sys.time()
print(paste('begin 0_02 at', StartTime_0_02))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep', 'Scripts',
                    '0_01_set_outcomeName_packages_folders.R'))
}

####*****************************
#### 1: Prepare Weather Data #### 
####*****************************

if(OutcomeName != 'fake'){
  source(here::here('DataPrep', 'Scripts', 'a_02_convert_grib_csv.R'))
}
source(here::here('DataPrep', 'Scripts', 'a_03_nldas_to_hourly_zcta_popweight.R'))
source(here::here('DataPrep', 'Scripts', 'a_04_compute_weather_variables.R'))

####****************************
#### 2: Prepare Health Data #### 
####****************************

# Note: the script to capture the stroke admissions from the SPARCS dataset 
# was written in PYthon, so it is not called in this script. 

# 2a If we are using a fake outcome, generate the fake outcome dataset
if(OutcomeName == 'fake'){
  source(here::here('DataPrep', 'Scripts', 
                    'b_02_create_syntheticDate_forCodeReview.R'))
}

####*************************
#### 3: Assign Exposures #### 
####*************************

# 3a Assign exposure
if(OutcomeName != 'fake'){
  source(here::here('DataPrep', 'Scripts', 
                    'c_01_create_assigned_case_control_data_mi.R'))
  source(here::here('DataPrep', 'Scripts', 
                    'c_02_create_assigned_case_control_data_stroke.R'))
}
if(OutcomeName == 'fake'){
  source(here::here('DataPrep', 'Scripts', 
                    'c_03_create_assigned_case_control_data_fake.R'))
}

# 3b Tell the analyst that the analysis is done
cat('completed 0_02 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_0_02), 1), ' min')

rm(StartTime_0_02)
