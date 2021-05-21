# Prepare Stroke Cases for CaseControl Study
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create Assigned Case Control Data for HourlyTemp_Stroke

####**************
#### N: Notes #### 
####**************

# Na Description
# The goal of this script is to process the case data 
# from step b_01b
# We need to do data cleaning, especially duplicate entries.
# We also apply most of the exclusion criteria in this script. 

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the script is beginning 
StartTime_c_02 <- Sys.time()
print(paste('begin c_02 at', StartTime_c_02))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep', 'Scripts',
                    '0_01_set_outcomeName_packages_folders.R'))
}

####****************************************************************
#### 1: Create Assigned Case Control Data for HourlyTemp_Stroke ####
####****************************************************************

# 1a Set CaseType 
CaseType <- 'stroke'

# 1b Set NLDASWeight
NLDASWeight <- 'Pop'

# 1c Set timing assumption
# in some previous studies we conducted sensitivity analyses 
# assessing the timing assumption 
# so the timing (prehospital delay time) assumption 
# is a parameter is processing the data and assigning exposures
Timing <-    '3hrDelay'

# 1d Prepare case data 
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00a_prepare_cases.R'))

# 1e Create matching control periods 
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00b_create_control_hours.R'))

# 1f Assign lagged hourly weather exposures
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00c_assign_hourly_weather_lag.R'))

# 1g Compute variability and mean metrics
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00d_compute_variability_averages_lag.R'))

# 1h Combine annual data and curate variables
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00e_combine_data.R'))

# 1i Tell the analyst that the script is done
cat('completed c_02 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_c_02), 1), ' min')

rm(StartTime_c_02)