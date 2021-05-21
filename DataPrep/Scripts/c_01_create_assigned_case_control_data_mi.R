# Prepare Myocardial Infarction Cases for CaseControl Study
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create Assigned Case Control Data for TV_MI Main Analysis
# 2: Create Assigned Case Control Data with Lead Exposures
# 3: Create Assigned Case Control Data for Alt Window Sensitivity Analysis

####**************
#### N: Notes #### 
####**************

# Na Description
# The goal of this script is to process the case data 
# from step b_1 
# We need to do data cleaning, especially duplicate entries.
# We also apply most of the exclusion criteria in this script. 
# We include all the potential MI cases, not just primary mi 
# so that we can easily apply alternative case definitions in sensitivity analyses 

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the script is beginning 
StartTime_c_01 <- Sys.time()
print(paste('begin c_01 at', StartTime_c_01))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep', 'Scripts', 
                    '0_01_set_outcomeName_packages_folders.R'))
}

####******************************************************************
#### 1: Create Assigned Case Control Data for TV_MI Main Analysis ####
####******************************************************************

# 1a Set CaseType 
CaseType <- 'mi'

# 1b Set NLDASWeight
NLDASWeight <- 'Pop'

# 1c Set timing assumption
# for one sensitivity analysis we try alternative exposure windows 
# in the manuscript we only use 'All23' - 
# meaning all exposure windows start at the 23rd hour of the day 
# STEMI3hrDelay is used in HourlyTemp-MI main analysis 
# STEMI2hrDelay is used in HourlyTemp-MI sensitivity analysis
# STEMI2hrDelay is used for TempVar-MI
Timing <-    'STEMI2hrDelay'

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

####**************************************************************
#### 2: Create Assigned Case Control Data with Lead Exposures ####
####**************************************************************

# 2a Assign lead hourly weather exposure 
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00a_assign_hourly_weather_lead.R'))

# 2b Calculate variability 
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00b_compute_variability_averages_lead.R'))

# 2c Combine with lag-exposured data  
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00c_combine_data_lagLeadExposure.R'))

####******************************************************************************
#### 3: Create Assigned Case Control Data for Alt Window Sensitivity Analysis ####
####******************************************************************************

# 3a Set CaseType 
CaseType <- 'mi'

# 3b Set NLDASWeight
NLDASWeight <- 'Pop'

# 3c Set timing assumption
# for one sensitivity analysis we try alternative exposure windows 
# in the manuscript we only use 'All23' - 
# meaning all exposure windows start at the 23rd hour of the day 
# STEMI3hrDelay is used in HourlyTemp-MI main analysis 
# STEMI2hrDelay is used in HourlyTemp-MI sensitivity analysis
# STEMI2hrDelay is used for TempVar-MI
Timing <-    'All23'

# 3d Prepare case data 
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00a_prepare_cases.R'))

# 3e Create matching control periods 
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00b_create_control_hours.R'))

# 3f Assign lagged hourly weather exposures
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00c_assign_hourly_weather_lag.R'))

# 3g Compute variability and mean metrics
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00d_compute_variability_averages_lag.R'))

# 3h Combine annual data and curate variables
source(here::here('DataPrep', 'Scripts','Functions',
                  'c_00e_combine_data.R'))

# 3i Tell the analyst that the script is done
cat('completed c_01 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_c_01), 1), ' min')

rm(StartTime_c_01)
