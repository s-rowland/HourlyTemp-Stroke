# Prepare Fake Stroke Cases for CaseControl Study
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create Fake Assigned Case Control Data for HourlyTemp_Stroke Reproducibility

####**************
#### N: Notes #### 
####**************

# Na Description
# The goal of this script is to process the case data 
# from step b_1 
# We need to do data cleaning, especially duplicate entries.
# We also apply most of the exclusion criteria in this script. 
# Right now the code has specifications for mi, 
# but could accomodate stroke in the future. 
# We include all the potential MI cases, not just primary mi 
# so that we can easily apply alternative case definitions in sensitivity analyses 

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the script is beginning 
StartTime_c_03 <- Sys.time()
print(paste('begin c_03 at', StartTime_c_03))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep', 'Scripts',
                    '0_01_set_outcomeName_packages_folders.R'))
}

####*************************************************************************************s
#### 1: Create Fake Assigned Case Control Data for HourlyTemp_Stroke Reproducibility ####
####*************************************************************************************

# 1a Set CaseType 
CaseType <- 'fake'

# 1b Set NLDASWeight
NLDASWeight <- 'Pop'

# 1c Set timing assumption
# for one sensitivity analysis we try alternative exposure windows 
# in the manuscript we only use 'All23' - 
# meaning all exposure windows start at the 23rd hour of the day 
# STEMI3hrDelay is used in HourlyTemp-MI main analysis 
# STEMI2hrDelay is used in HourlyTemp-MI sensitivity analysis
# STEMI2hrDelay is used for TempVar-MI
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
cat('completed c_03 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_c_03), 1), ' secs')
rm(StartTime_c_03)

