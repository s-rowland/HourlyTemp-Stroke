# Fit sensitivity models
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Assess Sensitivity to RH Adjustment 
# 2: Assess Sensitivity to Constraint Choice 
# 3: Assess Sensitivity to Number of Lags

####**************
#### N: Notes #### 
####**************

# You will get warnings about knot placement whenever you run the models
# with dlnm terms 
# this warning can be ignored. 
# It is just saying that one way making the constraints ('log knots') 
# used to be the default, but is no longer the default. 

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the script is beginning 
StartTime_D_01 <- Sys.time()
print(paste('begin D_01 at', StartTime_D_01))
print('This section takes 3 (fake data)-10 (real data) minutes to run')

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_analysis_0_01')){
  here::i_am('README.md')
  source(here::here('HourlyTemp_Stroke', 'Scripts',
                    '0_01_setOutcomeName_folders_packages_data.R'))}

####********************************************
#### 1: Assess Sensitivity to RH Adjustment ####
####********************************************

# 1a Fit models with no adjustment for RH
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeISC', 'RHnoAdj', 'fullpop', 'fullpop')
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeHEM', 'RHnoAdj', 'fullpop', 'fullpop')

# 1b Fit models with rh as an hourly dlnm
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeISC', 'RHdlnm', 'fullpop', 'fullpop')
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeHEM', 'RHdlnm', 'fullpop', 'fullpop')

####************************************************
#### 2: Assess Sensitivity to Constraint Choice ####
####************************************************

# 2a Fit models with constraints that yielded second-lowest AIC
identify_subOptimalConstraints_fitModel('HourlyTemp', 'strokeISC', 'fullpop', 'fullpop')
identify_subOptimalConstraints_fitModel('HourlyTemp', 'strokeHEM', 'fullpop', 'fullpop')

####*********************************************
#### 3: Assess Sensitivity to Number of Lags ####
####*********************************************

# 3a Fit models with 24 lag hours
perform_gridSearch('HourlyTemp_24lag', 'strokeISC', 'HourlyTemp_24lag', 'fullpop', 'fullpop')
perform_gridSearch('HourlyTemp_24lag', 'strokeHEM', 'HourlyTemp_24lag', 'fullpop', 'fullpop')
identifySelectedConstraints_fitModel('HourlyTemp_24lag', 'strokeISC', '24LagHr', 'fullpop', 'fullpop')
identifySelectedConstraints_fitModel('HourlyTemp_24lag', 'strokeHEM', '24LagHr', 'fullpop', 'fullpop')

# 3b Fit models with 48 lag hours
perform_gridSearch('HourlyTemp_48lag', 'strokeISC', 'HourlyTemp_48lag', 'fullpop', 'fullpop')
perform_gridSearch('HourlyTemp_48lag', 'strokeHEM', 'HourlyTemp_48lag', 'fullpop', 'fullpop')
identifySelectedConstraints_fitModel('HourlyTemp_48lag', 'strokeISC', '48LagHr', 'fullpop', 'fullpop')
identifySelectedConstraints_fitModel('HourlyTemp_48lag', 'strokeHEM', '48LagHr', 'fullpop', 'fullpop')

# 3c Tell the analyst that the analysis is done
cat('completed D_01 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_D_01)/60, 1), ' min')

rm(StartTime_D_01)