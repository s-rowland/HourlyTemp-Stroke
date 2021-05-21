# Fit the main HourlyTemp - Stroke health models
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Fit Full Population Main Models
# 2: Fit Stratified Models 

####**************
#### N: Notes #### 
####**************

# You will get warnings about knot placement whenever you run the models
# with dlnm terms 
# this warning can be ignored. 
# It is just saying that one way making the constraints ('log knots') 
# used to be the default, but is no longer the default. 

# A single grid search takes 10-15 minutes to run with the main subtypes

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the analysis is beginning 
StartTime_C_01 <- Sys.time()
print(paste('begin C_01 at', StartTime_C_01))
print('This section takes 5 (fake data)-20 (real data) minutes to run')

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_analysis_0_01')){
  here::i_am('README.md')
  source(here::here('HourlyTemp_Stroke', 'Scripts',
                    '0_01_setOutcomeName_folders_packages_data.R'))}

####****************************************
#### 1: Fit Full Population Main Models ####
####****************************************

# 1a Conduct grid search and then fit and save final model
perform_gridSearch('HourlyTemp', 'strokeISC', 'Main', 'fullpop', 'fullpop')
perform_gridSearch('HourlyTemp', 'strokeHEM', 'Main', 'fullpop', 'fullpop')

# 1b Fit and save final models 
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeISC', 'Main', 'fullpop', 'fullpop')
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeHEM', 'Main', 'fullpop', 'fullpop')

####******************************
#### 2: Fit Stratified Models ####
####******************************

# 2a Fit models stratified by hypertension
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeISC', 'Main', 'HTN', 'HTN')
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeISC', 'Main', 'HTN', 'noHTN')
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeHEM', 'Main', 'HTN', 'HTN')
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeHEM', 'Main', 'HTN', 'noHTN')

# 2b Fit models stratified by atrial fibrillation
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeISC', 'Main', 'Afib', 'Afib')
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeISC', 'Main', 'Afib', 'noAfib')
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeHEM', 'Main', 'Afib', 'Afib')
identifySelectedConstraints_fitModel('HourlyTemp', 'strokeHEM', 'Main', 'Afib', 'noAfib')

# 2c Tell the analyst that the analysis is done
cat('completed C_01 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_C_01)/60, 1), ' min')

rm(StartTime_C_01)