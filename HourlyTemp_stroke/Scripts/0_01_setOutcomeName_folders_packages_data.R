# Create project folder structure
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Set OutcomeName
# 2: Load Packages
# 3: Create Folder Structure
# 4: Create Tables for Grid Search
# 5: Readin the Data 
# 6: Source Functions

####**************
#### N: Notes ####
####**************

# Na First 
# Run this script before any other script! 

# Nb Description
# We use this script to create the strings of the various folders 
# if the folders are not already present in the project, 
# this script will also create the folders 
# All subsequent scripts use the folder-name strings created here 

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the script is beginning
StartTime_0_01 <- Sys.time()
print(paste('begin 0_01 at', StartTime_0_01))

# 0b Load Packages
library(pacman)

####************************
#### 1: Set OutcomeName #### 
####************************

# 1a Set outcomename 
# this is where you tell the script that you are using fake data. 
OutcomeName <- 'stroke'
#OutcomeName <- 'fake'
CaseName <- OutcomeName

# 1b Set user - this only controls the outputs path 
User <- 'Analyst' 
# User <- 'Reviewer'

# 1c Add marker 
Ran_analysis_0_01 <- 'Ran_analysis_0_01'

####**********************
#### 2: Load Packages #### 
####**********************

# 2a Load Packages
p_load(tidyverse,lubridate,magrittr,  # tidyverse packages
       fst,
       purrr, furrr, future, progress, progressr, # efficiency/paralleization packages
       survival, dlnm, splines, # stats packages
       egg) # plotting

####********************************
#### 3: Create Folder Structure #### 
####********************************

# 3a Name directories
project.folder <- paste0(print(here::here()),'/')
intermediate.data.folder <- paste0(project.folder, 'DataPrep/Data/Intermediate_Data/')
final.data.folder <- paste0(project.folder, 'DataPrep/Data/Final_Data/')
analysis.folder <- paste0(project.folder,'HourlyTemp_stroke/')
  data.folder <- paste0(analysis.folder, 'Data/')
  scripts.folder <- paste0(analysis.folder, 'Scripts/')
    functions.folder <- paste0(scripts.folder, 'Functions/')
  outputs.folder <- paste0(analysis.folder, 'Outputs/')
  OutputsPath <- 'Outputs'
  if(OutcomeName == 'fake' & User == 'Analyst'){
    outputs.folder <- paste0(analysis.folder, 'FakeOutputs_analyst/')
    OutputsPath <- 'FakeOutputs_analyst'
  }
  if(OutcomeName == 'fake' & User == 'Reviewer'){
    outputs.folder <- paste0(analysis.folder, 'FakeOutputs_reviewer/')
  OutputsPath <- 'FakeOutputs_reviewer'
  }
    models.folder   <- paste0(outputs.folder, 'Models/')
    estimates.folder <- paste0(outputs.folder, 'Estimates/')
    plots.folder <- paste0(outputs.folder, 'Plots/')
    tables.folder <- paste0(outputs.folder, 'Tables/')
    manuscript.folder <- paste0(outputs.folder, 'Manuscript/')

# 3b Identify list of folder locations which have just been created above
folder.names <- grep('.folder',names(.GlobalEnv),value=TRUE)

# 3c Create function to create list of folders
# note that the function will not create a folder if it already exists 
create_folders <- function(name){
  ifelse(!dir.exists(get(name)), dir.create(get(name), recursive=TRUE), FALSE)
}

# 3d Create the folders named above
lapply(folder.names, create_folders)

# 3e Clean up 
rm(list=ls(pattern='folder'))

####**************************************
#### 4: Create Tables for Grid Search #### 
####**************************************

# 4a Create table for model's AIC
if(!file.exists(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 'Model_AIC.csv'))){
  data.frame(ModelIdentifier = NA, 
             ERConstraint = NA, 
             LRConstraint = NA, 
             AIC = NA, 
             RunDate = NA) %>%  
    write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 'Model_AIC.csv'))
}

# 4b Create table for model's AIC weights
if(!file.exists(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 'Model_AICweights.csv'))){
  data.frame(ModelIdentifier = NA, 
             ERConstraint = NA, 
             LRConstraint = NA, 
             AIC = NA,
             AkaikeWeight = NA,
             RunDate = NA) %>%  
    write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 'Model_AICweights.csv'))
}

# 4c Create table of the final selected models
if(!file.exists(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 'SelectedModels.csv'))){
  data.frame(ModelIdentifier = NA, 
             ERConstraint = NA, 
             LRConstraint = NA, 
             RunDate = NA) %>%  
    write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 'SelectedModels.csv'))
}

####************************
#### 5: Readin the Data #### 
####************************

# 5a Readin the stroke data 
dta <- fst::read.fst(here::here('DataPrep', 'Data', 'Final_Data', 
                                paste0('prepared_cohort', '_', OutcomeName,
                                       '_identified', '_', 'Pop', '_', '3hrDelay', '.fst')))

# 5b Restrict to only primary, identified strokes 
# right now the python readin code also restricts to primary strokes.
# however, some of those strokes have ambiguous subtype
dta <- dta %>% filter(strokeISC_prim == '1' | strokeHEM_prim == '1')

# 5c Create vector of observed hourly temperatures 
dgk <- dta %>% 
  dplyr::select(contains('tLag_'))
dgk <- as.matrix(dgk)[,1:36]
obsHourlyTemp <- as.vector(dgk)

# 5d Calculate summary statistics
HourlyTemp.mean <- mean(obsHourlyTemp)
HourlyTemp.sd <- sd(obsHourlyTemp)
HourlyTemp.min <- min(obsHourlyTemp)
HourlyTemp.max <- max(obsHourlyTemp)
HourlyTemp.per05 <- quantile(obsHourlyTemp, 0.05, type = 1)
HourlyTemp.per95 <- quantile(obsHourlyTemp, 0.95, type = 1)

# 5e Create counterfactual exposure vector
# we do this step before identifying cases to make sure that 
# the estimates for ischemic and hemorrhagic stroke are comparable 
# the exposure distribution for the two outcomes has sufficient overlap 
# so we do have coverage for all temperature values, for each subtype 
# but the exact percentiles may different between subgroups.
# we use this exposure vector for creating the estimates 
# from the dlnm model 
# these are the counterfactual exposure levels
# and we generate effect estimates for a contrast between the reference level and this level. 
# Note: since the selected models have linear exposure-response curves
# this could be much more simple, 
# but making a big exposure contrast table 
# makes it much easier to deal with any changes to analysis, e.g., reporting a different effect estimate
expContrasts <- data.frame(  
  CounterfactualTemp = c(seq(HourlyTemp.min, HourlyTemp.max, length.out = 100), 
                         quantile(obsHourlyTemp, 0.01, type = 1), quantile(obsHourlyTemp, 0.99, type = 1), 
                         quantile(obsHourlyTemp, 0.05, type = 1), quantile(obsHourlyTemp, 0.95, type = 1), 
                         quantile(obsHourlyTemp, 0.10, type = 1), quantile(obsHourlyTemp, 0.90, type = 1),  
                         quantile(obsHourlyTemp, 0.15, type = 1), quantile(obsHourlyTemp, 0.85, type = 1),
                         quantile(obsHourlyTemp, 0.20, type = 1), quantile(obsHourlyTemp, 0.80, type = 1), 
                         quantile(obsHourlyTemp, 0.25, type = 1), quantile(obsHourlyTemp, 0.75, type = 1), 
                         HourlyTemp.mean - HourlyTemp.sd,  HourlyTemp.mean + HourlyTemp.sd, 
                         HourlyTemp.mean - 10,  HourlyTemp.mean + 10),
  Label = c(rep('ERValues', 100), 'per01','per99', 'per05', 'per95', 'per10', 'per90',
            'per15', 'per85', 'per20', 'per80', 'per25', 'per75', 'MeanMinusSD', 'MeanPlusSD', 
            'MeanMinus10', 'MeanPlus10')) %>% 
  mutate(CounterfactualTemp = round(CounterfactualTemp, 7))

# 5f Clean up 
rm(obsHourlyTemp)

####*************************
#### 6: Source Functions #### 
####*************************

# 6a Get the names of all of the scripts that are just functions
myFunctions <- list.files(path = here::here('HourlyTemp_Stroke', 'Scripts', 'Functions'))

# 6b Define function to run sources 
source_myFunction <- function(FunctionName){
  source(here::here('HourlyTemp_Stroke', 'Scripts', 'Functions', FunctionName))
}

# 6c Source all the function scripts
# we don't actually need the assignment, it just removes annoying 
# output generated by the sourcing code. 
# since we are just sourcing these, we can just use map. 
a <- map(myFunctions, source_myFunction)

# 6d Tell the analyst that the script has finished
cat('completed 0_01 at ', paste(Sys.time()), 
    '\n total time: ', round(Sys.time() - StartTime_0_01, 1), ' min')

rm(StartTime_0_01)