# Function: Fit final model, using selected constraints
# HourlyTemps-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Make Function to Fit a Model with the Selected Constraints

####**************
#### N: Notes #### 
####**************

# Na Description
# Grid search for any dlnm model 

####********************
#### 0: Preparation #### 
####********************

####*******************************************************************
#### 1: Make Function to Fit a Model with the Selected Constraints #### 
####*******************************************************************

# 1a Begin function  
identifySelectedConstraints_fitModel <- function(ExpTerm, CaseType, Sensitivity, SubPopVar, SubPop){
  #ExpTerm <- 'HourlyTemp'; Sensitivity <- 'Main';CaseType<-'strokeHEM'; 
  # SubPopVar <- 'fullpop'; SubPop <- 'fullpop'
  
  # 1b Create identification of the main model for that exposure term 
  ModelIdentifier <- paste0(ExpTerm, '_', CaseType)
  
  # 1c Readin dataset and keep just the constraints for our main model for this exposure-outcome combination
  SelectedModel <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                       'SelectedModels.csv')) %>% 
    filter(ModelIdentifier == !!ModelIdentifier)
  
  # 1d Identify appropriate model function
  if(str_detect(Sensitivity, 'MeanTFunctions')){ 
    analyze_model <- analyze_meanTemp       
  } else {analyze_model <- analyze_dlnmTemp }
  
  # 1e Fit and store selected model
  analyze_model(ExpTerm, CaseType, Sensitivity, 
                SelectedModel$ERConstraint[1], SelectedModel$LRConstraint[1], 
                SubPopVar, SubPop, 'SaveModel')
}