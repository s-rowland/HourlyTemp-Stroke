# Function: fit health model with second-lowest AIC
# HourlyTemp-Stroke Analysis
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

####********************
#### 0: Preparation #### 
####********************

####*******************************************************************
#### 1: Make Function to Fit a Model with the Selected Constraints #### 
####*******************************************************************

# 1a Start function
identify_subOptimalConstraints_fitModel <- function(ExpTerm, CaseType, SubPopVar, SubPop){
  
  # 1b Create ModelIdentifiers to identify the main models 
  ModelIdentifier <- paste0(ExpTerm, '_', CaseType)
  
  # 1c Keep only the AIC of models with our ModelIdentifier of interest
  aic.table <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                   'Model_AICweights.csv'), col_types = 'cccddT') %>% 
    filter(ModelIdentifier == !!ModelIdentifier)
  
  # 1d Choose model with second-lowest AIC 
  aic.table <- aic.table %>% arrange(AIC)
  SelectedModel <- aic.table[2,]

  # 1e Identify appropriate model function
    analyze_model <- analyze_dlnmTemp 
  
  # 1f Fit and store selected model
  analyze_model(ExpTerm, CaseType, 'altConstraints', 
                SelectedModel$ERConstraint[1], SelectedModel$LRConstraint[1], 
                SubPopVar, SubPop, 'SaveModel')
}