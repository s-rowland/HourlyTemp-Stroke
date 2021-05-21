# Function: Readin Effect Estimates from selected models
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# D: Descripton
# 1: Create Function

####********************
#### D: Description ####
####********************

# This function allows us to pull in the effect estimates for just 
# the model with the selected constraints

####************************
#### 1: Create Function ####
####************************

# 1a Name function
readin_selectedEstimates <- function(ExpTerm, CaseType, Sensitivity, SubPopVar, SubPop, IndCumul){
  # ExpTerm <- 'HourlyTemp_Afib'; CaseType <- 'strokeISC'; Sensitivity <- 'Main'
  # SubPopVar <- 'Afib'; SubPop <- 'Afib'; IndCumul <- 'EstInd'
  
  # 1b Create ModelIdentifiers
  ModelIdentifier <- paste0(ExpTerm, '_', CaseType)
  
  # 1c Extract the selected exposure terms 
  ERConstraint <- SelectedModels$ERConstraint[SelectedModels$ModelIdentifier == ModelIdentifier]
  LRConstraint <- SelectedModels$LRConstraint[SelectedModels$ModelIdentifier == ModelIdentifier]
  ExpConstraints <- paste0('ER', ERConstraint, '_LR', LRConstraint)
  
  # 1d Create ModelNames 
  ModelName <- paste0(ModelIdentifier,'_', Sensitivity, '_', ExpConstraints, '_', SubPopVar, '_', SubPop)
  if(Sensitivity == 'altConstraints'|Sensitivity == 'noLinTerm'){
    SearchString <- paste0('^', IndCumul, '_', ModelIdentifier, '_', Sensitivity)
    ModelName <- list.files(here::here('HourlyTemp_Stroke', OutputsPath, 'Estimates'),
                            SearchString) %>% 
      str_replace(., paste0(IndCumul, '_'), '') %>% 
      str_replace(., '.csv', '') 
  }
  
  # 1e Readin predictions of selected models
  # this line will generate the warning 'Missing column names filled in: 'X1' [1]'
  # this warning is just about some nonsense column, probably made by excel. 
  # since this warning shows up a lot, and it is annoying and uninformative 
  # I suppress it here. 
  suppressWarnings(
  est.table <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Estimates',
                                   paste0(IndCumul, '_', ModelName, '.csv'))) %>% 
    dplyr::select(-X1) %>% 
    mutate(Sensitivity = !!Sensitivity, CaseType = !!CaseType, 
           SubPopVar = !!SubPopVar, SubPop = !!SubPop, IndCumul = !!IndCumul) 
  )
}
