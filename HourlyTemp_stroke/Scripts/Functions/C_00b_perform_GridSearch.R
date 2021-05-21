# Function: Perform grid search for exposure crossbasis constraints
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Begin Function
# 2: Get AIC of Candidate Models
# 3: Calcluate AIC Weights
# 4: Select Model 

####**************
#### N: Notes #### 
####**************

# Na Description
# Grid search for any dlnm model 

####********************
#### 0: Preparation #### 
####********************

####***********************
#### 1: Begin Function ####
####***********************

# 1a Name function
perform_gridSearch <- function(ExpTerm, CaseType, Sensitivity, SubPopVar, SubPop){
  # ExpTerm <- 'HourlyTemp'; CaseType <- 'strokeISC'; Sensitivity <- 'Main'
  # SubPopVar <- 'fullpop'; SubPop <- 'fullpop'
  
  # 1b Identify appropriate model and constraint grid
  # note that E_00 is only used in exploratory analyses not included in the manuscript 
  # we only look at 1-hr averages in the manuscript
  if(str_detect(Sensitivity, 'MeanT')){ 
    analyze_model <- analyze_meanTemp
    CandidateConstraintsGrid <- data.frame(
      ERConstraint = c('3dfevenknots','4dfevenknots', '5dfevenknots'), 
      LRConstraint = c('NA', 'NA', 'NA'))
  } else {
    analyze_model <- analyze_dlnmTemp
    CandidateConstraintsGrid <- data.frame(
      ERConstraint = rep(c('lin', '3dfevenknots','4dfevenknots', '5dfevenknots'), 3), 
      LRConstraint = c(rep('3dfevenknots',4), rep('4dfevenknots',4), rep('5dfevenknots',4)))
  }
  
  ####************************************
  #### 2: Get AIC of Candidate Models ####
  ####************************************
  
  # 2a Initialize loop over grid cells
  for(i in 1:nrow(CandidateConstraintsGrid)){
    # i <- 1
    # 2b Fit model with candidate constraints
    analyze_model(ExpTerm, CaseType, Sensitivity, 
                  CandidateConstraintsGrid$ERConstraint[i], CandidateConstraintsGrid$LRConstraint[i], 
                  SubPopVar, SubPop, 'StoreAIC')
  }
  
  ####******************************
  #### 3: Calcluate AIC Weights ####
  ####******************************
  
  # 3a Create ModelIdentifier
  ModelIdentifier <- paste0(ExpTerm, '_', CaseType)
  
  # 3b Readin table of model AICs  
  aic.table0 <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                    'Model_AIC.csv'), 
                         col_types = 'cccdT')
  
  # 3c Keep only models of this model identifier
  aic.table <- aic.table0 %>% filter(ModelIdentifier == !!ModelIdentifier)
  
  # 3d Find minAIC
  AIC.min <- min(aic.table$AIC)
  
  # 3e Calculate deltaAIC 
  aic.table <- aic.table  %>% 
      mutate(deltaAIC = as.numeric(AIC) - as.numeric(AIC.min))
  
  # 3f Calculate weight denominator
  denom.aic <- sum(exp(-0.5 * aic.table$deltaAIC)) 
    
  # 3g Calculate AIC weights 
  aic.table$AkaikeWeight <- exp(-0.5*aic.table$deltaAIC) / denom.aic
    
  # 3h Put weights in pretty format
  aic.table$AkaikeWeight <- round(100*aic.table$AkaikeWeight, 1)
    
  # 3i Add to previous AIC table
  aicWeights.table <- aic.table %>% 
      dplyr::select(ModelIdentifier, ERConstraint, LRConstraint, AIC, AkaikeWeight, RunDate) %>%
     bind_rows(read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                   'Model_AICweights.csv'), 
                        col_types = 'cccddT'))
  
  # 3j Keep only the most recent versions of each model and save 
  aicWeights.table %>% 
      group_by(ModelIdentifier,ERConstraint, LRConstraint) %>% 
      arrange(desc(RunDate)) %>% 
      slice(0:1) %>% 
      filter(!is.na(ModelIdentifier)) %>%
      write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 'Model_AICweights.csv'))
    
  ####*********************
  #### 4: Select Model ####
  ####*********************
  
  # 4a Identify selected model for this population, casetype
  SelectedModel <- aic.table %>% 
      filter(AIC == AIC.min) %>% 
      dplyr::select(ModelIdentifier, ERConstraint, LRConstraint, RunDate) 
  
  # 4b Add to previous selected models 
  SelectedModels.table <- SelectedModel %>% 
    bind_rows(read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                  'SelectedModels.csv'),  col_types = 'cccT')) 
  
  # 4c Keep only the AIC of the most recent versions of each model and save 
  SelectedModels.table %>%
    group_by(ModelIdentifier) %>% 
    arrange(desc(RunDate)) %>% 
    slice(0:1) %>% 
    filter(!is.na(ModelIdentifier)) %>%
    write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                         'SelectedModels.csv'))
}