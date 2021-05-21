# Identify the relevant windows for the hourlytemp-stroke association
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Define Function to Identify Relevant Lags
# 2: Identify the Relevant Lags

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('Ran_analysis_0_01')){
  here::i_am('README.md')
  source(here::here('HourlyTemp_Stroke', 'Scripts',
                    '0_01_setOutcomeName_folders_packages_data.R'))}

####**************************************************
#### 1: Define Function to Identify Relevant Lags ####
####**************************************************

# 1a Name function to identify the lag
Get_MaxSig_Lag <- function(CaseType){
  
  # 1b Readin Estimates
  est.table <- readin_selectedEstimates('HourlyTemp', CaseType, 'Main', 'fullpop', 'fullpop', 'EstInd')
  
  # 1c Keep only relevant exposure contrast
  est.table <- est.table %>% 
    filter(Label == 'MeanPlus10')
  
  # 1d Wrangle estimates 
  est.table <- est.table %>% 
    dplyr::select(-Sensitivity, -CounterfactualTemp, -Label, -CaseType, -SubPopVar, -SubPop, -IndCumul) %>%
    gather('LagName', 'Estimate' ) %>% 
    mutate(VarName = str_sub(LagName, 1, 3), Lag = as.numeric(str_sub(LagName, 11))) %>%  
    dplyr::select(-LagName) %>%
    spread(VarName, Estimate) %>% 
    mutate(fit.pc = convert_to_percent(fit), 
           lci.pc = convert_to_percent(lci), uci.pc = convert_to_percent(uci)) 
  
  # 1e Keep only whole number lags 
  est.table <- est.table %>% 
    mutate(Lag = as.character((Lag))) %>%
    filter(!str_detect(Lag, '\\.')) %>% 
    mutate(Lag = as.numeric(Lag))
  
  # 1f Identify the most proximate period of association 
  if(est.table$fit[1]>1){
    est.table <- est.table %>% 
      mutate(Period = cummin(lci)) %>% 
      filter(Period > 1)
  }
  if(est.table$fit[1]<1){
    est.table <- est.table %>% 
      mutate(Period = cummax(uci)) %>% 
      filter(Period < 1)
  }
  
  # 1g Identify the most distal lag with significant association 
  max(est.table$Lag)
}

####***********************************
#### 2: Identify the Relevant Lags ####
####***********************************

# 2a Readin selected models table 
SelectedModels <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath,'Tables', 
                                      'SelectedModels.csv'))

# 2b Actually identify max lags 
# You will get a warning saying 'Missing column names filled in: 'X1' [1]' 
# This is not a problem 
# Sometimes excel will create extra columns in csv's 
# we will also get the information about how the columns were parsed. 
if(CaseName != 'fake'){
  MaxSigLag.isc <- Get_MaxSig_Lag('strokeISC')
  MaxSigLag.hem <- Get_MaxSig_Lag('strokeHEM')
}
if(CaseName == 'fake'){
  MaxSigLag.isc <- 3
  MaxSigLag.hem <- 4
}

# 2c Save results 
data.frame(CaseType = c('strokeISC', 'strokeHEM'), 
             relevantWindows = c(MaxSigLag.isc, MaxSigLag.hem)) %>% 
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 'relevantWindows.csv'))
