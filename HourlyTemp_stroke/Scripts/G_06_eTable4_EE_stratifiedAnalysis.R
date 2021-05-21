# Create eTable 4: effect estimates of stratified analyses 
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Table

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('Ran_analysis_0_01')){
  here::i_am('README.md')
  source(here::here('HourlyTemp_Stroke', 'Scripts',
                    '0_01_setOutcomeName_folders_packages_data.R'))
}

# 0b Create the plotting objects, if you haven't already
if (!exists('HH.fig')){
  source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_01_set_PlottingObjects.R'))
}

####*********************
#### 1: Create Table ####
####*********************

# 1a Setup table of models to tabulate
#SensitivityList <- rep('Main', 4)
#ExpTermList <- rep(c('HourlyTemp'), 4)
#CaseTypeList <- c(rep('strokeISC', 2), rep('strokeHEM', 2))
#SubPopVarList <- rep(c( 'HTN', 'HTN'), 2)
#SubPopList <- rep(c('HTN', 'noHTN'), 2)
#IndCumulList <- rep('EstCumul', length(SensitivityList))
SensitivityList <- rep('Main', 8)
ExpTermList <- rep(c('HourlyTemp'), 8)
CaseTypeList <- c(rep('strokeISC', 4), rep('strokeHEM', 4))
SubPopVarList <- rep(c( 'HTN', 'HTN', 'Afib', 'Afib'), 2)
SubPopList <- rep(c('HTN', 'noHTN', 'Afib', 'noAfib'), 2)
IndCumulList <- rep('EstCumul', length(SensitivityList))

# 1b Readin estimates 
est.list <- purrr::pmap(list(ExpTermList, CaseTypeList, SensitivityList,
                             SubPopVarList, SubPopList, IndCumulList), 
                        readin_selectedEstimates)
est.table <- bind_rows(est.list)

# 1c Keep only the exposure contrasts of interest 
est.table <- est.table %>% 
  filter(Label %in% c('MeanPlus10')) 

# 1d As usual, wrangle the estimates
est.table  <- est.table %>% 
  dplyr::select(-CounterfactualTemp, -Label, -SubPopVar, -Sensitivity, -IndCumul) %>% 
  gather('Lag', 'Estimate', -CaseType,  -SubPop) %>% 
  mutate(VarName = str_sub(Lag, 1, 3), Lag = as.numeric(str_sub(Lag, 11))) %>%  
  spread(VarName, Estimate) %>% 
  mutate(Lag = as.character(str_pad(Lag, 2, 'left', '0')))%>% 
  mutate(fit.pc = convert_to_percent(fit), 
         lci.pc = convert_to_percent(lci), 
         uci.pc = convert_to_percent(uci))

# 1e Set order of sensitivityanalyses
est.table <- est.table %>% 
  mutate(SubPop = factor(SubPop, levels = c('fullpop', NameArray$HTN)))

# 1f Save one version of table
est.table %>% 
  arrange(desc(CaseType)) %>% 
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                       'EE_cumulative_stratifiedAnalysis.csv'))

# 1g Put table in a tidy format, with 3 columns
est.table <- est.table %>% 
  mutate(EE = paste0(format(round(fit.pc, 1), nsmall = 1), ' (', 
                       format(round(lci.pc, 1), nsmall = 1), ', ', 
                       format(round(uci.pc, 1), nsmall = 1), ')')) %>%
    arrange(SubPop) %>% 
    arrange(Lag) %>% 
    arrange(desc(CaseType)) %>% 
  filter(Lag == '00' | Lag == '35' | 
           (CaseType=='strokeISC' & Lag == str_pad(MaxSigLag.ISC, 2, 'left', '0')) | 
           (CaseType=='strokeHEM' & Lag == str_pad(MaxSigLag.HEM, 2, 'left', '0'))) %>% 
  mutate(EXPName = paste0(CaseType, '_', Lag)) %>%
  dplyr::select(-fit, -lci, -uci, -fit.pc, -lci.pc, -uci.pc, -CaseType, - Lag) %>% 
  spread(EXPName, EE)

# 1h Save table
est.table %>% 
  dplyr::select(SubPop, contains('ISC'), contains('HEM') ) %>%
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript',
                       'etable4_EE_stratifiedAnalysis.csv'))
