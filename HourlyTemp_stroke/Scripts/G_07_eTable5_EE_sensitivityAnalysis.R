# Create eTable2: effect estimates of sensitivity analyses 
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
  source(here::here('HourlyTemp_Stroke/Scripts',
                    '0_01_setOutcomeName_folders_packages_data.R'))
}

# 0b Create the plotting objects, if you haven't already
if (!exists('HH.fig')){
  source(here::here('HourlyTemp_Stroke/Scripts', 'G_01_set_PlottingObjects.R'))
}

####*********************
#### 1: Create Table ####
####*********************

# 1a Setup table of models to tabulate
SensitivityList <- rep(NameArray$SensitivityCode, 2)
ExpTermList <- rep(c('HourlyTemp', 'HourlyTemp', 'HourlyTemp', 'HourlyTemp', 'HourlyTemp_24lag', 'HourlyTemp_48lag'), 2)
CaseTypeList <- c(rep('strokeISC', length(SensitivityList)/2), rep('strokeHEM', length(SensitivityList)/2))
SubPopVarList <- rep('fullpop', length(SensitivityList))
SubPopList <- rep('fullpop', length(SensitivityList))
IndCumulList <- rep('EstCumul', length(SensitivityList))

# 1b Readin estimates 
est.list <- purrr::pmap(list(ExpTermList, CaseTypeList, SensitivityList, SubPopVarList, SubPopList, IndCumulList), 
                        readin_selectedEstimates)
est.table <- bind_rows(est.list)

# 1c Keep only the exposure contrasts of interest 
est.table <- est.table %>% 
  filter(Label %in% c('MeanPlus10')) 

# 1d As usual, wrangle the estimates
est.table  <- est.table %>% 
  dplyr::select(-CounterfactualTemp, -Label, -SubPopVar, -SubPop, -IndCumul) %>% 
  gather('Lag', 'Estimate', -CaseType,-Sensitivity) %>% 
  mutate(VarName = str_sub(Lag, 1, 3), Lag = as.numeric(str_sub(Lag, 11))) %>%  
  spread(VarName, Estimate) %>% 
  mutate(Lag = as.character(str_pad(Lag, 2, 'left', '0')))%>% 
  mutate(fit.pc = convert_to_percent(fit), 
         lci.pc = convert_to_percent(lci), 
         uci.pc = convert_to_percent(uci))

# 1f Save one version of table
est.table %>% 
  arrange(desc(CaseType)) %>% 
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                       'EE_cumulative_sensitivtyAnalysis.csv'))

# 1g Put table in a tidy format, with 3 columns
est.table <- est.table %>% 
  mutate(EE = paste0(format(round(fit.pc, 1), nsmall = 1), ' (', 
                     format(round(lci.pc, 1), nsmall = 1), ', ', 
                     format(round(uci.pc, 1), nsmall = 1), ')')) %>%
  arrange(Sensitivity) %>% 
  arrange(Lag) %>% 
  arrange(desc(CaseType)) %>% 
  filter(Lag == '00' | Lag == '35' | 
           (CaseType=='strokeISC' & Lag == str_pad(MaxSigLag.ISC, 2, 'left', '0')) | 
           (CaseType=='strokeHEM' & Lag == str_pad(MaxSigLag.HEM, 2, 'left', '0'))) %>% 
  mutate(EXPName = paste0(CaseType, '_', Lag)) %>%
  dplyr::select(-fit, -lci, -uci, -fit.pc, -lci.pc, -uci.pc, -CaseType, - Lag) %>% 
  spread(EXPName, EE)

# 1h Set order of sensitivityanalyses
est.table <- est.table %>% 
  mutate(Sensitivity = factor(Sensitivity, levels = NameArray$SensitivityCode))

# 1i Save table
est.table %>% 
  arrange(Sensitivity) %>%
  dplyr::select(Sensitivity, contains('ISC'), contains('HEM') ) %>%
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript', 
                       'etable5_EE_sensitivityAnalysis.csv'))
