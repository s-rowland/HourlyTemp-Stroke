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
SensitivityList <- rep('Main', 4)
ExpTermList <- rep('HourlyTemp', 4)
CaseTypeList <- rep(c('strokeISC', 'strokeHEM'), 2)
SubPopVarList <- rep('fullpop', 4)
SubPopList <- rep('fullpop', 4)
IndCumulList <- c('EstInd', 'EstInd', 'EstCumul', 'EstCumul')

# 1b Readin estimates 
est.list <- purrr::pmap(list(ExpTermList, CaseTypeList, SensitivityList, SubPopVarList, SubPopList, IndCumulList), 
                        readin_selectedEstimates)
est.table <- bind_rows(est.list)

# 1c Keep only the exposure contrasts of interest 
est.table <- est.table %>% 
  filter(Label %in% c('MeanPlus10')) 

# 1d As usual, wrangle the estimates
est.table  <- est.table %>% 
  dplyr::select(-SubPopVar, -Sensitivity, -CounterfactualTemp, -Label, -SubPop) %>% 
  gather('Lag', 'Estimate', -CaseType, -IndCumul) %>% 
  mutate(VarName = str_sub(Lag, 1, 3), Lag = as.numeric(str_sub(Lag, 11))) %>%  
  filter(!str_detect(Lag, '\\.')) %>%
  spread(VarName, Estimate) %>% 
  mutate(Lag = as.character(str_pad(Lag, 2, 'left', '0')))%>% 
  mutate(fit.pc = convert_to_percent(fit), 
         lci.pc = convert_to_percent(lci), 
         uci.pc = convert_to_percent(uci))

# 1e Save one version of table
est.table %>% 
  arrange(desc(CaseType)) %>% 
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                       'EE_fullpopAnalysis.csv'))

# 1f Put table in a tidy format, with 3 columns
est.table <- est.table %>% 
  mutate(EE = paste0(format(round(fit.pc, 1), nsmall = 1), ' (', 
                     format(round(lci.pc, 1), nsmall = 1), ', ', 
                     format(round(uci.pc, 1), nsmall = 1), ')')) 

# 1g Spread out effect estimates into 4 columns 
est.table <- est.table %>% 
  mutate(EXPName = paste0(CaseType, '_', IndCumul)) %>%
  dplyr::select(EXPName, Lag, EE) %>% 
  spread(EXPName, EE)  

# 1h Save table
est.table %>% 
  dplyr::select(Lag, strokeISC_EstInd, strokeISC_EstCumul, strokeHEM_EstInd, strokeHEM_EstCumul) %>% 
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript',
                       'etable3_EE_fullpopAnalysis.csv'))
