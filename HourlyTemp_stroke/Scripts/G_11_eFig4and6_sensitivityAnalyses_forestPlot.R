# Create eFig4: forest plot of sensitivity analyses
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Table
# 2: Make Function for Plot 
# 3: Create Plots

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

# 1a Setup table of models to plot
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
  dplyr::select(-SubPopVar, -SubPop, -CounterfactualTemp, -Label, -IndCumul) %>% 
  gather('Lag', 'Estimate', -CaseType, -Sensitivity) %>% 
  mutate(VarName = str_sub(Lag, 1, 3), Lag = as.numeric(str_sub(Lag, 11))) %>%  
  spread(VarName, Estimate) %>% 
  mutate(Lag = as.character(str_pad(Lag, 2, 'left', '0'))) %>% 
  mutate(fit.pc = convert_to_percent(fit), 
         lci.pc = convert_to_percent(lci), 
         uci.pc = convert_to_percent(uci))

# 1e Rename variables and set order
est.table <- est.table %>% 
  mutate(         
    Sensitivity = case_when(
      Sensitivity == 'Main' ~ 'Main', 
      Sensitivity == 'RHnoAdj' ~ 'No RH Adjustment',
      Sensitivity == 'RHdlnm' ~ 'DLNM for RH',
      Sensitivity == 'altConstraints' ~ 'Alternative Constraints',
      Sensitivity == '24LagHr' ~ '24 Lag Hours',
      Sensitivity == '48LagHr' ~ '48 Lag Hours'
    )) %>% 
  mutate(Sensitivity = factor(Sensitivity, levels = rev(NameArray$SensitivityManu)))

####*******************************
#### 2: Make Function for Plot ####
####*******************************

# 2a Begin function
plot_forest_sensitivity <- function(CaseType, ActiveLag){
# CaseType <- 'strokeISC'; ActiveLag <- 10
  
  # 2b Restrict by lag 
  est.table <- est.table %>% 
    filter(Lag == ActiveLag)
  
  # 2c Set CI for main model 
  # which we will use to fill in the rectangle background
  main <- est.table %>% 
    filter(Sensitivity == 'Main') %>% 
    filter(CaseType == !!CaseType)
  
  # 2d Set CaseNames
  if(CaseType == 'strokeISC'){CaseName <- 'Ischemic Stroke'; FigNum <- 4}
  if(CaseType == 'strokeHEM'){CaseName <- 'Hemorrhagic Stroke'; FigNum <- 6}
  
  # 2e Create plot
  TP <- est.table %>% 
    filter(CaseType == !!CaseType) %>% 
    ggplot(aes(Sensitivity)) + 
    geom_blank() + 
    geom_rect(aes(xmin = 0, xmax = 0.5+length(NameArray$SensitivityManu)), 
                  ymin = main$lci.pc[1], ymax = main$uci.pc[1], 
                  alpha = 0.01, fill = ColorArray$TempContrast[2]) + 
    geom_hline(yintercept = 0, color = 'grey' ) + 
    geom_point(aes(y = fit.pc), color = ColorArray$TempContrast[2], size = 3, shape = 18) + 
    geom_errorbar(aes(ymin = lci.pc, ymax = uci.pc), color = ColorArray$TempContrast[2] ) + 
    tema +
    theme(legend.title = element_blank()) +
    coord_flip() + 
    labs(y = paste0('Change in Hospitalizations (%)'), 
         x = paste0(' ')) + 
    theme(strip.background = element_blank(), 
          strip.text.x = element_blank())  + 
    theme(axis.text.x = element_text(size = 20), 
          axis.text.y = element_text(size = 18))
  
  # 2f Save plot
  png(here::here('HourlyTemp_stroke', OutputsPath, 'Manuscript',
                 paste0('eFig', FigNum, '_sensitivityAnalysis_forestPLot_',
             CaseType, '.png')), 
      height = HH.fig, width = WW.fig, res = RR.fig)
  print(TP)
  dev.off()

}

####*********************
#### 3: Create Plots ####
####*********************

# 3a Make plots
# here the sensitivity is for the cumulative association 
# based on just the most relevant exposure 
plot_forest_sensitivity('strokeISC',str_pad(MaxSigLag.ISC, 2, 'left', '0'))
plot_forest_sensitivity('strokeHEM',str_pad(MaxSigLag.HEM, 2, 'left', '0'))

