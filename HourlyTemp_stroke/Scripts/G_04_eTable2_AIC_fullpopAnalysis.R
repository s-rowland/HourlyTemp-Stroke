# Create eTable1: AIC table 
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
  source(here::here('HourlyTemp_Stroke', 'Scripts','G_01_set_PlottingObjects.R'))
}

####*********************
#### 1: Create Table ####
####*********************

# 1a Readin table of model AICs  
aic.table <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 
                                  'Model_AICweights.csv'))

# 1b Keep only AIC of main models 
aic.table <- aic.table %>%
  filter(str_detect(ModelIdentifier, 'HourlyTemp')) %>% 
  mutate(Outcome = str_sub(ModelIdentifier, 12)) %>% 
  mutate(Outcome = if_else(Outcome == 'strokeISC', 'Ischemic Stroke', 'Hemorrhagic Stroke')) %>% 
  mutate(Outcome = factor(Outcome, levels=c('Ischemic Stroke', 'Hemorrhagic Stroke'))) %>%
  mutate(ERConstraint = if_else(ERConstraint == 'lin', 'Linear', paste0('ns ', str_sub(ERConstraint, 0, 1), ' df')), 
         LRConstraint = paste0('ns ', str_sub(LRConstraint, 0, 1), ' df')) %>% 
  dplyr::select(Outcome, ERConstraint, LRConstraint, AIC, AkaikeWeight)

# 1c Save table 
aic.table %>% 
  mutate(ERConstraint = factor(ERConstraint, levels = c('Linear', 'ns 3 df', 'ns 4 df','ns 5 df'))) %>% 
  arrange(ERConstraint) %>% 
  arrange(Outcome) %>%
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript',
            'eTable1_AIC_fullPopAnalysis.csv'))
