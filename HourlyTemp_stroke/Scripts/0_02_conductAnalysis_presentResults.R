# Run entire project
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Conduct Analysis
# 2: Present Results 

####**************
#### N: Notes ####
####**************

# This is a master script, to run everything. 

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the analysis is beginning 
StartTime_0_02 <- Sys.time()
print(paste('begin 0_02 at', StartTime_0_02))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_analysis_0_01')){
  here::i_am('README.md')
  source(here::here('HourlyTemp_Stroke', 'Scripts',
                    '0_01_setOutcomeName_folders_packages_data.R'))
}

####*************************
#### 1: Conduct Analysis #### 
####*************************

# 1a Fit main analyses 
source(here::here('HourlyTemp_Stroke', 'Scripts', 'C_01_fit_mainModels.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'C_02_identify_relevantWindows.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'C_03_assess_EMM_byHTN.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'C_04_assess_EMM_byAfib.R'))

# 1b Fit sensitivity analyses 
source(here::here('HourlyTemp_Stroke', 'Scripts', 'D_01_fit_sensitivityModels.R'))

# 1c Fit secondary analyses 
source(here::here('HourlyTemp_Stroke', 'Scripts', 'E_01_fit_emmAfib.R'))

####************************
#### 2: Present Results #### 
####************************

# 2a Set Plotting features
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_01_set_plottingObjects.R'))

# 2b Figures and tables for manuscript
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_02_Table1_TableOne.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_03_Fig1and2_fullpopLR_htnLR.R'))

# 2c Figures and tables for supplement
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_04_eTable2_AIC_fullpopAnalysis.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_05_eTable3_EE_fullpopAnalysis.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_06_eTable4_EE_stratifiedAnalysis.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_07_eTable5_EE_sensitivityAnalysis.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_08_eFig1_timingDistribution_bySubType.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_09_eFig2_fullpopAnalysis_LRCumulative.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_10_eFig3and8_htnLR.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_11_eFig4and6_sensitivityAnalyses_forestPlot.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_12_eFig5and7_selectedSensitivityAnalyses_LR.R'))
source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_13_Manuscript_calculateNumbers.R'))

# 2d Tell the analyst that the analysis is done
cat('completed 0_02 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_0_02)/60, 1), ' min')

rm(StartTime_0_02)