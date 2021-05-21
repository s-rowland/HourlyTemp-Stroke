# Set features for plots and create related objects
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# D: Descripton
# 0: Preparation
# 1: Set Common Plotting Features
# 2: Readin Tables Required for Plotting

####********************
#### D: Description ####
####********************

# By setting common plotting features in a separate script 
# We can ensure consistency across plots 
# It also helps make some choices more explicit 
# eg, this is the script where I set the colors for subgroups

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('Ran_analysis_0_01')){
  here::i_am('README.md')
  source(here::here('HourlyTemp_Stroke', 'Scripts',
                    '0_01_setOutcomeName_folders_packages_data.R'))}

####*************************************
#### 1: Set Common Plotting Features ####
####*************************************

# 1a Set common plot dimensions 
# not all figures are this size, but these are common sizes 
HH.fig <- 1000 
WW.fig <- 1000 
RR.fig <- 150

HH.efig <- 600 
WW.efig <- 600 
RR.efig <- 200

# 1b Create plotting theme 
tema <-   theme_classic() +
  theme(plot.title = element_text(size = 36, vjust = 1, hjust =0.5)) +
  theme(axis.title.x = element_text(size = 16, vjust =0, lineheight=0.25)) + 
  theme(axis.title.y = element_text(size = 16, angle = 90, vjust= 0.5)) + 
  theme(axis.text.x = element_text(size = 12)) + 
  theme(axis.text.y = element_text(size = 12, angle = 0))

# 1c Set individual colors
colList.ISC <- colorRampPalette(c('darkolivegreen', 'darkolivegreen1'))
colList.HEM <- colorRampPalette(c('darkorange4', 'tan1'))
col.cold <- 'deepskyblue2'
col.hot <- 'tomato2'
col.htn <- 'sienna' #'peru' #'burlywood4'
col.nohtn <- 'darkorchid1'

# 1d Set order of colors
# this is used in the scale_manual step of the plot 
ColorArray <- list(
  CaseType = c(colList.ISC(5)[3], colList.HEM(5)[3]),
  Lag = terrain.colors(7),
  TempContrast = c(col.cold, col.hot),
  HTN = c(col.htn, col.nohtn), 
  Afib = c(col.htn, col.nohtn)
)

# 1e Set order of casetypes
NameArray <- list(
  CaseType = c('strokeISC', 'strokeHEM'),
  HTN = c('HTN', 'noHTN'),
  Afib = c('Afib', 'noAFib'),
  SensitivityCode = c('Main', 'RHnoAdj', 'RHdlnm', 'altConstraints',  '24LagHr', '48LagHr'),
  SensitivityManu = c('Main', 'No RH Adjustment','DLNM for RH', 'Alternative Constraints',
                      '24 Lag Hours', '48 Lag Hours')
)

####********************************************
#### 2: Readin Tables Required for Plotting ####
####********************************************

# 2a Readin selected models table 
SelectedModels <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                      'SelectedModels.csv'))

# 2b Readin relevant windows table 
relevantWindows <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                       'relevantWindows.csv'))

# 2c Extract the maximum signficant lags 
MaxSigLag.ISC <- relevantWindows$relevantWindows[relevantWindows$CaseType=='strokeISC']
MaxSigLag.HEM <- relevantWindows$relevantWindows[relevantWindows$CaseType=='strokeHEM']
