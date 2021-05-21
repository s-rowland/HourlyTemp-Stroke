# Set Figure Size
# TempVar-MI Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 09/14/2020

####***********************
#### Table of Contents #### 
####***********************

# D: Descripton
# 0: Preparation
# 1: Set Figure Size
# 2: Create Plot-Wide Objects

####********************
#### D: Description ####
####********************

# By setting common plotting features in a separate script 
# We can ensure consistency across plots 
# It also helps make some choices more explicit

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists("project.folder")){
  source(paste0("HourlyTemp_Stroke/Scripts/", "0_01_create_folder_structure.R"))
}

####************************
#### 1: Set Figure Size ####
####************************

# not all figures are this size, but these are common sizes 
HH.fig <- 1000 
WW.fig <- 1000 
RR.fig <- 150

HH.efig <- 600 
WW.efig <- 600 
RR.efig <- 200

# colors
colList.isc <- colorRampPalette(c("darkolivegreen", "darkolivegreen1"))
colList.hem <- colorRampPalette(c("darkorange4", "tan1"))
col.isc <- colList.isc(5)[3]
col.hem <- colList.hem(5)[3]
colList.TempContrast <- c("deepskyblue2", "tomato2")

# 1a Create plotting theme 
tema <-   theme_classic() +
  theme(plot.title = element_text(size = 36, vjust = 1, hjust =0.5)) +
  theme(axis.title.x = element_text(size = 30, vjust =0, lineheight=0.25)) + 
  theme(axis.title.y = element_text(size = 30, angle = 90, vjust= 0.5)) + 
  theme(axis.text.x = element_text(size = 20)) + 
  theme(axis.text.y = element_text(size = 20, angle = 0))

####*********************************
#### 2: Create Plot-Wide Objects ####
####*********************************

# 2a Declare convert to precent function 
convert_to_percent <- function(x){
  100 * (x - 1)
}

# function to readin estimates 
readin_selectedEstimates <- function(Sensitivity, CaseType, SubPopVar, SubPop, IndCumul){
  # 2A.a Create ModelIdentifiers
  ModelIdentifier <- paste0(Sensitivity, "_", CaseType, "_", SubPopVar, "_", SubPop)
  # 2A.b Extract the selected exposure terms 
  ERConstraint <- SelectedModels$ERConstraint[SelectedModels$ModelIdentifier == ModelIdentifier]
  LRConstraint <- SelectedModels$LRConstraint[SelectedModels$ModelIdentifier == ModelIdentifier]
  ExpTerms <- paste0("_ER", ERConstraint, "_LR", LRConstraint)
  # 2A.c Create ModelNames 
  ModelName <- paste0(ModelIdentifier, ExpTerms)
  # 2A.d Readin predictions of selected models
  est.table <- read_csv(paste0(estimates.folder, IndCumul, "_", ModelName, ".csv")) %>% 
    dplyr::select(-X1) %>% 
    mutate(Sensitivity = !!Sensitivity, CaseType = !!CaseType, 
           SubPopVar = !!SubPopVar, SubPop = !!SubPop)
}

# IndCumul can be "EstInd" or "EstCumul"

####*********************************
#### 3: Create Plot-Wide Objects ####
####*********************************

# 2b Determine 5th and 95th temperature interval 
tempPercentile.5th <- -6
tempPercentile.95th <- 27

# 2c Readin selected models table 
SelectedModels <- read_csv(paste0(tables.folder, "Selected_Models.csv"))