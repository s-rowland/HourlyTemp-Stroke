# Create Folder Structure
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# D: Description
# 0: Preparation 
# 1: Set OutcomeName 
# 2: Load Packages
# 3: Create Folder Structure

####********************
#### D: Description ####
####********************

# Here we create the folder structure for the rest of the analyses 

####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
library(pacman)

####************************
#### 1: Set OutcomeName #### 
####************************

# 0a Set outcomename 
# this is where you tell the script that you are using fake data. 
OutcomeName <- 'stroke'
#OutcomeName <- 'fake'

# 0c Add marker 
Ran_dataprep_0_01 <- 'Ran_dataprep_0_01'

####**********************
#### 2: Load Packages #### 
####**********************

# 2a Load packages 
# Install if necesary
p_load(tidyverse,lubridate,magrittr,  # tidyverse packages
       fst, muStat,
       furrr, future, progress, progressr, # efficiency/paralleization packages
       rgdal, sf, lwgeom, dismo, deldir) # spatial packages

####********************************
#### 3: Create Folder Structure #### 
####********************************

# 3a Declare directory names
project.folder <- paste0(print(here::here()),'/')
  dataprep.folder <- paste0(project.folder,'DataPrep/')
    data.folder <- paste0(dataprep.folder, 'Data/')
      raw.data.folder          <- paste0(data.folder, 'Raw_Data/')
        raw.outcome.folder       <- paste0(raw.data.folder, 'Outcome_Data/')
        raw.nldas.folder         <- paste0(raw.data.folder, 'NLDAS_raw/')
      intermediate.data.folder <- paste0(data.folder, 'Intermediate_Data/')
        nldas.vor.folder <- paste0(intermediate.data.folder, 'NLDAS_VOR/')
        aggregated.nldas.folder <- paste0(intermediate.data.folder, 'NLDAS_ZCTA_Data_PopWeighted/')
        nyc.nldas.folder <- paste0(intermediate.data.folder, 'NLDAS_NYC_Data_PopWeighted/')
        weather.long.folder <- paste0(intermediate.data.folder, 'Weather_Data_Long/')
      final.data.folder        <- paste0(data.folder, 'Final_Data/')
    scripts.folder  <- paste0(dataprep.folder, 'Scripts/')
      functions.folder  <- paste0(scripts.folder, 'Functions/')

# 3b Create list of folder locations which was created above
folder.names <- grep('.folder',names(.GlobalEnv),value=TRUE)

# 3c Create function to create list of folders
# note that the function will not create a folder if it already exists 
create_folders <- function(name){
  ifelse(!dir.exists(get(name)), dir.create(get(name), recursive=TRUE), FALSE)
}

# 3d Create the folders named above
lapply(folder.names, create_folders)

# 3e Clean up 
rm(list=ls(pattern='folder'))
