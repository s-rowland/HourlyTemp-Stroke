# Convert NLDAS from Grib to CSV format
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation
# 1: Readin All of the Grib Data 
# 2: Split Long Data into Temperature and Humidity

####***************
#### N: Notes  ####
####***************

# Na Description
# From the NLDAS website you can downloand the hourly weather data. 
# However, NLDS will report values via less R-friendly formats 
# (i.e., not csv)
# I downloaded the data in grib format.
# This script converts the data to long format, and saves as a csv

# Nb Directories 
# Since the data was installed from NLDAS site on my personal computer 
# and I have not transferred the individual gribs to this computer 
# The directories are for my personal computer. 

# Nc Downloading process
# I assume that you downloaded each grib file as a individual file - 
# You did not concatenate the files during the terminal command. 
# I also assume you have one folder for each year.

####*********************
#### 0: Preparation  ####
####*********************

# 0.a Declare Directories 
raw.data.folder <- '/Users/sebastianrowland/Desktop/assign_temp_zip/Raw Data/'
intermediate.data.folder <- '~/Desktop/assign_temp_zip/Raw Data/NLDAS_raw'

NLDAS.data.folder <- '~/Desktop/assign_grid_zip2/Data/Raw Data/NLDAS_raw/'

####*************************************
#### 1: Readin All of the Grib  Data #### 
####*************************************

# 1a Make a list of all of the grib files in the year's folder 
for(YYYY in 1999:2016){
  raw.grib.folder <- paste0('~/Desktop/ExtraSteps_Temp/Data/Raw Data/NLDAS_', YYYY)
  setwd(raw.grib.folder)
  flist <- list.files(pattern = 'SUB.grb')
  
  # 1b Define the function to read in the grib data, convert to dataframe, and define timeHour 
  # This strategy is the most robust because it makes the least assumptions about the structure of your data or 
  # how R works. 
  make.grib.hour <- function( i ){
    readGDAL(flist[i]) %>% 
      data.frame(.) %>% 
      mutate(TimeHour := flist[i]) %>%
      mutate(TimeHour = str_sub(TimeHour, start = 19L, end = -19L))
  }
  
  # 1c Initialize the empty, properly named dataframe that you will fill with the grib data
  L <- make.grib.hour(1) %>% sample_frac(0, replace = TRUE)
  
  # 1d Fill in all of the nldas data
  for (i in 1:length(flist)){ 
    L <- make.grib.hour(i) %>% 
      bind_rows(L,.)
  }
  
  # 1d Rename the variables
  L1 <- L %>% 
    rename(temp = band1, spfh = band2, pres = band3) %>%
    mutate(nldas_uid = paste0(x, '_', y)) 
  # we have to make up an nldas uid to keep track interally,
  # but the ultimate join to zip 
  # only references to nldas_uid's within the year. 
  
  ####************************************************
  #### 2: Split Long Data into Weather Parameters ####
  ####************************************************
  
  # 2a Make long  dataframes
  #choose only lat, long, and weather variable
  tempL <- L1 %>% dplyr::select(x,y, nldas_uid, TimeHour, temp )    
  spfhL <- L1 %>% dplyr::select(x,y,  nldas_uid, TimeHour, spfh ) 
  presL <- L1  %>% dplyr::select(x,y,  nldas_uid, TimeHour, pres ) 
  
  # 2b Save data 
  tempL %>% spread(TimeHour, temp) %>% 
    fst::write_fst(paste0(NLDAS.data.folder, YYYY, '_TMP', '_NLDAS_NY.fst')) 
  spfhL %>% spread(TimeHour, spfh) %>% 
    fst::write_fst(paste0(NLDAS.data.folder, YYYY, '_SPFH', '_NLDAS_NY.fst')) 
  presL %>% spread(TimeHour, pres) %>% 
    fst::write_fst(paste0(NLDAS.data.folder, YYYY, '_PRES', '_NLDAS_NY.fst')) 
}