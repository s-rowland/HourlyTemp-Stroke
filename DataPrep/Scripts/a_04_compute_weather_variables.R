# Compute Weather Variables
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Organize
# 2: Declare Function to Compute Weather Variables
# 3: Compute Weather Variables

####**************
#### N: Notes #### 
####**************

# Na Description
# NLDAS does not report relative humidity
# so we must calculate it from the temperature, pressure and specific humidity
# Additionally, temperature is reported in Kelvin and so we convert it to Celcius. 
# This script calculates the weather parameters for each zcta-hour combination. 
# After this step, the weather data is ready to be joined to the health data
# approximate runtime: <30 min for the full dataset (1994-2016)

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the script is beginning 
StartTime_a_04 <- Sys.time()
print(paste('begin a_04 at', StartTime_a_04))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep', 'Scripts', 
                    '0_01_set_outcomeName_packages_folders.R'))
}

# 0c Set type of averaging
# Options are 'Pop' and 'Area'
# For HourlyTemp-MI analysis, all analyses used area-weighted averaging 
# all subsequent analyses, including TempVar_MI, use population-weighted averaging
NLDASWeight <- 'Pop' # 'Area' 

####*****************
#### 1: Organize #### 
####*****************

# 1a Set active years 
if(OutcomeName == 'fake'){
  ActiveYearListA <- c(1999, 2000)
  ActiveYearListB <- c(2001)
  } else {
  ActiveYearListA <- c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 
                       2001, 2002,2003, 2004, 2005) 
  ActiveYearListB <- c(2006,2007, 2008, 2009, 2010, 2011, 2012,
                      2013, 2014, 2015, 2016)
}

# 1b Declare RH calculator function
##' Convert specific humidity to relative humidity
##'
##' converting specific humidity into relative humidity
##' NCEP surface flux data does not have RH
##' from Bolton 1980 The computation of Equivalent Potential Temperature 
##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @title qair2rh
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @export
##' @author David LeBauer
qair2rh <- function(qair, temp, press = 1013.25){
  es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}

####******************************************************
#### 2: Declare Function to Compute Weather Variables #### 
####******************************************************

# 2a Declare function
compute_wea <- function(ActiveYearNumber){
  
  # 2b Add progress bar 
  p()

  # 2c Set the ActiveYYY
  ActiveYYYY <- ActiveYearNumber
  
  # 2d Readin weatherdata
  # this should have 1794 obs by 8761 variables 
  # If ActiveYYYY is a leap year, dim are 1794 by 8785 (24 more hours)
  ActiveWeaVar <- 'PRES'
  
  presdf <- fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data', 
                                     'NLDAS_ZCTA_Data_PopWeighted', 
                                 paste0('zcta_', ActiveYYYY, '_', ActiveWeaVar, '.fst')))
  ActiveWeaVar <- 'SPFH'
  spfhdf <- fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data', 
                                     'NLDAS_ZCTA_Data_PopWeighted',
                                     paste0('zcta_', ActiveYYYY, '_', ActiveWeaVar, '.fst')))
  ActiveWeaVar <- 'TMP'
  tempdf <- fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data', 
                                     'NLDAS_ZCTA_Data_PopWeighted',
                                     paste0('zcta_', ActiveYYYY, '_', ActiveWeaVar, '.fst')))

  # 2e Put data in long format 
  presdf.L <- presdf %>% 
    gather('DateHour', 'pres', -zcta)
  spfhdf.L <- spfhdf %>% 
    gather('DateHour', 'spfh', -zcta) %>% 
    dplyr::select(-zcta, -DateHour) 
  tempdf.L <- tempdf %>% 
    gather('DateHour', 'temp', -zcta) %>% 
    dplyr::select(-zcta, -DateHour)
  
  # 2f Combine weather variables
   wea.L <- presdf.L %>% 
     bind_cols(spfhdf.L, tempdf.L) %>% 
     dplyr::select(zcta, DateHour, pres, spfh, temp)

  # 2g Convert units
  wea.L$temp_c <- wea.L$temp - 273.15
  wea.L$pres_mb <- wea.L$pres * 0.01
  
  # 2h Calculate relative humidity 
  wea.L <- wea.L %>% mutate(rh = qair2rh(spfh, temp_c, pres_mb))
  
  # 2i Keep only variables of interest 
  wea.L <- wea.L %>% 
    dplyr::select(zcta, DateHour, rh, temp_c) %>% 
    rename(temp = temp_c)

  # 2j Compute HourIndex 
  # Sometime excel etc will mess up dates 
  # so by imposing a common numeric index we can avoid this issue. 
  # also helps us avoid time zone and other time-related issues.
  wea.L <- wea.L %>% 
    mutate(DateHour = str_sub(DateHour, 0, 11)) %>%
    mutate(DateHourTime = parse_date_time(DateHour, 'ymd H', tz = 'UTC')) %>% 
    mutate(EHourIndex0 = as.duration(interval(parse_date_time('1990/01/01 00:00:00', 
                                'ymd HMS', tz='America/New_York'), DateHourTime))) %>%
    mutate(HourIndex = as.numeric(EHourIndex0, 'hours')) %>% 
    dplyr::select(zcta, HourIndex, rh, temp)

  # 2k Write final dataset
  # this dataset should have 15715440 obs of 4 var
  # for a leap year, dim should be 15758496 obs of 4 var
  wea.L %>% 
    fst::write_fst(here::here('DataPrep', 'Data', 'Intermediate_Data', 
                              'Weather_Data_Long', 
                              paste0('weather_long_', NLDASWeight, '_', 
                                         ActiveYYYY, '.fst')))
  
  # 2l Clean environment
  rm(list = ls(pattern = '^pres'))
  rm(list = ls(pattern = '^spfh'))
  rm(list = ls(pattern = '^temp'))
}

####**********************************
#### 3: Compute Weather Variables ####
####**********************************
 
# 3a Compute weather variables for all active years

# 3a.i Set up future
future::plan(multisession)

# 3a.ii Actually run the function, in parallel, with a progress bar
# we split into two lists because the computer memory could not handle doign all the lists at once. 
with_progress({
  p <- progressor(steps = length(ActiveYearListA)*2)
  
  result <- future_map(ActiveYearListA, compute_wea,
                       .options = furrr_options(seed = TRUE))
})
with_progress({
  p <- progressor(steps = length(ActiveYearListB)*2)
  
  result <- future_map(ActiveYearListB, compute_wea, 
                       .options = furrr_options(seed = TRUE))
})

# 3b Tell the analyst that the script is done
cat('completed a_04 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_a_04), 1), ' min')

rm(StartTime_a_04)