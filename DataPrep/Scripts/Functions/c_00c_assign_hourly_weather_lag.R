# Assign Lagged Weather Variables
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Define Function to Create Lag Hours
# 2: Define Function to Process Lagged Case and Weather Data
 # 2A: Process Outcome Timing Data
 # 2B: Process Weather Data
 # 2C: Split Data
# 3: Define Join Function
# 4: Create Function to Assign Lagged Weather Exposure
# 5: Assign Lagged Hourly Temperature and RH

####**************
#### N: Notes #### 
####**************

# Na Description
# We first assign hourly exposures, 
# up to 72 hours prior to the time of event 
# and then we calculate variability and averages 
# from that hourly data 
# At this step we make the assumption of the timing of the event 
# although we could alternatively modify the datetime of admission in c_1

# Nb Break up by year
# This step is broken up by year because it is computationally intensive
# it is still faster to split by year and then furrr() within year

# Nc separate process from join 
# We first read in and process the data, and output it as an object in the R environment 
# and then do the join
# in order to reduce the required working memory. 
# It sped things up and prevented crashes

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the analysis is beginning 
StartTime_c_00c <- Sys.time()
print(paste('begin c_00c at', StartTime_c_00c))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep/Scripts', '0_01_set_outcomeName_packages_folders.R'))
}

####********************************************
#### 1: Define Function to Create Lag Hours ####
####********************************************

# 1a Define function to create lag hours
make_lag <- function(hours, HR, weather.variable){ 
  VarName <- paste0(substr(weather.variable, 0,1), 'Lag', '_', str_pad(HR,2, pad = '0'))
  hours %>% mutate(!!VarName := DayHourIndex - HR)
} 

####****************************************************************
#### 2: Define Function to Process Lagged Case and Weather Data ####
####****************************************************************

# 2a Name process lags function
process_lags_cases_weather <- function(ActiveYYYY, weather.variable){
  #ActiveYYYY <- '2000'; weather.variable <- 'temp'
  
  ####************************************
  #### 2A: Process Outcome Timing Data ####
  ####************************************
  
  # 2A.a Readin casecontrol data 
  hours <- fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data',  
                                paste0('c_2_casecontrolhours_unassigned_', CaseType, '_', 
                                ActiveYYYY, '_', Timing, '.fst')))

  # 2A.b Keep only variables of interest
  hours <- hours %>% 
    mutate(DayHourIndex = as.numeric(DayHourIndex))
  
  # 2A.c Replicate dataset
  # we will use this dataset to remember the ICD & demographic info of each patient
  hours0 <- hours
  
  # 2A.d Create lags for 72 hours (3 days)
  for(i in 0:71){ 
    hours <- make_lag(hours, i, weather.variable)
  }
  
  # 2A.e Put data in long format via lags 
  hours2 <- hours %>%
    dplyr::select(-DayHourIndex) %>% 
    gather('LagName', 'LagHourIndex', contains('Lag_')) 
  
  # 2A.f Make list of active hourindexes for that year 
  # we will use this to help curate the previous and next year data
  ActiveHI <- hours2 %>% 
    dplyr::select(LagHourIndex) %>% 
    distinct() %>% 
    arrange(LagHourIndex)
  ActiveHI.list <- as.list(ActiveHI$LagHourIndex)
  Hours <- hours2 
  
  # 2A.g Clean environment
  rm(hours2)
  
  ####******************************
  #### 2B: Process Weather Data ####
  ####******************************
  
  # 2B.a Readin current year's weather data 
  wea1 <-  fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data',
                                    'Weather_Data_Long', 
                                    paste0('weather_Long_', NLDASWeight, '_',
                                           ActiveYYYY, '.fst'))) %>% 
    as.data.frame() %>%
    rename(wea := weather.variable) %>% 
    filter(HourIndex %in% ActiveHI.list)%>% 
    dplyr::select(zcta, HourIndex, wea)
  
  # 2B.b Readin previous year's data 
  # 2B.b.i Identify previous year
  PrevYYYY <- as.character(as.numeric(ActiveYYYY) - 1)
  # 2B.b.ii Readin previous years weather
  wea0 <-  fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data',
                                    'Weather_Data_Long',  
                                paste0('weather_Long_', NLDASWeight, '_', 
                                       PrevYYYY, '.fst'))) %>% 
    as.data.frame() %>%
    rename(wea := weather.variable) %>% 
    filter(HourIndex %in% ActiveHI.list) %>% 
    dplyr::select(zcta, HourIndex, wea)
  
  # 2B.c Readin subsequent year's data 
  # due to the difference in timezones, NLDAS is in UTC, which ends before EST
  # so you need the first 4-6 hours of the subsequent year to capture exposure
  # for the very last days of december
  # 2B.c.i Identify subsequent year
  NextYYYY <- as.character(as.numeric(ActiveYYYY) + 1 )
  # 2B.c.ii Readin subsequent year's exposure
  wea2 <-  fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data',
                                    'Weather_Data_Long', 
                                paste0('weather_Long_', NLDASWeight, '_', 
                                       NextYYYY, '.fst'))) %>% 
    as.data.frame() %>%
    rename(wea := weather.variable) %>% 
    filter(HourIndex %in% ActiveHI.list) %>% 
    dplyr::select(zcta, HourIndex, wea)
  
  # 2B.d Keep only the active climate variable 
  wea.L <- bind_rows(wea1, wea0, wea2)
  
  ####********************
  #### 2C: Split Data ####
  ####********************
  
  # 2C.a Clear the environment
  rm(wea0, wea1, wea2)
  
  # 2C.b Prepare hours data 
  h <- Hours %>% 
    mutate(UPID_event_lag = paste0(UPID,'_', CaseHourIndex, '.', HourName, '.', LagName)) %>% 
    dplyr::select(zcta, UPID_event_lag, LagHourIndex) %>% 
    arrange(LagHourIndex)
  
  # 2C.c Prepare weather data
  w <- wea.L %>% 
    mutate(zcta = as.character(zcta)) %>% 
    arrange(HourIndex) 
  
  # 2C.d Split data into list 
  Hlist <- split(h, h$LagHourIndex)
  Wlist <- split(w, w$HourIndex)
  
  # 2C.e Double check that split was successful
  if (length(Hlist) != length(Wlist))  stop('check w and h size')
  
  # 2C.f Output the three lists
  list(Hlist, Wlist, hours0)
}

####*****************************
#### 3: Define Join Function ####
####*****************************

# 3a Define our function to join by zcta 
# the special function drops variables we do not need 
# and by making it a function we can do it with future_map2
join_by_zcta <- function(hours.df, exp.df) { 
  hours.df %>% 
    dplyr::select(-LagHourIndex) %>%
    left_join(exp.df, by = 'zcta') %>%
    dplyr::select(-HourIndex) 
}

####**********************************************************
#### 4: Create Function to Assign Lagged Weather Exposure ####
####**********************************************************

# 4a Name function to assign lagged weather
assign_lagged_weather <- function(YYYY, weather.variable){
  #YYYY <- 1995; weather.variable <- 'temp'
  
  # 4b Add progress bar 
  pb$tick()
  
  # 4c Create the lags for each hour
  a.ls <- process_lags_cases_weather(YYYY, weather.variable)
  Hlist <- a.ls[[1]]
  Wlist <- a.ls[[2]]
  hours0 <- a.ls[[3]]
  
  # 4d Join by zcta for the active Year
  DGK <- future_map2(Hlist, Wlist, join_by_zcta)
  
  # 4e Combine the hour-specific lists into a year-long list
  dta <- do.call('rbind', DGK)
  
  # 4f Clean dataframe
  dta1 <- dta %>% 
    separate(UPID_event_lag, sep = '\\.', c('UPID_event', 'HourName', 'LagName')) %>%
    mutate(HourName0 = paste0(UPID_event, '_', HourName)) %>%
    dplyr::select(-UPID_event)
  DGK.wide <- tidyr::spread(dta1, LagName, wea)
  
  # 4g Combine weather with demographic data 
  # and save results
  hours0 <- hours0 %>% 
    mutate(HourName0 = paste0(UPID,'_', CaseHourIndex, '_', HourName)) %>% 
    dplyr::select(-zcta, -HourName) %>%
    inner_join(DGK.wide, by = 'HourName0') %>% 
    fst::write_fst(here::here('DataPrep', 'Data', 'Intermediate_Data',
                          paste0('c_3_casecontrolhours_assigned_hourly_', CaseType,'_', YYYY,'_',weather.variable, '_', 
                          NLDASWeight, '_', Timing, '.fst' )))
}

####************************************************
#### 5: Assign Lagged Hourly Temperature and RH ####
####************************************************

# 5a Tell furrr how to parallelize
future::plan(multisession)

# 5b Create progress bar
pb <- progress_bar$new(
  format = '  processing [:bar] :percent eta: :eta',
  total = 100, clear = FALSE, width= 60)

# 5c Assign temp across years
# It is computationally much faster to run these separately 
# rather than in parallel
# this step eats a lot of memory. 

if(CaseType != 'fake'){
  #assign_lagged_weather('1995', 'temp')  
  assign_lagged_weather('1996', 'temp') 
  assign_lagged_weather('1997', 'temp') 
  assign_lagged_weather('1998', 'temp') 
  assign_lagged_weather('1999', 'temp') 
  assign_lagged_weather('2000', 'temp') 
  assign_lagged_weather('2001', 'temp') 
  assign_lagged_weather('2002', 'temp') 
  assign_lagged_weather('2003', 'temp') 
  assign_lagged_weather('2004', 'temp') 
  assign_lagged_weather('2005', 'temp')   
  assign_lagged_weather('2006', 'temp') 
  assign_lagged_weather('2007', 'temp') 
  assign_lagged_weather('2008', 'temp') 
  assign_lagged_weather('2009', 'temp') 
  assign_lagged_weather('2010', 'temp') 
  assign_lagged_weather('2011', 'temp') 
  assign_lagged_weather('2012', 'temp') 
  assign_lagged_weather('2013', 'temp') 
  assign_lagged_weather('2014', 'temp') 
  assign_lagged_weather('2015', 'temp') 
  
  # 4e Assign rh across years
  #assign_lagged_weather('1995', 'rh')  
  assign_lagged_weather('1996', 'rh') 
  assign_lagged_weather('1997', 'rh') 
  assign_lagged_weather('1998', 'rh') 
  assign_lagged_weather('1999', 'rh') 
  assign_lagged_weather('2000', 'rh') 
  assign_lagged_weather('2001', 'rh') 
  assign_lagged_weather('2002', 'rh') 
  assign_lagged_weather('2003', 'rh') 
  assign_lagged_weather('2004', 'rh') 
  assign_lagged_weather('2005', 'rh')   
  assign_lagged_weather('2006', 'rh') 
  assign_lagged_weather('2007', 'rh') 
  assign_lagged_weather('2008', 'rh') 
  assign_lagged_weather('2009', 'rh') 
  assign_lagged_weather('2010', 'rh') 
  assign_lagged_weather('2011', 'rh') 
  assign_lagged_weather('2012', 'rh') 
  assign_lagged_weather('2013', 'rh') 
  assign_lagged_weather('2014', 'rh') 
  assign_lagged_weather('2015', 'rh') 
}
if(CaseType == 'fake'){
  assign_lagged_weather('2000', 'temp') 
  assign_lagged_weather('2000', 'rh')
} 

# 5d Tell the analyst that the script is done
cat('completed c_00c at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_c_00c), 1), ' min')
rm(StartTime_c_00c)