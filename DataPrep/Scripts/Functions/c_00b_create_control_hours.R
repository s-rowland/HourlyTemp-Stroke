# Create Control Hours
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Readin Data
# 2: Define Function to Create Control Hours
# 3: Create Control Hours

####**************
#### N: Notes #### 
####**************

# Na Description
# We choose control hours via time-stratified bidirectional matching 
# We match by year, month, day of the week, and hour of the day 
# We also use the function to generate the case hour, 
# so that all of the hours have parallel format.
# This step is pretty fast < 10 min for the full dataset

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the analysis is beginning 
StartTime_c_00b <- Sys.time()
print(paste('begin c_00b at', StartTime_c_00b))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep', 'Scripts',
                    '0_01_set_outcomeName_packages_folders.R'))
}

####********************
#### 1: Readin Data ####
####********************

# 1a Readin data 
# the MI dataset should have 1023730 obs and 46 variables
cases <- fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data',
                                  paste0('c_1_all_cases_unassigned_', CaseType, '_', 
                                         Timing, '.fst')))

# 1b Recreate date variable
cases <- cases %>% 
  mutate(CaseDateTime = parse_date_time(CaseDateRaw, 'ymd H', tz='America/New_York')) 

# 1c Split data by years
# Splitting by year speeds up the matching. 
# we could alternatively split by MM-YYYY 
# but the current version is sufficient.
years.list <- cases %>% split(cases$YYYY)

####************************************************
#### 2: Define Function to Create Control Hours ####
####************************************************

# 2a Define function to create potentially matching datehours
# Note: our data is in Eastern Standard Time (EST)
# as such, we are matching on the socially-recognized time that the case would have experienced
# In particular, due to daylight savings time, certain hours do not 'exist' in EST
# This will lead to some NA's in the dataset 
# While converting to UTC would avoid these NA's, 
# the cases experienced time according to EST 
# and their time-varying factors would follow EST, not UTC 
# so if we match on UTC we would not be matching on hour of the day 

make_control_hr <- function(hours1, BeforeAfter, WK){ 
  # hours1 <- df.YYYY; BeforeAfter <- 'Before'; WK <- 4
  # The name of the hour; accounts for if control or case
  VarName <- paste0(BeforeAfter, '_', str_trunc(WK, 1, 'left', ''))    
  # adds WKs number of weeks, preserves hour of day even in daylight saving
  hours1 %>% mutate(!!VarName := CaseDateTime + as.period(7 * WK, 'day'))  
}

# 2b Name function to create control hours
create_control_hours_by_year <- function(df.YYYY){
  #df.YYYY <- years.list[[6]]
  
  # 2c Add progress bar
  p()
  
  # 2d Use function to create bidirectionally symmetric datehours 
  hours1 <-  df.YYYY
  hours1 <- hours1 %>% 
    mutate(CaseDateTime = parse_date_time(CaseDateRaw, 'ymd H', tz = 'America/New_York')) 
  ActiveYYYY <- hours1$YYYY[1]
  hours1 <- make_control_hr(hours1, 'Before', -4)
  hours1 <- make_control_hr(hours1, 'Before', -3)
  hours1 <- make_control_hr(hours1, 'Before', -2)
  hours1 <- make_control_hr(hours1, 'Before', -1)
  hours1 <- make_control_hr(hours1, 'CaseHour', 0)
  hours1 <- make_control_hr(hours1, 'After', 1)
  hours1 <- make_control_hr(hours1, 'After', 2)
  hours1 <- make_control_hr(hours1, 'After', 3)
  hours1 <- make_control_hr(hours1, 'After', 4)

  # 2e Put in long format by HourName
  hours2 <- hours1 %>% 
    gather('HourName', 'DayDateTime', contains('CaseHour_'),
           contains('Before_'), contains('After_') ) 
  
  # 2f Stratify by month of event 
  hours3 <- hours2 %>% filter(month(CaseDateTime) == month(DayDateTime))

  # 2g Convert times to HourIndex
  hours4 <- hours3 %>% 
    mutate(HourIndex0 = 
           as.duration(interval(parse_date_time('1990/01/01 00:00:00', 'ymd HMS', 
                                tz='America/New_York'), DayDateTime))) %>% 
    mutate(DayHourIndex = as.numeric(HourIndex0, 'hours')) %>% 
    dplyr::select(-HourIndex0) %>% 
    mutate(HourIndex0 = 
             as.duration(interval(parse_date_time('1990/01/01 00:00:00', 'ymd HMS', 
                                  tz='America/New_York'), CaseDateTime))) %>% 
    mutate(CaseHourIndex = as.numeric(HourIndex0, 'hours')) %>% 
    dplyr::select(-HourIndex0) 

  # double Check timezone
  #tz(days4$DayDateTime[1])

  # 2h Save results 
  hours4 %>% 
    fst::write_fst(here::here('DataPrep', 'Data', 'Intermediate_Data', 
                           paste0('c_2_casecontrolhours_unassigned_', CaseType, '_', 
                           ActiveYYYY, '_', Timing, '.fst')))
}

####*****************************
#### 3: Create Control Hours ####
####*****************************

# test with a single year
#create_control_hours_by_year(cases.list[[1]])

# 3a Create control hours for all active years
# 3a.i Set up future
future::plan(multisession)

# 3a.ii Actually run the function, in parallel, with a progress bar


with_progress({
  p <- progressor(steps = length(years.list)*2)
  
  result <- future_map(years.list, create_control_hours_by_year, 
                       .options = furrr_options(seed = TRUE))
})

# 3b Tell the analyst that the script is done
cat('completed c_00b at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_c_00b), 1), ' min')
rm(StartTime_c_00b)