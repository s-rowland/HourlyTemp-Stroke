# Combine Years of Exposure + Outcome Data
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Readin Data
# 2: Apply Exclusion Criteria
# 3: Create Data-Derived Variables
# 4: Save Data

####**************
#### N: Notes #### 
####**************

# Na Description
# Here we combine all the data, 
# do some additional manipulations 
# and then save 3 datasets: full, TV only, and deidentified. 
# this script takes ~ 15 min to run on the full data

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the analysis is beginning 
StartTime_c_00e <- Sys.time()
print(paste('begin c_00e at', StartTime_c_00e))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep', 'Scripts', 
                    '0_01_set_outcomeName_packages_folders.R'))
}

####********************
#### 1: Readin Data ####
####********************

# 1a Make function to readin data 
read_c4 <- function(YYYY){
  fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data',
                           paste0('c_4_casecontrolhours_assigned_variability_',
                       CaseType, '_', YYYY, '_', NLDASWeight, '_', Timing , '.fst')))
}

# 1b Create list of years
YYYY.list <-  c('1995', '1996', '1997', '1998', '1999', 
                '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008',
                '2009', '2010', '2011', '2012', '2013', '2014', '2015')
if(CaseType == 'fake'){YYYY.list <- c('2000')}

# 1c Readin the data 
# 1c.i Tell future how to parallelize
future::plan(multisession)
# 1c.ii Actually readin the data
dta <- future_map(YYYY.list, read_c4, 
                  .options = furrr_options(seed = TRUE)) %>%
  bind_rows()

####*********************************
#### 2: Apply Exclusion Criteria ####
####*********************************

# 2a Filter by admission type
# We only do this for MI as stroke is associated with pregnancy and can lead to trauma
if(CaseType == 'mi') {dta <- dta %>% filter(TPADM !='4' & TPADM != '5')}

# 2b Remove obs with incorrect hourly temp for 48 hour lag
# Here we remove observations with values that exceed the minimum or maximum values observed by NOAA
# (https://www.ncdc.noaa.gov/extremes/scec/records
# rephrase this to align with the language in the methods section
# include that link in the code 
count_original <- nrow(dta)
dta <- dta %>% 
  filter(MinT_48hr > -46) %>% 
  filter(MaxT_48hr < 42)
# check the number obs we removed 
# but only if the timing is not the one used in the main analysis

if(Timing != 'All23'){
  Miss <- read_csv(here::here('DataPrep', 'Data', 'Final_Data',
                              paste0('PercentMissing_', CaseType, '.csv')))
  Miss$NLDASErr <- count_original - nrow(dta)
  Miss %>% write_csv(here::here('DataPrep', 'Data', 'Final_Data',
                                paste0('PercentMissing_', CaseType, '.csv')))
}

# 2c Assign insurance information
# will be unnecesary in subsequent runs, once I can return everything.
cases <- fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data',
                                  paste0('c_1_all_cases_unassigned_', CaseType, 
                                         '_', Timing, '.fst'))) 
cases <- cases %>% 
  dplyr::select(UPID, ADMDateTime, AnyIns, contains('Source'))
dta <- inner_join(cases, dta, by = c('UPID', 'ADMDateTime'))


####**************************************
#### 3: Create Data-Derived Variables ####
####**************************************

# 3a Create Case and TimetoEvent variables 
# These variables are used in the coxph command
# by making the data structure parallel to a survival dataset
dta <- dta %>% 
  mutate(Case = if_else(str_detect(HourName0, 'Case'), 1, 0),
         TimetoEvent = if_else(str_detect(HourName0, 'Case'), 1, 2))

# 3b Create identified stratum identifier
# this variable identifies our case-control sets
dta <- dta %>% 
  mutate(CaseHourIndex2 = str_pad(CaseHourIndex, 7,'left', 'T'))%>% 
  mutate(EventID = paste0(UPID, '_', CaseHourIndex2))

# 3c Create First variable 
dta <- dta %>% 
  mutate(First = if_else(is.na(wait), 'First', 'Recurrent')) 

# 3d Create MIStatus variable 
# MI status is what sort of MI this is for the subject - is it their first MI?
if(CaseType == 'mi') {
  dta <- dta %>% 
    mutate(MIStatus = if_else(wait <= 29, 'Reinfarction', 'Recurrent')) %>% 
    mutate(MIStatus = if_else(is.na(wait), 'First', MIStatus))
}

# 3e Identify primary & alternative primary cases 
# 3e.i Create MI outcome variables
# alternative outcome definitions
# (a) 410.x1 in the primary diagnosis
# (b) 410.x1 or 410.x0 in the four diagnostic positions,
# (c) 410.xx in the primary diagnosis
# (d) 410.x1 in the primary diagnosis; excluded if recurrent by 6 mo or less
# (e) 410.x1 in the four diagnostic positions, drop reinfarctions
if(CaseType == 'mi') {
  # 3e.ii Apply alternative outcome definitions
  # Converting the NA to character allows us to do boolean evaluations. 
  # since now if that dx is missing the boolean will yield 0 rather than na. 
  dta <- dta %>% 
    mutate(DXA = if_else(is.na(DXA), 'NA', DXA), 
           DX01 = if_else(is.na(DX01), 'NA', DX01),
           DX02 = if_else(is.na(DX02), 'NA', DX02),
           DX03 = if_else(is.na(DX03), 'NA', DX03)) %>% 
    mutate(DXA_410x1 = str_sub(DXA, 1, 3) == '410' & str_sub(DXA,5,5) == '1') %>% 
    mutate(DXA_I21 = str_detect(DXA, 'I21')) %>% 
    mutate(DX01_410x1 = str_sub(DX01, 1, 3) == '410' & str_sub(DX01,5,5) == '1') %>% 
    mutate(DX01_I21 = str_detect(DX01, 'I21')) %>% 
    mutate(DX02_410x1 = str_sub(DX02, 1, 3) == '410' & str_sub(DX02,5,5) == '1') %>% 
    mutate(DX02_I21 = str_detect(DX02, 'I21')) %>% 
    mutate(DX03_410x1 = str_sub(DX03, 1, 3) == '410' & str_sub(DX03,5,5) == '1') %>% 
    mutate(DX03_I21 = str_detect(DX03, 'I21')) %>% 
    mutate(MI_prim_tf = as.numeric(DXA_410x1) + as.numeric(DX01_410x1) + as.numeric(DX02_410x1) + as.numeric(DX03_410x1) + 
             as.numeric(DXA_I21) + as.numeric(DX01_I21) + as.numeric(DX02_I21) + as.numeric(DX03_I21)) %>% 
    mutate(MI_prim = as.numeric(MI_prim_tf > 0)) 
  
  dta <- dta %>% 
    mutate(prim_alt_410x1_A_tf = as.numeric(DXA_410x1) + as.numeric(DX01_I21) ) %>% 
    mutate(prim_alt_410x1_A = as.numeric(prim_alt_410x1_A_tf > 0))  
  
  dta <- dta %>% 
    mutate(DXA_410xx = str_sub(DXA, 1, 3) == '410') %>% 
    mutate(prim_alt_410xx_A_tf = as.numeric(DXA_410xx) + as.numeric(DX01_I21) ) %>% 
    mutate(prim_alt_410xx_A = as.numeric(prim_alt_410xx_A_tf > 0))
  
  dta <- dta %>% 
    mutate(DXA_410x0 = str_sub(DXA, 1, 3) == '410' & str_sub(DXA,5,5) == '0') %>% 
    mutate(DX01_410x0 = str_sub(DX01, 1, 3) == '410' & str_sub(DX01,5,5) == '0') %>% 
    mutate(DX02_410x0 = str_sub(DX02, 1, 3) == '410' & str_sub(DX02,5,5) == '0') %>% 
    mutate(DX03_410x0 = str_sub(DX03, 1, 3) == '410' & str_sub(DX03,5,5) == '0') %>% 
    mutate(prim_alt_410x01_A4_tf = 
             as.numeric(DXA_410x1) + as.numeric(DX01_410x1) + as.numeric(DX02_410x1) + as.numeric(DX03_410x1) + 
             as.numeric(DXA_410x0) + as.numeric(DX01_410x0) + as.numeric(DX02_410x0) + as.numeric(DX03_410x0) + 
             as.numeric(DXA_I21) + as.numeric(DX01_I21) + as.numeric(DX02_I21) + as.numeric(DX03_I21)) %>% 
    mutate(prim_alt_410x01_A4 = as.numeric(prim_alt_410x01_A4_tf > 0)) 
 
  dta <- dta %>% 
    mutate(prim_alt_NoReinfarction = if_else(MIStatus != 'Reinfarction', 1, 0))
  
  dta <- dta %>% select(-contains('tf'))
 }

# 3f Create AgeGroup Variable
dta <- dta %>% 
  mutate(age = as.numeric(age)) %>%
  mutate(ageGroup = if_else(age >= 65, 'AgeGTE65', 'AgeLT65'))

# 3g Save frequency of hours 
dta %>% 
  mutate(CaseDate = parse_date_time(CaseDateRaw, 'ymd h')) %>% 
  mutate(MM = month(CaseDate), HH = hour(CaseDate), WDay = wday(CaseDate)) %>%
  filter(Case == 1) %>% 
  group_by(HH) %>% 
  summarize(HHCount = n()) %>% 
  ungroup() %>% 
  mutate(HHFreq = HHCount/sum(dta$Case)) %>% 
  write_csv(here::here('DataPrep', 'Data', 'Final_Data',
                       paste0('HHFreq', '_', CaseType, '_', Timing, '.csv')))

####******************
#### 4: Save Data ####
####******************

# 4a Keep only variables of interest 
if(CaseType == 'mi'){
  dta <- dta %>% 
    dplyr::select(contains('prim'), EventID, HourName0,# Case features / strata identifiers
                 TimetoEvent, Case,                        # coxph variables
                contains('tLag'), contains('rLag'),       # hourly weather
                contains('24'), contains('48'), contains('72'), contains('36'),
                CaseDateRaw, CaseHourIndex, UPID, zcta, 
                sex, race, age, ageGroup, wait, First, MIStatus, 
                contains('DX'), contains('pr'), STEMI, ADMDateTime, PublicIns, AnyIns)
}
if(CaseType == 'stroke' | CaseType == 'fake'){
  dta <- dta %>% 
    dplyr::select(contains('prim'), StrokeSubType, EventID, HourName0,# Case features / strata identifiers
                  TimetoEvent, Case,                        # coxph variables
                  contains('tLag'), contains('rLag'),       # hourly weather
                  contains('24'), contains('48'), contains('72'), contains('36'),
                  CaseDateRaw, CaseHourIndex, UPID, zcta, 
                  sex, race, age, ageGroup, wait, First, 
                  contains('DX'), contains('pr'), contains('PR'), ADMDateTime)
}

# 4b Save identified dataset
dta %>%
  mutate(year = as.numeric(str_sub(CaseDateRaw, 0, 4))) %>%
  filter(year >= 2000) %>% 
  fst::write_fst(here::here('DataPrep', 'Data', 'Final_Data',
                            paste0('prepared_cohort', '_', CaseType,
                              '_identified', '_', NLDASWeight, '_', Timing, '.fst')))

# 4c Save variability-only exposures
dta %>% 
  mutate(year = as.numeric(str_sub(CaseDateRaw, 0, 4))) %>%
  filter(year >= 2000) %>% 
  dplyr::select(-contains('tLag'), -contains('rLag')) %>%
  fst::write_fst(here::here('DataPrep', 'Data', 'Final_Data',
                            paste0('prepared_cohort', '_', CaseType, 
                        '_identified', '_', NLDASWeight, '_', Timing, '_TVOnly.fst')))

# 4d Deidentify cases
# 4d.i Deidentify the EventID variable - replace identifiable numbers with random letters
dta.deid <- dta %>% 
  mutate(CaseHourIndex2 = as.numeric(as.factor(CaseHourIndex)))%>% 
  mutate(CaseHourIndex2 = str_pad(CaseHourIndex2, 7,'left', 'T'))%>% 
  mutate(ID = as.numeric(as.factor(UPID)))%>% 
  mutate(ID = str_pad(ID, 6,'left', 'A'))%>% 
  mutate(EventID = paste0(ID, '_', CaseHourIndex2))

# 4e Data for Inurance-specific analysis
if(CaseType == 'mi'){
  # 4e.i Calculate sub-daily averages
  dta.nina <-  dta.deid %>% 
      filter(MI_prim == 1) %>% 
      mutate(YYYY = str_sub(CaseDateRaw, 0, 4)) %>%
      mutate(MeanT_6hr = (1/6) *(tLag_00+tLag_01+tLag_02+tLag_03+tLag_04+tLag_05), 
             MeanR_6hr = (1/6) *(rLag_00+rLag_01+rLag_02+rLag_03+rLag_04+rLag_05)) %>% 
      mutate(MeanT_12hr = (1/12) *(tLag_00+tLag_01+tLag_02+tLag_03+tLag_04+tLag_05+
                                     tLag_06+tLag_07+tLag_08+tLag_09+tLag_10+tLag_11),
             MeanR_12hr = (1/12) *(rLag_00+rLag_01+rLag_02+rLag_03+rLag_04+rLag_05+
                                     rLag_06+rLag_07+rLag_08+rLag_09+rLag_10+rLag_11)) %>%
      dplyr::select(EventID, # Case features / strata identifiers
                    TimetoEvent, Case,                        # coxph variables
                    MeanT_6hr, MeanT_12hr, MeanT_24hr, MeanT_48hr, #temp exposures
                    MeanR_6hr, MeanR_12hr, MeanR_24hr, MeanR_48hr, 
                    sex, ageGroup, AnyIns, YYYY) 
  # 4e.ii Save data 
  dta.nina %>% 
    filter(YYYY < 2005) %>%
      fst::write_fst(here::here('DataPrep', 'Data', 'Final_Data',
                                paste0('de_identified_MI_for_Nina_before2005.fst')))
  dta.nina %>% 
    filter(YYYY > 2004) %>%
    fst::write_fst(here::here('DataPrep', 'Data', 'Final_Data',
                              paste0('de_identified_MI_for_Nina_after2004.fst')))
}

# 4f Deidentify cases: keep minimum set of variables 
if(CaseType =='mi'){
  dta.deid <- dta.deid %>% 
    filter(MI_prim == 1) %>% 
    dplyr::select(EventID, # Case features / strata identifiers
                  TimetoEvent, Case,                        # coxph variables
                  contains('tLag'), contains('rLag'),       # hourly weather
                  contains('24'), contains('48'), contains('72')) # variability metrics and means
}

if(CaseType =='stroke' | CaseType == 'fake'){
  dta.deid <- dta.deid %>% 
    mutate(stroke_prim = if_else(strokeISC_prim==1 | strokeHEM_prim==1, 1, 0)) %>%
   filter(stroke_prim == 1) %>% 
    dplyr::select(EventID, StrokeSubType, # Case features / strata identifiers
                  TimetoEvent, Case,                        # coxph variables
                  contains('tLag'), contains('rLag'),       # hourly weather
                  contains('24'), contains('36'), contains('48'), contains('72')) # variability metrics and means
}

# 4g Save deidentified dataset
dta.deid %>% fst::write_fst(here::here('DataPrep', 'Data', 'Final_Data',
                                       paste0('prepared_cohort', '_',CaseType,
                                   '_deidentified',  '_', NLDASWeight, '_', Timing, '.fst')))

dta.deid %>%  
  dplyr::select(-contains('tLag'), -contains('rLag')) %>%
  fst::write_fst(here::here('DataPrep', 'Data', 'Final_Data',
                            paste0('prepared_cohort', '_', CaseType, 
                        '_deidentified', '_', NLDASWeight, '_', Timing, '_TVOnly.fst')))

# 4h Tell the analyst that the script is done
cat('completed c_00e at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_c_00e), 1), ' min')
rm(StartTime_c_00e)
