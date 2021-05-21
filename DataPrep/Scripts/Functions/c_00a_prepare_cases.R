# Prepare identified cases 
## Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Readin Data 
# 2: Process Variables
# 3: Apply Exclusion Criteria
# 4: Data Checks and Record Missingness

####**************
#### N: Notes #### 
####**************

# Na Description
# The goal of this script is to process the case data 
# from step b_1 
# We need to do data cleaning, especially duplicate entries.
# We also apply most of the exclusion criteria in this script. 
# Right now the code has specifications for mi, 
# but could accommodate stroke in the future. 
# We include all the potential MI cases, not just primary mi 
# so that we can easily apply alternative case definitions in sensitivity analyses 

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the analysis is beginning 
StartTime_c_00a <- Sys.time()
print(paste('begin c_00a at', StartTime_c_00a))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep', 'Scripts',
                    '0_01_set_outcomeName_packages_folders.R'))
}

####********************
#### 1: Readin Data ####
####********************

# 1a Create readin function, which includes some cleaning
ReadOutcomeData <- function(YY, PP, casetype){
  cases <- read_csv(here::here('DataPrep', 'Data', 'Raw_Data', 'Outcome_Data',
                               paste0('raw_', YY, '_', PP, '_', casetype, '.csv')), 
                               col_types = cols(.default = 'c') ) %>% 
    mutate(InOut = !!PP, EventType = !!casetype, Year = str_sub(ADMDT, 0,4)) 
  if(PP == 'op'){cases <- cases %>% mutate(TPADM = 'op')}
cases
}

# 1b Readin Real data 
if(CaseType != 'fake'){
  # 1b.i Setup empty dataframe to fill
  cases <- ReadOutcomeData('00', 'ip', CaseType) %>% 
    sample_frac(0)
  
  # 1b.ii Declare datasets we will readin
  # we use 95-99 only to hep identify subsequent cases
  YYList <- c('95', '96', '97', '98', '99', '00', '01', '02', '03','04', '05', '06','07', 
              '08', '09', '10', '11', '12', '13', '14', '15')
  PPList <- c('ip', 'op')
  
  # 1b.iii Readin and combine datasets
  for(i in 1:length(YYList)){
    for(j in 1:length(PPList)){
      cases <- ReadOutcomeData(YYList[i], PPList[j], CaseType) %>%
        bind_rows(cases)
    }
  }
}
# the resulting dataframe for MI should have 1609158 obs of 28 variables. 
# it will shrink quickly

# 1c Readin fake stroke data for code review 
if(CaseType =='fake'){
  cases <- ReadOutcomeData('00', 'ip', 'fake')
} 

# 1d Drop cases with missing datetime information 
# we have to drop these cases here because otherwise
# they will interfere with determining first/last status
# after we run this line we should have 1609158 obs of 28 variables for MI 
cases <- cases %>% 
  filter(!is.na(ADMDT)) %>%
  filter(!is.na(UPID))

####**************************
#### 2: Process Variables ####
####**************************

# 2a Convert ZIP Code to ZCTA 
# ZCTA are approximately equivalent to ZIP codes, 
# especially at the large spatial scale of NLDAS 
# but some single-building, or single-location ZIP codes 
# do not have corresponding ZCTA with the same numbers 
# The Missour Census Data Center created a crosswalk
# downloaded from: 
# https://mcdc.missouri.edu/geography/ZCTAs-2010.html
Zip_to_ZCTA <- read_csv('DataPrep/Data/Raw_Data/Zip_to_ZCTA_crosswalk_2015_JSI.csv')
cases <- cases %>% 
  left_join(Zip_to_ZCTA_crosswalk_2015_JSI, by = 'ZIP') %>% 
  dplyr::select(-ZIP)

# 2b Rename some variables that are all caps, but not acronyms
# we also have a lowercase zcta
cases <- cases %>% 
  rename(sex = SEX, age = AGE, race = RACE, zcta = ZCTA)

# 2c Create proper datetime variable
cases <- cases %>% 
  mutate(ADMDateTime = paste0(ADMDT, ' ', ADMHR)) %>%
  mutate(CaseDateTime = parse_date_time(ADMDateTime, 'ymd H', tz = 'America/New_York')) 

# 2d Replace bad CaseDates with NA
cases$CaseDateTime[is.na(cases$ADMHR)] <- NA

# 2e Adjust CaseDateTime for prehospital delay 
# 2e.i Identify ICD codes that correspond to STEMI 
# Note: the STEMI variable is only used for the MI analyses, but its created here regardless of subtype 
# so that we can select the same columns regardless of subtye (fewer switch functions)

STEMIICDList <- c('41000', '41001', '41002', '41010', '41011', '41012', '41020', '41021', '41022', 
                  '41030', '41031', '41032','41040', '41041', '41042', '41050', '41051', '41052', 
                  '41060', '41061', '41062', '41080', '41081', '41082', '41090', '41091', '41092', 
                  'I21', 'I210', 'I2100', 'I2101', 'I2102', 'I2109','I211', 'I2110', 'I2111', 'I2119',
                  'I212', 'I2120','I2121', 'I2129', 'I213', 'I2130', 'I21A', 'I21A0', 'I21A1', 'I21A9')
# 2e.ii Identify STEMI cases
cases <- cases %>% 
  mutate(STEMI = if_else(DXA %in% STEMIICDList | DX01 %in% STEMIICDList |
                         DX02 %in% STEMIICDList | DX03 %in% STEMIICDList, 1, 0)) 
# 2e.iii Determine assumed prehospital delay time 
# delay was set to 3 for all MI in an earlier analysis (HourlyTemp_MI)
if(CaseType == 'mi' & Timing == 'STEMI3hrDelay'){cases <- cases %>% mutate(delay = 3)}
if(CaseType == 'mi' & (Timing == 'STEMI2hrDelay'| Timing == 'All23')){
  cases <- cases %>% mutate(delay = if_else(STEMI == 1, 2, 3))
}
if(CaseType == 'stroke' | CaseType =='fake'){cases <- cases %>% mutate(delay = 3)}
# 2e.iv Apply assumed prehospital delay time 
cases <- cases %>% 
  mutate(CaseDateTime = CaseDateTime - 3600*delay) 

# 2f Compute datetime variables 
cases <- cases %>% 
  mutate(YYYY = year(CaseDateTime), 
         MM = month(CaseDateTime),
         DD = day(CaseDateTime),
         HH = hour(CaseDateTime), 
         YDay = yday(CaseDateTime)) %>% 
  mutate(YYYY = if_else(is.na(YYYY), as.numeric(str_sub(ADMDT, 0, 4)), YYYY)) %>% 
  mutate(CaseDateRaw = paste0(YYYY, str_pad(MM, 2, 'left', '0'),
                              str_pad(DD, 2, 'left', '0'), ' ', str_pad(HH, 2, 'left', '0'))) %>% 
  mutate(ADMDateTime = parse_date_time(ADMDateTime, 'ymd H', tz = 'America/New_York')) %>%
  mutate(YYYYAdm = year(ADMDateTime), 
         YDayAdm = yday(ADMDateTime)) 

# 2g Identify primary cases
# 2g.i for MI 
if(CaseType == 'mi'){
  cases <- cases %>% 
    mutate(DXA  = if_else(is.na(DXA), 'NA', DXA), 
           DX01 = if_else(is.na(DX01), 'NA', DX01),
           DX02 = if_else(is.na(DX02), 'NA', DX02),
           DX03 = if_else(is.na(DX03), 'NA', DX03)) %>% 
    mutate(DXA_410x1 = str_sub(DXA, 1, 3) == '410' & str_sub(DXA, 5, 5) == '1') %>% 
    mutate(DXA_410x0 = str_sub(DXA, 1, 3) == '410' & str_sub(DXA, 5, 5) == '0') %>% 
    mutate(DXA_I21 = str_detect(DXA, 'I21')) %>% 
    mutate(DX01_410x1 = str_sub(DX01, 1, 3) == '410' & str_sub(DX01, 5, 5) == '1') %>% 
    mutate(DX01_410x0 = str_sub(DX01, 1, 3) == '410' & str_sub(DX01, 5, 5) == '0') %>% 
    mutate(DX01_I21 = str_detect(DX01, 'I21')) %>% 
    mutate(DX02_410x1 = str_sub(DX02, 1, 3) == '410' & str_sub(DX02, 5, 5) == '1') %>% 
    mutate(DX02_410x0 = str_sub(DX02, 1, 3) == '410' & str_sub(DX02, 5, 5) == '0') %>% 
    mutate(DX02_I21 = str_detect(DX02, 'I21')) %>% 
    mutate(DX03_410x1 = str_sub(DX03, 1, 3) == '410' & str_sub(DX03, 5, 5) == '1') %>% 
    mutate(DX03_410x0 = str_sub(DX03, 1, 3) == '410' & str_sub(DX03, 5, 5) == '0') %>% 
    mutate(DX03_I21 = str_detect(DX03, 'I21')) %>% 
    mutate(MI_prim_tf = 
             as.numeric(DXA_410x1) + as.numeric(DX01_410x1) + as.numeric(DX02_410x1) + 
             as.numeric(DX03_410x1) + as.numeric(DXA_410x0) + as.numeric(DX01_410x0) +
             as.numeric(DX02_410x0) + as.numeric(DX03_410x0) +as.numeric(DXA_I21) + 
             as.numeric(DX01_I21) +  as.numeric(DX02_I21) + as.numeric(DX03_I21)) %>% 
    mutate(MI_prim = as.numeric(MI_prim_tf > 0))  %>% 
    mutate(outcome_prim = MI_prim)
}

# 2g.ii for stroke 
# note that case_when evaluates rows sequentially. 
# If a row meets the first criteria, case_when will stop evaluating that row and move on to the next one 
# the dx_strokeisc and dx_strokehem variables are binary variable for 
# whether an admission has at least 1 dx code for that subtype 
# we identify some ischemic stroke cases by treatment in step 1 
# so, if a case has only codes for 1 subtype, it is assigned a label in step 2 
# of the remaining cases, we try to identify them according to DXA in step 3
# any that are unassigned after this point are labeled as unknown
if(CaseType == 'stroke' | CaseType == 'fake'){
  ISC <- c(43301, 43311, 43321, 43331, 43381, 43391,
           43401, 43411, 43491, 436)
  HEM <- c(430, 431, 4310, 43100, 432, 4320, 43200, 4321, 4329)
  cases <- cases %>% 
    mutate(dx_strokeISC = if_else(DXA%in%ISC|DX01%in%ISC|DX02%in%ISC|DX03%in%ISC, 1, 0), 
           dx_strokeHEM = if_else(DXA%in%HEM|DX01%in%HEM|DX02%in%HEM|DX03%in%HEM, 1, 0)) %>%
    mutate(
      StrokeSubType  = case_when(
        PR01 == '9910' | PR02 == '9910' | PR03 =='9910' | PR04 =='9910' | PR05 == '9910' ~ 'strokeISC', # step 1
        PR01 == '61645' | PR02 == '61645' | PR03 =='61645' | PR04 =='61645' | PR05 == '61645' ~ 'strokeISC', #step 1 
        dx_strokeISC == 1 & dx_strokeHEM == 0 ~ 'strokeISC', # step 2
        dx_strokeISC == 0 & dx_strokeHEM == 1 ~ 'strokeHEM',# step 2
        DXA%in%ISC ~ 'strokeISC', # step 3
        DXA%in%HEM ~ 'strokeHEM', # step 3
        TRUE  ~ 'unkn' # step 3
      )) %>% 
    mutate(strokeISC_prim = if_else(StrokeSubType == 'strokeISC', 1, 0),
           strokeHEM_prim = if_else(StrokeSubType == 'strokeHEM', 1, 0)) %>% 
    mutate(outcome_prim = strokeISC_prim + strokeHEM_prim)
}

# 2h Organize race variable
# Collapse race categories in order to have sufficiently large racial groups to assess
# 2h.i Read in the simplified race codebook
# you will get the warning 'Missing column names filled in: 'X4' [4]', this is fine
# You can ignore this comment, it is just an empty column that we can ignore (excel artifact)
RaceCodeBook <- read_csv(here::here('DataPrep', 'Data', 'Raw_Data', 'RaceCodeBook.csv'), 
                         col_types = cols(race  = col_character(),
                                          Race1 = col_character(),
                                          Race2 = col_character()))
RaceCodeBook <- RaceCodeBook %>% mutate(race = str_pad(race, 2, 'left', '0'))
# 2h.ii Assign simplified Race categories + hispanic 
cases1 <- cases %>% 
  left_join(RaceCodeBook, by = 'race') %>%
  mutate(RaceF = if_else(ETHNIC == '1', 'Hispanic', Race2)) 
# 2h.iii Keep only the simplified race categories
cases2 <- cases1 %>% 
  dplyr::select(-ETHNIC, -race, -Race1, -Race2) %>% 
  rename(race = RaceF) 
# 2h.iv Identify the patients who are inconsistently assigned two races 
# We assume that these subjects are multiracial, and it is not an error in coding. 
TwoRace <- cases2 %>%  
  group_by(UPID) %>%
  dplyr::select(UPID, race) %>%
  distinct() %>% 
  summarize(NumRace = n()) %>%
  filter(NumRace !=1) %>% 
  mutate(MR = 'Multi-Racial')
# 2h.v Assign multiracial category to the subjects that were assigned more than 1 race
cases3 <- cases2 %>% 
  left_join(TwoRace, by = 'UPID')  %>% 
  mutate(raceZ = if_else(is.na(MR), race, 'Multi-Racial')) %>% 
  dplyr::select(-race, -MR, -NumRace) %>% 
  rename(race = raceZ)  %>% 
  mutate(race = if_else(race == 'MultiRacial', 'Multi-Racial', race)) %>% 
  mutate(race = if_else(is.na(race), 'OtherRace', race))
# 2h.vi Remove any remaining duplicates by race
# a very small number of rows are duplicates, where all the information is the same except for patient race
cases <- cases3 %>% distinct()

# 2i Determine insurance type
PubInsKey <- c('C', 'D', 'E', 'H', 'J', 'K')
AnyInsKey <- c('B', 'C', 'D', 'E', 'F', 'H', 'I', 'J', 'K', 'L')
cases <- cases %>% 
  mutate(PublicIns = if_else(SOURCE1 %in%(PubInsKey)| SOURCE2 %in%(PubInsKey)| 
                             SOURCE3 %in%(PubInsKey)| SOURCE4 %in%(PubInsKey)|
                             SOURCE5 %in%(PubInsKey), 
                             'Public Insurance', 'Not Public Insurance'), 
         AnyIns = if_else(SOURCE1 %in%(AnyInsKey)| SOURCE2 %in%(AnyInsKey)| 
                          SOURCE3 %in%(AnyInsKey)| SOURCE4 %in%(AnyInsKey)|
                          SOURCE5 %in%(AnyInsKey),
                          'Any Insurance', 'No Insurance (self-pay or work comp)')) 

####*********************************
#### 3: Apply Exclusion Criteria ####
####*********************************

# Note that we first apply exclusion criteria based on study population criteria
# and then based on data availability/ accuracy 

# 3a Remove admissions on the same day as another MI admission 
# we assume that these are the same MI event. 
# we remove all subsequent admissions, but for the first event of the day,
# wait will not be zero
# we need the hh2 to keep the true primary MIs that are missing ADMHR 
# so that we can exclude them later and have them count for the % missing
cases <- cases %>% 
  mutate(HH2 = if_else(is.na(HH), 23, as.numeric(HH))) %>%
  group_by(UPID, ADMDT) %>% 
  arrange(desc(HH2)) %>% 
  arrange(desc(outcome_prim)) %>% 
  slice(0:1) %>% 
  ungroup()

# 3b Calculate recurrence time and subsequent status
# This step must be done at this point in the code 
# before we have excluded any cases based on data availability, etc
# note that subsequent status is based on time since primary event admission,
# not any event admission
cases <- cases %>% 
  mutate(SecularDay = YDayAdm + 365 * (as.numeric(YYYYAdm) - 1995)) %>%  
  group_by(UPID) %>% 
  arrange(SecularDay) %>% 
  mutate(wait = SecularDay-lag(SecularDay) * as.numeric(lag(outcome_prim))) %>%
  ungroup()

# 3c Remove events that occur within 2 days after another event 
cases <- cases %>% 
  filter(wait>2  | is.na(wait))

# 3d Remove admissions before 1995 or after 2015
# you will get a warning about YYYY NAs induced by coercion. 
# this is okay, just due to some admissions with missing admission date. 
cases <- cases %>% 
  mutate(YYYY = as.numeric(str_sub(CaseDateRaw, 0, 4))) %>%
  filter(YYYY >= 1995 | is.na(YYYY)) %>%
  filter(YYYY <= 2015 | is.na(YYYY))

# 3e Remove entries with residence outside of NYS zipcodes 
ZCTA5CE10 <- sf::st_read(here::here('DataPrep', 'Data', 'Raw_Data',
                                'tl_2010_36_zcta510', 'tl_2010_36_zcta510.shp'))

# 3f Add a row so that we do not yet lose the cases with missing zcta 
ZCTA5CE10[nrow(ZCTA5CE10)+1, ] <- rep(NA, ncol(ZCTA5CE10))

# 3g Remove the admissions outside NYS
ZCTA5CE10 <- ZCTA5CE10 %>% 
  rename(zcta = ZCTA5CE10) %>% 
  dplyr::select(zcta) %>% 
  as.data.frame() %>% 
  dplyr::select(zcta)
cases <- cases %>% right_join(ZCTA5CE10, by = 'zcta')

# 3h Remove subjects under 18 
cases <- cases %>% 
  filter(as.numeric(age) >= 18 | is.na(as.numeric(age)))

# 3i Remove those with missing admit hour or ZIP
# or invalid ZIP (eg contains letters)
# 3i.i Count the number of cases prior to excluding due to missing data
casesTot <- nrow(cases)
if(CaseType == 'stroke' | CaseType == 'fake'){
  casesTotISC <- sum(cases$strokeISC_prim, na.rm =TRUE)
  casesTotHEM <- sum(cases$strokeHEM_prim, na.rm =TRUE)
}
# 3i.ii Exclude based on missing data
cases <- cases %>% 
  filter(!is.na(ADMHR)) %>% 
  filter(!is.na(CaseDateTime)) %>%
  mutate(zcta = as.character(zcta)) %>%
  filter(str_length(zcta) == 5) %>% 
  filter(!str_detect(zcta, '[:alpha:]')) %>% 
  filter(!str_detect(zcta, '[:ALPHA:]'))
# 3i.iii Record the number of missing 
PercentMissing <- 100*(1-(nrow(cases) / casesTot))
if(CaseType == 'stroke' | CaseType == 'fake'){
  PercentMissingISC <-   100*(1-(sum(cases$strokeISC_prim) / casesTotISC))
  PercentMissingHEM <-   100*(1-(sum(cases$strokeHEM_prim) / casesTotHEM))
}

# 3j Change HH for alternative window sensitivity analysis
# Note that this is only done for a sensitivity analysis for TempVar_MI
if(CaseType == 'mi' & Timing == 'All23'){
  cases <- cases %>% mutate(CaseDateRaw = paste0(str_sub(ADMDateTime, 1, 9), 23))
}

# 3k Keep only necesary variables 
cases$TPADM <- 'NotMeasured'
cases <- cases %>% 
  dplyr::select(UPID, zcta, CaseDateRaw, 
                contains('DX'), contains('PR'), contains('stroke'), contains('pr'), 
                YYYY, InOut, TPADM, STEMI, wait, 
                sex, age, race, PublicIns, AnyIns, contains('SOURCE'), ADMDateTime)

# 3l Save Results 
# for MI, this dataframe should have 1024335 obs of 31 variables
cases %>% 
  fst::write_fst(here::here('DataPrep', 'Data', 'Intermediate_Data',
                            paste0('c_1_all_cases_unassigned_', CaseType, '_', 
                                   Timing, '.fst'))) 

####*******************************************
#### 4: Data Checks and Record Missingness #### 
####*******************************************

# 4a Count of primary outcome
sum(cases$outcome_prim)

# 4b Calculate insurance percentages
a <- cases %>% 
  group_by(PublicIns) %>% 
  summarize(Count2 = n()) %>% 
  mutate(PercentIns = 100*Count2/nrow(cases))
PPubIns <- a$PercentIns[a$PublicIns=='Public Insurance']
if(CaseType == 'fake'){PPubIns <- 1}

# 4c Determine number of unique individuals 
b <- cases %>% dplyr::select(UPID) %>% distinct()
NInd <- nrow(b)

# 4d Percent from NYC 
# csv file taken from 
# NYC Open Data 
# at https://data.cityofnewyork.us/Health/Modified-Zip-Code-Tabulation-Areas-MODZCTA-Map/5fzm-kpwv
# on 11/17/2020s
# 4d.i Readin and wrangle data 
nyc <- read_csv(here::here('DataPrep', 'Data', 'Raw_Data', 
                           'Modified_Zip_Code_Tabulation_Areas__MODZCTA_.csv'))
# You will get the following warning: 
# 'Expected 12 pieces. Missing pieces filled with `NA` in 178 rows [1:20, ...].'
# This warning is expected. Basically, the original columns contain multiple ZCTA 
# but they do not contain the same number of ZCTA. 
# So when we separate into 12 pieces, many of those pieces will by empty ('NA')
nyc <- nyc %>% 
  separate(ZCTA, c('z1', 'z2', 'z3', 'z4', 'z5', 'z6', 'z7', 'z8', 'z9', 'z10', 'z11', 'z12'), 
           extra = 'merge')
# 4d.ii Extract list of zcta in NYC
nyc.zcta <- c(nyc$z1, nyc$z2, nyc$z3, nyc$z4, nyc$z5, nyc$z6, nyc$z7, nyc$z8, 
              nyc$z9, nyc$z10, nyc$z11, nyc$z12)
# 4d.iii Isolate cases in nyc
PNYC <- nrow(filter(cases, zcta %in% nyc.zcta))

# 4e Save results
if(Timing != 'All23' & CaseType == 'mi'){
  data.frame(
    PercentMissing = PercentMissing,
           NInd = NInd, 
           PPubIns = PPubIns, 
           PNYC = PNYC,
           AgeMean = mean(as.numeric(cases$age), na.rm = TRUE), 
           AgeSD = sd(cases$age, na.rm =TRUE)) %>% 
    write_csv(here::here('DataPrep', 'Data', 'Final_Data',
                         paste0('PercentMissing_', CaseType, '.csv')))
}
if(Timing != 'All23' & (CaseType == 'stroke'| CaseType == 'fake')){
  data.frame(
    PercentMissingISC = PercentMissingISC,
    PercentMissingHEM = PercentMissingHEM,
    NInd = NInd, 
    PPubIns = PPubIns, 
    PNYC = PNYC,
    AgeMean = mean(as.numeric(cases$age), na.rm = TRUE), 
    AgeSD = sd(cases$age, na.rm =TRUE)) %>% 
    write_csv(here::here('DataPrep', 'Data', 'Final_Data',
                         paste0('PercentMissing_', CaseType, '.csv')))
}

# 1i Tell the analyst that the script is done
cat('completed c_00a at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_c_00a), 1), ' min')
rm(StartTime_c_00a)
