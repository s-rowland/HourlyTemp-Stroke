# Create TableOne 
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# D: Description
# 0: Preparation 
# 1: Organize Case Data
# 2: Create Table One

####********************
#### D: Description ####
####********************

# Table one of description of demographics, ect, of study population 

####********************
#### 0: Preparation ####
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('Ran_analysis_0_01')){
  here::i_am('README.md')
  source(here::here('HourlyTemp_Stroke', 'Scripts',
                    '0_01_setOutcomeName_folders_packages_data.R'))
}

# 0b Create the plotting objects, if you haven't already
if (!exists('HH.fig')){
  source(here::here('HourlyTemp_Stroke', 'Scripts', 
                    'G_01_set_PlottingObjects.R'))
}

####***************************
#### 1: Organize Case Data ####
####***************************

# 1a Keep only cases 
cases <- dta %>% filter(Case == 1)

# 1b Keep only primary admissions according to main outcome definition

cases <- cases %>% filter(strokeISC_prim == '1' | strokeHEM_prim == '1')

# 1c Convert zcta to numeric 
cases <- cases %>% mutate(zcta = as.numeric(zcta))
tot.isc <- cases %>% filter(strokeISC_prim == 1) %>% nrow()
tot.hem <- cases %>% filter(strokeHEM_prim == 1) %>% nrow()

####*************************
#### 2: Create Table One ####
####*************************
# note that we will get a comment 
# `summarise()` ungrouping output (override with `.groups` argument)
# it is not relevant. 

# 2a Counts by SubType
TableOne <- cases %>% 
  group_by(StrokeSubType) %>%
  summarize(NCases = n()) %>% 
  mutate(Variable = 'Count', Category = 'Count') 

# 2b Tabulation by sex 
TableOne <- cases %>% 
  group_by(sex, StrokeSubType) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = sex) %>% 
  mutate(Category = 'Sex') %>%
  bind_rows(TableOne,.)

# 2c Tabulation by AgeGroup 
TableOne <- cases %>% 
  group_by(ageGroup, StrokeSubType) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = ageGroup) %>% 
  mutate(Category = 'Age Group') %>%
  bind_rows(TableOne,.)

# 2d Tabulation by simplified race 
TableOne <- cases %>%
  mutate(raceGroup = if_else(race == 'PacificIslander' | race == 'Asian', 'Asian-American', race)) %>% 
  group_by(raceGroup, StrokeSubType) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = raceGroup) %>% 
  mutate(Category = 'Racial Group') %>%
  bind_rows(TableOne, .)

# 2e Tabulation by NYC residence 
# 2e.i Readin and wrangle data 
# csv file taken from 
# NYC Open Data 
# at https://data.cityofnewyork.us/Health/Modified-Zip-Code-Tabulation-Areas-MODZCTA-Map/5fzm-kpwv
# on 11/17/2020
nyc <- read_csv(here::here('DataPrep', 'Data', 'Raw_Data', 
                       'Modified_Zip_Code_Tabulation_Areas__MODZCTA_.csv'))
# we will get a warning about 'Expected 12 pieces. Missing pieces filled with `NA`'
# we can ignore it. 
nyc <- nyc %>% 
  separate(ZCTA, c('z1', 'z2', 'z3', 'z4', 'z5', 'z6', 'z7', 'z8', 'z9', 'z10', 'z11', 'z12'), 
           extra = 'merge')
# 2e.ii Extract list of zcta in NYC
nyc.zcta <- c(nyc$z1, nyc$z2, nyc$z3, nyc$z4, nyc$z5, nyc$z6, nyc$z7, nyc$z8, 
              nyc$z9, nyc$z10, nyc$z11, nyc$z12)
# 2e.iii Isolate cases in nyc
PNYC <- nrow(filter(cases, zcta %in% nyc.zcta))
# 2e.iv
TableOne <- cases %>% 
  mutate(NYC = if_else(zcta %in% nyc.zcta, 'Outside NYC', 'NYC')) %>% 
  group_by(NYC, StrokeSubType) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = NYC) %>% 
  mutate(Category = 'NYC Residency') %>%
  bind_rows(TableOne,.)

# 2f Tabulation by First/Recurrent Status 
TableOne <- cases %>% 
  group_by(First, StrokeSubType) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = First) %>% 
  mutate(Category = 'First/Recurrent Status') %>%
  bind_rows(TableOne,.)

# 2g Tabulate by HTN status
HTN <- c('401', '4010', '4011', '4019', '402', '4020', '40200', '40201', '403', '4030',
         '40300', '40301', '40310', '40311', '40390', '40391', '404', '4040', '40400',
         '40401', '40402', '40403', '4041', '40410', '40411', '40412', '40413', '4049', '40490', '40491', 
         '40492', '40493', '405', '4050', '40501', '40509', '4051', '40511', '40519',
         '4059', '40591', '40599', '642', '6420', '64200', '64201', '64202', '64203', '64204', '6421', 
         '64210', '64211', '64212', '64213', '64214', '6422', '64220', '64221', '64222',
         '64223', '64224', '6427', '64270', '64271', '64272', '64273', '64274')  
cases <- cases %>% 
  mutate(HTN = if_else(DXA%in%HTN|DX01%in%HTN|DX02%in%HTN|DX03%in%HTN|DX04%in%HTN|DX05%in%HTN|
                         DX06%in%HTN|DX07%in%HTN|DX08%in%HTN|DX09%in%HTN|DX10%in%HTN|DX11%in%HTN|
                         DX12%in%HTN|DX13%in%HTN|DX14%in%HTN|DX15%in%HTN|DX16%in%HTN|DX17%in%HTN|
                         DX18%in%HTN|DX19%in%HTN|DX20%in%HTN|DX21%in%HTN|DX22%in%HTN|DX23%in%HTN|
                         DX24%in%HTN|DX25%in%HTN, 'HTN', 'noHTN'))
TableOne <- cases %>% 
  group_by(HTN, StrokeSubType) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = HTN) %>% 
  mutate(Category = 'HTN Status') %>%
  bind_rows(TableOne,.)

# 2h Tabulate by HTN status
AF <- c('42731')
cases <- cases %>% 
  mutate(Afib = if_else(DXA%in%AF|DX01%in%AF|DX02%in%AF|DX03%in%AF|DX04%in%AF|DX05%in%AF|
                          DX06%in%AF|DX07%in%AF|DX08%in%AF|DX09%in%AF|DX10%in%AF|DX11%in%AF|
                          DX12%in%AF|DX13%in%AF|DX14%in%AF|DX15%in%AF|DX16%in%AF|DX17%in%AF|
                          DX18%in%AF|DX19%in%AF|DX20%in%AF|DX21%in%AF|DX22%in%AF|DX23%in%AF|
                          DX24%in%AF|DX25%in%AF, 
                        'Afib', 'noAfib')) 

TableOne <- cases %>% 
  group_by(Afib, StrokeSubType) %>% 
  summarize(NCases = n()) %>% 
  rename(Variable = Afib) %>% 
  mutate(Category = 'Afib Status') %>%
  bind_rows(TableOne,.)


# 2i Compute percentages 
TableOne <-  TableOne %>% 
  mutate(StrokeSubType = paste0('Count_', StrokeSubType)) %>% 
  spread(StrokeSubType, NCases) %>%
  mutate(Percentage_strokeISC = format(round(100 * Count_strokeISC/tot.isc, 2), nsmall = 1), 
         Percentage_strokeHEM = format(round(100 * Count_strokeHEM/tot.hem, 2), nsmall = 1))

# 2j Rearrange columns 
TableOne <- TableOne %>% 
   mutate(Category = factor(Category, levels=c('Count', 'Sex', 'Age Group', 'Racial Group', 
                                               'NYC Residency', 'First/Recurrent Status', 'HTN')), 
          Variable = factor(Variable, levels=c('Count', 'F', 'M', 'U', 'AgeLT65', 'AgeGTE65', 
                                               'Asian-American', 'Black', 'Hispanic', 'Multi-Racial', 'NativeAmerican',  
                                               'OtherRace','White', 'Outside NYC', 'NYC', 'First', 'Recurrent', 
                                               'HTN', 'noHTN'))) 
 
# 2k Format numbers nicely and save table 
TableOne %>% 
  arrange(Variable) %>%
  mutate(Count_strokeISC = scales::comma(Count_strokeISC),
         Count_strokeHEM = scales::comma(Count_strokeHEM)) %>% 
  mutate(N_strokeISC = paste0(Count_strokeISC, ' (',  Percentage_strokeISC, '%)'), 
         N_strokeHEM = paste0(Count_strokeHEM, ' (',  Percentage_strokeHEM, '%)')) %>% 
  dplyr::select(Category, Variable, N_strokeISC, N_strokeHEM) %>%
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript', 
                       'Table1_TableOne.csv'))
