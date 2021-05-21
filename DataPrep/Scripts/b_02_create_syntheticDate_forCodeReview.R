# Prepare identified cases 
# Data Prep
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create Synthetic Data 

####**************
#### N: Notes #### 
####**************

# Na Description
# The goal of this script is to create a fake dataset for reproducibility testing
# this fake dataset should have a null association between temp and stroke

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the script is beginning 
StartTime_b_02 <- Sys.time()
print(paste('begin b_02 at', StartTime_b_02))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep', 'Scripts',
                    '0_01_set_outcomeName_packages_folders.R'))
}

####******************************
#### 1: Create Synthetic Data ####
####******************************

# 1a Create a vector of zcta in nys 
ZCTA5CE10 <- sf::st_read(here::here('DataPrep', 'Data', 'Raw_Data', 'tl_2010_36_zcta510',
                                    'tl_2010_36_zcta510.shp'))
ZCTA5CE10 <- ZCTA5CE10 %>% 
  rename(ZIP = ZCTA5CE10) %>% 
  dplyr::select(ZIP) %>% 
  as.data.frame() %>% 
  dplyr::select(ZIP) %>% 
  mutate(zIndex = row_number())

# 1b Begin dataset with Index variable
cases <- data.frame(
  Index = 1:10000, 
  DXA = rep(c('43301','43301','431'), 5000),
  DX01 = rep(c('401','123'), 75000)
)

# 1c Create non-data, non-zcta variables 
cases <- cases %>%  
  mutate(AGE = 75, 
         DOB = '19210615', 
        DX02 = DXA, DX03 = DXA, DX04 = DXA, DX05 = DXA, DX06 = DXA, 
         DX07 = DXA, DX08 = DXA, DX09 = DXA, DX10 = DXA, DX11 = DXA, DX12 = DXA, 
         DX13 = DXA, DX14 = DXA, DX15 = DXA, DX16 = DXA, DX17 = DXA, DX18 = DXA, 
         DX19 = DXA, DX20 = DXA, DX21 = DXA, DX22 = DXA, DX23 = DXA, DX24 = DXA, 
         DX25 = DXA,
         ETHNIC = '2', 
         PR01 = '10', PR02 = '10', PR03 = '10', PR04 = '10', PR05 = '10', 
         RACE = '45', 
         SEX = 'F', 
         SOURCE1= '10', SOURCE2 = '10', SOURCE3 = '10', SOURCE4='10',SOURCE5='10',
         UPID = paste0('AAA', str_pad(row_number(), 7, 'left', '0')), 
         zIndex = Index %% nrow(ZCTA5CE10), 
         dIndex = Index %% 364, 
         ADMHR = str_pad(Index %% 24, 2, 'left', pad ='0')) %>% 
  inner_join(ZCTA5CE10, by = 'zIndex') %>% 
  mutate(ADMDT = str_remove_all(parse_date_time('1/1/2000', 'mdy') + dIndex*60*60*24, '-')) %>%
dplyr::select(-contains('Index'))

# 1d Save data 
cases %>% 
  write_csv(here::here('DataPrep', 'Data', 'Raw_Data', 'Outcome_Data',
                       'raw_00_ip_fake.csv'))

# 1e Tell the analyst that the script is done
cat('completed b_02 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_b_02), 1), ' min')
rm(StartTime_b_02)
