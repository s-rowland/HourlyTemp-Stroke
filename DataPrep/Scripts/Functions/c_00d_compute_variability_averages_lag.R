# Compute Temperature Variability and Averages for Lagged Exposure 
# Data Preparation
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Define Function to Compute Variability of for One Parameter
# 2: Define Function to Compute Variability and Averages for One Year
# 3: Compute Variability and Averages for Each Year

####**************
#### N: Notes #### 
####**************

# Na Description
# In this script we compute various variability metrics and some averages 
# Some of these metrics are not included in the manuscript, 
# but were used in some initial analyses 
# We also calculate the metrics for 48- and 72- hour exposure windows 
# Though these are not used in the manuscript. 
# everything we compute for temperature we also compute for RH
# this script takes > 4 hours to run on the full data

# Nb Break up by year
# This step is broken up by year because it is computationally intensive. 
# overall this sequence is a bit hard to follow, 
# but runs much much faster than previous versions

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the analysis is beginning 
StartTime_c_00d <- Sys.time()
print(paste('begin c_00d at', StartTime_c_00d))

# 0b Create the folder structure, if you haven't already
if (!exists('Ran_dataprep_0_01')){
  here::i_am('README.md')
  source(here::here('DataPrep', 'Scripts',
                    '0_01_set_outcomeName_packages_folders.R'))
}

####******************************************************************************
#### 1: Define Function to Compute Variability and Averages for One Parameter ####
####******************************************************************************

# 1a Name function
make_var_ave_data <- function(dta, VarInitial){
  
  # 1b Make metrics of the various periods
  dta.e.lag <- dta %>% 
    dplyr::select(contains(paste0(VarInitial, 'Lag_')))
  dta.e.lag.24 <- dta.e.lag[,1:24]
  dta.e.lag.48 <- dta.e.lag[,1:48]
  dta.e.lag.36 <- dta.e.lag[,1:36]
  dta.e.lag.25_48 <- dta.e.lag[,25:48]
  
  # 1c Compute first differences 
  dta.e.fd.24 <- dta %>% dplyr::select('HourName')
  for(i in 1:24){
    dta.e.fd.24[,i] <- dta.e.lag[,i] - dta.e.lag[,(i + 1)]
  }
  
  # 1d Create dataframe of minimums and maximums 
  # only used to calculate SDMinMax, for now. 
  dta.e.MinMax <- data.frame(a = rep(NA, nrow(dta)))
  dta.e.MinMax[,1] <- apply(dta.e.lag.24, 1, function(x) min(x))
  dta.e.MinMax[,2] <- apply(dta.e.lag.24, 1, function(x) max(x))
  dta.e.MinMax[,3] <- apply(dta.e.lag.25_48, 1, function(x) min(x))
  dta.e.MinMax[,4] <- apply(dta.e.lag.25_48, 1, function(x) max(x))
  
  # 1e Calculate means 
  # used for DayDiffT
  dta.e.DayMean <- data.frame(a = rep(NA, nrow(dta)))
  dta.e.DayMean$Mean24hr <- apply(dta.e.lag.24, 1, function(x) mean(x))
  dta.e.DayMean$Mean25_48hr <- apply(dta.e.lag.25_48, 1, function(x) mean(x))
  
  # 1f Fill dataframe of variability metrics and means 
  dta.var <- dta %>% dplyr::select('HourName0')
  dta.var[,2] <- apply(dta.e.fd.24, 1, function(x) mean(abs(x)))
  dta.var[,3] <- apply(dta.e.fd.24, 1, function(x) max(abs(x)))
  dta.var[,4] <- apply(dta.e.lag.24, 1, function(x) muStat::stdev(x, unbiased=FALSE))
  dta.var[,5] <- apply(dta.e.lag.24, 1, function(x) max(x) - min(x))
  dta.var[,6] <- apply(dta.e.MinMax, 1, function(x) muStat::stdev(x, unbiased=FALSE))
  dta.var[,7] <- apply(dta.e.fd.24, 1, function(x) mean(x))
  dta.var[,8] <- dta.e.DayMean$Mean24hr - dta.e.DayMean$Mean25_48hr 
  dta.var[,9] <- apply(dta.e.lag.24, 1, function(x) mean(x))
  dta.var[,10] <- apply(dta.e.lag.48, 1, function(x) mean(x))
  dta.var[,11] <- apply(dta.e.lag.24, 1, function(x) min(x))
  dta.var[,12] <- apply(dta.e.lag.24, 1, function(x) max(x))
  dta.var[,13] <- apply(dta.e.lag.48, 1, function(x) min(x))
  dta.var[,14] <- apply(dta.e.lag.48, 1, function(x) max(x))
  dta.var[,15] <- apply(dta.e.lag.36, 1, function(x) mean(x))
  
  # 1g Assign names for columns based on variable
  if(VarInitial == 't'){
    colnames(dta.var) <- c('HourName0', 
                        'MeanAbsFDT_24hr', 'MaxAbsFDT_24hr',
                        'SDT_24hr', 'DRT_24hr', 'SDMinMaxT_48hr',
                        'MeanFDT_24hr', 'DayDiffT_48hr', 
                        'MeanT_24hr', 'MeanT_48hr',
                        'MinT_24hr', 'MaxT_24hr', 'MinT_48hr', 'MaxT_48hr', 'MeanT_36hr')
  }
  if(VarInitial == 'r'){
    colnames(dta.var) <- c('HourName0', 
                          'MeanAbsFDR_24hr', 'MaxAbsFDR_24hr',
                          'SDR_24hr', 'DRR_24hr', 'SDMinMaxR_48hr',
                          'MeanFDR_24hr', 'DayDiffR_48hr', 
                          'MeanR_24hr', 'MeanR_48hr',
                          'MinR_24hr', 'MaxR_24hr', 'MinR_48hr', 'MaxR_48hr', 'MeanR_36hr')
  }
  
  # 1h return dataframe
  dta.var
}

####*************************************************************************
#### 2: Define Function to Compute Variability and Averages for One Year ####
####*************************************************************************

# 2a Declare overall function 
# We need this overall function so that we can run the variability calculation function 
# twice: for temp and for RH
calculate_variability <- function(YYYY){
  
  # 2b Add progress bar
  pb$tick()
  
  # 2c Readin the datasets
  DGK.temp <- fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data',
                                   paste0('c_3_casecontrolhours_assigned_hourly_', CaseType,'_', YYYY,
                                   '_','temp', '_', NLDASWeight, '_', Timing, '.fst')))
  DGK.rh <- fst::read_fst(here::here('DataPrep', 'Data', 'Intermediate_Data', 
                                 paste0('c_3_casecontrolhours_assigned_hourly_', CaseType,'_', YYYY,'_', 
                                 'rh', '_', NLDASWeight, '_', Timing, '.fst')))
  
  # 2d Calculate the means and variability metrics
  dgk.var.temp <- make_var_ave_data(DGK.temp, 't')
  dgk.var.rh <- make_var_ave_data(DGK.rh, 'r')
  
  # 2e Combine the calculated variables and hourly exposures 
  # note that only DGK.rh will keep the patient characteristic variables. 
  dta <- DGK.temp %>% 
    dplyr::select(HourName0, contains('tLag_')) %>% 
    inner_join(DGK.rh, by = 'HourName0') %>% 
    inner_join(dgk.var.temp, by = 'HourName0') %>%
    inner_join(dgk.var.rh, by = 'HourName0') 
  
  # 2f Save results
  dta %>% fst::write_fst(here::here('DataPrep', 'Data', 'Intermediate_Data', 
                                    paste0('c_4_casecontrolhours_assigned_variability_',
                              CaseType, '_', YYYY, '_', NLDASWeight, '_', Timing , '.fst')))
}

####*******************************************************
#### 3: Compute Variability and Averages for Each Year ####
####*******************************************************

# 3a Break up the years into chunks
YYYY.listA <- c('1995', '1996', '1997', '1998', '1999')
YYYY.listB <- c('2000', '2001', '2002', '2003')
YYYY.listC <- c('2004', '2005', '2006', '2007') 
YYYY.listD <- c('2008', '2009', '2010', '2011') 
YYYY.listE <- c('2012', '2013', '2014', '2015')
YYYY.listfake <- c('2000')

# 3b Set up mapping
future::plan(multisession)

if (CaseType == 'fake'){
  with_progress({
    p <- progressor(steps = length(YYYY.listfake)*2)
    result <- future_map(YYYY.listfake, calculate_variability, 
                         .options = furrr_options(seed = TRUE))
  })
  
} else{
  # 3c Compute variability
  with_progress({
    p <- progressor(steps = length(YYYY.listA)*2)
    result <- future_map(YYYY.listA, calculate_variability, 
                         .options = furrr_options(seed = TRUE))
  })
  with_progress({
    p <- progressor(steps = length(YYYY.listB)*2)
    result <- future_map(YYYY.listB, calculate_variability, 
                         .options = furrr_options(seed = TRUE))
  })
  with_progress({
    p <- progressor(steps = length(YYYY.listC)*2)
    result <- future_map(YYYY.listC, calculate_variability, 
                         .options = furrr_options(seed = TRUE))
  })
  with_progress({
    p <- progressor(steps = length(YYYY.listD)*2)
    result <- future_map(YYYY.listD, calculate_variability, 
                         .options = furrr_options(seed = TRUE))
  })
  with_progress({
    p <- progressor(steps = length(YYYY.listE)*2)
    result <- future_map(YYYY.listE, calculate_variability, 
                         .options = furrr_options(seed = TRUE))
  })
}

# 3d Tell the analyst that the script is done
cat('completed c_00d at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_c_00d), 1), ' min')
rm(StartTime_c_00d)
