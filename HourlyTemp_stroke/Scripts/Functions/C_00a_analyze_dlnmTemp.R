# Function: Analyze association between HourlyTemp and stroke
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Begin Function
# 2: Prepare Data
# 3: Stratify Data 
# 4: Fit Health Model 
# 5: Save Results
#   5A: Save Model AIC
#   5B: Save Model Results

####**************
#### N: Notes #### 
####**************

# Na Description
# This code is the function for fitting all of the models 
# for the analysis 
# except for the negative control exposure sensitivity analysis 
# By using the same code, we can ensure that the exact same dataprocessing 
# and output is applied to each model, 
# without updating many individual codes 
# The code outputs a) the model, b) estimate for the mean to 10th and 90th percentile 
# c) for penalized spline models only, an ER plot.

# Nb ModelName 
# A critical object in this project is the ModelName 
# This identifiers provides all the unique information about a model 
# When you look a file name, you know exactly what the model is about

####********************
#### 0: Preparation #### 
####********************

####***********************
#### 1: Begin Function ####
####***********************

# 1a Name function
analyze_dlnmTemp <- function(ExpTerm, CaseType, Sensitivity, ERConstraint, 
                             LRConstraint, SubPopVar, SubPop, SaveModel){
  # ExpTerm <- 'HourlyTemp'; CaseType <- 'strokeISC'; Sensitivity <- 'Main'; 
  # ERConstraint <- '4dfevenknots'; LRConstraint <- '3dfevenknots';
  # SubPopVar <- 'Afib'; SubPop <- 'noAfib';  SaveModel <- 'SaveModel'

  # 1b Create ModelName
  ModelIdentifier <- paste0(ExpTerm, '_', CaseType)
  ExpConstraints <- paste0('ER', ERConstraint, '_LR', LRConstraint)
  ModelName <- paste0(ModelIdentifier,'_', Sensitivity, '_', ExpConstraints, '_', SubPopVar, '_', SubPop)
  
  # 1c Determine number of lag hours to include
  if(!str_detect(Sensitivity, 'LagHr') & !str_detect(Sensitivity, '2hrAve')){NumLag<- 36} 
  if(str_detect(Sensitivity, '2hrAve')){NumLag <- 17} 
  if(str_detect(Sensitivity, 'LagHr')){NumLag<- as.numeric(str_remove_all(Sensitivity, '[A-z]'))} 
  
  ####*********************
  #### 2: Prepare Data ####
  ####*********************
  
  # 2a Keep only primary stroke admissions according to outcome definition
  # we use the main primary outcome except for sensitivity analyses
  # the use different outcome criteria
  # this logical statement captures all analyses that  
  # use the main outcome definition
  if(CaseType == 'strokeISC' & !str_detect(Sensitivity, 'Prim')){
    dta <- dta %>% filter(strokeISC_prim == 1)
  } 
  if(CaseType == 'strokeHEM' & !str_detect(Sensitivity, 'Prim')){
    dta <- dta %>% filter(strokeHEM_prim == 1)
  }   

  # 2b Rename variable for mean RH, which we will adjust for. 
  RHMeanVar <- paste0('MeanR_', NumLag, 'hr')
  if(str_detect(Sensitivity, '2hrAve')){RHMeanVar <- paste0('MeanR_', 36, 'hr')} 
  dta <- dta %>% 
    rename(RHMean := !!RHMeanVar) 
  
  ####**********************
  #### 3: Stratify Data ####
  ####**********************
  
  # 3a Create HTN variable
  HTN <- c('401', '4010', '4011', '4019', '402', '4020', '40200', '40201', '403', '4030',
           '40300', '40301', '40310', '40311', '40390', '40391', '404', '4040', '40400',
           '40401', '40402', '40403', '4041', '40410', '40411', '40412', '40413', '4049', '40490', '40491', 
           '40492', '40493', '405', '4050', '40501', '40509', '4051', '40511', '40519',
           '4059', '40591', '40599', '642', '6420', '64200', '64201', '64202', '64203', '64204', '6421', 
           '64210', '64211', '64212', '64213', '64214', '6422', '64220', '64221', '64222',
           '64223', '64224', '6427', '64270', '64271', '64272', '64273', '64274')  
  dta <- dta %>% 
    mutate(HTN = if_else(DXA%in%HTN|DX01%in%HTN|DX02%in%HTN|DX03%in%HTN|DX04%in%HTN|DX05%in%HTN|
                           DX06%in%HTN|DX07%in%HTN|DX08%in%HTN|DX09%in%HTN|DX10%in%HTN|DX11%in%HTN|
                           DX12%in%HTN|DX13%in%HTN|DX14%in%HTN|DX15%in%HTN|DX16%in%HTN|DX17%in%HTN|
                           DX18%in%HTN|DX19%in%HTN|DX20%in%HTN|DX21%in%HTN|DX22%in%HTN|DX23%in%HTN|
                           DX24%in%HTN|DX25%in%HTN, 'HTN', 'noHTN'))
  
  # 3b Create TPA treatment variable
  dta <- dta %>% 
    mutate(PR01 = if_else(is.na(PR01), 'NA', PR01), 
           PR02 = if_else(is.na(PR02), 'NA', PR02),
           PR03 = if_else(is.na(PR03), 'NA', PR03),
           PR04 = if_else(is.na(PR04), 'NA', PR04),
           PR05 = if_else(is.na(PR05), 'NA', PR05)) %>%
    mutate(TPA = if_else(PR01 == '9910' | PR02 == '9910' | PR03 =='9910' | PR04 =='9910' | PR05 == '9910',
                         'ReceivedTPA', 'noTPA'))
  
  # 3c Create TPA+MT treatment variable
  dta <- dta %>% 
    mutate(TPA_MT = if_else(PR01 == '9910' | PR02 == '9910' | PR03 =='9910' | PR04 =='9910' | PR05 == '9910' |
                            PR01 == '61645' | PR02 == '61645' | PR03 =='61645' | PR04 =='61645' | PR05 == '61645',
                         'ReceivedTPA_MT', 'noTPA_MT'))

  # 3d Create Intracerebral subtype variable
  dta <- dta %>% 
    mutate(HEMSubType = if_else(str_sub(DXA, 0, 3)=='431'|str_sub(DX01, 0, 3)=='431'|
                                  str_sub(DX02, 0, 3)=='431'|str_sub(DX03, 0, 3)=='431', 
                                'Intracerebral', 'NotIntracerebral')) %>% 
    mutate(HEMSubType = if_else(str_sub(DXA, 0, 3)=='430'|str_sub(DX01, 0, 3)=='430'|
                                  str_sub(DX02, 0, 3)=='430'|str_sub(DX03, 0, 3)=='430', 
                                'Subarachnoid', HEMSubType))
  # 3e Create atrial fibrillation variable
  AF <- c('42731')
  dta <- dta %>% 
    mutate(Afib = if_else(DXA%in%AF|DX01%in%AF|DX02%in%AF|DX03%in%AF|DX04%in%AF|DX05%in%AF|
                            DX06%in%AF|DX07%in%AF|DX08%in%AF|DX09%in%AF|DX10%in%AF|DX11%in%AF|
                            DX12%in%AF|DX13%in%AF|DX14%in%AF|DX15%in%AF|DX16%in%AF|DX17%in%AF|
                            DX18%in%AF|DX19%in%AF|DX20%in%AF|DX21%in%AF|DX22%in%AF|DX23%in%AF|
                            DX24%in%AF|DX25%in%AF, 
                          'Afib', 'noAfib')) 
  # 3f Apply any stratification 
  if(SubPopVar != 'fullpop'){
    dta <- dta %>% 
      rename(SUBPOPVAR = !!SubPopVar) %>%
      filter(SUBPOPVAR == SubPop)
  }
  
  ####**************************
  #### 4: Fit Health Models ####
  ####**************************
  
  # 4a Create cross basis for hourly temperature 
  # 4a.i Set ER and LR constraints
  # we remove all of the letters from the  parameter, leaving just the number of df
  ERdf <- as.numeric(str_remove_all(ERConstraint, '[A-z]'))
  LRdf <- as.numeric(str_remove_all(LRConstraint, '[A-z]'))
  # 4a.ii Create crossbasis
  # Here I include more options than I use in the main model 
  if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){
    cb.hrtemp <- crossbasis(
      # in this first line we create a matrix with just the temperature lags of interest
      as.matrix(dplyr::select(dta, contains('tLag_')))[,1:NumLag], 
                            lag=c(0,(NumLag-1)),
                            argvar=list(fun='lin'),
                            arglag=list(fun='ns', df = LRdf))}
  if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){
    cb.hrtemp <- crossbasis(
      as.matrix(dplyr::select(dta, contains('tLag_')))[,1:NumLag],
                            lag=c(0,(NumLag-1)),
                            argvar=list(fun='ns', df = ERdf),
                            arglag=list(fun='ns', df = LRdf))}
  if(str_detect(LRConstraint, 'psp')& !str_detect(ERConstraint, 'psp')){
    cb.hrtemp <- crossbasis(
      as.matrix(dplyr::select(dta, contains('tLag_')))[,1:NumLag],
                            lag=c(0,(NumLag-1)),
                            argvar=list(fun='ns', df = ERdf),
                            arglag=list(fun='ps', df=10))}

  # 4b Fit Main Models
  # 4b.i Typical non-linear model 
  # a few sensitivity analyses change the regression formula; 
  # these are in the formulaList 
  # coxph() does not like when the formula is an object from the environment
  # so we need to use the if else statements to switch models.
  if(!(Sensitivity %in% c('RHnoAdj', 'RHdlnm'))){
    mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrtemp + 
                 ns(RHMean, df = 4) +
                 strata(EventID),
               dta, ties = 'efron')
  }
  if(Sensitivity == 'RHnoAdj'){
    mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrtemp + 
                   strata(EventID),
                 dta, ties = 'efron')
  }
  
  if(Sensitivity == 'RHdlnm'){
    #rh.profiles <- as.matrix(dplyr::select(dta, contains('rLag_')))[,1:NumLag]
    #rhp1 <- rh.profiles
    cb.hrrh <- crossbasis(
      as.matrix(dplyr::select(dta, contains('rLag_')))[,1:NumLag], 
                            lag=c(0,(NumLag-1)),
                            argvar=list(fun='ns', df = 4),
                            arglag=list(fun='ns', df = 3))
    mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrtemp + 
                   cb.hrrh +
                   strata(EventID),
                 dta, ties = 'efron')
  }
  
  ####*********************
  #### 5: Save Results ####
  ####*********************
  
  ####************************
  #### 5A: Save Model AIC ####
  ####************************
  
  # 5A.a Begin option
  # when we do the grid search, we want to just save the model's AIC and not its results
  if(SaveModel == 'StoreAIC'){
    
    # 5A.b Readin the table of AIC's
    aic.table <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 
                                      'Model_AIC.csv'), col_types = 'cccdT')
    
    # 5A.c Add this aic to the set of AIC's
    aic.table[1+nrow(aic.table),] <- list(ModelIdentifier,
                                          ERConstraint, LRConstraint, AIC(mod), Sys.time())
    
    # 5A.d Remove any old AICs and then save
    # at the slice step you keep only the earliest AIC for each model-constraint combo
    aic.table %>% 
      group_by(ModelIdentifier,ERConstraint, LRConstraint) %>% 
      arrange(desc(RunDate)) %>% 
      slice(0:1) %>% 
      filter(!is.na(ModelIdentifier)) %>%
      write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                           'Model_AIC.csv'))
  }
  
  ####****************************
  #### 5B: Save Model Results ####
  ####****************************
  
  # 5B.a Begin option  
  if(SaveModel == 'SaveModel'){
    
    # 5B.b Save the model 
    mod %>% saveRDS(here::here('HourlyTemp_Stroke', OutputsPath, 'Models',
                               paste0(ModelName, '.RDS')))

    # 5B.c Generate estimates
    # the cen argument sets the reference exposure level for our effect estimates
    # crosspred() will yield estimates for 100 exposure levels plus those exposure levels we set
    est <- crosspred(cb.hrtemp,
                      mod,
                      cen = HourlyTemp.mean,
                      at = expContrasts$CounterfactualTemp, 
                      cumul=TRUE, 
                      bylag=0.2)
  
    # 5B.d Extract coefficient fit and CI 
    fit.table <- as.data.frame(est$matRRfit)  
    colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
    fit.table <- fit.table %>%  
      mutate(CounterfactualTemp = as.numeric(row.names(fit.table)))

    lci.table <- as.data.frame(est$matRRlow)  
    colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
    
    uci.table <- as.data.frame(est$matRRhigh)  
    colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
    
    # 5B.e Combine fit and se for individual lags 
    # note that all OR are relative to the mean temperature. 
    est.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # 5B.f Attach the labels of the exposure contrasts
    est.table <- est.table %>% full_join(expContrasts, by = 'CounterfactualTemp')
    
    # 5B.g Save estimate table for individual lags 
    est.table %>%
      write.csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Estimates', 
                                       paste0('EstInd_', ModelName, '.csv')))
    
    # 5B.h Extract cumulative coefficient fit and ci  
    fit.table <- as.data.frame(est$cumRRfit)  
    colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
    fit.table <- fit.table %>%  
      mutate(CounterfactualTemp = as.numeric(row.names(fit.table)))
    
    lci.table <- as.data.frame(est$cumRRlow)  
    colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
    
    uci.table <- as.data.frame(est$cumRRhigh)  
    colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
    
    # 5B.i Combine fit and se for individual lags 
    # note that all OR are relative to the mean temperature. 
    est.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # 5B.j Attach the labels of the exposure contrasts
    est.table <- est.table %>% full_join(expContrasts, by = 'CounterfactualTemp')
    
    # 5B.k Save cumulative estimates table 
    est.table %>% 
      write.csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Estimates',
                           paste0('EstCumul_', ModelName, '.csv')))
  } 
  
}
