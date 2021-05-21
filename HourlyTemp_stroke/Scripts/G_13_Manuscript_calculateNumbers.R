# Calculate numbers for manuscript
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated Jan 26, 2021

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Abstract
# 2: Methods 
# 3: Results - Ischemic Stroke - Data Description
# 4: Results - Ischemic Stroke - Effect Estimates
# 5: Results - Hemorrhagic Stroke - Data Description
# 6: Results - Hemorrhagic Stroke - Effect Estimates

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the script is beginning 
StartTime_G_13 <- Sys.time()
print(paste("begin G_13 at", StartTime_G_13))

# 0b Create the folder structure, if you haven't already
if (!exists("Ran_analysis_0_01")){
  here::i_am("README.md")
  source(here::here('HourlyTemp_Stroke', 'Scripts',
                    "0_01_setOutcomeName_folders_packages_data.R"))
}

# 0c Create the plotting objects, if you haven't already
if (!exists("HH.fig")){
  source(here::here('HourlyTemp_Stroke', 'Scripts', "G_01_set_plottingObjects.R"))
}

# 0d Define rounding function 
# mp stands for "make pretty" 
# we just use this function for getting these numbers 
# since we have to round everything anyways
mp <- function(Number){round(Number,1)}

####*****************
#### 1: Abstract ####
####*****************

# 1a Number ISCHEMic strokes 
# 1a.i Readin TableOne
TableOne <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript',
                                "Table1_TableOne.csv"))
# 1a.ii Get number
a1_NumISC <- str_remove(TableOne$N_strokeISC[1], " (100.00%)")

# 1b Number HEMorrhagic strokes
a2_NumHEM <- str_remove(TableOne$N_strokeHEM[1], " (100.00%)")

# 1c Mean and SD age 
# 1c.i Create set of cases 
cases.ISC <- dta %>% 
  filter(Case == 1 & strokeISC_prim == 1) %>% 
  mutate(woman = if_else(sex == "F", 1, 0))
cases.HEM <- dta %>% 
  filter(Case == 1 & strokeHEM_prim == 1) %>% 
  mutate(woman = if_else(sex == "F", 1, 0))
# 1c.ii Calculate numbers
a3.AgeMeanSD <- paste0(mp(mean(cases.ISC$age)), "(", mp(sd(cases.ISC$age)), ")", 
            mp(mean(cases.HEM$age)), "(", mp(sd(cases.HEM$age)), ")")

# 1d Percent women 
a4.PercWomen <- paste0(mp(100*mean(cases.ISC$woman)), "and", mp(100*mean(cases.HEM$woman)))

# 1e Temperature range 
a5.TempRange <- paste0("(", mp(HourlyTemp.min), ", ", mp(HourlyTemp.max), ")")

# 1f Temperature mean and SD 
a6.TempMeanSD <- paste0("(", mp(HourlyTemp.mean), ", ", mp(HourlyTemp.sd), ")")

# 1g Relevant window for ISC 
a7.WindowISC <- MaxSigLag.ISC + 1

# 1h Effect estimate ( + CI) for cumulative association for relevant window for ISC 
# 1h.i Readin table of effect estimates 

# this should read in that EE table for the supplement, for all parts of this 
est.table <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript',
                                 "etable3_EE_fullpopAnalysis.csv"))

a8.EEISC <- est.table$strokeISC_EstCumul[(MaxSigLag.ISC+1)]

# 1i Relevant lags for HEM 
a9.WindowHEM <- MaxSigLag.HEM +1

# 1j Effect estimate for HEM 
a10.EEHEM <- est.table$strokeHEM_EstCumul[(MaxSigLag.HEM+1)]

# 1k Create list of numbers

Abstract <- c(a1_NumISC=a1_NumISC, a2_NumHEM=a2_NumHEM, a3.AgeMeanSD=a3.AgeMeanSD, 
              a4.PercWomen=a4.PercWomen, a5.TempRange=a5.TempRange, a6.TempMeanSD=a6.TempMeanSD, 
              a7.WindowISC= a7.WindowISC, a8.EEISC=a8.EEISC, a9.WindowHEM=a9.WindowHEM, 
              a10.EEHEM=a10.EEHEM)
Abstract 
rm(a1_NumISC, a2_NumHEM, a3.AgeMeanSD, a4.PercWomen, a5.TempRange, 
   a6.TempMeanSD, a7.WindowISC, a8.EEISC, a9.WindowHEM, a10.EEHEM)

####****************
#### 2: Methods ####
####****************

# 2a Cases that recieved TPA or MT 
dta0 <- fst::read.fst(here::here('DataPrep', 'Data', 'Final_Data', 
                                paste0("prepared_cohort", "_", OutcomeName,
                                       "_identified", "_", "Pop", "_", "3hrDelay", ".fst")))

cases.ascertain0 <- dta0 %>% 
  filter(Case == 1) %>%
  mutate(PR01 = if_else(is.na(PR01), "NA", PR01), 
         PR02 = if_else(is.na(PR02), "NA", PR02),
         PR03 = if_else(is.na(PR03), "NA", PR03),
         PR04 = if_else(is.na(PR04), "NA", PR04),
         PR05 = if_else(is.na(PR05), "NA", PR05)) %>%
  mutate(TPA_MT = if_else(PR01 == "9910" | PR02 == "9910" | PR03 =="9910" | PR04 =="9910" | PR05 == "9910" |
                            PR01 == "61645" | PR02 == "61645" | PR03 =="61645" | PR04 =="61645" | PR05 == "61645",
                          "ReceivedTPA_MT", "noTPA_MT"))
# Identify cases
cases.ascertain1.ISC <- cases.ascertain0 %>% 
  filter(TPA_MT == "ReceivedTPA_MT")
# Isolated not-yet-ascertained cases 
cases.ascertain1.undef <- cases.ascertain0 %>% 
  filter(TPA_MT == "noTPA_MT")
# Get number
b1.Rule1ISC <- nrow(cases.ascertain1.ISC)

# 2b number that had just codes for ISC 
# among cases that didn’t recieve TPA or MT
# These cases have already been filtered to contain at least 
# one relevant stroke case 
ISC <- c(43301, 43311, 43321, 43331, 43381, 43391,
         43401, 43411, 43491, 436, 436)
HEM <- c(430,431, 4310, 43100, 432, 4320,43200, 4321,4329)
cases.ascertain2.ISC <- cases.ascertain1.undef %>% 
  filter(!DXA%in%HEM & !DX01%in%HEM & !DX02%in%HEM & !DX03%in%HEM)
b2.Rule2ISC <- nrow(cases.ascertain2.ISC)

# 2c number that had just codes for HEM 
cases.ascertain2.HEM <- cases.ascertain1.undef %>% 
  filter(!DXA%in%ISC & !DX01%in%ISC & !DX02%in%ISC & !DX03%in%ISC)
b3.Rule2HEM <- nrow(cases.ascertain2.HEM)

# 2d DXA is ISC 
#    among those with both diagnoses, 
# remove the cases we identified in steps 2b and 2c
cases.ascertain2.ISC.EventID <- cases.ascertain2.ISC %>% dplyr::select(EventID)
cases.ascertain2.HEM.EventID <- cases.ascertain2.HEM %>% dplyr::select(EventID)
cases.ascertain2.undef <- cases.ascertain1.undef %>% 
  anti_join(cases.ascertain2.ISC.EventID, by = "EventID") %>% 
  anti_join(cases.ascertain2.HEM.EventID, by = "EventID")
# Get number of ISC 
cases.ascertain.3.ISC <- cases.ascertain2.undef %>% 
  filter(DXA%in%ISC)
b4.Rule3ISC <- nrow(cases.ascertain.3.ISC)
# 2e DXA  is HEM 
#    among those with both diagnoses, 
cases.ascertain.3.HEM <- cases.ascertain2.undef %>% 
  filter(DXA%in%HEM)
b5.Rule3HEM <- nrow(cases.ascertain.3.HEM)
 
# 2f remaining cases (ambiguous)  
cases.ascertain3.undef <- cases.ascertain2.undef %>% 
  filter(!DXA%in%ISC & !DXA%in%HEM)
b6.Rule3Ambig <- nrow(cases.ascertain3.undef)

# 2g Create list of numbers
Methods <- c(b1.Rule1ISC=b1.Rule1ISC, b2.Rule2ISC=b2.Rule2ISC, b3.Rule2HEM=b3.Rule2HEM,
             b4.Rule3ISC=b4.Rule3ISC, b5.Rule3HEM=b5.Rule3HEM, b6.Rule3Ambig=b6.Rule3Ambig)
Methods 

# 2h Clean up the environment
rm(b1.Rule1ISC, b2.Rule2ISC, b3.Rule2HEM, b4.Rule3ISC, b5.Rule3HEM, b6.Rule3Ambig, 
   cases.ascertain0, cases.ascertain1.ISC, cases.ascertain1.undef, cases.ascertain2.ISC, 
   cases.ascertain2.HEM, cases.ascertain2.ISC.EventID, cases.ascertain2.HEM.EventID, 
   cases.ascertain2.undef, cases.ascertain.3.ISC, cases.ascertain.3.HEM, cases.ascertain3.undef)

####*****************************************************
#### 3: Results - Ischemic Stroke - Data Description ####
####*****************************************************

# 3a number ISC 
cases.ICS.distinct <- cases.ISC %>% 
  dplyr::select(UPID) %>% 
  distinct()
c1.ISCnum <- c( nrow(cases.ISC), nrow(cases.ICS.distinct))

# 3b Number missing due to missing admission datetime or zip code 
missingData <- read_csv(here::here(
  paste0("DataPrep/Data/Final_Data/PercentMissing_", OutcomeName, ".csv")))

c2.ISCmiss <- missingData$PercentMissingISC[1]

# 3c mean & sd age for ISC 
c3.ISCage <- paste0(mp(mean(cases.ISC$age)), "(", mp(sd(cases.ISC$age)), ")")

# 3d mean temp among ISC cases and among ISC controls 
controls.ISC <- dta %>% 
  filter(Case == 0 & strokeISC_prim == 1)

# 3e Calculate range 
cases.ISC.exp <- cases.ISC %>% 
  dplyr::select(contains("tLag_"))

cases.ISC.exp <- as.matrix(cases.ISC.exp)[,1:36]
cases.ISC.exp <- as.vector(cases.ISC.exp)

controls.ISC.exp <- controls.ISC %>% 
  dplyr::select(contains("tLag_"))

controls.ISC.exp <- as.matrix(controls.ISC.exp)[,1:36]

controls.ISC.exp <- as.vector(controls.ISC.exp)

c4.ISCtRange <- c(mp(min(c(min(cases.ISC.exp), min(controls.ISC.exp)))), 
                  mp(max(c(max(cases.ISC.exp), max(controls.ISC.exp)))))

# 3f mean temp among ISC cases and among ISC controls 
c5.ISCtMeanSD <- paste0(mp(mean(cases.ISC.exp)), " (", 
                        mp(sd(cases.ISC.exp)), ") ", "cases and ", 
                        mp(mean(controls.ISC.exp)), " (", 
                        mp(sd(controls.ISC.exp)), ") ","controls")

# 3g Create list of numbers
ResultsISCdesc <- c(c1.ISCnum=c1.ISCnum, c2.ISCmiss=c2.ISCmiss, 
                        c3.ISCage=c3.ISCage, c4.ISCtRange=c4.ISCtRange, 
                        c5.ISCtMeanSD=c5.ISCtMeanSD)
ResultsISCdesc
rm(c1.ISCnum, c2.ISCmiss, c3.ISCage, c4.ISCtRange, c5.ISCtMeanSD,
   controls.ISC, cases.ISC.exp, controls.ISC.exp)

####*****************************************************
#### 4: Results - ISCHEMic Stroke - Effect Estimates ####
####*****************************************************

# 4a Constraints
# Readin AIC Table 
aic.table <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                 "Model_AICweights.csv"), col_types = "cccddT") 
# Get main model for ISC 
aic.ISC.main <- aic.table %>% 
  filter(ModelIdentifier == paste0("HourlyTemp", "_", "strokeISC")) %>% 
  arrange(AIC)
SelectedModel <- aic.ISC.main[1,]
d1.ISCconstraints <- paste0("ER: ", SelectedModel$ERConstraint[1], 
             " LR: ", SelectedModel$LRConstraint[1])

# 4b Akaike weight 
d2.ISCaic <- SelectedModel$AkaikeWeight

# 4c EE for T_00 
d3.ISCeelag0 <- est.table$strokeISC_EstInd[1]

# 4d EE for cumulative for T_07 
d4.ISCeelag07 <- est.table$strokeISC_EstCumul[7]

# 4e EE for ucmulative for T_35 
d5.ISCeelag35 <- est.table$strokeISC_EstCumul[36]

# 4f p value for interaction
# get emm table 
emm_table <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                 "emm_table.csv"))
d6.ISCemm <- emm_table$PValue[emm_table$CaseType=="strokeISC"]

# Get numbers for secondary analysis
est_tableAfib <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 
                            "estimates_table_Afib.csv")) %>% 
  mutate(CaseAfib = paste0(CaseType, '_', SubPop))

d7.AfibEE <- paste0(round(est_tableAfib$fit[est_tableAfib$CaseAfib == 'strokeISC_Afib'], 2), '% (', 
             round(est_tableAfib$lci[est_tableAfib$CaseAfib == 'strokeISC_Afib'], 2), ', ', 
             round(est_tableAfib$uci[est_tableAfib$CaseAfib == 'strokeISC_Afib'], 2), ')')
d8.noAfibEE <- paste0(round(est_tableAfib$fit[est_tableAfib$CaseAfib == 'strokeISC_noAfib'], 2), '% (', 
             round(est_tableAfib$lci[est_tableAfib$CaseAfib == 'strokeISC_noAfib'], 2), ', ', 
             round(est_tableAfib$uci[est_tableAfib$CaseAfib == 'strokeISC_noAfib'], 2), ')')

emm_tableAfib <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 
                            "emm_table_Afib.csv"))

d9.Afibpval <- emm_tableAfib$PValue[emm_tableAfib$CaseType =='strokeISC'] 

# 4g Create list of numbers
ResultsISCee <- c(d1.ISCconstraints=d1.ISCconstraints, 
                d2.ISCaic=d2.ISCaic, d3.ISCeelag0=d3.ISCeelag0, 
                d4.ISCeelag07=d4.ISCeelag07, d5.ISCeelag35=d5.ISCeelag35, 
                d6.ISCemm=d6.ISCemm, d7.AfibEE=d7.AfibEE, d8.noAfibEE=d8.noAfibEE, 
                d9.Afibpval=d9.Afibpval)
ResultsISCee

rm(d1.ISCconstraints, d2.ISCaic, d3.ISCeelag0, d4.ISCeelag07,
   d5.ISCeelag35, d6.ISCemm, aic.ISC.main, d7.AfibEE, d8.noAfibEE, d9.Afibpval)

####********************************************************
#### 5: Results - Hemorrhagic Stroke - Data Description ####
####********************************************************

# 5a Number missing due to missing admission datetime or zip code 

missingData <- read_csv(here::here(paste0("DataPrep/Data/Final_Data/PercentMissing_", 
                                          OutcomeName, ".csv")))

e1.HEMmiss <- missingData$PercentMissingHEM[1]

# 5b number HEM 
cases.ICS.distinct <- cases.HEM %>% 
  dplyr::select(UPID) %>% 
  distinct()
e2.HEMnum <- c( nrow(cases.HEM), nrow(cases.ICS.distinct))

# 5c mean & sd age for HEM 
e3.HEMage <- paste0(mp(mean(cases.HEM$age)), "(", mp(sd(cases.HEM$age)), ")")

# 5d % HEM of each of the two big subtypes 
# 5d.i Create Intracerebral subtype variable
cases.HEM <- cases.HEM %>% 
  mutate(HEMSubType = if_else(str_sub(DXA, 0, 3)=="431"|str_sub(DX01, 0, 3)=="431"|
                                str_sub(DX02, 0, 3)=="431"|str_sub(DX03, 0, 3)=="431", 
                              "Intracerebral", "NotIntracerebral")) %>% 
  mutate(HEMSubType = if_else(str_sub(DXA, 0, 3)=="430"|str_sub(DX01, 0, 3)=="430"|
                                str_sub(DX02, 0, 3)=="430"|str_sub(DX03, 0, 3)=="430", 
                              "Subarachnoid", HEMSubType))
cases.HEM.intercerebral <- cases.HEM %>% filter(HEMSubType=="Intracerebral")
cases.HEM.subarach <- cases.HEM %>% filter(HEMSubType=="Subarachnoid")
e4.HEMsubsubtype <- paste0(nrow(cases.HEM.intercerebral), " Intercereb and ",
                           nrow(cases.HEM.subarach), "subarach")

# 5e mean temp among HEM cases and among HEM controls 
controls.HEM <- dta %>% 
  filter(Case == 0 & strokeHEM_prim == 1)

# 5f Calculate range 
cases.HEM.exp <- cases.HEM %>% 
  dplyr::select(contains("tLag_"))
cases.HEM.exp <- as.matrix(cases.HEM.exp)[,1:36]
cases.HEM.exp <- as.vector(cases.HEM.exp)

controls.HEM.exp <- controls.HEM %>% 
  dplyr::select(contains("tLag_"))
controls.HEM.exp <- as.matrix(controls.HEM.exp)[,1:36]
controls.HEM.exp <- as.vector(controls.HEM.exp)

e5.HEMtRange <- c(mp(min(c(min(cases.HEM.exp), min(controls.HEM.exp)))), 
                  mp(max(c(max(cases.HEM.exp), max(controls.HEM.exp)))))

# 5g mean temp among HEM cases and among HEM controls 
e6.HEMtMeanSD <- paste0(mp(mean(cases.HEM.exp)), " (", 
                        mp(sd(cases.HEM.exp)), ") ", "cases and ", 
                        mp(mean(controls.HEM.exp)), " (", 
                        mp(sd(controls.HEM.exp)), ") ","controls")




# 5h Create list of numbers
ResultsHEMdesc <- c(e1.HEMmiss=e1.HEMmiss, e2.HEMnum=e2.HEMnum,
                        e3.HEMage=e3.HEMage, e4.HEMsubsubtype=e4.HEMsubsubtype, 
                        e5.HEMtRange=e5.HEMtRange, e6.HEMtMeanSD=e6.HEMtMeanSD)
ResultsHEMdesc

rm(e1.HEMmiss, e2.HEMnum, e3.HEMage, e5.HEMtRange, e6.HEMtMeanSD,
   controls.HEM, cases.HEM.exp, controls.HEM.exp)

####********************************************************
#### 6: Results - Hemorrhagic Stroke - Effect Estimates ####
####********************************************************

# 6a Constraints
# 6a.i Readin AIC Table 
aic.table <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                 "Model_AICweights.csv"), 
                      col_types = "cccddT") 
# 6a.ii Get main model for HEM 
aic.HEM.main <- aic.table %>% 
  filter(ModelIdentifier == paste0("HourlyTemp", "_", "strokeHEM")) %>% 
  arrange(AIC)
SelectedModel <- aic.HEM.main[1,]
f1.HEMconstraints <- paste0("ER: ", SelectedModel$ERConstraint[1], 
                            " LR: ", SelectedModel$LRConstraint[1])

# 6b Akaike weight 
f2.HEMaic <- SelectedModel$AkaikeWeight

# 6c EE for T_00 
f3.HEMeelag0 <- est.table$strokeHEM_EstInd[1]

# 6d EE for cumulative for T_07 
f4.HEMeelag04 <- est.table$strokeHEM_EstCumul[4]

# 6e EE for cumulative for T_35 
f5.HEMeelag35 <- est.table$strokeHEM_EstCumul[36]

# 6f p value for interaction
# get emm table 
emm_table <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables',
                                 "emm_table.csv"))
f6.HEMemm <- emm_table$PValue[emm_table$CaseType=="strokeHEM"]


f7.AfibEE <- paste0(round(est_tableAfib$fit[est_tableAfib$CaseAfib == 'strokeHEM_Afib'], 2), '% (', 
                    round(est_tableAfib$lci[est_tableAfib$CaseAfib == 'strokeHEM_Afib'], 2), ', ', 
                    round(est_tableAfib$uci[est_tableAfib$CaseAfib == 'strokeHEM_Afib'], 2), ')')
f8.noAfibEE <- paste0(round(est_tableAfib$fit[est_tableAfib$CaseAfib == 'strokeHEM_noAfib'], 2), '% (', 
                      round(est_tableAfib$lci[est_tableAfib$CaseAfib == 'strokeHEM_noAfib'], 2), ', ', 
                      round(est_tableAfib$uci[est_tableAfib$CaseAfib == 'strokeHEM_noAfib'], 2), ')')


f9.Afibpval <- emm_tableAfib$PValue[emm_tableAfib$CaseType =='strokeHEM'] 

# 6g Create list of numbers
ResultsHEMee <- c(f1.HEMconstraints=f1.HEMconstraints, 
                  f2.HEMaic=f2.HEMaic, f3.HEMeelag0=f3.HEMeelag0, 
                  f4.HEMeelag04=f4.HEMeelag04, f5.HEMeelag35=f5.HEMeelag35, 
                  f6.HEMemm=f6.HEMemm, f7.AfibEE=f7.AfibEE, f8.noAfibEE=f8.noAfibEE, 
                  f9.Afibpval=f9.Afibpval)

ResultsHEMee
rm(f1.HEMconstraints, f2.HEMaic, f3.HEMeelag0, f4.HEMeelag04,
   f5.HEMeelag35, f6.HEMemm, aic.HEM.main, f7.AfibEE, f8.noAfibEE, 
   f9.Afibpval)

# 6h Tell the analyst that the analysis is done
cat("completed G_13 at ", paste(Sys.time()), 
    "\n total time: ", round((Sys.time() - StartTime_G_13)/60, 1), " min")

rm(StartTime_G_13)
