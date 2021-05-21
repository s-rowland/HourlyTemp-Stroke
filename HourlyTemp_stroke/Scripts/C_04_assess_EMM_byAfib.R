# Assess effect modification by hypertension
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated Jan 26, 2021

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Table of Estimates
# 2: Define Comparison Function
# 3: Compare Estimates

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists("Ran_analysis_0_01")){
  here::i_am("README.md")
  source(here::here('HourlyTemp_Stroke', 'Scripts',
                    "0_01_setOutcomeName_folders_packages_data.R"))}

####**********************************
#### 1: Create Table of Estimates ####
####**********************************

# 1a Readin table of the relevant Windows
relevantWindows <- read_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 
                                       "relevantWindows.csv"))

# 1b Setup table of models to tabulate
SensitivityList <- rep("Main", 4)
ExpTermList <- rep(c("HourlyTemp"), 4)
CaseTypeList <- c(rep("strokeISC", 2), rep("strokeHEM", 2))
SubPopVarList <- rep(c("Afib", "Afib"), 2)
SubPopList <- rep(c("Afib", "noAfib"), 2)
IndCumulList <- rep("EstCumul", length(SensitivityList))

# 1c Readin estimates 
# we will get a warning about "Missing column names filled in: 'X1' [1]"
# We can ignore this warning because we don't interact with this column 
# which is empty anyways - an artifact of Excel.
est.list <- purrr::pmap(list(ExpTermList, CaseTypeList, SensitivityList, SubPopVarList, SubPopList, IndCumulList), 
                        readin_selectedEstimates)
est.table <- bind_rows(est.list)

# 1d Keep only the exposure contrasts of interest 
est.table <- est.table %>% 
  filter(Label == "MeanPlus10")

# 1e As usual, wrangle the estimates
est.table  <- est.table %>% 
  dplyr::select(-SubPopVar, -Sensitivity) %>% 
  gather("Lag", "Estimate", -CounterfactualTemp, -Label, -SubPop, -CaseType, -IndCumul) %>% 
  mutate(VarName = str_sub(Lag, 1, 3), Lag = as.numeric(str_sub(Lag, 11))) %>%  
  spread(VarName, Estimate) %>% 
  mutate(Lag = as.character(str_pad(Lag, 2, "left", "0")))%>% 
  mutate(fit.beta = log(fit), lci.beta = log(lci),uci.beta = log(uci))



# Save the relevant estimates 
est.table %>% 
  filter((Lag == '07' & CaseType == 'strokeISC')|(Lag == '04' & CaseType == 'strokeHEM')) %>% 
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 
                       "estimates_table_Afib.csv"))

####***********************************
#### 2: Define Comparison Function ####
####***********************************

# 2a Name the Function  
compare_stratified_effect_estimates <- function(CaseType0, Lag0, SubPop1, SubPop2){
  # CaseType0 <- "strokeISC"; Lag0 <- "00"
  # SubPop1 <- "HTN"; SubPop2 <- "noHTN"
  # 2b Keep the right percentile and tempvarmetric
  est.table <- est.table %>% 
    filter(CaseType == CaseType0, Lag == Lag0)
  # 2c Identify corresponding ORs
  beta1 <- est.table$fit.beta[est.table$SubPop == SubPop1]
  beta2 <- est.table$fit.beta[est.table$SubPop == SubPop2]
  # 2d Calculate difference of the logRR 
  diff.beta <- beta1 - beta2 
  # 2e Get the se
  # we need to recalculate this becuase we did not save it 
  # during the  model-fitting step
  lci.beta1 <- est.table$lci.beta[est.table$SubPop == SubPop1]
  uci.beta1 <- est.table$uci.beta[est.table$SubPop == SubPop1]
  se1 <- (uci.beta1 - lci.beta1 ) / (2 * 1.96)
  lci.beta2 <- est.table$lci.beta[est.table$SubPop == SubPop2]
  uci.beta2 <- est.table$uci.beta[est.table$SubPop == SubPop2]
  se2 <- (uci.beta2 - lci.beta2 ) / (2 * 1.96)
  # 2f Calculate the joint se 
  se.diff <- sqrt(se1^2 + se2^2)
  # 2g Calculate the z score 
  z.score <- diff.beta / se.diff
  # 2h Calculate P value 
  p.value <- 2 * (1 - pnorm(abs(z.score)))
  # 2i Return value 
  round(p.value, 4)
}

####**************************
#### 3: Compare Estimates ####
####**************************

# 3a Create table 
emm_table <- data.frame(
  CaseType = c("strokeISC", "strokeHEM"),
  Lag = str_pad(relevantWindows$relevantWindows, 2, "left", "0"),
  SubPop1 = rep("Afib", 2),
  SubPop2 = rep("noAfib", 2),
  PValue = rep(NA, 2)
)

# 3b  Fill table 
for (i in 1: nrow(emm_table)){
  emm_table$PValue[i] <- compare_stratified_effect_estimates(emm_table$CaseType[i], 
                                                             emm_table$Lag[i],
                                                             emm_table$SubPop1[i],
                                                             emm_table$SubPop2[i]) 
}

# 3c Save table 
emm_table %>% 
  write_csv(here::here('HourlyTemp_Stroke', OutputsPath, 'Tables', 
                       "emm_table_Afib.csv"))
