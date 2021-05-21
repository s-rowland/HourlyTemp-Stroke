# Plot efigure 3: Effect Modification by HTN status, E-R for even lags 
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated Jan 26, 2021

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Plotting Function
  # 1A: Wrangle Model Independent Estimates
  # 1B: Generate Plot
# 2: Create Plots

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists("Ran_analysis_0_01")){
  here::i_am("README.md")
  source(here::here('HourlyTemp_Stroke', 'Scripts',
                    "0_01_setOutcomeName_folders_packages_data.R"))
}

# 0b Create the plotting objects, if you haven't already
if (!exists("HH.fig")){
  source(here::here('HourlyTemp_Stroke', 'Scripts', "G_01_set_PlottingObjects.R"))
}

####*********************************
#### 1: Create Plotting Function ####
####*********************************

# 1a Name function 
plot_ELR_subtype <- function(CaseType, Sensitivity, SubPopVar){
  #Sensitivity <- "Main"; CaseType <- "strokeHEM" 
  #SubPopVar <- "Afib" 
  
  ####*********************************************
  #### 1A: Wrangle Model Independent Estimates ####
  ####*********************************************
  
  # 1A.a Create list of SubPops to loop over
  SubPopList <- NameArray[[SubPopVar]]
  
  # 1A.b Create function to readin the estimates
  wrangle_estimates_forER <- function(SubPop){
    #SubPop <- SubPopList[1]
    # 1A.b.i Readin model estimates
    est.table <- readin_selectedEstimates("HourlyTemp", CaseType, Sensitivity, SubPopVar, SubPop, "EstInd")
    # 1A.b.ii Isolate the estimates for a 10-degree increase in temperature
    est.table <- est.table %>% 
      filter(Label %in% c("MeanPlus10")) 
    # 1A.b.ii Put estimates in long format 
    est.table <- est.table %>% 
      dplyr::select(-Label, -Sensitivity, -CaseType, -CounterfactualTemp, -SubPopVar, -IndCumul) %>% 
      gather("Lag", "Estimate", -SubPop) %>% 
      mutate(VarName = str_sub(Lag, 1, 3), Lag = as.numeric(str_sub(Lag, 11))) %>%  
      spread(VarName, Estimate) %>% 
      mutate(fit.pc = convert_to_percent(fit), 
             lci.pc = convert_to_percent(lci), uci.pc = convert_to_percent(uci)) 
  }
  
  # 1A.c Readin estimates & combine into a single dataframe 
  est.table <- bind_rows(map(SubPopList, wrangle_estimates_forER))
  
  # 1A.d Rename the SubPop variable 
  if(SubPopVar == 'HTN'){
    SupPopVarName <- "Hypertension"
    est.table <- est.table %>%
      mutate(Hypertension = if_else(SubPop == "HTN", "Hypertension", "No Hypertension"))
  }
  if(SubPopVar == 'Afib'){
    SupPopVarName <- "Atrial Fibrillation"
    est.table <- est.table %>%
      mutate(Hypertension = if_else(SubPop == "Afib", "Atrial Fibrillation", "No Atrial Fibrillation"))
  }
  ####***********************
  #### 1B: Generate Plot ####
  ####***********************  

    # 1B.a Set up colors and Labels and labs 
  if(CaseType == "strokeISC"){
    Ymin <- -4; Ymax <- 2;  YStep <- 0.5; legend.xy <- c(0.6, 0.8)
    CaseName <- 'Ischemic Stroke'; LabelSet <- "A"
  }
  if(CaseType == "strokeHEM"){
    Ymin <- -4; Ymax <- 2;  YStep <- 0.5 ; legend.xy <- "none"
    CaseName <- 'Hemorrhagic Stroke'; LabelSet <- "B"
  }
  
  # 1B.b Create plot of Exposure-Response for Even Lags
  TP <- est.table %>%
    ggplot(aes(Lag)) + 
    geom_hline(yintercept=0, color ="grey" ) + 
    geom_ribbon(aes(ymin= lci.pc,  ymax = uci.pc, fill= Hypertension),  alpha  = 0.25, col=NA) +
    geom_line(aes(y = fit.pc, col = Hypertension)) + 
    labs(y = paste0("Change in ", CaseName, "\n Hospitalizations (%)"), 
         x = paste0("Hourly Lag")) + 
    #coord_cartesian(ylim = c(Ymin, Ymax), expand = TRUE,
    #                default = FALSE, clip = "off") + 
    scale_y_continuous(breaks = seq(Ymin, Ymax, by = YStep)) +
    #scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
    scale_color_manual(values = ColorArray[[SubPopVar]]) + 
    scale_fill_manual(values = ColorArray[[SubPopVar]]) + 
    tema + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank())  +
    theme(panel.background = element_rect(fill= NA, color = "black")) +
    theme(legend.title=element_text(size=20),
          legend.text=element_text(size=16),
          legend.position = legend.xy, 
          legend.key.size = unit(0.4, "cm")) + 
    theme(axis.title.y = element_text(size = 20), 
          axis.text.x = element_text(size = 16), 
          axis.text.y = element_text(size = 16)) + 
    labs(fill = SupPopVarName, color = SupPopVarName)
  
  # 1B.c Return plot 
  tag_facet(TP, "", "", LabelSet, x = 0, y = Ymax, size = 8)
}

####*********************
#### 2: Create Plots ####
####*********************

# 2a Save each subtype's plot as an object
TP.a <- plot_ELR_subtype("strokeISC", "Main", "HTN")
TP.b <- plot_ELR_subtype("strokeHEM", "Main", "HTN")

# 2b Combine the plots via cowplot and then print the multi-panel figue 
png(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript', 
               "eFig3_htnLR.png"), 
    width = WW.fig*2, height = HH.fig*0.75, res =RR.fig*1)
print(cowplot::plot_grid(TP.a, TP.b, ncol = 2))
dev.off()

# 2c Save each subtype's plot as an object
TP.a <- plot_ELR_subtype("strokeISC", "Main", "Afib")
TP.b <- plot_ELR_subtype("strokeHEM", "Main", "Afib")

# 2d Combine the plots via cowplot and then print the multi-panel figue 
png(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript', 
               "eFig8_afibLR.png"), 
    width = WW.fig*2, height = HH.fig*0.75, res =RR.fig*1)
print(cowplot::plot_grid(TP.a, TP.b, ncol = 2))
dev.off()
