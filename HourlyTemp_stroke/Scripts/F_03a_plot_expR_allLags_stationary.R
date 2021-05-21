# Create Folder Structure
# TempVar-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated 10/30/2020

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Set Up Plotting Objects 
# 2: Create Plotting Function 
# 3: Create Plots

####********************
#### 0: Preparation #### 
####********************

# 0b source this function
source(paste0(scripts.folder, "G_00_set_plottingFeaturesObjects.R"))

####********************************
#### 1: Set Up Plotting Objects #### 
####********************************

# 1a Create plotting theme 
tema <-   theme_classic() +
  theme(plot.title = element_text(size = 36, vjust = 1, hjust =0.5)) +
  theme(axis.title.x = element_text(size = 30, vjust =0, lineheight=0.25)) + 
  theme(axis.title.y = element_text(size = 30, angle = 90, vjust= 0.5)) + 
  theme(axis.text.x = element_text(size = 20)) + 
  theme(axis.text.y = element_text(size = 20, angle = 0))


# 1b fit linear term models 
# 1b.i Start function
fit_noLinearConstraints <- function(ExpTerm, CaseType, SubPopVar, SubPop){
  
  # 1b.ii Create ModelIdentifiers to identify the main models 
  ModelIdentifier <- paste0(ExpTerm, "_", CaseType)
  
  # 1b.iii Keep only the AIC of models with our ModelIdentifier of interest
  # and remove linear constraints
  aic.table <- read_csv(paste0(tables.folder, "Model_AICweights.csv"), 
                        col_types = "cccddT") %>% 
    filter(ModelIdentifier == !!ModelIdentifier) %>% 
    filter(ERConstraint != "lin")
  
  # 1b.iv Choose model with lowest AIC among remaining constraints
  aic.table <- aic.table %>% arrange(AIC)
  SelectedModel <- aic.table[1,]
  
  # 1b.v Identify appropriate model function
  source(paste0(scripts.folder, "C_00a_analyze_dlnmTemp_function.R"))
  analyze_model <- analyze_dlnmTemp 
  
  # 1b.vi Fit and store selected model
  analyze_model(ExpTerm, CaseType, "noLinTerm", 
                SelectedModel$ERConstraint[1], SelectedModel$LRConstraint[1], 
                SubPopVar, SubPop, "SaveModel")
}
# 1b.vii Actually fit the models
fit_noLinearConstraints("HourlyTemp", "strokeISC", "fullpop", "fullpop")
fit_noLinearConstraints("HourlyTemp", "strokeHEM", "fullpop", "fullpop")

####**********************************
#### 2: Create Plotting Function  ####
####**********************************

# 2a Name function 
plot_hourly_associations <- function(ExpTerm, CaseType, Sensitivity, SubPopVar, SubPop){
  # ExpTerm <- "HourlyTemp"; Sensitivity <- "noLinTerm",;CaseType <- "strokeISC"; 
  # SubPopVar <- "fullpop"; SubPop <- "fullpop"
  
  # 1b Create ModelNameShort
  ModelNameShort <- paste0(ExpTerm, "_", CaseType, "_", Sensitivity)
  
  ####**********************************
  #### 2A: Readin Model Predictions ####
  ####**********************************
  
  # 2A.a Readin Table 
  est.table <- readin_selectedEstimates(ExpTerm, CaseType, Sensitivity, SubPopVar, SubPop, "EstInd")
  
  # 2A.b Set parameter for lags
  G <- 1
  if(str_detect(Sensitivity, "LagHr")){
    LagHr <- as.numeric(str_remove_all(Sensitivity, "[A-z]"))
  }else if(Sensitivity == "2hrAveTDLNM"){
    LagHr <- 18; G <- 2
  }else {LagHr <- 36} 
  
  # 2f Set up colors 
     LagColor.list <- topo.colors(LagHr)
     LC.l <- LagColor.list 
  
  # 2g Plot associations
     pdf(paste0(plots.folder, ModelNameShort, "_03a_allLagsStationary_", "00to100", ".pdf"))
     for(i in 0:(LagHr-1)){
       name.fit <- paste0("fit.or.lag", i)
       name.lci <- paste0("lci.or.lag", i)
       name.uci <- paste0("uci.or.lag", i)
       
       d.r.plot <- est.table %>% 
         rename(fit.or = !!name.fit, 
                lci.or = !!name.lci, 
                uci.or = !!name.uci) %>% 
         mutate(fit.pc = convert_to_percent(fit.or), 
                lci.pc = convert_to_percent(lci.or),
                uci.pc = convert_to_percent(uci.or)) %>%
         ggplot(aes(CounterfactualTemp)) + 
         geom_hline(yintercept=0, color ="grey" ) + 
         geom_vline(xintercept=HourlyTemp.per05, color ="grey", linetype="dashed") +
         geom_vline(xintercept=HourlyTemp.per95, color ="grey", linetype="dashed") +
         geom_ribbon(aes(ymin= lci.pc,  ymax = uci.pc), fill = LC.l[i +1],  alpha  = 0.3)+
         geom_line(aes(y = fit.pc),  color = LC.l[i + 1])+ 
         labs(y = paste0("Change in \nStroke Hospitalizations (%)"), 
              x = expression(Hourly~Temperature~(degree*C))) + 
         ggtitle(paste0( "Exposure-Response \nfor Lag ", i, " Hours"))+
         coord_cartesian(ylim=c(-10, 10), expand = TRUE,
                         default = FALSE, clip = "off") + 
         scale_y_continuous(breaks = seq(-10, 10, by = 1)) +
         annotate("text", x = 11, y = 9, label = CaseType, size = 9) + 
         tema
       print(d.r.plot)
     }
     dev.off()
     
}

####*********************
#### 3: Create Plots ####
####*********************

# 3a Main Models 
plot_hourly_associations("HourlyTemp", "strokeISC", "Main", "fullpop", "fullpop")
plot_hourly_associations("HourlyTemp", "strokeHEM", "Main", "fullpop", "fullpop")

# 3b NonLinear constraints
plot_hourly_associations("HourlyTemp", "strokeISC", "noLinTerm", "fullpop", "fullpop")
plot_hourly_associations("HourlyTemp", "strokeHEM", "noLinTerm", "fullpop", "fullpop")
