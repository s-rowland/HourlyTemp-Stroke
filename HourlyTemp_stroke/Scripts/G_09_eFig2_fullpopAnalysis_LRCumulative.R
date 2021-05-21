# Create efig2: cumulative lag-response plots
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Plotting Function
  # 1A: Wrangle Model Independent Estimates
  # 1B: Generate the Two Plots
# 2: Create Plots

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
  source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_01_set_PlottingObjects.R'))
}

####*********************************
#### 1: Create Plotting Function ####
####*********************************

# 1a Name function 
plot_cumulLR <- function(ExpTerm, CaseType, Sensitivity, SubPopVar, SubPop){
  #CaseType <- 'strokeISC'; ExpTerm <- 'HourlyTemp'
  #SubPopVar <- 'fullpop'; SubPop <- 'fullpop'; Sensitivity <- 'Main'
  
  ####*********************************************
  #### 1A: Wrangle Model Independent Estimates ####
  ####*********************************************
  
  # 1A.a Readin model estimates
  est.table <- readin_selectedEstimates(ExpTerm, CaseType, Sensitivity, SubPopVar, SubPop, 'EstCumul')
  
  # 1A.b Isolate the estimates for the fifth percentile of temperature 
  est.table <- est.table %>% 
    filter(Label %in% c('MeanPlus10')) %>%
    dplyr::select(-Label, -Sensitivity, -CounterfactualTemp, -CaseType, -SubPopVar, -SubPop, -IndCumul) %>%
    gather('LagName', 'Estimate', ) %>% 
    mutate(VarName = str_sub(LagName, 1, 3), Lag = as.numeric(str_sub(LagName, 11))) %>%  
    dplyr::select(-LagName) %>%
    spread(VarName, Estimate) %>% 
    mutate(fit.pc = convert_to_percent(fit), 
           lci.pc = convert_to_percent(lci), uci.pc = convert_to_percent(uci)) 
  
  ####********************************
  #### 1B: Generate the Two Plots ####
  ####********************************  
  
    # 1B.a Set up colors and Labels and labs 
    if(CaseType == 'strokeISC'){
      Ymin <- -1; Ymax <- 7;  YStep <- 1; 
      CaseName <- 'Ischemic Stroke'; LabelSet <- 'A'
    }
    if(CaseType == 'strokeHEM'){
      Ymin <- -11; Ymax <- 0;  YStep <- 1; 
      CaseName <- 'Hemorrhagic Stroke'; LabelSet <- 'B'
    }
  
  # 1B.b Create Plot 
    TP <- est.table %>%
      ggplot(aes(Lag)) + 
      geom_hline(yintercept=0, color ='grey' ) + 
      geom_ribbon(aes(ymin= lci.pc,  ymax = uci.pc), fill= ColorArray$TempContrast[2],  
                  alpha  = 0.35, col=NA)+
      geom_line(aes(y = fit.pc), col = ColorArray$TempContrast[2])+ 
      labs(y = paste0('Change in ', CaseName, '\n Hospitalizations (%)'), 
           x = paste0('Hourly Lag')) + 
      coord_cartesian(ylim = c(Ymin, Ymax), expand = TRUE,
                      default = FALSE, clip = 'off') + 
      scale_y_continuous(breaks = seq(Ymin, Ymax, by = YStep)) +
      scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
      tema + 
      theme(strip.background = element_blank(), 
            strip.text = element_blank())  +
      theme(panel.background = element_rect(fill= NA, color = 'black')) +
      theme(axis.title.y = element_text(size = 20), 
            axis.text.x = element_text(size = 16), 
            axis.text.y = element_text(size = 16))

  # 1B.c Return plot 
  tag_facet(TP, '', '', LabelSet, x = -1, y = Ymax, size = 8)
  
}

####*********************
#### 2: Create Plots ####
####*********************

# 2a Make plots for main Models
TP.a <- plot_cumulLR('HourlyTemp', 'strokeISC', 'Main', 'fullpop', 'fullpop')
TP.b <- plot_cumulLR('HourlyTemp', 'strokeHEM', 'Main', 'fullpop', 'fullpop')

# 2b Print the plot in a single panel
png(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript',
               'eFig2_fullpopAnalysis_LRCumulative.png'), 
    width = WW.fig*2, height = HH.fig*0.75, res =RR.fig)
cowplot::plot_grid(TP.a, TP.b, 
                   align = 'v', nrow = 1)
dev.off()
