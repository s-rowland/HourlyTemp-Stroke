# Create efigrues 5 and 6: L-R for alterntative Constraints
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create Plotting Function 
  # 1A: Wrangle Model Independent Estimates
  # 1B: Generate the Plot
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
  source(here::here('HourlyTemp_Stroke', 'Scripts','G_01_set_PlottingObjects.R'))
}

####*********************************
#### 1: Create Plotting Function ####
####*********************************

# 1a Name function 
plot_laggedER <- function(ExpTerm, CaseType, Sensitivity, SubPopVar, SubPop){
  #ExpTerm <- 'HourlyTemp'; CaseType <- 'strokeHEM'
  #SubPopVar <- 'fullpop'; SubPop <- 'fullpop'; Sensitivity <- 'altConstraints'
  
  ####*********************************************
  #### 1A: Wrangle Model Independent Estimates ####
  ####*********************************************
  
  # 1A.a Readin model estimates
  est.table <- readin_selectedEstimates(ExpTerm, CaseType, Sensitivity, SubPopVar, SubPop, 'EstInd') %>% 
    bind_rows(readin_selectedEstimates('HourlyTemp', CaseType, 'Main', SubPopVar, SubPop, 'EstInd'))
  
  # 1A.b Isolate the estimates for a 10-degree increase in temperature
  est.table<- est.table %>% 
    filter(Label %in% c('MeanPlus10')) 
  
  # 1A.c Put estimates in long format 
  est.table <- est.table %>% 
    dplyr::select(-CaseType, -SubPopVar, -SubPop, -CounterfactualTemp, -Label, -IndCumul) %>%
    gather('LagName', 'Estimate', -Sensitivity) %>% 
    mutate(VarName = str_sub(LagName, 1, 3), Lag = as.numeric(str_sub(LagName, 11))) %>%  
    dplyr::select(-LagName) %>%
    spread(VarName, Estimate) %>% 
    mutate(fit.pc = convert_to_percent(fit), 
           lci.pc = convert_to_percent(lci), uci.pc = convert_to_percent(uci)) 
  
  # 1A.d Rename variables and set order
  est.table <- est.table %>% 
    mutate(         
      Sensitivity = case_when(
        Sensitivity == 'Main' ~ 'Main', 
        Sensitivity == 'RHnoAdj' ~ 'No RH Adjustment',
        Sensitivity == 'RHdlnm' ~ 'DLNM for RH',
        Sensitivity == 'altConstraints' ~ 'Alternative Constraints',
        Sensitivity == '24LagHr' ~ '24 Lag Hours',
        Sensitivity == '48LagHr' ~ '48 Lag Hours'
      )) %>% 
    mutate(Sensitivity = factor(Sensitivity, levels = rev(NameArray$SensitivityManu)))
  
  ####***************************
  #### 1B: Generate the Plot ####
  ####***************************  
  
  # 1B.a Set up colors and Labels and labs 
  if(CaseType == 'strokeISC'){
    Ymin <- -1; Ymax <- 2;  YStep <- 0.5;legend.xy <- c(0.75, 0.75)
    CaseName <- 'Ischemic Stroke'
  }
  if(CaseType == 'strokeHEM'){
    Ymin <- -3; Ymax <- 2;  YStep <- 0.5; legend.xy <- c(0.75, 0.25)
    CaseName <- 'Hemorrhagic Stroke'
  }

  # 1B.b Create plots c&d: exposure-response for hourly temperature
  TP <- est.table %>%
    ggplot(aes(Lag)) + 
    geom_hline(yintercept=0, color ='grey' ) + 
    geom_ribbon(aes(ymin= lci.pc,  ymax = uci.pc, fill= Sensitivity, alpha = Sensitivity), col=NA)+
    geom_line(aes(y = fit.pc, col = Sensitivity))+ 
    labs(y = paste0('Change in ', 'Stroke', '\n Hospitalizations (%)'), 
         x = paste0('Hourly Lag')) + 
    coord_cartesian(ylim = c(Ymin, Ymax), expand = TRUE,
                    default = FALSE, clip = 'off') + 
    scale_y_continuous(breaks = seq(Ymin, Ymax, by = YStep)) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45)) +
    scale_color_manual(values = c('aquamarine3', ColorArray$TempContrast[2]), name = 'Model') +  
    scale_fill_manual(values = c('aquamarine3', ColorArray$TempContrast[2]), name = 'Model') + 
    scale_alpha_manual(values = c(0.35, 0.35, 0.1), name = 'Model') + 
    tema + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank())  +
    theme(panel.background = element_rect(fill= NA, color = 'black')) +
    theme(legend.title=element_text(size=13),
          legend.text=element_text(size=11),
          legend.position = legend.xy, 
          legend.key.size = unit(0.4, 'cm')) + 
    theme(axis.title.y = element_text(size = 13), 
          axis.text.x = element_text(size = 11), 
          axis.text.y = element_text(size = 11)) 
  
  # 1B.c Return plots
  TP
 
}
  
####*********************
#### 2: Create Plots ####
####*********************

# 2a Set size of plots
LabelSize <- 7; LabelHjust <- -2.6; LabelVjust <- 1.7

# 2b Plot Sensitivity Analyses for StrokeISC
TP.a <- plot_laggedER('HourlyTemp', 'strokeISC', 'altConstraints', 'fullpop', 'fullpop')
TP.b <- plot_laggedER('HourlyTemp_48lag', 'strokeISC', '48LagHr', 'fullpop', 'fullpop')

# 2c Print plot
png(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript',
               'eFig5_selectedSensitivityAnalyses_LR_strokeISC.png'), 
    width = WW.fig*1.25, height = HH.fig*1, res =RR.fig*1.25)
cowplot::plot_grid(
  tag_facet(TP.a, open ='', close='', tag_pool=c('A'), 
            size=LabelSize, hjust = LabelHjust, vjust = LabelVjust),
  tag_facet(TP.b, open ='', close='', tag_pool=c('B'), 
            size=LabelSize, hjust = LabelHjust, vjust = LabelVjust),
  ncol = 1)
dev.off()

# 2d Plot Sensitivity Analyses for StrokeISC
TP.a <- plot_laggedER('HourlyTemp', 'strokeHEM', 'altConstraints', 'fullpop', 'fullpop')
TP.b <- plot_laggedER('HourlyTemp_48lag', 'strokeHEM', '48LagHr', 'fullpop', 'fullpop')

# 2e Print plot
png(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript',
               'eFig7_selectedSensitivityAnalyses_LR_strokeHEM.png'), 
    width = WW.fig*1.25, height = HH.fig*1, res =RR.fig*1.25)
cowplot::plot_grid(
  tag_facet(TP.a, open ='', close='', tag_pool=c('A'), 
            size=LabelSize, hjust = LabelHjust, vjust = LabelVjust),
  tag_facet(TP.b, open ='', close='', tag_pool=c('B'), 
            size=LabelSize, hjust = LabelHjust, vjust = LabelVjust),
  ncol = 1)
dev.off()

