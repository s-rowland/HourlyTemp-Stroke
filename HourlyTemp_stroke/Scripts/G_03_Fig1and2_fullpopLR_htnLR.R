# Create figures 1 and 2
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Plotting Function 
  # 1A: Wrangle Model Independent Estimates
  # 1B: Wrangle Exposure Data 
  # 1C: Generate the Two Plots
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
  source(here::here('HourlyTemp_Stroke', 'Scripts', 'G_01_set_plottingObjects.R'))
}

####*********************************
#### 1: Create Plotting Function ####
####*********************************

# 1a Name function 
plot_laggedER <- function(ExpTerm, CaseType, FigNum){
  #ExpTerm <- 'HourlyTemp'; CaseType <- 'strokeHEM'; 

  ####**************************************
  #### 1A: Wrangle Main Model Estimates ####
  ####**************************************
  
  # 1A.a Readin model estimates
  est.table <- readin_selectedEstimates(ExpTerm, CaseType, 'Main', 'fullpop', 'fullpop', 'EstInd')
  
  # 1A.b Keep only relevant exposure contrast
  est.table <- est.table %>% 
    filter(Label == 'MeanPlus10')
  
  # 1A.c Wrangle estimates 
  est.table <- est.table %>% 
    dplyr::select(-Sensitivity, -CaseType, -SubPopVar, -SubPop, -IndCumul, -CounterfactualTemp, -Label) %>%
    gather('LagName', 'Estimate') %>% 
    mutate(VarName = str_sub(LagName, 1, 3), Lag = as.numeric(str_sub(LagName, 11))) %>%  
    dplyr::select(-LagName) %>%
    spread(VarName, Estimate) %>% 
    mutate(fit.pc = convert_to_percent(fit), 
           lci.pc = convert_to_percent(lci), uci.pc = convert_to_percent(uci)) 
  
  ####************************************************
  #### 1B: Wrangle HTN-Stratified Model Estimates ####
  ####************************************************
  
  # 1B.a Readin estimates
  est.table.htn <- readin_selectedEstimates(ExpTerm, CaseType, 'Main', 'HTN', 'HTN', 'EstInd') %>% 
    bind_rows(readin_selectedEstimates(ExpTerm, CaseType, 'Main', 'HTN', 'noHTN', 'EstInd'))
  
  # 1B.b Keep only relevant exposure contrast
  est.table.htn <- est.table.htn %>% 
    filter(Label == 'MeanPlus10')
  
  # 1B.c Wrangle estimates 
  est.table.htn <- est.table.htn %>% 
    dplyr::select(-Sensitivity, -CaseType, -SubPopVar, -IndCumul, -CounterfactualTemp, -Label) %>%
    gather('LagName', 'Estimate', -SubPop) %>% 
    mutate(VarName = str_sub(LagName, 1, 3), Lag = as.numeric(str_sub(LagName, 11))) %>%  
    dplyr::select(-LagName) %>%
    spread(VarName, Estimate) %>% 
    mutate(fit.pc = convert_to_percent(fit), 
           lci.pc = convert_to_percent(lci), uci.pc = convert_to_percent(uci)) 
  
  # 1B.d Keep only the lags and exposure we will plot 
  LagList <- c(0: relevantWindows$relevantWindows[relevantWindows$CaseType == CaseType])
  est.table.htn <- est.table.htn %>% 
    filter(Lag %in% LagList)
   
  # 1B.e Adjust the lags so that we can get staggered error bars 
  est.table.htn <- est.table.htn %>% 
    mutate(Lag = if_else(SubPop == 'noHTN', Lag + 0.25, Lag))
  
  # 1B.f Rename the SubPop variable 
  est.table.htn <- est.table.htn %>%
  mutate(Hypertension = if_else(SubPop == 'HTN', 'Hypertension', 'No Hypertension'))
  
  ####********************************
  #### 1C: Generate the Two Plots ####
  ####********************************
  
  # 1C.a Set up colors and Labels and labs 
  if(CaseType == 'strokeISC'){
     Ymin <- -1; Ymax <- 2;  YStep <- 0.5; CaseName <- 'Ischemic Stroke'
    }
  if(CaseType == 'strokeHEM'){
    Ymin <- -4; Ymax <- 2;  YStep <- 0.5; CaseName <- 'Hemorrhagic Stroke'
    }
  
  # 1C.b Create plot a: lag-response for hourly temperature
  TP.a <- est.table %>%
    ggplot(aes(Lag)) + 
    geom_hline(yintercept=0, color ='grey' ) + 
    geom_ribbon(aes(ymin= lci.pc,  ymax = uci.pc), fill= ColorArray$TempContrast[2],  
                alpha  = 0.35, col=NA)+
    geom_line(aes(y = fit.pc), col = ColorArray$TempContrast[2], size = 2)+ 
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
    theme(legend.title=element_text(size=20),
          legend.text=element_text(size=16),
          legend.position = c(0.6, 0.84), 
          legend.key.size = unit(0.4, 'cm')) + 
    theme(axis.title.y = element_text(size = 20), 
          axis.text.x = element_text(size = 16), 
          axis.text.y = element_text(size = 16)) 
  
  # 1C.c Create plot b: Lag-response by hypertension status
  TP.b <- est.table.htn %>%
    ggplot(aes(Lag)) + 
    geom_hline(yintercept=0, color ='grey' ) + 
    geom_point(aes(y = fit.pc, fill= Hypertension, color = Hypertension, shape = Hypertension), size = 6)+
    geom_errorbar(aes(ymin= lci.pc,  ymax = uci.pc, color = Hypertension), size = 1.5)+
    labs(y = paste0('Change in ', CaseName, '\n Hospitalizations (%)'), x = paste0('Hourly Lag')) + 
    coord_cartesian(ylim = c(Ymin, Ymax), expand = TRUE,
                    default = FALSE, clip = 'off') + 
    scale_y_continuous(breaks = seq(Ymin, Ymax, by = YStep)) +
    scale_x_continuous(breaks = LagList) +
   # scale_color_manual(values = ColorArray[['HTN']], name = 'Hypertension Status') + 
    #scale_fill_manual(values = ColorArray[['HTN']], name = 'Hypertension Status') + 
     scale_color_manual(values = ColorArray[['HTN']]) + 
    scale_fill_manual(values = ColorArray[['HTN']]) + 
    scale_shape_manual(values=c(18, 20)) +
    tema + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank())  +
    theme(panel.background = element_rect(fill= NA, color = 'black')) +
    theme(legend.title=element_text(size=15),
          legend.text=element_text(size=13),
          legend.position = c(0.6, 0.84), 
          legend.key.size = unit(0.4, 'cm')) + 
    theme(axis.title.y = element_text(size = 22), 
          axis.text.x = element_text(size = 18), 
          axis.text.y = element_text(size = 18)) 
  
  # 1C.d Print plots
  LabelSize <- 7; LabelHjust <- -2.6; LabelVjust <- 1.7
  
  png(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript',
                 paste0('Fig', FigNum, '_fullpopLR_htnLR_', CaseType, '.png')), 
      width = WW.fig*2, height = HH.fig*0.75, res =RR.fig*1)
  print(cowplot::plot_grid( tag_facet(TP.a, open ='', close='', tag_pool=c('A'), 
                                           size=LabelSize, hjust = LabelHjust, vjust = LabelVjust),
                                tag_facet(TP.b, open ='', close='', tag_pool=c('B'), 
                                          size=LabelSize, hjust = LabelHjust, vjust = LabelVjust),
                                ncol = 2))
  dev.off()
}
  
####*********************
#### 2: Create Plots ####
####*********************

# 2a Main Models
# You will get a warning saying 'Missing column names filled in: 'X1' [1]' 
# This is not a problem - artifact of excel creating an unnamed column for the row numbers.
plot_laggedER('HourlyTemp', 'strokeISC', 1)
plot_laggedER('HourlyTemp', 'strokeHEM', 2)

