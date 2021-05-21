# Plot eFig 1 Distrbution of timing of strokes 
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation
# 1: Prepare Data
# 2: Define Plotting Function
# 3: Make Plots

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

####*********************
#### 1: Prepare Data ####
####*********************

# 1a Create case-onlydataset
cases <- dta %>% filter(Case==1)

# 1b Create DateTime variables
cases <- cases %>% 
  mutate(CaseDate = parse_date_time(CaseDateRaw, 'ymd h')) %>% 
  mutate(MM = month(CaseDate), HH = hour(CaseDate), WDay = wday(CaseDate)) %>% 
  mutate(WDay = case_when(
    WDay == 1 ~ 'Sun', 
    WDay == 2 ~ 'Mon', 
    WDay == 3 ~ 'Tues', 
    WDay == 4 ~ 'Weds', 
    WDay == 5 ~ 'Thurs', 
    WDay == 6 ~ 'Fri', 
    WDay == 7 ~ 'Sat' 
  )) %>% 
  mutate(MM_name = case_when(
    MM == 1 ~ 'Jan', 
    MM == 2 ~ 'Feb', 
    MM == 3 ~ 'Mar', 
    MM == 4 ~ 'Apr', 
    MM == 5 ~ 'May', 
    MM == 6 ~ 'June', 
    MM == 7 ~ 'July', 
    MM == 8 ~ 'Aug', 
    MM == 9 ~ 'Sept', 
    MM == 10 ~ 'Oct', 
    MM == 11 ~ 'Nov', 
    MM == 12 ~ 'Dec'))

cases$WDay <- factor(cases$WDay, c('Sun', 'Mon', 'Tues', 'Weds', 'Thurs', 'Fri', 'Sat'))
cases$MM_name <- factor(cases$MM_name, c('Jan','Feb','Mar', 'Apr','May', 'June', 
                                         'July','Aug','Sept','Oct','Nov',  'Dec'))

####*********************************
#### 2: Define Plotting Function ####
####*********************************

# 2a Begin histogram plotting function
plot_daytimeFreq <- function(CaseType){
  
  # 2b Isolate by subtype
  cases.subtype <- cases %>% filter(StrokeSubType == CaseType)

  # 2c Set thetitle for the y-axis
  YAxisTitle <- 'Frequency (%)'
  
  # 2d Plot the distribution by hour of the day
  HourPlot <- cases.subtype %>% 
    group_by(HH) %>% 
    summarize(Count = n()) %>% 
    mutate(Frequency = 100* Count/nrow(cases.subtype)) %>%
    ggplot() + 
    geom_bar(aes(HH, y=Frequency), stat = 'identity') + #,  color = 'lightblue') +
    labs(y = YAxisTitle , x = 'Hour of Day') + 
    tema 
  
  # 2e Plot the distribution by day of the week 
  DayPlot <- cases.subtype %>% 
    group_by(WDay) %>% 
    summarize(Count = n()) %>% 
    mutate(Frequency = 100* Count/nrow(cases.subtype)) %>%
    ggplot() + 
    geom_bar(aes(WDay, y=Frequency), stat = 'identity') + #,  color = 'lightblue') +
    labs(y = YAxisTitle , x = 'Day of Week') + 
    tema 
  
  # 2f Plot the distribution by month
  MonthPlot <-  cases.subtype %>% 
    group_by(MM_name) %>% 
    summarize(Count = n()) %>% 
    mutate(Frequency = 100 * Count/nrow(cases.subtype)) %>%
    ggplot() + 
    geom_bar(aes(MM_name, y=Frequency), stat = 'identity') + #,  color = 'lightblue') +
    labs(y = YAxisTitle , x = 'Month') + 
    coord_cartesian(ylim = c(0, 11), expand = TRUE,
                    default = FALSE, clip = 'off') + 
    tema 
  
  # 2g Output the plots
  list(HourPlot, DayPlot, MonthPlot)
}

####*******************
#### 3: Make Plots ####
####*******************

# 3a Set up plotting theme
tema <-   theme_classic() +
  theme(axis.title.x = element_text(size = 17, vjust =0, lineheight=0.25)) + 
  theme(axis.title.y = element_text(size = 17, angle = 90, vjust= 1)) + 
  theme(axis.text.x = element_text(size = 10)) + 
  theme(axis.text.y = element_text(size = 10, angle = 0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = 'black')) + 
  theme(panel.background = element_rect(fill= NA, color = 'black'))

# 3b Generate plots
plots.isc <- plot_daytimeFreq('strokeISC')
plots.hem <- plot_daytimeFreq('strokeHEM')

# 3c Save Plots
# Set Label Locations 
LabelSize <- 7; LabelHjust <- -0.5; LabelVjust <- 1.6
png(here::here('HourlyTemp_Stroke', OutputsPath, 'Manuscript',
               'eFig1_timingDistribution_by_SubType.png'), 
    width = WW.fig*1.5, height = HH.fig, res =RR.fig) 
grid.arrange(tag_facet(plots.isc[[1]], open ='', close='', tag_pool=c('A', ''), 
                       size=LabelSize, hjust = LabelHjust, vjust = LabelVjust), 
             tag_facet(plots.hem[[1]], open ='', close='', tag_pool=c('B', ''), 
                       size=LabelSize, hjust = LabelHjust, vjust = LabelVjust), 
             tag_facet(plots.isc[[2]], open ='', close='', tag_pool=c('C', ''), 
                       size=LabelSize, hjust = LabelHjust, vjust = LabelVjust), 
             tag_facet(plots.hem[[2]], open ='', close='', tag_pool=c('D', ''), 
                       size=LabelSize, hjust = LabelHjust, vjust = LabelVjust), 
             tag_facet(plots.isc[[3]], open ='', close='', tag_pool=c('E', ''), 
                       size=LabelSize, hjust = LabelHjust, vjust = LabelVjust), 
             tag_facet(plots.hem[[3]], open ='', close='', tag_pool=c('F', ''), 
                       size=LabelSize, hjust = LabelHjust, vjust = LabelVjust), 
             ncol = 2)
dev.off()
