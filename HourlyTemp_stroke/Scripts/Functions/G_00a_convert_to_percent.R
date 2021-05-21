# Function: Convert to percentage
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 

####***********************
#### Table of Contents #### 
####***********************

# D: Descripton
# 1: Create Function

####********************
#### D: Description ####
####********************

# This function is used to convert rate ratio to percent change

####************************
#### 1: Create Function ####
####************************

# 1a Declare convert to percent function 
convert_to_percent <- function(x){
  100 * (x - 1)
}

