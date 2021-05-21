# Compute Population-Weighted Average Hourly Weather Parameters
# Data Prep
# Temperature-CVD-NYS Project
# Sebastian T. Rowland
# Updated 09/01/2020

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation 
# 1: Prepare zcta Data
# 2: Create Function to Compute Weighted Weather
#  2A: Prepare weather grid data
#  2B: Convert Grid Centroids to SpatialPoints
#  2C: Create Voroni or Thiessien Polygons from Centroids 
#  2D: Compute Population Within Each Intersection Segment
#  2E: Create the Weights Matrix
#  2F: Prepare weather Data 
#  2G: Compute Population-Weighted zcta-year Weather
#  2H: Save Results
# 3: Create Pop-weighted Averages

####**************
#### N: Notes ####
####**************

# Na Description
# Here we calculate population-weighted averages. 
# The core idea is that we first create a matrix of weights 
# so that each NLDAS grid has a weight for each ZCTA 
# if the grid does not overlap with a ZCTA, then the weight is zero.
# A more precise method like ABODE would better distinguish
# population density within a census tract 
# but this approach should be sufficient 
# for a feature like ambient temperature that is fairly homogeneous across space
# Once we have the weights, we just matrix multiply the weights by the nldas data.
# and all the contributions add up as they should. 
# by doing matrix multiplication, we only need to compute the weights once
# the code maps of years so that it can easily accomodate changes in grid location 
# or change sin the population density dataset (e.g., if we allow population to vary by year)s
# This script takes about 3.5 hours to run for the full dataset

# Nb Sequence
# in Step 1, we calculate pop density for each census tract 
# and then intersect with the zcta layer 
# in steps 2A, 2B, and 2C, we create a spatial layer from NLDAS 
# in step 2D, we intersect the zcta-tract intersections with NLDAS grid 
# in Step 2D, we then estimate the population of each polygon 
# using area and density
# in Step 2E, we aggregate these populations to determine what % of each zcta's population 
# falls into each nldas grid 
# This percentage of the populaiton is then used as the weights for all years 
# in Step 2F, we organize the NLDAS data, 
# and we multiple NLDAS by the weights to get to the population-weights ZCTA 

# i.e., 1) we calculate pop density for each census tract, then 2) we intersect with zcta data,
# then 3) we instersect with nldas grid, and calculate the area of intersection b/w the zcta area and the nldas grid cells
# then 4) we estimate pop of a grid cell using the pop density for the zcta, and the area of intersection, etc. etc. etc.
# that would then make your inline comments throughout this file much more readable.

# Nc Assumption 
# this method is very efficient, but does assume that the weather grid has a measurement 
# for every grid for every time

# Nd Assumption: Planar
# currently step 2d.i Create Intersections 
# treats the coordinates as planar

# Ne Assumption: Constant Population 
# Unlikely to impact results for temperature, sph, or pressure. 
# we assign constant weights across all years. 
# This can easily be updated with more Census data.

# Nf Multiple spatial packages 
# currently the script uses both the sf package and the sp package to handle spatial data 
# it would be ideal to convert everything to sf package in a later update 
# but the code is correct at it is. 

####********************
#### 0: Preparation ####
####********************

# 0a Tell the analyst that the script is beginning 
StartTime_a_03 <- Sys.time()
print(paste("begin a_03 at", StartTime_a_03))

# 0b Create the folder structure, if you haven't already
if (!exists("Ran_dataprep_0_01")){
  here::i_am("README.md")
  source(here::here('DataPrep', 'Scripts', 
                    '0_01_set_outcomeName_packages_folders.R'))
}

####**************************
#### 1: Prepare zcta Data ####
####**************************

# 1a Declare active years and variables 
if(OutcomeName == "fake"){
  VarList <- c(rep("TMP", 3), rep("SPFH", 3), rep("PRES", 3))
  YearList <- c(rep(c(1999, 2000, 2001),3))
  ComboList <- paste0(YearList, "_", VarList)
} else {
  VarList <- c(rep("TMP", 23), rep("SPFH", 23), rep("PRES", 23))
  YearList <- c(rep(c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 
                      2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,2014, 2015, 2016),3))
  ComboList <- paste0(YearList, "_", VarList)
}
# ComboList <- ComboList[1]

# 1b Read in area-tract data 
area.tract <- st_read(here::here('DataPrep', 'Data', 'Raw_Data', 'tl_2010_36_tract10', 
                                 'tl_2010_36_tract10.shp'))
area.tract <- area.tract %>% 
  mutate(GEO.id2 = as.character(GEOID10), 
         AreaLand = as.numeric(as.character(ALAND10)), 
         AreaWater = as.numeric(as.character(AWATER10)))

# 1c Convert projection  
area.tract <- st_transform(area.tract, "WGS84")

# 1d Read in tract-level population data 
pop.tract <- read_csv(here::here('DataPrep', 'Data', 'Raw_Data', 'NYS_Census_raw', 
                                 'ACS_10_5YR_S0101_with_ann.csv'))

# 1e Wrangle data
pop.tract <- pop.tract[2:nrow(pop.tract),]

pop.tract <- pop.tract %>% 
  dplyr::select(GEO.id2, HC01_EST_VC01) %>% 
  rename(pop = HC01_EST_VC01)  %>% 
  mutate(GEO.id2 = as.character(GEO.id2), 
         pop = as.numeric(pop))

# 1f Combining area and pop data 
tract <- left_join(area.tract, pop.tract, by = "GEO.id2") 

# 1g Calculate density 
tract <- tract %>% 
  mutate(area = AreaLand + AreaWater) %>%
  mutate(density = pop / area)

# 1h Keep variables of interest 
tract <- tract %>% dplyr::select(density, GEO.id2, geometry)

# 1i Read in Area-zcta data 
area.zcta <- st_read(here::here('DataPrep', 'Data', 'Raw_Data', 'tl_2010_36_zcta510',
                                'tl_2010_36_zcta510.shp'))
area.zcta<- area.zcta %>% mutate(zcta = as.character(ZCTA5CE10), 
                                AreaLand = as.numeric(as.character(ALAND10)), 
                                AreaWater = as.numeric(as.character(AWATER10)))
# 1j Convert projection 
area.zcta <- st_transform(area.zcta, "WGS84")

# 1k Adjust the sf 
# without this step we get an error in the intersection step
area.zcta <- area.zcta %>% 
  st_set_precision(1e5) %>% 
  st_make_valid()
tract <- tract %>% 
  st_set_precision(1e5) %>% 
  st_make_valid()

# 1l Make intersection 
# again, the warning message "attribute variables are assumed to be spatially constant throughout all geometries"
# refers to the fact that we are ignoring the spherical nature of the globe.
# for a small area like zcta's and census tracts, this assumption leads to negligible error
zcta_inter_tract <- st_intersection(tract, area.zcta)

# 1m Keep variables of interest
zcta_inter_tract <- zcta_inter_tract %>% 
  dplyr::select(geometry, density, zcta, GEO.id2) %>% 
  rename(tract = GEO.id2)

# 1n Clean up environment
rm(area.zcta, area.tract, pop.tract, tract)

####****************************************************
#### 2: Create Function to Compute Weighted Weather ####
####****************************************************

# 2a Name Function
calc_popW_average <- function(YYYY_VVV){
  # YYYY_VVV <- "1994_tmp"
  # 2b Add progress bar
  p()
  
  ####**********************************
  #### 2A Prepare Weather Grid Data ####
  ####**********************************
  # 2A.a Readin the raw weather data 
  nldas <- read.fst(here::here('DataPrep', 'Data', 'Raw_Data','NLDAS_raw', 
                               paste0(YYYY_VVV, '_NLDAS_NY.fst')))
  # 2A.b Put weather data in long format
  nldas.l <- nldas %>% 
    gather("Time", "temp", 4:NCOL(nldas)) %>% 
    rename(lat = y, lon = x)
  # 2A.c Choose just the first hour of weather data
  # since we first construct a weight matrix relating 
  # each weather grid to each zcta 
  # we do not actually need weather values 
  # we really just want the grid centroid locations 
  zcta_inter_tract2 <- zcta_inter_tract
  zcta_inter_tract2$DateTime <- nldas.l$Time[1]
  nldasgrid <- nldas.l %>% filter(Time == nldas.l$Time[1]) 
  
  ####************************************************
  #### 2B Convert Grid Centroids to SpatialPoints ####
  ####************************************************
  # 2B.a Isolate just the lat and lon values
  nldas.coord <- cbind(nldasgrid$lon, nldasgrid$lat)
  nldasgrid2 <- nldasgrid 
  nldasgrid2$lat <- NULL 
  nldasgrid2$lon <- NULL
  pcs <- CRS("+proj=longlat +datum=WGS84")
  # 2B.b Convert to spatial points
  nldasgrid.sp <- SpatialPointsDataFrame(nldas.coord, nldasgrid2, proj4string = pcs, match.ID = FALSE)
 
  ####***********************************************************
  #### 2C Create Voroni or Thiessien Polygons from Centroids ####
  ####***********************************************************
  # 2C.a Create Voroni or Thiessien Polygons from Centroids 
  # each pixel of area is assigned to the nearest centroid. 
  # you will get a warning "In proj4string(xy) : CRS object has comment, which is lost in output"
  # this warning can be safely ignored. 
  
  vor <- voronoi(nldasgrid.sp)
  # 2C.b Convert to simple features
  nldas.vor <- st_as_sf(vor)
  # 2C.d Convert projection  
  nldas.vor <- st_transform(nldas.vor, "WGS84")
  
  ####************************************************************
  #### 2D Compute Population Within Each Intersection Segment ####
  ####************************************************************
  # we are treating the area as a flat plane 
  # which is probably not valid for whole US
  # but sufficient for New York State & this size grid
  
  # 2D.a Create Intersections
  # again, the warning message "attribute variables are assumed to be spatially constant throughout all geometries"
  # refers to the fact that we are ignoring the spherical nature of the globe.
  # for a small area like zcta's and census tracts, this assumption leads to negligible error
  
  zcta_inter_tract_inter_nldas <- st_intersection(zcta_inter_tract2, nldas.vor)
  # 2D.b Compute area of each intersection
  # some intersections are rounded off as having zero area 
  # Here I give them 1 m^2 of area so they at least contribute some 
  # most intersections have much larger areas, so this will not distort estimates
  zcta_inter_tract_inter_nldas <- zcta_inter_tract_inter_nldas %>% 
    mutate(area = st_area(.) %>% as.numeric()) %>% 
    mutate(area = if_else(area == 0, 1, area))
  # 2D.c Compute populations 
  zcta_inter_tract_inter_nldas <- zcta_inter_tract_inter_nldas %>% mutate(pop = area * density)
  
  ####**********************************
  #### 2E Create the Weights Matrix ####
  ####**********************************
  # weights are how much each grid cells contributes to each zcta
  # now convert to a matrix 
  # 2E.a Clean up dataframe of interscetions
  dta.ztn <- as.data.frame(zcta_inter_tract_inter_nldas)
  dta.ztn  <- dta.ztn  %>% dplyr::select(zcta, nldas_uid, pop)

  # 2E.b Compute the total area for each zcta
  dta.z <- dta.ztn  %>% 
    group_by(zcta) %>% 
    summarize(ZCTA.pop = sum(pop))

  # 2E.c For each zcta, the percent of its area contributed by each grid
  # note that it only computes the contribution of grids that contribute non-zero
  dta.ztn2 <- dta.ztn %>% 
    left_join(dta.z, by = "zcta") %>% 
    mutate(percent.pop = pop/ZCTA.pop)

  # this line just test that indeed, the contribute of the relevant grids sums to 100%
  test <- dta.ztn2 %>% 
    group_by(zcta) %>% 
    summarize(Tot = sum(percent.pop))

  # 2E.d Keep just what we want 
  dta.ztn3 <- dta.ztn2 %>% dplyr::select(zcta, nldas_uid, percent.pop)

  # 2E.e Combine spatial units from the same NLDAS grid   
  dta.zn3 <- dta.ztn3 %>% 
    group_by(zcta, nldas_uid) %>% 
    summarize(percent.pop = sum(percent.pop)) %>% 
    ungroup()

  # 2E.f Put weights in wide matrix format
  # Spread puts our data in wide format 
  # fill = 0 means that for grid-zcta combinations where the 
  # grid does not contribute to that zcta 
  # the value of the cell will be zero
  dta.zn4 <- dta.zn3 %>% 
    spread(nldas_uid, percent.pop, fill = 0) %>% 
    arrange(zcta)
  weights.matrix <- dta.zn4 %>% 
    dplyr::select(-zcta) %>% 
    as.matrix()

  # 2E.g Identify active grid cells that contribute to NYS
  # often NLDAS data is a rectangle, 
  # so we can just remove the grid cells that do not overlap with NYS
  ActiveGrids <- zcta_inter_tract_inter_nldas %>% 
    as.data.frame() %>% 
    dplyr::select(nldas_uid) %>% 
    distinct()

  # 2E.h Track order of gridid's
  # since the weights matrix does not have the names of the gridids
  # we make this list to make sure the weather data is 
  # ordered the same way as the weights matrix
  GridOrderList <- names(dta.zn4%>%dplyr::select(-zcta))
  
  ####******************************
  #### 2F: Prepare weather Data ####
  ####******************************
  # 2F.a Once again we readin the weather data 
  nldas2 <- read.fst(here::here('DataPrep', 'Data', 'Raw_Data', 'NLDAS_raw',
                                paste0(YYYY_VVV, "_NLDAS_NY.fst")))
  
  # 2F.b Then, we eliminate any gridids that did not contribute to any zctas 
  nldas3 <- nldas2 %>% 
    as.data.frame() %>% 
    mutate(x = as.character(x), y = as.character(y)) %>%
    inner_join(ActiveGrids, by = "nldas_uid") 
  
  # 2F.c We then order our weather data according to the order of the weights matrix
  nldas3$nldas_uid <- factor(nldas3$nldas_uid, levels = GridOrderList)
  nldas4 <- nldas3 %>% arrange(nldas_uid)
  
  # 2F.d Convert weather to matrix form
  nldas.clean <- nldas4 %>% 
    dplyr::select(-nldas_uid, -x, -y) %>% 
    as.matrix()
  
  ####*******************************************************
  #### 2G: Compute Population-Weighted zcta-Year Weather ####
  ####*******************************************************
  # 2G.a Perform matrix multiplication
  year_zcta <- weights.matrix %*% nldas.clean
  
  ####**********************
  #### 2H: Save Results ####
  ####**********************
  # 2H.a Convert zcta-level weather matrix to dataframe
  yz <- year_zcta %>% as.data.frame() 
  
  # 2H.b Attach the names of the zctas
  yz.withzcta <- yz %>% bind_cols(dta.zn4%>%dplyr::select(zcta),.) 
  
  # 2H.c Save results
  yz.withzcta %>% 
    write_fst(here::here('DataPrep', 'Data', 'Intermediate_Data', 
                         'NLDAS_ZCTA_Data_PopWeighted', 
                         paste0("zcta_",YYYY_VVV,".fst")))
}

####*************************************
#### 3: Create Pop-weighted Averages ####
####*************************************

# 3a Create the averages for all the years and weather parameters
# 3a.i Set up future
plan(multisession)

# 3a.ii Run the averaging function in parallel
with_progress({
  p <- progressor(steps = length(ComboList)*2)
  
  result <- future_map(ComboList, calc_popW_average)
})

# 3c Tell the analyst that the script is done
cat("completed a_03 at ", paste(Sys.time()), 
    "\n total time: ",round((Sys.time() - StartTime_a_03), 1), " min")

rm(StartTime_a_03)