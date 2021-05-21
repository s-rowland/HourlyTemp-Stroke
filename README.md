# HourlyTemp_StrokeReview
Code Review for HourlyTemp_Stroke Analysis

Readme 

1. Before running any code, open the 0_01 scripts and set the CaseName to 'stroke' if you are using real data 
or 'fake' if you are creating synthetic data. Make sure to also set the user for the HourlyTempStroke/Scripts/0_01 script. 
2. These 0_01 scripts are sources at the beginning of all other scripts 
but only if you haven't already run it during the R session
3. You need to install 2 packages: here and pacman. After that, the 0_01 script will take care of installing and loading all other packages.

2. Folder Structure 
lvl 1: Project (eg Temp_CVD_Analysis)
lvl 2: Analysis (eg TempVar_mi)
lvl 3: Components (Data, Scripts, Outputs) 

3. Prefix categories
Folder: DataPrep
0_ Setting up the R environment
a_ Prepare hourly weather data from raw gribs from nldas 
b_ Extract cases from SPARCS data 
c_ Create case-crossover dataset and assign exposure
SubFolder: Functions 
note that these scripts are not actually functions, just because of memory constraints, 
so they are sourced each time you want to run them

Folder: TempVar_MI
0_ Setting up the R environment
A_ Data processing 
B_ All descriptive analyses and summary (no health models) 
C_ All main health models (selecting model constraints, etc) 
D_ Sensitivity analyses
E_ Exploratory analyses that may be new inquiries (not necessarily part of paper)
F_ Plotting models or similar post-modeling processing (not necessarily part of paper)
G_ Final versions of plots, tables, numeric results for manuscript
H_ Other 
Note that B, E, F, and H are not included in the GitHub because they did not contribute to the manuscript
Most of the descriptive analyses wound up in the G_ scripts.
SubFolder: Functions 
note that these scripts are actually functions, and c functions depend on each other (c3 needs c2; c2 needs c1)

4. Naming Conventions
letter_00 refers to scripts that only generate a function. 
functions are sourced in the 0_01 scripts
Sometimes a function is used for two different clusters of code, esp. C & D. 
scripts should be written in camelCase
