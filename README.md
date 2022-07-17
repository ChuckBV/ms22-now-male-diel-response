# Data and Anlysis for Chapter 2, Foster Hengst Thesis 

Repository: github.org/chuckbv/ms22-now-male-diel-response

## Overview

Five remote automated camera traps baited with a pheromone monitoring lure 
were deployed in 2019 and 2020 to characterize diel patterns of response
of navel orangeworm **Amyelois transitella** (Walker) as effected by 
environmental factors over the field season. See the manuscript in "./doc".

x

## ./data

***allsites_y19.csv***" and "***allsites_y20.csv***" are equivalent files of trap 
events for the two years. The ***trapview_temps_degf_y19.csv*** and 
***trapview_temps_degf_y19.csv*** temperature files are temperature data 
reported by the traps over the same period.

The two ***_scrubbed.csv*** provide count data after using REGEX to removed 
erroneous counts. 

## Scripts
 - **script10_merge_count_and_temperature.R** Infile to load 4 relevant csv 
 files into dataframes in Global Environment, head these files. Creates
 individual temperature and count profiles for 2019 and 2020, and uses
 ggpubr to combine these into composite graphics ***Fig2.jpg*** and 
 ***Fig3.jpg***
 - **script11_explore_count_and_temperature.R** Creates ***Fig4.jpg***
 - **script12_explore_count_and_temperature2.R** Creates ***Fig5.jpg*** 
 
## Output files
 - Fig2.jpg - Daily count and temperature profile, 2019
 - Fig3.jpg - Daily count and temperature profile, 2020
 - Fig4.jpg - Monthly boxplots on capture by time of day
 - Fig5.jpg - Monthly scatterplots of daily median capture by temperature
 

 