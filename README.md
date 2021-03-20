# Data and Anlysis for Chapter 2, Foster Hengst Thesis 

Repository: github.org/chuckbv/ms-hengst-thesis-chap2-trapview

Simplified and cleaned version of Trapview data and analysis

## Overview

Five Trapview camera traps baited with pheromone lures were run over a 
two-year period to examine night-to-night and hourly variation in trap 
capture. The traps took photos once every 0.5 hours fromsunset to sunrise.
In 2019 the five traps were in five different locations. In 2020 the traps 
were in one location where heavier abundance was found the previous year.

## Data

"allsites_y19.csv" and "allsites_y20.csv" are equivalevent files of trap 
events for the two years. The _y19 and _y20 temperature files are temperature 
data reported by the traps over the same period.

## Scripts
 - **script0_head_data_files.R** Infile to load 4 relevant csv files into 
 dataframes in Global Environment, head these files (43 lines)
 - **script1_y19_plot_weeks_hour.R** Creates a cool figure showing flights 
 for 2019
 - **script2_y19_temperature_data.R** Plot temperature and RH readings daily 
 at 3AM (once per day so the chart is not too noisy)
 - 
 