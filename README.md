# Data and Anlysis for Chapter 2, Foster Hengst Thesis 

Repository: github.org/chuckbv/ms22-now-male-diel-response

## Overview

Five remote automated camera traps baited with a pheromone monitoring lure 
were deployed in 2019 and 2020 to characterize diel patterns of response
of navel orangeworm **Amyelois transitella** (Walker) as effected by 
environmental factors over the field season. See the manuscript in "./doc".

## Abstract

Navel orangeworm, Amyelois transitella (Walker), is the primary insect pest 
of walnuts, pistachio, and almonds in California. Sex pheromone based mating 
disruption has recently been commercialized for this pest in the form of timed 
aerosol dispensers. Dispenser efficiency may be increased by timing releases 
with the active mating period of navel orangeworm. Past work found that the 
peak time of sexual activity for navel orangeworm females is 2 hours before 
sunrise when temperatures are above 18° C. Inference of male responsiveness 
from data collected in that study was limited by the necessity of using 
laboratory-reared females as a source of sex pheromone emission to attract 
males and the inherent limitations of human observers for nocturnal events. 
Here we used camera traps baited with artificial pheromone to observe male 
navel orangeworm mating response in the field over two field seasons. Male 
response to synthetic pheromone exhibited diel patterns broadly similar to 
females, i.e., they were active for a brief period of 2 to 3 hours before dawn 
under summer conditions and began responding to pheromone earlier and over 
a longer period of time during spring and fall. Contrary to the previous 
findings with females, some males were captured at all hours of the day and 
night, and there was no evidence of short-term change of pheromone 
responsiveness in response to temperature. Environmental effects on the 
response of navel orangeworm males to an artificial pheromone source differs 
in important ways from the environmental effects on female release of sex 
pheromone.

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
 

 