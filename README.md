# Data and Anlysis for Chapter 2, Foster Hengst Thesis 

Repository: github.org/chuckbv/ms-hengst-thesis-chap2-trapview

Simplified and cleaned version of Trapview data and analysis

## Interpretive Summary

 1. The impact of environment and the biological clock on the timing of sexual
 activity of pest moths like the navel orangeworm is important for 
 optimization of pheromone mating disruption for their control.
 2. Remote automated camera traps offer a more efficient way to monitor such 
 pests for treatment decisions, and can also provide a research tool to 
 improve understanding of biology of these pests.
 3. Two years of camera trap data corroborated that the peak time of arrival 
 of males to a synthetic pheromone source was influenced by seasonal factors,
 but also revealed more variability and less direct effect of temperature 
 compared to earlier studies primarily examining females.
 4. Improve understanding of photoperiodic responses of males will improve
 understanding of monitoring data and optimization of mating disruption. 

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
 - **script2_y19_temperature_data.R** Plot temperature and RH readings daily 
 at 3AM (once per day so the chart is not too noisy)
 - 
 
## Output files
 - Fig2.jpg -- Daily Counts and hourly temperatures in 2019
 - Fig3.jpg -- Daily Counts and hourly temperatures in 2020
 - Fig4.jpg -- x
 - Fig5.jpg
 
 
 