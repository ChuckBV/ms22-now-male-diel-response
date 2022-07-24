# Data and Anlysis for a Camera Trap Study of Time of Diel Patterns in Male NOW 

Repository: github.org/chuckbv/ms22-now-male-diel-response

## Overview

Five remote automated camera traps baited with a pheromone monitoring lure 
were deployed in 2019 and 2020 to characterize diel patterns of response
of navel orangeworm **Amyelois transitella** (Walker) as effected by 
environmental factors over the field season. See the manuscript in "./doc".

The two algorithm scripts demonstrate how to: 

### 1. Shift data and hour of  day

Shift date and hour of day so that the Julian date is from 6Pm PST and 6PM PST 
is 0 hr, midnight is 06:00, 6Am is 12:00, and so on.

### 2. Expand from a frequency table to case form 

Case form is one observation for each moth. It is helpful for accurate 
determinatation of the median and boxplots


## Scripts
 - **script1** Infile and clean count data for 2019 and 2020
 - **script2** Infile and clean temperature data for 2019 and 2020
 - **script3** Produce the combine phenology figures, Fig2 and Fig3
 - **script4** Do chisqr test of daytime fliers vs year, Kruskal-Willis test of 
 central  tendency of male capture by moth, and generate Fig4 boxplot of time 
 of capture by month.
 - **script5** Manipulate date to get median hour of capture vs. temperature
 at time of median hour by month. Generate Fig5 facet_wrap of scatterplots,
 and perform Pearson correlation by month
  
## Output files
 - Fig2.jpg - Daily count and temperature profile, 2019
 - Fig3.jpg - Daily count and temperature profile, 2020
 - Fig4.jpg - Monthly boxplots on capture by time of day
 - Fig5.jpg - Monthly scatterplots of daily median capture by temperature
 

 