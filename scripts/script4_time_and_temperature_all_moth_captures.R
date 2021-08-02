#===========================================================================#
# script4_time_and_temperature_all_moth_captures.R
# 
#
# PARTS
# 1. Load combined 2019 and 2020 count and temperature data files
# 2. Merge and plot 2019 data (64)
# 3. Merge and plot 2020 data 
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(timechange)

#---------------------------------------------------------------------------
# 1. Load and clean 2019 and 2020 data files (same as script3)
#---------------------------------------------------------------------------#

# Load original data files into memory
allsites19 <- read.csv("./allsites_y19_scrubbed.csv") 
allsites20 <- read.csv("./allsites_y20_scrubbed.csv")
alltemps19 <- readr::read_csv("./trapview_temps_degf_y19.csv")
alltemps20 <- readr::read_csv("./trapview_temps_degf_y20.csv")

# Examine and fix discrepancy in site names in 2020 capture and temperature data
unique(allsites20$site)
# [1] "mikewoolf1" "mikewoolf2" "mikewoolf3" "mikewoolf4" "mikewoolf5"
unique(alltemps20$site)
# [1] "MWT1" "MWT2" "MWT3" "MWT4" "MWT5"

alltemps20$site[alltemps20$site == "MWT1"] <- "mikewoolf1"
alltemps20$site[alltemps20$site == "MWT2"] <- "mikewoolf2"
alltemps20$site[alltemps20$site == "MWT3"] <- "mikewoolf3"
alltemps20$site[alltemps20$site == "MWT4"] <- "mikewoolf4"
alltemps20$site[alltemps20$site == "MWT5"] <- "mikewoolf5"

#---------------------------------------------------------------------------
# 2. Merge and 2019 and 2020 data
#---------------------------------------------------------------------------#

# Modify for merge compatibility)
alltemps19 <- alltemps19 %>% 
  dplyr::rename(datetime = Date_time)
alltemps20 <- alltemps20 %>% 
  dplyr::rename(datetime = Date_time)

alltemps19
# A tibble: 18,969 x 6
# site      datetime            degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7
# 3 UCKearney 2019-05-16 19:00:00     55.0    53.8    56.8   90.4

# how many temperature records per day?
alltemps19 %>% 
  mutate(Yr = year(datetime),
         Julian = yday(datetime)) %>% 
  group_by(site,Yr,Julian) %>% 
  summarise(nObs = n()) %>% #798 records before filtering
  filter(nObs != 24) #18 exceptions
  # 24 in almost all cases


