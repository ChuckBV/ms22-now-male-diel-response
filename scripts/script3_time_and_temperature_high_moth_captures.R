#===========================================================================#
# script3_time_and_temperature_high_moth_captures.R
# 
# Examine temperatures and times of capture fore sites and nighs with high
# moth capture
#
#===========================================================================#

library(tidyverse)
library(lubridate)

# Load previous "gt20" files into memory
gt20_y19 <- read_csv("./data/nights_w_counts_gt20_y19.csv")
gt20_y20 <- read_csv("./data/nights_w_counts_gt20_y20.csv")

head(gt20_y19)
head(gt20_y20)

# Load original data files into memory
allsites19 <- readr::read_csv("./allsites_y19.csv")
allsites20 <- readr::read_csv("./allsites_y20.csv")
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

alltemps19 <- alltemps19 %>% 
  rename(datetime = Date_time)
alltemps20 <- alltemps20 %>% 
  rename(datetime = Date_time)

# Introduce Julian date into original files
allsites19$Julian <- yday(allsites19$datetime)
allsites20$Julian <- yday(allsites20$datetime)
alltemps19$Julian <- yday(alltemps19$datetime)
alltemps20$Julian <- yday(alltemps20$datetime)

# Use the gt20 files to narrow records in allsites and alltemps
counts19 <- left_join(gt20_y19,allsites19)
  # by = c("site","Julian"), reduces from 38202 to 1534 obs
temps19 <- left_join(gt20_y19,alltemps19)
  # by = c("site","Julian"), reduces from 18969 to 816 obs
counts20 <- left_join(gt20_y20,allsites20)
  # by = c("site","Julian"), reduces from 31723 to 407 obs
temps20 <- left_join(gt20_y20,alltemps20)
# by = c("site","Julian"), reduces from 1750 to 218 obs

# Determine how to compbine data sets in tidy format for ggplot
head(counts19,2)
# A tibble: 2 x 10
#   site         Year Julian  nObs NowPrDay datetime            pest_nmbr pest_dif reviewed event
#   <chr>       <dbl>  <dbl> <dbl>    <dbl> <dttm>                  <dbl>    <dbl> <chr>    <chr>
# 1 MWoolf_east  2019    187    48       28 2019-07-06 00:28:00        20        0 Yes      NA   
# 2 MWoolf_east  2019    187    48       28 2019-07-06 00:58:00        20        0 Yes      NA   

head(temps19,2)
# A tibble: 2 x 10
#   site         Year Julian  nObs NowPrDay datetime            degf_avg degf_lo degf_hi rh_avg
#   <chr>       <dbl>  <dbl> <dbl>    <dbl> <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 MWoolf_east  2019    187    48       28 2019-07-06 00:00:00     70.6    68.9    72.3   53.4
# 2 MWoolf_east  2019    187    48       28 2019-07-06 01:00:00     70.1    68.7    71.4   51.9