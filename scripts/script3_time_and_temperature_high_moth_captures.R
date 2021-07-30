#===========================================================================#
# script3_time_and_temperature_high_moth_captures.R
# 
# Examine temperatures and times of capture fore sites and nights with high
# moth capture. Note that the gt20 files are are from the "scrubbed data"
#
# This script uses the "greater than 20" file to select traps and days 
# examined. These observations are defined and selected in script 2, and
# essentially means that only trap-day combinations with >20 moths trapped
# are considered. This is merged back onto the total data set to get an
# hourly trace. 
#
# PARTS
# 1. Load previous "gt20" files into global memory (and other input files) (24)
# 2. Merge and plot 2019 data (64)
# 3. Merge and plot 2020 data 
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(timechange)

#---------------------------------------------------------------------------
# 1. Load previous "gt20" files into global memory (and other input files)
#---------------------------------------------------------------------------#
gt20_y19 <- readr::read_csv("./data/nights_w_counts_gt20_y19.csv")
gt20_y19
# A tibble: 34 x 5
#   site         Year Julian  nObs NowPrDay
#   <chr>       <dbl>  <dbl> <dbl>    <dbl>
# 1 MWoolf_east  2019    187    48       28
# 2 MWoolf_east  2019    189    48       39

gt20_y20 <- readr::read_csv("./data/nights_w_counts_gt20_y20.csv")
  # Created in script 2

### This file identifies instances of high capture by site and night
### using year and julian day. In order to merge with and filter other files, 
### it is necessary to have Julian day in the files to be merged

head(gt20_y19)
head(gt20_y20)

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

# Introduce Julian date into original files in order to merge to gt20 subset
allsites19$Julian <- lubridate::yday(allsites19$datetime)
allsites20$Julian <- lubridate::yday(allsites20$datetime)
alltemps19$Julian <- lubridate::yday(alltemps19$datetime)
alltemps20$Julian <- lubridate::yday(alltemps20$datetime)

# Use the gt20 files to narrow records in allsites and alltemps
counts19 <- dplyr::left_join(gt20_y19,allsites19)
  # by = c("site","Julian"), reduces from 38202 to 1534 obs
temps19 <- dplyr::left_join(gt20_y19,alltemps19)
  # by = c("site","Julian"), reduces from 18969 to 816 obs
counts20 <- dplyr::left_join(gt20_y20,allsites20)
  # by = c("site","Julian"), reduces from 31723 to 407 obs
temps20 <- dplyr::left_join(gt20_y20,alltemps20)
  # by = c("site","Julian"), reduces from 1750 to 218 obs

#---------------------------------------------------------------------------
# 3. clean and plot 2019 data
#---------------------------------------------------------------------------#
head(counts19,2) # 1534 obs
# A tibble: 2 x 10
#   site         Year Julian  nObs NowPrDay datetime            pest_nmbr pest_dif reviewed event
#   <chr>       <dbl>  <dbl> <dbl>    <dbl> <chr>                   <int>    <int> <chr>    <chr>
# 1 MWoolf_east  2019    187    48       28 2019-07-06 00:28:00        20        0 Yes      NA   
# 2 MWoolf_east  2019    187    48       28 2019-07-06 00:58:00        20        0 Yes      NA   

# datetime is char, needs to be dttm
counts19$datetime <- as.POSIXct(counts19$datetime)
counts19

head(temps19,2) #407 obs
# A tibble: 2 x 10
#   site         Year Julian  nObs NowPrDay datetime            degf_avg degf_lo degf_hi rh_avg
#   <chr>       <dbl>  <dbl> <dbl>    <dbl> <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 MWoolf_east  2019    187    48       28 2019-07-06 00:00:00     70.6    68.9    72.3   53.4
# 2 MWoolf_east  2019    187    48       28 2019-07-06 01:00:00     70.1    68.7    71.4   51.9


realtz  <- "America/Los_Angeles"

### Get time from midnight as a numeric variable 
    # Not doing this because prelminary says it is not worth it

# Set timezone as recorded (R defaults to UTC, but was PDT)
counts19$datetime <- timechange::time_force_tz(counts19$datetime, tz = realtz)
attr(counts19$datetime, "tzone")
# [1] "America/Los_Angeles"

# Get time since zero hour
counts19$mnight <- lubridate::make_datetime(year(counts19$datetime),
                                                       month(counts19$datetime),
                                                       day(counts19$datetime),
                                                       0,0,0,realtz)

counts19$since_night <- difftime(counts19$datetime,counts19$mnight, units = "hours")
# finds time since midnight
counts19$since_night2 <- as.numeric(counts19$since_night)
# makes the time unit numeric

# Examine output data
glimpse(counts19)
select(counts19, c(site,Julian,pest_dif,datetime,since_night2))

# graph data
ggplot(counts19, aes(x = since_night2, y = pest_dif)) +
  geom_point() +
  ylim(1,50) +
  facet_grid(site ~ .)
##### #
# Results
# - Most obs between 3 and 6:30 (drill in to confirm)
# - Can examine individual obs outside this time
# 

# Filter for flyers at unusual hours
x <- counts19 %>% 
  filter(pest_dif > 0) %>% 
  filter(since_night2 < 2.5 | since_night2 > 6.5)

### 26 observations. The most "unusual is 7 moths at 11AM 2019-08-16, MWoolf-east


#---------------------------------------------------------------------------
# 2. clean and plot 2020 data
#---------------------------------------------------------------------------#

# Determine how to combine data sets in tidy format for ggplot
head(counts20,2) 
# A tibble: 2 x 10
#site   Year Julian  nObs NowPrDay datetime           
#<chr> <dbl>  <dbl> <dbl>    <dbl> <dttm>             
#1 mike…  2020    170    49       20 2020-06-18 00:27:00
#2 mike…  2020    170    49       20 2020-06-18 00:57:00
# … with 4 more variables: pest_nmbr <dbl>, pest_dif <dbl>,
#   reviewed <chr>, event <chr>

head(temps20,2) 
# A tibble: 2 x 10
#site   Year Julian  nObs NowPrDay datetime            degf_avg
#<chr> <dbl>  <dbl> <dbl>    <dbl> <dttm>                 <dbl>
#1 mike…  2020    170    49       20 2020-06-18 00:00:00     60.4
#2 mike…  2020    170    49       20 2020-06-18 01:00:00     60.9
# … with 3 more variables: degf_lo <dbl>, degf_hi <dbl>,
#   rh_avg <dbl>

### Get Date and Minutes from sunset


counts20
# datetime is char, needs to be dttm
counts20$datetime <- as.POSIXct(counts20$datetime)
counts20

# Set timezone as recorded (R defaults to UTC, but was PDT)
counts20$datetime <- timechange::time_force_tz(counts20$datetime, tz = realtz)
attr(counts20$datetime, "tzone")
# [1] "America/Los_Angeles"

# Get time since zero hour (midnight)
counts20$mnight <- lubridate::make_datetime(year(counts20$datetime),
                                                 month(counts20$datetime),
                                                 day(counts20$datetime),
                                                 0,0,0,realtz)

counts20$since_night <- difftime(counts20$datetime,counts20$mnight, units = "hours")
# finds time since midnight
counts20$since_night2 <- as.numeric(counts20$since_night)
# makes the time unit numeric

# Examine output data
glimpse(counts20)
select(counts20, c(site,Julian,pest_dif,datetime,since_night2))

# graph data
ggplot(counts20, aes(x = since_night2, y = pest_dif)) +
  geom_point() +
  ylim(1,20) +
  facet_grid(site ~ .)

### Overall patterns similar to 2019
x2 <- counts20 %>% 
  filter(pest_dif > 0) %>% 
  filter(since_night2 < 2.5 | since_night2 > 6.5)
