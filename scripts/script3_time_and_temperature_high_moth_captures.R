#===========================================================================#
# script3_time_and_temperature_high_moth_captures.R
# 
# Examine temperatures and times of capture fore sites and nights with high
# moth capture. Note that the gt20 files are are from the "scrubbed data"
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(timechange)

# Load previous "gt20" files into memory
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

alltemps19 <- alltemps19 %>% 
  dplyr::rename(datetime = Date_time)
alltemps20 <- alltemps20 %>% 
  dplyr::rename(datetime = Date_time)

# Introduce Julian date into original files
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

# Determine how to combine data sets in tidy format for ggplot
head(counts19,2) # 1534 obs
# A tibble: 2 x 10
#   site         Year Julian  nObs NowPrDay datetime            pest_nmbr pest_dif reviewed event
#   <chr>       <dbl>  <dbl> <dbl>    <dbl> <chr>                   <int>    <int> <chr>    <chr>
# 1 MWoolf_east  2019    187    48       28 2019-07-06 00:28:00        20        0 Yes      NA   
# 2 MWoolf_east  2019    187    48       28 2019-07-06 00:58:00        20        0 Yes      NA   

# datetime is char, needs to be dttm


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

# Get time at UTC -1 (So zero hour is 6PM)
counts19$time_cverde <- timechange::time_at_tz(counts19$datetime, tz = faketz)
attr(counts19$time_cverde, "tzone")
# [1] "Atlantic/Cape_Verde" 

# Get time since zero hour
counts19$mnight_cverde <- lubridate::make_datetime(year(counts19$time_cverde),
                                                       month(counts19$time_cverde),
                                                       day(counts19$time_cverde),
                                                       0,0,0,faketz)

counts19$since_night <- difftime(counts19$time_cverde,counts19$mnight_cverde, units = "hours")
# finds time since midnight
counts19$since_night2 <- as.numeric(counts19$since_night)
# makes the time unit numeric

# Examine output data
glimpse(counts19)
select(counts19, c(site,Julian,pest_dif,datetime,time_cverde,since_night2))

# graph data
ggplot(counts19, aes(x = since_night2, y = pest_dif)) +
  geom_point() +
  facet_grid(site ~ .)
 # MWoolf_west heaviest, sort out outlier MWoolf_east

# Filter for apparent outliers
counts19 %>% 
  filter(site == "MWoolf_east" & pest_dif > 40)
#   A tibble: 2 x 14
#   site    Year Julian  nObs NowPrDay datetime            pest_nmbr pest_dif reviewed event time_cverde        
#   <chr>  <dbl>  <dbl> <dbl>    <dbl> <dttm>                  <dbl>    <dbl> <chr>    <chr> <dttm>             
# 1 MWool~  2019    226    42       55 2019-08-14 04:26:00        44       44 Yes      NA    2019-08-14 10:26:00
# 2 MWool~  2019    230    48       94 2019-08-18 23:59:00        85       85 Yes      NA    2019-08-19 05:59:00

suspect <- counts19 %>% 
  filter(pest_dif > 0 & pest_dif == pest_nmbr)
suspect
  # plausible for <15, not for more
suspect %>% 
  group_by(site) %>% 
  summarise(nObs = n())
# A tibble: 4 x 2
#   site         nObs
#   <chr>       <int>
# 1 MWoolf_east     9
# 2 MWoolf_west    12
# 3 Perez           4
# 4 usda            1

# Remove outliers and 0 counts
counts19b <- counts19 %>% 
  filter(!(pest_dif > 15 & pest_dif == pest_nmbr)) %>% 
  filter(pest_dif > 0)

glimpse(counts19b)

# graph data
ggplot(counts19b, aes(x = since_night2, y = pest_dif)) +
  geom_point() +
  facet_grid(site ~ .)
  # Need to determine what is happening with daytime captures

dayfliers <- counts19b %>% 
  filter(since_night2 > 12 & pest_dif != pest_nmbr) %>% 
  select(c(site,datetime,pest_dif,since_night2))
dayfliers
  # Daytime fliers were rare, generally occured in May or September,
  # and occasionally in August

duskfliers <- counts19b %>% 
  filter(since_night2 < 6 & pest_dif != pest_nmbr) %>% 
  select(c(site,datetime,pest_dif,since_night2))
duskfliers
  # In 2019, male capture before midnight was sufficiently rare that it
  # could be ignored

### Time of first flight for nights w over 20 NOW captured
# counts19c <- counts19 %>% 
#   filter(!(pest_dif > 15 & pest_dif == pest_nmbr)) %>% 
#   filter(pest_dif > 0) %>% 
#   group_by(Julian) %>% 

# Need a categorical variable based on sincenight 2. >6 & <= 7, etc.
# Could reduce categories to half hours if all nObs = 48







# Attempt to duplicate with the 2020 data sets 

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

### Get time from midnight as a numeric variable
xa <- head(counts20,20)
xa$datetime # show the datetime vector

### Store local time zone in memory for subsequent steps
realtz  <- "America/Los_Angeles"
faketz <- "Atlantic/Cape_Verde" # UCT -1

### Get Date and Minutes from sunset

# Set timezone as recorded (R defaults to UTC, but was PDT)
counts20$datetime <- timechange::time_force_tz(counts20$datetime, tz = realtz)
attr(counts20$datetime, "tzone")
# [1] "America/Los_Angeles"

# Get time at UTC -1 (So zero hour is 6PM)
counts20$time_cverde <- timechange::time_at_tz(counts20$datetime, tz = faketz)
attr(counts20$time_cverde, "tzone")
# [1] "Atlantic/Cape_Verde" 

# Get time since zero hour
counts20$mnight_cverde <- lubridate::make_datetime(year(counts20$time_cverde),
                                                   month(counts20$time_cverde),
                                                   day(counts20$time_cverde),
                                                   0,0,0,faketz)

counts20$since_night <- difftime(counts20$time_cverde,counts20$mnight_cverde, units = "hours")
# finds time since midnight
counts20$since_night2 <- as.numeric(counts20$since_night)
# makes the time unit numeric

# Examine output data
glimpse(counts20)
select(counts20, c(site,Julian,pest_dif,datetime,time_cverde,since_night2))

# graph data
ggplot(counts20, aes(x = since_night2, y = pest_dif)) +
  geom_point() +
  facet_grid(site ~ .)


# Filter for apparent outliers
counts20 %>% 
  filter(site ==  pest_dif > 40)


suspect <- counts20 %>% 
  filter(pest_dif > 0 & pest_dif == pest_nmbr)
suspect
# plausible for <15, not for more
suspect %>% 
  group_by(site) %>% 
  summarise(nObs = n())
# A tibble: 4 x 2
#site        nObs
#<chr>      <int>
#1 mikewoolf1     4
#2 mikewoolf2     3
#3 mikewoolf3     3
#4 mikewoolf4     3

# Remove outliers and 0 counts
counts20b <- counts20 %>% 
  filter(!(pest_dif > 15 & pest_dif == pest_nmbr)) %>% 
  filter(pest_dif > 0)

glimpse(counts20b)

# graph data
ggplot(counts20b, aes(x = since_night2, y = pest_dif)) +
  geom_point() +
  facet_grid(site ~ .)
# Need to determine what is happening with daytime captures

dayfliers2 <- counts20b %>% 
  filter(since_night2 > 12 & pest_dif != pest_nmbr) %>% 
  select(c(site,datetime,pest_dif,since_night2))
dayfliers2
# Daytime fliers were rare, generally occured in May or September,
# and occasionally in August

duskfliers2 <- counts19b %>% 
  filter(since_night2 < 6 & pest_dif != pest_nmbr) %>% 
  select(c(site,datetime,pest_dif,since_night2))
duskfliers2

