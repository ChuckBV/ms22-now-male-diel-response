#===========================================================================#
# script2_nightly_variation_moth_capture
# 
# Find sites and nights with over 20 captures per night (better estimate of
# distribution of capture times). 
#
# "script2b_... R deleted (but can be resurrected through version control)
# This second version of this script attempted to correct an error introduced
# by the cleaning feature of Trapview. That issue was instead address by 
# Jake Wenger (see wenger_scrubbed_role_files.pdf)
#
#===========================================================================#

library(tidyverse)
library(lubridate)

# Load count data
x <- read.csv("./data/allsites_y19_scrubbed.csv")
  # Use base R read.csv because specifying data type in read_csv is too complicated
str(x) # reveals that x$event is a character variable
unique(x$event)
  # Using base R because many NAs make column specification problems and 
  # specifying columns in readr::read_csv is too difficult

allsites19 <- read.csv("./data/allsites_y19_scrubbed.csv") 
allsites20 <- read.csv("./data/allsites_y20_scrubbed.csv")

# Names of site
unique(allsites19$site)
# [1] "UCKearney"   "MWoolf_east" "MWoolf_west" "Perez"       "usda"  
unique(allsites20$site)
# [1] "mikewoolf1" "mikewoolf2" "mikewoolf3" "mikewoolf4" "mikewoolf5"


# Identify starts and gaps in data. Perhaps simpler in Julian days
nightly19 <- allsites19 %>% 
  mutate(Year = year(datetime),
         Julian = yday(datetime)) %>% 
  group_by(site, Year, Julian) %>% 
  summarise(nObs = n(),
            NowPrDay = sum(pest_dif, na.rm = TRUE))
nightly19
# A tibble: 852 x 4
# Groups:   site [5]
#   site        Julian  nObs NowPrDay
#   <chr>        <dbl> <int>    <dbl>
# 1 MWoolf_east    155     7        0
# 2 MWoolf_east    156    23        0
# 3 MWoolf_east    157    24        0


# Season summary of days and captures by site
nightly19 %>% 
  group_by(site) %>% 
  summarise(Year = mean(Year, na.rm = TRUE),
            Min = min(Julian),
            Max = max(Julian),
            nDays = n_distinct(Julian),
            NowPrDay = sum(NowPrDay/nDays, na.rm = TRUE))
# A tibble: 5 x 6
#   site         Year   Min   Max nDays NowPrDay
#   <chr>       <dbl> <dbl> <dbl> <int>    <dbl>
# 1 MWoolf_east  2019   155   304   150    4.46 
# 2 MWoolf_west  2019   142   304   163    8.11 
# 3 Perez        2019   154   308   155    1.55 
# 4 UCKearney    2019   116   308   193    0.591
# 5 usda         2019   114   304   191    1.24 

# Nights with >= 20 moths (i.e., enought ot make comparisons) 
CountsGt20 <- nightly19 %>% 
  filter(NowPrDay >= 20)
FSA::headtail(CountsGt20)
# site Year Julian nObs NowPrDay
# 1  MWoolf_east 2019    187   48       28
# 2  MWoolf_east 2019    189   48       39
# 3  MWoolf_east 2019    228   47       26
# 25       Perez 2019    230   47       29
# 26       Perez 2019    232   47       25
# 27        usda 2019    266   47       20

unique(CountsGt20$site)
# [1] "MWoolf_east" "MWoolf_west" "Perez"       "usda" 

# four of the five sites represented, but more the two Mike Woolf sites than 
# the others. Only 34 nights total. CountsGt20 provides a way to filter 2019
# data by Julian date
write.csv(CountsGt20,
          "./data/nights_w_counts_gt20_y19.csv",
          row.names = FALSE)

# Similar characterization for 2020
nightly20 <- allsites20 %>% 
  mutate(Year = year(datetime),
         Julian = yday(datetime)) %>% 
  group_by(site, Year, Julian) %>% 
  summarise(nObs = n(),
            NowPrDay = sum(pest_dif, na.rm = TRUE))
nightly20
# A tibble: 729 x 5
# Groups:   site, Year [5]
#   site        Year Julian  nObs NowPrDay
#   <chr>      <dbl>  <dbl> <int>    <dbl>
# 1 mikewoolf1  2020    113    27        8
# 2 mikewoolf1  2020    114    38        0
# 3 mikewoolf1  2020    115    32        4

# Season summary of days and captures by site
nightly20 %>% 
  group_by(site) %>% 
  summarise(Year = mean(Year,na.rm = TRUE),
            Min = min(Julian),
            Max = max(Julian),
            nDays = n_distinct(Julian),
            NowPrDay = sum(NowPrDay, na.rm = TRUE))
#   site        Year   Min   Max nDays NowPrDay
#   <chr>      <dbl> <dbl> <dbl> <int>    <int>
# 1 mikewoolf1  2020   113   266   146      278
# 2 mikewoolf2  2020    72   266   179      381
# 3 mikewoolf3  2020   112   266   145      241
# 4 mikewoolf4  2020    66   266   181      376
# 5 mikewoolf5  2020   189   266    78       58

CountsGt20_y20 <- nightly20 %>% 
  filter(NowPrDay >= 20)
FSA::headtail(CountsGt20_y20)
#         site Year Julian nObs NowPrDay
# 1 mikewoolf1 2020    265   46       20
# 2 mikewoolf2 2020    265   40       54
# 3 mikewoolf2 2020    266   19       51
# 6 mikewoolf4 2020    261   48       20
# 7 mikewoolf4 2020    263   47       20
# 8 mikewoolf4 2020    265   51       38

unique(CountsGt20_y20$site)
# [1] "mikewoolf1" "mikewoolf2" "mikewoolf3" "mikewoolf4"

# four of the five sites represented, but more the two Mike Woolf sites than 
# the others. Only 34 nights total. CountsGt20 provides a way to filter 2019
# data by Julian date
write.csv(CountsGt20_y20,
          "./data/nights_w_counts_gt20_y20.csv",
          row.names = FALSE)
