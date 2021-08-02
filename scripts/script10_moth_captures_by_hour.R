#===========================================================================#
# script10_moth_captures_by_hour.R
# 
# PARTS
# 1. Load combined 2019 and 2020 count and temperature data files (line 15)
# 2. Merge and 2019 and 2020 count data (line 25)
# 3. Explore count data only by year, month, site, etc (line 62)
#
#===========================================================================#

library(tidyverse)
library(lubridate)
#library(timechange)

#-- 1. Load and clean 2019 and 2020 data files (same as script3) ------------

# Load original data files into memory
allsites19 <- read.csv("./allsites_y19_scrubbed.csv") 
allsites20 <- read.csv("./allsites_y20_scrubbed.csv")

# Examine and fix discrepancy in site names in 2020 capture and temperature data
unique(allsites20$site)
# [1] "mikewoolf1" "mikewoolf2" "mikewoolf3" "mikewoolf4" "mikewoolf5"

#-- 2. Merge and 2019 and 2020 count data --------------------------

# Modify for merge compatibility)
head(allsites19)
head(allsites20)

allsites19 <- allsites19 %>% 
  filter(pest_dif > 0)
#-- 802 observations

allsites20 <- allsites20 %>% 
  filter(pest_dif > 0)
#-- 681 observations

counts_y19y20 <- rbind(allsites19,allsites20)

counts_y19y20 %>% 
  filter(reviewed == "No")
# datetime pest_nmbr pest_dif reviewed event  site
# 1 2019-08-16 15:56:00         3        3       No  <NA> Perez
# 2 2019-08-19 04:28:00         1        1       No  <NA> Perez
# 3 2019-08-19 00:28:00         1        1       No  <NA>  usda
# 4 2019-08-31 05:27:00         1        1       No  <NA>  usda

counts_y19y20 <- counts_y19y20 %>% 
  filter(reviewed != "No") %>% 
  select(datetime,pest_dif,site) %>% 
  mutate(Yr = year(datetime),
         Mnth = month(datetime, label = TRUE, abbr = TRUE),
         Julian = yday(datetime),
         Hr = hour(datetime))
#-- 1479 obs

offhrs <- counts_y19y20 %>% 
  filter(Hr > 7)
#-- 201 obs

#-- 3. Explore count data only by year, month, site, etc --------------------

head(offhrs,2)
#              datetime pest_dif      site   Yr Mnth Julian Hr
# 1 2019-04-26 13:56:00        7 UCKearney 2019  Apr    116 13
# 2 2019-05-03 20:56:00        1 UCKearney 2019  May    123 20

offhrs %>% 
  group_by(Yr,site) %>% 
  summarise(nObs = n())
# A tibble: 10 x 3
# Groups:   Yr [2]
# Yr site         nObs
#    <dbl> <chr>       <int>
# 1  2019 MWoolf_east    16
# 2  2019 MWoolf_west    70
# 3  2019 Perez           7
# 4  2019 UCKearney      13
# 5  2019 usda           17
# 6  2020 mikewoolf1     18
# 7  2020 mikewoolf2     13
# 8  2020 mikewoolf3     20
# 9  2020 mikewoolf4     21
# 10  2020 mikewoolf5     12
#-- Proportional to the overall number captured at the sites? We can come back
#-- to that. The question is more meaningful for 2019 than 2020

offhrs %>% 
  group_by(Yr,Mnth) %>% 
  summarise(nObs = n())
# A tibble: 13 x 3
# Groups:   Yr [2]
# Yr Mnth   nObs
#    <dbl> <ord> <int>
#  1  2019 Apr       3
#  2  2019 May       9
#  3  2019 Jun       5
#  4  2019 Jul       6
#  5  2019 Aug      16
#  6  2019 Sep      36
#  7  2019 Oct      48
#  8  2020 Apr      13
#  9  2020 May      12
# 10  2020 Jun      13
# 11  2020 Jul      15
# 12  2020 Aug      10
# 13  2020 Sep      21
#-- Here, too, totals must be known to put these numbers in perspective

offhrs %>% 
  group_by(Yr,Mnth,Hr) %>% 
  summarise(nObs = n())
# A tibble: 106 x 4
# Groups:   Yr, Mnth [13]
#      Yr Mnth     Hr  nObs
#   <dbl> <ord> <int> <int>
# 1  2019 Apr      13     1
# 2  2019 Apr      17     1
# 3  2019 Apr      20     1
# 4  2019 May       9     2
# 5  2019 May      10     1

offhrs3 <- offhrs %>% 
  group_by(Mnth,Hr) %>% 
  summarise(nObs = n())

#-- 4. Merge to temperature data and examine by temperature -------

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

alltemps19 <- readr::read_csv("./trapview_temps_degf_y19.csv")
alltemps20 <- readr::read_csv("./trapview_temps_degf_y20.csv")

# Modify for merge compatibility)
alltemps19 <- alltemps19 %>% 
  dplyr::rename(datetime = Date_time)
alltemps20 <- alltemps20 %>% 
  dplyr::rename(datetime = Date_time)

### Make variables in alltemps19 and alltemps20 compatible with each
### other and with the counts data set

alltemps19
# A tibble: 18,969 x 6
# site      datetime            degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7
# 3 UCKearney 2019-05-16 19:00:00     55.0    53.8    56.8   90.4

alltemps20 
# A tibble: 17,580 x 6
#   datetime            degf_avg degf_lo degf_hi rh_avg site 
#   <dttm>                 <dbl>   <dbl>   <dbl>  <dbl> <chr>
# 1 2020-04-22 02:00:00     51.5    50.2    52.5   90.3 MWT1 
# 2 2020-04-22 03:00:00     49.5    48.9    49.8   93.8 MWT1 
# 3 2020-04-22 04:00:00     49.7    48.9    50.4   94.2 MWT1 

alltemps20$site[alltemps20$site == "MWT1"] <- "mikewoolf1"
alltemps20$site[alltemps20$site == "MWT2"] <- "mikewoolf2"
alltemps20$site[alltemps20$site == "MWT3"] <- "mikewoolf3"
alltemps20$site[alltemps20$site == "MWT4"] <- "mikewoolf4"
alltemps20$site[alltemps20$site == "MWT5"] <- "mikewoolf5"

unique(alltemps19$site)
# [1] "UCKearney"   "usda"        "Perez"       "MWoolf_west" "MWoolf_east"

unique(alltemps20$site)
# [1] "mikewoolf1" "mikewoolf2" "mikewoolf3" "mikewoolf4" "mikewoolf5"

unique(counts_y19y20$site)
# [1] "UCKearney"   "MWoolf_east" "MWoolf_west" "Perez"       "usda"        "mikewoolf1"  "mikewoolf2"  "mikewoolf3"  "mikewoolf4"  "mikewoolf5" 

