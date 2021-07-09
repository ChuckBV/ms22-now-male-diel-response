#===========================================================================#
# script2b_nightly_variation_moth_capture.R
# 
# Find sites and nights with over 20 captures per night (better estimate of
# distribution of capture times). 
#
#===========================================================================#

library(tidyverse)
library(lubridate)

#--------------------------------------------------------------------------
# Examine data for which there is a large pest_diff with an equal pest_nmbr
#-------------------------------------------------------------------------#

# load count data into global memory
allsites19 <- readr::read_csv("./allsites_y19.csv")
allsites20 <- readr::read_csv("./allsites_y20.csv")

# address differences in 2019 and 2020 data
allsites19
# A tibble: 38,202 x 6
#   datetime            pest_nmbr pest_dif reviewed event site     
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>    
# 1 2019-04-26 13:56:00         7        7 Yes      NA    UCKearney
# 2 2019-04-26 14:56:00         7        0 Yes      NA    UCKearney
# 3 2019-04-26 15:57:00         7        0 Yes      NA    UCKearney

allsites20
# A tibble: 31,726 x 6
#   datetime            pest_nmbr pest_dif reviewed event site      
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>     
# 1 2020-04-22 03:18:00         5        4 Yes      NA    mikewoolf1
# 2 2020-04-22 05:02:00         7        2 Yes      NA    mikewoolf1
# 3 2020-04-22 06:35:00         9        2 Yes      NA    mikewoolf1

# combine 2 years data
allsites <- rbind(allsites19,allsites20)
allsites
# A tibble: 69,928 x 6
#   datetime            pest_nmbr pest_dif reviewed event site     
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>    
# 1 2019-04-26 13:56:00         7        7 Yes      NA    UCKearney
# 2 2019-04-26 14:56:00         7        0 Yes      NA    UCKearney
# 3 2019-04-26 15:57:00         7        0 Yes      NA    UCKearney
    # Apparently lumping data for the two years works

# Get year and Julian date in combined data set
allsites$Yr <- year(allsites$datetime)
allsites$Julian <- yday(allsites$datetime)
allsites
# A tibble: 69,928 x 8
#  datetime            pest_nmbr pest_dif reviewed event site         Yr Julian
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>     <dbl>  <dbl>
# 1 2019-04-26 13:56:00         7        7 Yes      NA    UCKearney  2019    116
# 2 2019-04-26 14:56:00         7        0 Yes      NA    UCKearney  2019    116
# 3 2019-04-26 15:57:00         7        0 Yes      NA    UCKearney  2019    116

# Examine how frequently we see pest_dif equal to pest_nmbr
x <- allsites %>% 
  filter(pest_nmbr > 0 & pest_dif == pest_nmbr & pest_nmbr == lag(pest_nmbr, n = 1)) %>% 
  # reduces to 339 of 69928 observations
  group_by(Yr,site,Julian) %>% 
  summarise(nObs = n())
# reduces from 69928 to 4 obs

# Use resultant data set x to filer allsites
x <- left_join(x,allsites)
x # 145 obs

### Only 4 nights were there were oddities like this. All <5, not likely to 
### affect the data.

#--------------------------------------------------------------------------
# Examine when and where daytime fliers occur (defined as 6AM to 6PM)
#-------------------------------------------------------------------------#

# Examine how the lubridate::hour() bins time
allsites %>%  #69928 obs before modification
  group_by(Yr,site,Julian) %>% 
  summarise(nObs = n()) %>%  #1571 obs to this point
  filter(nObs > 45) %>% #1256 obs
  head(10)
# A tibble: 10 x 4
# Groups:   Yr, site [1]
#    Yr site        Julian  nObs
#    <dbl> <chr>        <dbl> <int>
# 1  2019 MWoolf_east    166    47
# 2  2019 MWoolf_east    167    47
# 3  2019 MWoolf_east    168    48
# 4  2019 MWoolf_east    169    48
# 5  2019 MWoolf_east    170    48
# 6  2019 MWoolf_east    171    48
# 7  2019 MWoolf_east    173    48
# 8  2019 MWoolf_east    174    48
# 9  2019 MWoolf_east    175    48
# 10  2019 MWoolf_east    176    48

x <- allsites %>% 
  filter(Yr == 2019 & site == "MWoolf_east" & Julian %in% seq(from = 166, to = 176, by = 1))
  # captured nothing at this site in this period, but will serve for demo

x$hr <- hour(x$datetime)
x
# A tibble: 523 x 9
#   datetime            pest_nmbr pest_dif reviewed event site           Yr Julian    hr
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>       <dbl>  <dbl> <int>
# 1 2019-06-15 00:28:00         0        0 Yes      NA    MWoolf_east  2019    166     0
# 2 2019-06-15 00:58:00         0        0 Yes      NA    MWoolf_east  2019    166     0
# 3 2019-06-15 01:28:00         0        0 Yes      NA    MWoolf_east  2019    166     1
# 4 2019-06-15 01:58:00         0        0 Yes      NA    MWoolf_east  2019    166     1
# 5 2019-06-15 02:28:00         0        0 Yes      NA    MWoolf_east  2019    166     2
# 6 2019-06-15 02:58:00         0        0 Yes      NA    MWoolf_east  2019    166     2
# 7 2019-06-15 03:28:00         0        0 Yes      NA    MWoolf_east  2019    166     3
# 8 2019-06-15 03:58:00         0        0 Yes      NA    MWoolf_east  2019    166     3
# 9 2019-06-15 04:29:00         0        0 Yes      NA    MWoolf_east  2019    166     4
# 10 2019-06-15 04:59:00         0        0 Yes      NA    MWoolf_east  2019    166     4
  # so hour means time >= hr and < hr+1
  # important in own right, and when synching temp
  # so for daytime flyer, hr >= 6 and hr < 18

# How many moths alltogether
sum(allsites$pest_dif, na.rm = TRUE)
# [1] 3968

dayhr <- allsites %>% 
  mutate(hr = hour(datetime)) %>% 
  filter(hr >= 6 & hr < 18 & pest_dif > 0)
dayhr 
# A tibble: 245 x 9
#   datetime            pest_nmbr pest_dif reviewed event site         Yr Julian    hr
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>     <dbl>  <dbl> <int>
# 1 2019-04-26 13:56:00         7        7 Yes      NA    UCKearney  2019    116    13
# 2 2019-05-16 09:56:00         3        3 Yes      NA    UCKearney  2019    136     9
# 3 2019-05-16 10:56:00         1        1 Yes      NA    UCKearney  2019    136    10
# 4 2019-08-17 17:27:00         1        1 Yes      NA    UCKearney  2019    229    17

sum(dayhr$pest_dif, na.rm = TRUE)
# [1] 668

668/3968
# [1] 0.1683468

# 16% is a higher proportion of day fliers than I anticipated

dayhr$mnth <- month(dayhr$datetime)
dayhr

dayhr <- dayhr %>% 
  group_by(Yr,mnth) %>% 
  summarise(dfly = sum(pest_dif, na.rm = TRUE))

all_mnthly <- allsites %>% 
  mutate(mnth = month(datetime)) %>% 
  group_by(Yr,mnth) %>% 
  summarise(totals = sum(pest_dif, na.rm = TRUE))

left_join(dayhr,all_mnthly) %>% 
  mutate(pct_dflier = 100*dfly/totals)
# A tibble: 13 x 5
# Groups:   Yr [2]
#    Yr  mnth  dfly totals pct_dflier
#     <dbl> <dbl> <dbl>  <dbl>      <dbl>
#  1  2019     4     9     30      30   
#  2  2019     5    21    258       8.14
#  3  2019     6     6     94       6.38
#  4  2019     7     7    516       1.36
#  5  2019     8   228    833      27.4 
#  6  2019     9   165    709      23.3 
#  7  2019    10    11    143       7.69
#  8  2020     4    19    140      13.6 
#  9  2020     5     8     44      18.2 
# 10  2020     6    43    111      38.7 
# 11  2020     7    27    270      10   
# 12  2020     8    22    379       5.80
# 13  2020     9   102    435      23.4 

#--------------------------------------------------------------------------
# Determine when and where early night fliers occur (>6PM and <12AM)
#-------------------------------------------------------------------------#

early <- allsites %>% 
  mutate(hr = hour(datetime)) %>% 
  filter(hr >= 18 & hr <= 24 & pest_dif > 0)
early 
  # 85 moths at midnight in June?

allsites %>% 
  filter(site == "MWoolf_east" & Julian == 230 & hour(datetime) > 20)
# A tibble: 3 x 8
#   datetime            pest_nmbr pest_dif reviewed event             site           Yr Julian
#   <dttm>                  <dbl>    <dbl> <chr>    <chr>             <chr>       <dbl>  <dbl>
# 1 2019-08-18 23:29:00        85        0 Yes      NA                MWoolf_east  2019    230
# 2 2019-08-18 23:29:01         0        0 No       Sticky roll tweak MWoolf_east  2019    230
# 3 2019-08-18 23:59:00        85       85 Yes      NA                MWoolf_east  2019    230  
  # 85 in this case is an error

allsites %>% 
  filter(site == "MWoolf_east" & Yr == 2019 & Julian == 227 & hour(datetime) >= 18)
