#===========================================================================#
# script2_nightly_variation_moth_capture
# 
# Examine variation in nightly temperatures, and determine the number of
# observations in a range relevant to shifting mating activity earlier
#
#===========================================================================#

library(tidyverse)
library(lubridate)

# Load count data
allsites19 <- readr::read_csv("./allsites_y19.csv")
allsites20 <- readr::read_csv("./allsites_y20.csv")

head(allsites19)
# A tibble: 6 x 6
#   datetime            pest_nmbr pest_dif reviewed event site     
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>    
# 1 2019-04-26 13:56:00         7        7 Yes      NA    UCKearney
# 2 2019-04-26 14:56:00         7        0 Yes      NA    UCKearney
# 3 2019-04-26 15:57:00         7        0 Yes      NA    UCKearney
# 4 2019-04-26 16:56:00         7        0 Yes      NA    UCKearney
# 5 2019-04-26 17:57:00         7        0 Yes      NA    UCKearney
# 6 2019-04-26 18:57:00         7        0 Yes      NA    UCKearney

# Names of site
unique(allsites19$site)
# [1] "UCKearney"   "MWoolf_east" "MWoolf_west" "Perez"       "usda"  

# Identify starts and gaps in data. Perhaps simpler in Julian days
nightly19 <- allsites19 %>% 
  mutate(Julian = yday(datetime)) %>% 
  group_by(site, Julian) %>% 
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
  summarise(Min = min(Julian),
            Max = max(Julian),
            nDays = n_distinct(Julian),
            NowPrDay = sum(NowPrDay, na.rm = TRUE))
#   site          Min   Max nDays NowPrDay
#   <chr>       <dbl> <dbl> <int>    <dbl>
# 1 MWoolf_east   155   304   150      669
# 2 MWoolf_west   142   304   163     1322
# 3 Perez         154   308   155      241
# 4 UCKearney     116   308   193      114
# 5 usda          114   304   191      237

# Nights with >= 20 moths (i.e., enought ot make comparisons) 
CountsGt20 <- nightly19 %>% 
  filter(NowPrDay >= 20)
FSA::headtail(CountsGt20)
#           site Julian nObs NowPrDay
# 1  MWoolf_east    187   48       28
# 2  MWoolf_east    189   48       39
# 3  MWoolf_east    221   50       21
# 32       Perez    230   49       29
# 33       Perez    232   49       25
# 34        usda    266   49       20

unique(CountsGt20$site)
# [1] "MWoolf_east" "MWoolf_west" "Perez"       "usda" 

# four of the five sites represented, but more the two Mike Woolf sites than 
# the others. Only 34 nights total. CountsGt20 provides a way to filter 2019
# data by Julian date
write.csv(CountsGt20,
          "./nights_w_counts_gt20_y19.csv",
          row.names = FALSE)

# Similar characterization for 2020
nightly20 <- allsites20 %>% 
  mutate(Julian = yday(datetime)) %>% 
  group_by(site, Julian) %>% 
  summarise(nObs = n(),
            NowPrDay = sum(pest_dif, na.rm = TRUE))
nightly20
# A tibble: 729 x 4
# Groups:   site [5]
#   site       Julian  nObs NowPrDay
#   <chr>       <dbl> <int>    <dbl>
# 1 mikewoolf1    113    27        8
# 2 mikewoolf1    114    38        0
# 3 mikewoolf1    115    32        4

# Season summary of days and captures by site
nightly20 %>% 
  group_by(site) %>% 
  summarise(Min = min(Julian),
            Max = max(Julian),
            nDays = n_distinct(Julian),
            NowPrDay = sum(NowPrDay, na.rm = TRUE))
# A tibble: 5 x 5
#   site         Min   Max nDays NowPrDay
#   <chr>      <dbl> <dbl> <int>    <dbl>
# 1 mikewoolf1   113   266   146      294
# 2 mikewoolf2    72   266   179      394
# 3 mikewoolf3   112   266   145      257
# 4 mikewoolf4    66   266   181      376
# 5 mikewoolf5   189   266    78       64

CountsGt20_y20 <- nightly20 %>% 
  filter(NowPrDay >= 20)
FSA::headtail(CountsGt20_y20)
#    site Julian nObs NowPrDay
# 1  mikewoolf1    170   49       20
# 2  mikewoolf1    174   22       25
# 3  mikewoolf1    265   50       21
# 8  mikewoolf4    261   48       20
# 9  mikewoolf4    263   47       20
# 10 mikewoolf4    265   51       38

unique(CountsGt20_y20$site)
# [1] "mikewoolf1" "mikewoolf2" "mikewoolf3" "mikewoolf4"

# four of the five sites represented, but more the two Mike Woolf sites than 
# the others. Only 34 nights total. CountsGt20 provides a way to filter 2019
# data by Julian date
write.csv(CountsGt20,
          "./nights_w_counts_gt20_y20.csv",
          row.names = FALSE)
