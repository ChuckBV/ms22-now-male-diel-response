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

# In at least some cases, all moths accumulated on a sticky roll after a 
# "tweak" are counted again. The pattern seems to be pest_nmbr = 0, pest_dif
# = 0 in the tweak, with the tweak 1 sec after the previous reading. Need to
# get previous and next records for tweaks. In principle this can be done 
# by built-in indexing in R

tweaks <- which(allsites$event %in% "Sticky roll tweak")
tweaks
# [1]    88   475   837  1327  1329  2680  3595  3672  3817  4007  4290  4489  4806  5110  5485  5787  6296  6655  8393
# [20]  9372  9655  9673  9907 10053 10799 11085 11097 11235 11272 11285 11309 11414 11533 11582 11715 11723 11987 12327
# [39] 13308 14536 14584 14633 14659 14695 14902 15201 15610 16292 16567 16593 16596 16734 16834 16888 16981 17032 17261
# [58] 17356 17533 17610 17755 17894 17993 18081 18093 18230 18325 18589 18750 18753 18759 19030 19055 19438 19575 19737
# [77] 19774 19873 20046 20248 20353 20396 20612 20910 21281 21376 21702 21899 22203 22254 22279 22496 23206 23480 23490
# [96] 23508 23511 23547 23748 24443 24849 25138 25234 25338 25454 25550 25667 25784 26325 26343 27509 28806 28814 29165
# [115] 29384 29524 30165 30874 32448 32749 32752 32788 32888 32990 33413 33415 33687 33908 34046 34145 34383 34470 34581
# [134] 34698 34794 34904 35207 35410 35581 35883 36195 36394 36755 38269 38297 38395 38434 38460 38513 38600 38668 38940
# [153] 39031 39045 39048 39176 39250 39297 39344 39454 39483 39634 39768 39967 40158 40355 40443 40502 40687 40776 40912
# [172] 40943 41183 41473 41656 41798 41944 42041 42149 42484 42797 43478 43494 44274 45005 45143 45333 45434 45474 45617
# [191] 45719 45777 46013 46327 46393 46987 47089 47165 48490 48632 48727 48793 48802 49044 49097 49334 49380 49464 49623
# [210] 49640 49771 49868 49977 50248 50354 50589 50660 51210 51222 51235 51293 51394 51402 51436 52529 52792 53030 53177
# [229] 53903 54007 54153 54180 54277 54486 54861 54950 55010 55141 55185 55369 55430 55612 55719 55997 56044 56313 56460
# [248] 56665 56936 57052 57317 57379 57992 58008 58033 66369 66415 66501 66746 66899 67327 67572 67863 68187 68536 68810
# [267] 69188 69864 69880

allsites[tweaks, ]
# A tibble: 269 x 8
#   datetime            pest_nmbr pest_dif reviewed event             site         Yr Julian
#   <dttm>                  <dbl>    <dbl> <chr>    <chr>             <chr>     <dbl>  <dbl>
# 1 2019-04-30 03:58:01         0        0 No       Sticky roll tweak UCKearney  2019    120
# 2 2019-05-16 09:56:01         0        0 No       Sticky roll tweak UCKearney  2019    136
# 3 2019-05-31 06:57:01         0        0 No       Sticky roll tweak UCKearney  2019    151
  # Shows we can use this vector of row indexes to retrieve the rows

# find adjacent rows
tweaks_prev <- tweaks - 1
tweaks_prev

tweaks_nex <- tweaks + 1
tweaks_nex

tweaks_all <- sort(c(tweaks_prev,tweaks,tweaks_nex))

selected <- allsites[tweaks_all, ]
selected

selected2 <- selected %>% 
  filter(pest_dif != 0)

selected2[selected2$pest_dif == lag(selected2$pest_dif, n = 1), ]
# A tibble: 12 x 8
#   datetime            pest_nmbr pest_dif reviewed event site           Yr Julian
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>       <dbl>  <dbl>
# 1 NA                         NA       NA NA       NA    NA             NA     NA
# 2 2019-08-09 13:59:00         7        7 Yes      NA    MWoolf_east  2019    221
# 3 2019-08-13 14:40:00        33       33 Yes      NA    MWoolf_east  2019    225
# 4 2019-04-30 03:58:00         1        1 Yes      NA    usda         2019    120
# 5 2019-08-18 23:28:00         7        1 Yes      NA    usda         2019    230
# 6 2020-06-18 05:58:00        14        1 Yes      NA    mikewoolf1   2020    170
# 7 2020-06-22 13:59:00        24        6 Yes      NA    mikewoolf1   2020    174
# 8 2020-09-21 05:57:00         1        1 Yes      NA    mikewoolf1   2020    265
# 9 2020-08-10 10:58:00        23        1 Yes      NA    mikewoolf2   2020    223
# 10 2020-09-21 04:58:00         9        9 Yes      NA    mikewoolf2   2020    265
# 11 2020-07-17 10:59:00        13        1 Yes      NA    mikewoolf3   2020    199
# 12 2020-09-21 05:26:00         5        1 Yes      NA    mikewoolf5   2020    265
   # does not appear to capture all events of concern

allsites %>% 
  filter(pest_nmbr == 85 & pest_dif == 85 & lag(event == "Sticky roll tweak", n = 1))

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
