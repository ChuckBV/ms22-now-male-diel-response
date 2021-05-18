#===========================================================================#
# script0c_head_data_file_take2.R
#
# Show that file structures are equivalent
# 1. Load 2019 and 2020 data sets
# 2. Observations, time period and time interval
#
#===========================================================================#

library(tidyverse)

#-- 1. Load 2019 and 2020 data sets ------------------------------------------

allsites19 <- read_csv("./data/allsites_y19.csv")
allsites19$cal_date <-as.Date(allsites19$datetime) 
allsites19

# A tibble: 38,202 x 6
# datetime            pest_nmbr pest_dif reviewed event site     
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>    
# 1 2019-04-26 13:56:00         7        7 Yes      NA    UCKearney
# 2 2019-04-26 14:56:00         7        0 Yes      NA    UCKearney

allsites20 <- read_csv("./data/allsites_y20.csv")
allsites20$cal_date <-as.Date(allsites20$datetime) 
allsites20


allsites20
# A tibble: 31,726 x 6
#   datetime            pest_nmbr pest_dif reviewed event site      
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>     
# 1 2020-04-22 03:18:00         5        4 Yes      NA    mikewoolf1
# 2 2020-04-22 05:02:00         7        2 Yes      NA    mikewoolf1


alltemps19 <- read_csv("./data/trapview_temps_degf_y19.csv")
alltemps19
# A tibble: 18,969 x 6
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7
# 3 UCKearney 2019-05-16 19:00:00     55.0    53.8    56.8   90.4

alltemps20 <- read_csv("./data/trapview_temps_degf_y20.csv")
alltemps20
# A tibble: 17,580 x 6
#   Date_time           degf_avg degf_lo degf_hi rh_avg site 
#   <dttm>                 <dbl>   <dbl>   <dbl>  <dbl> <chr>
# 1 2020-04-22 02:00:00     51.5    50.2    52.5   90.3 MWT1 
# 2 2020-04-22 03:00:00     49.5    48.9    49.8   93.8 MWT1 
# 3 2020-04-22 04:00:00     49.7    48.9    50.4   94.2 MWT1 

#-- 2. Observations, time period and time interval  ---------------------------

### Get overall date range for both years

ovrvw_events19 <- allsites19 %>%
  group_by(site) %>% 
  summarise(nObs = sum(!is.na(pest_dif)),
            min_date = as.Date(min(datetime)),
            max_date = as.Date(max(datetime)))
ovrvw_events19
# A tibble: 5 x 4
#   site         nObs min_date   max_date  
#   <chr>       <int> <date>     <date>    
# 1 MWoolf_east  6274 2019-06-04 2019-10-31
# 2 MWoolf_west  7525 2019-05-22 2019-10-31
# 3 Perez        7099 2019-06-03 2019-11-04
# 4 UCKearney    8244 2019-04-26 2019-11-04
# 5 usda         9056 2019-04-24 2019-10-31

ovrvw_events20 <- allsites20 %>% 
  group_by(site) %>% 
  summarise(nObs = sum(!is.na(pest_dif)),
            min_date = as.Date(min(datetime)),
            max_date = as.Date(max(datetime)))
ovrvw_events20
# A tibble: 5 x 4
# site        nObs min_date   max_date  
#   <chr>      <int> <date>     <date>    
# 1 mikewoolf1  5337 2020-04-22 2020-09-22
# 2 mikewoolf2  7736 2020-03-12 2020-09-22
# 3 mikewoolf3  6782 2020-04-21 2020-09-22
# 4 mikewoolf4  8215 2020-03-06 2020-09-22
# 5 mikewoolf5  3656 2020-07-07 2020-09-22

### Determine photos per hour/photos per day/interval between photos

photos_pr_day19 <- allsites19 %>%
  group_by(site,cal_date) %>% 
  summarise(nObs = sum(!is.na(pest_dif)))
photos_pr_day19
# A tibble: 852 x 3
# Groups:   site [5]
# site        cal_date    nObs
#   <chr>       <date>     <int>
# 1 MWoolf_east 2019-06-04     7
# 2 MWoolf_east 2019-06-05    23

hist(photos_pr_day19$nObs)
  # Shows that 24 and 48 are strongly modal

photos_pr_day19 %>% 
  group_by(nObs) %>% 
  summarise(events = n()) %>% 
  filter(events > 20)
# A tibble: 6 x 2
# nObs events
#   <int>  <int>
# 1    24     34
# 2    44     21
# 3    46     30
# 4    47    134
# 5    48    414
# 6    49     90
     
# To examine timing, select days with 47-49 camera events. 
photos_30_min_yr19 <- photos_pr_day19[photos_pr_day19$nObs >= 47 & photos_pr_day19$nObs <= 49, ]
photos_30_min_yr19
# A tibble: 638 x 3
# Groups:   site [5]
# site        cal_date    nObs
#   <chr>       <date>     <int>
# 1 MWoolf_east 2019-06-15    47
# 2 MWoolf_east 2019-06-16    47

### Doing a left_join merger on photos_30_min_yr19[,1:2] gives 2019 obs
### with photos at 30 minute intervals (as illustrated below)

left_join(photos_30_min_yr19,allsites19)
# Joining, by = c("site", "cal_date")
# # A tibble: 30,584 x 8
# # Groups:   site [5]
# site        cal_date    nObs datetime            pest_nmbr pest_dif reviewed event
# <chr>       <date>     <int> <dttm>                  <dbl>    <dbl> <chr>    <chr>
# 1 MWoolf_east 2019-06-15    47 2019-06-15 00:28:00         0        0 Yes      NA   
# 2 MWoolf_east 2019-06-15    47 2019-06-15 00:58:00         0        0 Yes      NA   
# 3 MWoolf_east 2019-06-15    47 2019-06-15 01:28:00         0        0 Yes      NA   