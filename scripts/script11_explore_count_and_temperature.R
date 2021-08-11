#===========================================================================#
# script11_explore_count_and_temperature.R
# 
# Uses the "combined" data set created in script10 to explore relationsips 
#
# PARTS
# 1. Basic questions about combined data set (multiple obs/night?)(line 14)  
# 2.  
#
#===========================================================================#

library(tidyverse)
library(lubridate)

#-- 1. Basic questions about combined data set (multiple obs/night?) --------

combined <- read_csv("./data/merged_count_and_temp_data.csv")
combined
# A tibble: 1,428 x 11
#   pest_dif site         Yr Mnth.x Julian    Hr degf_avg degf_lo degf_hi rh_avg Mnth.y
#      <dbl> <chr>     <dbl> <chr>   <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <chr> 
# 1        1 UCKearney  2019 Jun       153     5     60.2    59.7    61.2   89.8 Jun   
# 2        1 UCKearney  2019 Jul       191     4     61.2    60.3    62.2   83.6 Jul   
# 3        1 UCKearney  2019 Jul       194     4     64.2    63.9    65.1   93.2 Jul 

length(combined$pest_dif)
# [1] 1428
length(combined$pest_dif[is.na(combined$pest_dif)])
# [1] 0

#-- First confirms r object and syntax. Second confirms that NAs have already
#-- been filtered out, and can be assumed not to exist for subsequent code

### How many cases of multiple entries per day?
combined %>%  
  group_by(site,Yr,Julian) %>% 
  summarise(moths = sum(pest_dif),
            nObs = n())
# A tibble: 577 x 5
# Groups:   site, Yr [10]
#   site          Yr Julian moths  nObs
#   <chr>      <dbl>  <dbl> <dbl> <int>
# 1 mikewoolf1  2019    142     1     1
# 2 mikewoolf1  2019    143     3     2
# 3 mikewoolf1  2019    144     2     1

combined %>% 
  group_by(site,Yr,Julian) %>% 
  summarise(moths = sum(pest_dif),
            nObs = n()) %>% 
  filter(nObs > 1) 

# A tibble: 331 x 5
# Groups:   site, Yr [10]
#   site          Yr Julian moths  nObs
#   <chr>      <dbl>  <dbl> <dbl> <int>
# 1 mikewoolf1  2019    143     3     2
# 2 mikewoolf1  2019    145    60     3
# 3 mikewoolf1  2019    148    66     4

#-- Multiple obs per night in ca. 60% of cases, even though
#-- data are in hour increments

#-- 2. Exam---------