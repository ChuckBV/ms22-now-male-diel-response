#===========================================================================#
# script12_explore_count_and_temperature2.R
# 
# Uses the "combined" data set created in script10 to explore relationsips 
#
# PARTS
# 1. Basic questions about combined data set (multiple obs/night?)(line 14)  
# 2. Examine captures after 7AM (before midnight) (line 64)  
#
# combined--imported as created in script10
# combined2--adds categorical variable for off hours
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

