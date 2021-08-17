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
# combined3
#  - Night only
#  - Only first obs of night for that site
#===========================================================================#

library(tidyverse)
library(lubridate)
library(timechange)

#-- 1. (Heading to be determined) --------

combined <- read_csv("./data/merged_count_and_temp_data.csv")
combined
# A tibble: 1,428 x 11
#   pest_dif site         Yr Mnth.x Julian    Hr degf_avg degf_lo degf_hi rh_avg Mnth.y
#      <dbl> <chr>     <dbl> <chr>   <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <chr> 
# 1        1 UCKearney  2019 Jun       153     5     60.2    59.7    61.2   89.8 Jun   
# 2        1 UCKearney  2019 Jul       191     4     61.2    60.3    62.2   83.6 Jul   
# 3        1 UCKearney  2019 Jul       194     4     64.2    63.9    65.1   93.2 Jul 

################################
### Reduce to night observations

combined3 <- combined %>% 
  filter(Hr <= 7 | Hr >= 18) %>% 
  group_by(Yr,site,Mnth.x,Julian) %>% 
  summarise(hr_obs1 = min(Hr),
            degf = mean(degf_avg))

combined3
# A tibble: 556 x 6
# Groups:   Yr, site, Mnth.x [52]
# Yr site       Mnth.x Julian hr_obs1  degf
#    <dbl> <chr>      <chr>   <dbl>   <dbl> <dbl>
# 1  2019 mikewoolf1 Aug       213       5  59.2
# 2  2019 mikewoolf1 Aug       215       5  62.7

ggplot(combined3, aes(x = Julian, y = degf)) +
  geom_point()

ggplot(combined3, aes(x = Julian, y = hr_obs1)) +
  geom_point() 

ggplot(combined3, aes(x = degf, y = hr_obs1)) +
  geom_point() 
