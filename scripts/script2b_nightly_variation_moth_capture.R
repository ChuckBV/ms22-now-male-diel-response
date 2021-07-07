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
# Examine when and where daytime fliers occur (defined and 6AM to 6PM)
#-------------------------------------------------------------------------#


#--------------------------------------------------------------------------
# Determine when and where early night fliers occur (>6PM and <12AM)
#-------------------------------------------------------------------------#

