#===========================================================================#
# script13_low_temp_and_capture.R
# 
# Examine the hypothesis that, above a certain nightly low temperature,
# the capture per night drops off
#
#===========================================================================#

library(tidyverse)
library(lubridate)

# Need to examine data by night using the counts and temperature data merged
# in script10, but need to re-define night as done with Julian2 and Hour2
# in script12

# Get counts_y19y20 using script10 (stop at line62)
rm(allsites19,allsites20)
head(counts_y19y20,2)
#              datetime pest_dif      site   Yr Mnth Julian Hr
# 1 2019-04-26 13:56:00        7 UCKearney 2019  Apr    116 13
# 2 2019-05-02 01:57:00        1 UCKearney 2019  May    122  1

# Get temps  using script10
temps
# A tibble: 36,549 x 6
#   site      datetime            degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7
# 3 UCKearney 2019-05-16 19:00:00     55.0    53.8    56.8   90.4

# Get Yr, Julian, Hr, Julian2, and Hr2 in temps data set
temps <- temps %>% 
  mutate(Yr = year(datetime),
         Mnth = month(datetime, label = TRUE, abbr = TRUE),
         Julian = yday(datetime),
         Hr = hour(datetime))

temps <- temps %>% 
  mutate(Julian2 = ifelse(Hr < 18, Julian - 1, Julian),
         hr2 = ifelse(Hr < 18, Hr + 6, Hr - 18))
  # different from script12 because all hours included

# What time of day had the most low temperatures?
lo_temp <- temps %>% 
  group_by(Yr,site,Mnth,Julian2) %>% 
  summarise(min_temp = min(degf_lo))

lo_temp2 <- left_join(lo_temp,temps) %>% 
  filter(min_temp == degf_lo)
lo_temp2

ggplot(lo_temp2) +
  geom_col(aes(x = hr2, y = ))
