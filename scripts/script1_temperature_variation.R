#===========================================================================#
# script1_temperature_variation.R
# 
# Examine variation in nightly temperatures, and determine the number of
# observations in a range relevant to shiftying mating activity earler
#
#===========================================================================#


library(tidyverse)
library(lubridate)

## Variation in 2019 data
alltemps19 <- readr::read_csv("./trapview_temps_degf_y19.csv")

ggplot(alltemps19, aes(x = Date_time, y = degf_avg)) +
  geom_line() +
  facet_grid(site ~ .)

# May is in 2 of the 6 data sets, nothing earlier

# May is in 2 of the 6 data sets, nothing earlier

# Examine by month in order to contrast and to expand the x-scale. In order
# to do that, get month and day of month as variables

alltemps19
# A tibble: 18,969 x 6
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7

nightly_19 <- alltemps19 %>% # 18969 to 798
  mutate(mnth = month(Date_time, label = TRUE, abbr = TRUE),
         day_of_mnth = mday(Date_time)) %>% 
  group_by(site, mnth, day_of_mnth) %>% 
  summarise(degf_lo = min(degf_lo)) %>% 
  mutate(coolnt = ifelse(degf_lo <= 52,1,0))

monthly19 <- nightly_19 %>% 
  group_by(site,mnth) %>% 
  summarise(coolnt = sum(coolnt),
            nObs = n(),
            pct_cool = coolnt/nObs)

monthly19 
 # Over 70% of nights reached <= 66F as measured by TrapView, but before October
 # few nights were < 52F

# Nightly low was in this range in almost all months (lower in October?)
# Need to look at diurnal variation, but these data suggest no great differences
# from June to September

alltemps19$hr = hour(alltemps19$Date_time)
alltemps19$mnth = month(alltemps19$Date_time, label = TRUE, abbr = TRUE)
alltemps19

ggplot(alltemps19, aes(x = hr, y = degf_lo)) +
  geom_point() + 
  facet_grid(mnth ~ .) +
  xlim(0,8) + 
  ylim(40,70)
# Shows a range of temperatures, many in the region in which NOW shifts calling 
# earlier.


