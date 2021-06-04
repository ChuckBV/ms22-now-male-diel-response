#===========================================================================#
# script1_temperature_variation.R
#
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

  + coord_cartesian(ylim = c(52, 66))


ggplot(alltemps19, aes(x = Date_time, y = degf_avg)) +
  geom_line() +
  facet_grid(site ~ .) 

# May is in 2 of the 6 data sets, nothing earlier

# Examine by month in order to contrast and to expand the x-scale. In order
# to do that, get month and day of month as variables

alltemps19
# A tibble: 18,969 x 6
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7

alltemps19 %>% 
  mutate(mnth = month(Date_time, label = TRUE, abbr = TRUE),
         day_of_mnth = mday(Date_time)) %>% 
  group_by(site, mnth, day_of_mnth) %>% 
  summarise(mintemp = min(degf_lo)) %>% 
  ggplot(., aes(x = day_of_mnth, y = mintemp, color = site)) +
    geom_line() +
    facet_grid(mnth ~ .) + #, scales = "free_y")
    ylim(52,66)

# Nightly low was in this range in almost all months (lower in October?)
# Need to look at diurnal variation, but these data suggest no great differences
# from June to September