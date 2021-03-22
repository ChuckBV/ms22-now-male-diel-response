#============================================================================
# script2_y19_temperature_data.R
#
# Summarize temperature data from the 5 stations as preparation to 
# association of count and temperature data
#
# PARTS
#  1. Read in 2019 data (line 19)
#  2. Plots of nightly temperature and humidity at 3AM (line 33)
#  3. Load and merge count data to search for daily corr temp & rh (line 110)
#    - No pattern to be found at day-to-day level
#  4. 
#============================================================================

library(tidyverse) # Preferred dialect of R
library(lubridate) # Work with Dates
library(timechange)

#-- 1. Read in 2019 data --------------------------------------------------

alltemps19 <- read_csv("./data/trapview_temps_degf_y19.csv")
alltemps19
# A tibble: 18,969 x 6
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7
# 3 UCKearney 2019-05-16 19:00:00     55.0    53.8    56.8   90.4

### Set local time zone
localtz <- "America/Los_Angeles"
alltemps19$Date_time <- time_force_tz(alltemps19$Date_time, tz = localtz)

#-- 2. Plots of nightly temperature and humidity at 3AM  -------------------

### Subset to a single daily observation

### Daily climate at 3AM
three_am_daily19 <- alltemps19 %>% 
  filter(hour(Date_time) == 3)
three_am_daily19
# A tibble: 792 x 6
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-17 03:00:00     52.3    51.6    52.9   90.5
# 2 UCKearney 2019-05-18 03:00:00     49.1    48.6    49.5   97.6

### Temperature at 3AM
p1 <- ggplot(three_am_daily19, aes(x = Date_time, y = degf_avg)) +
  geom_line() + 
  facet_grid(site ~ .) +
  theme_bw() + 
  xlab("") +
  ylab("degrees F") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))


p1

ggsave(filename = "Y19_trapview_nightly_degf_yrlong.jpg", p1, path = "./results",
       width = 5.83, height = 5.83, dpi = 300, units = "in", device='jpg')


### relative humidity at 3AM
p2 <- ggplot(three_am_daily19, aes(x = Date_time, y = rh_avg)) +
  geom_line() + 
  facet_grid(site ~ .) +
  theme_bw() + 
  xlab("") +
  ylab("degrees F") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p2

ggsave(filename = "Y19_trapview_nightly_rh_yrlong.jpg", p1, path = "./results",
       width = 5.83, height = 5.83, dpi = 300, units = "in", device='jpg')

### Get Date value
three_am_daily19 <- three_am_daily19 %>% 
  mutate(caldate = as.Date(Date_time))

head(three_am_daily19,2)
# A tibble: 2 x 7
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg caldate   
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl> <date>    
# 1 UCKearney 2019-05-17 03:00:00     52.3    51.6    52.9   90.5 2019-05-17
# 2 UCKearney 2019-05-18 03:00:00     49.1    48.6    49.5   97.6 2019-05-18

tail(three_am_daily19,2)
# A tibble: 2 x 7
#   site        Date_time           degf_avg degf_lo degf_hi rh_avg caldate   
#   <chr>       <dttm>                 <dbl>   <dbl>   <dbl>  <dbl> <date>    
# 1 MWoolf_east 2019-10-30 03:00:00     44.6    42.8    47.8   48.6 2019-10-30
# 2 MWoolf_east 2019-10-31 03:00:00     37.6    36.1    39.6   45.0 2019-10-31

### Start with July 15
three_am_daily19 <- three_am_daily19 %>% 
  filter(caldate >= as.Date("2019-06-15"))


#-- 3. Load and merge count data to search for daily corr temp & rh ---------

### Load Counts data 
allsites <- read_csv("./data/allsites_y19.csv")
allsites$datetime <- time_force_tz(allsites$datetime, tz = localtz)
allsites$caldate <- as.Date(allsites$datetime) # Date-only variable

allsites <- allsites %>% 
  group_by(site,caldate) %>% 
  summarise(NOW = sum(pest_dif, na.rm = TRUE)) %>% 
  filter(NOW > 0)

allsites <- allsites %>% 
  filter(caldate >= as.Date("2019-06-15"))

both <- left_join(allsites,three_am_daily19)

### Capture vs temperature at 3AM by site
ggplot(data = both, aes(x = degf_avg, y = NOW)) +
  geom_point() +
  facet_grid(site ~ .)
    # The data suggest poor correlation
    # Confirmed with cor() procedure (see below)

### Capture vs rh at 3AM by site
ggplot(data = both, aes(x = rh_avg, y = NOW)) +
  geom_point() +
  facet_grid(site ~ .)

### Examine temperature data, Woolf sites only, above 50F only
mw_east <- both %>% 
  filter(site == "MWoolf_east" & degf_avg > 50)
mw_east

cor(mw_east$degf_avg, mw_east$NOW, method = "pearson")
# [1] 0.08412898
  # No there there



