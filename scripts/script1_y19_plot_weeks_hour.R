#============================================================================
# script1_y19_plot_weeks_hour.R
#
# Explore 2019 field season data at half hour intervals from five TrapView
# stations
# PARTS
#  1. Read in data and recode for useful time values (line 17)
#       Not dealt with in useful manner until later script
#  2. Summarize data with tables and graphs (line 55)
#       Create daily profiles by site
#
#============================================================================

library(tidyverse) # Preferred dialect of R
library(lubridate) # Work with Dates
library(timechange)

#-- 1. Read in data and recode for useful time values -----------------------

### Retrieve the 2019 data set
allsites <- read_csv("./data/allsites_y19.csv")
allsites
# A tibble: 38,202 x 6
#   datetime            pest_nmbr pest_dif reviewed event site     
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>    
# 1 2019-04-26 13:56:00         7        7 Yes      NA    UCKearney
# 2 2019-04-26 14:56:00         7        0 Yes      NA    UCKearney

### NB See 2020 script. We have to tell R that these time values are 
### California local time and not UTC

### Store local time zone in memory for subsequent steps
localtz <- "America/Los_Angeles"

### Get Date and Minutes from midnight
attr(allsites$datetime, "tzone")
# [1] "UTC"
# Even though the data from Trapview show correct times for PDT 
# (aka. "America/Los_Angeles"), the timezone attribute shows UTC. 
# If we don't use a function to change this it will mess up datetime math
allsites$datetime <- time_force_tz(allsites$datetime, tz = localtz)
attr(allsites$datetime, "tzone")
# attr(allsites$datetime, "tzone")
# See also algorith in  test_timechange.R for setting 24:00 to local sundown

allsites$caldate <- as.Date(allsites$datetime) # Date-only variable

allsites <- allsites %>%  # Midnight for that date
  mutate(midnite = make_datetime(year(datetime),
                                 month(datetime),
                                 day(datetime),0,0,0,localtz))

# Find minutes from midnight
allsites$elapsed_min <- as.numeric(allsites$datetime - allsites$midnite)/60

# Use integer division to bin by hour
allsites$hr <- allsites$elapsed_min%/%60

# Get Julian date and epiweek (Day as 1-365 and week as 1-52)
allsites$julian <- yday(allsites$caldate)
allsites$wk <- epiweek(allsites$caldate)

# variables as rows, shows data types
glimpse(allsites)

# Get site as factor
allsites$site <- factor(allsites$site, levels = unique(allsites$site))

#-- 2. Summarize data with tables and graphs  -------------------------------

### Summarize counts per week as a table
Obs_by_date <- allsites %>% 
  group_by(wk,caldate,site) %>% 
  summarize(nObs = sum(pest_dif)) %>% 
  spread(key = site, value = nObs)

#write.csv(Obs_by_date,"./data/table_counts_wk_by_site.csv")

### Creat a plot of counts by week
wkly <- allsites %>% 
  group_by(wk,caldate,site) %>% 
  summarize(Date = min(caldate),
            orangeworm = sum(pest_dif))
wkly2 <- wkly %>% 
  group_by(wk,site) %>% 
  summarize(Date = min(Date),
            orangeworm = sum(orangeworm))

plot_wkly_by_site <- ggplot(wkly2, aes(x = Date,y = orangeworm)) +
  geom_col(width = 10) +
  facet_grid(site ~ .) +
# any width over 3.0 gives a warning about non-overlapping x intervals
# I think it looks better with width = 5.0
  theme_bw() +
  xlab("") +
  ylab("NOW per night") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

plot_wkly_by_site

ggsave(filename = "Y19_trapview_wkly_by_site.jpg", plot_wkly_by_site, path = "./output",
       width = 5.83, height = 5.83, dpi = 300, units = "in", device='jpg')


### Mess--see how handled for 2020
# hrly <- allsites %>%
#   group_by(wk,site,hr) %>%
#   summarise(Count = sum(pest_dif, na.rm = TRUE))
# 
# hrly_by_week <- ggplot(hrly, aes(x = hr, y = Count)) +
#   geom_col() +
#   scale_y_continuous(breaks = c(0,80)) +
#   facet_grid(wk ~ site) +
#   theme_bw() +
#   xlab("Hour of day") +
#   ylab("NOW captured") +
#   theme(axis.text.x = element_text(color = "black", size = 10),
#         axis.text.y = element_text(color = "black", size = 8),
#         axis.title.x = element_text(color = "black", size = 10),
#         axis.title.y = element_text(color = "black", size = 10),
#         legend.title = element_text(color = "black", size = 14),
#         legend.text = element_text(color = "black", size = 14))
# 
# hrly_by_week
# 
# ggsave(filename = "Y19_hrly_by_week.jpg", hrly_by_week, path = "./output",
#        width = 7.5, height = 7.5, dpi = 300, units = "in", device='jpg')


