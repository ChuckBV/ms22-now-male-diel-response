#===========================================================================#
# script0_head_data_files.R
#
# Show that file structures are equivalent
# 1. Load 2019 and 2020 data sets
# 2. Observations, time period and time interval
#
#===========================================================================#

library(tidyverse)

# To do: clean up graphs

##### LOAD SEASON-LONG DATA SETS ############################################

# Load season-long count data sets for five traps in 2019 and 2020

allsites19 <- readr::read_csv("./allsites_y19.csv")
allsites20 <- readr::read_csv("./allsites_y20.csv")

# Show size and top lines of the count data tibbles

allsites19
# A tibble: 38,202 x 6
#   datetime            pest_nmbr pest_dif reviewed event site     
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>    
# 1 2019-04-26 13:56:00         7        7 Yes      NA    UCKearney
# 2 2019-04-26 14:56:00         7        0 Yes      NA    UCKearney

allsites20
# A tibble: 31,726 x 6
#   datetime            pest_nmbr pest_dif reviewed event site      
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>     
# 1 2020-04-22 03:18:00         5        4 Yes      NA    mikewoolf1
# 2 2020-04-22 05:02:00         7        2 Yes      NA    mikewoolf1

# Load temperature data for each trap in 2019 and 2020

<<<<<<< HEAD
alltemps19 <- readr::read_csv("./trapview_temps_degf_y19.csv")
alltemps20 <- readr::read_csv("./trapview_temps_degf_y20.csv")
=======
alltemps19 <- read_csv("./trapview_temps_degf_y19.csv")
alltemps20 <- read_csv("./trapview_temps_degf_y20.csv")
>>>>>>> f946b22999152ebb05c6cd3e7d74f3b557a9f2e5

# Show size and top lines of the temperature data tibbles

alltemps19
# A tibble: 18,969 x 6
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7
# 3 UCKearney 2019-05-16 19:00:00     55.0    53.8    56.8   90.4

alltemps20
# A tibble: 17,580 x 6
#   Date_time           degf_avg degf_lo degf_hi rh_avg site 
#   <dttm>                 <dbl>   <dbl>   <dbl>  <dbl> <chr>
# 1 2020-04-22 02:00:00     51.5    50.2    52.5   90.3 MWT1 
# 2 2020-04-22 03:00:00     49.5    48.9    49.8   93.8 MWT1 
# 3 2020-04-22 04:00:00     49.7    48.9    50.4   94.2 MWT1 


##### FURTHER CHARACTERIZE SEASON-LONG COUNT DATA SETS ########################

# Determine date range and number of observations for each of the 2019 traps

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

write.csv(ovrvw_events19,
          "./results/ovrvw_events19.csv",
          row.names = FALSE)

# Determine date range and number of observations for each of the 2020 traps

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

# Determine interval between events in the count data. Note that sometimes the
# traps captured photos every 30 minutes and sometimes (early season) it was
# every 60 minutes. Start with 2019 data set

# Examine the "event" variable. Is it mostly NA? How important is it?

length(allsites19$event[!is.na(allsites19$event)])

# 147 of ~38,000 obs have a value other than NA 

unique(allsites19$event) 
# [1] NA                       "Sticky roll tweak"      "Replace of sticky roll"

allsites19 <- allsites19[ ,-5] 
allsites19 <- allsites19[complete.cases(allsites19), ] 
allsites19

# Create a date variable (as opposed to datetime) to group obs by days

x19 <- allsites19 %>% 
  mutate(caldate = as.Date(datetime)) %>% # group into days
  group_by(site, caldate) %>% 
  summarise(nObs = n())
x19
# A tibble: 852 x 3
# Groups:   site [5]
# site        caldate     nObs
#   <chr>       <date>     <int>
# 1 MWoolf_east 2019-06-04     7
# 2 MWoolf_east 2019-06-05    23
# 3 MWoolf_east 2019-06-06    24

# Plot readings per day by site

p1 <- ggplot(x19, aes(x = caldate, y = nObs)) +
  geom_line() + 
  facet_grid(site ~ .)

p1

ggsave(filename = "trapview_events_pr_hr_2019.jpg", 
       plot = p1, 
       device = "jpg", 
       path = "./results", 
       dpi = 300, width = 5.83, height = 5.83, units = "in")


### Continuing with 2020 data

allsites20 <- allsites20[ ,-5] 
allsites20 <- allsites20[complete.cases(allsites20), ]
allsites20

# Create temporary data frame containing readings per day

x20 <- allsites20 %>% 
  mutate(caldate = as.Date(datetime)) %>% # group into days
  group_by(site, caldate) %>% 
  summarise(nObs = n())

# Plot readings per day by site

p2 <- ggplot(x20, aes(x = caldate, y = nObs)) +
  geom_line() + 
  facet_grid(site ~ .)

p2

ggsave(filename = "trapview_events_pr_hr_2019.jpg", 
       plot = p2, 
       device = "jpg", 
       path = "./results", 
       dpi = 300, width = 5.83, height = 5.83, units = "in")


## Filter for nObs > 42 for when needed

# First 2019

x19  # review
# # A tibble: 852 x 3
# # Groups:   site [5]
# site        caldate     nObs
#   <chr>       <date>     <int>
# 1 MWoolf_east 2019-06-04     7
# 2 MWoolf_east 2019-06-05    23
# 3 MWoolf_east 2019-06-06    24

thirty_min_ints19 <- x19 %>%   # filter 
  filter(nObs > 42)
write.csv(thirty_min_ints19,   # save
          "./results/thirty_min_ints19.csv",
          row.names = FALSE)

# Test for days with gaps after intervals set to 30 minutes
# Mike Woolf East had many such days in Aug and Sep 2019, need to determine
# what happened.

x <- x19  %>% 
  filter(nObs <= 42 & caldate >= as.Date("2019-06-15"))
 
# Repeat for 2020

x20  # review
# A tibble: 729 x 3
# Groups:   site [5]
# site       caldate     nObs
#   <chr>      <date>     <int>
# 1 mikewoolf1 2020-04-22    27
# 2 mikewoolf1 2020-04-23    38
# 3 mikewoolf1 2020-04-24    32

thirty_min_ints20 <- x20 %>%   # filter 
  filter(nObs > 42)
write.csv(thirty_min_ints20,   # save
          "./results/thirty_min_ints20.csv",
          row.names = FALSE)

# Test for days with gaps after intervals set to 30 minutes
# Examine nObs vs days more closely.

##### OVERVIEW OF TEMPERATRE DATA SETS ########################

# Re-plot for daily highs and lows rather than hourly temperatures
# The period previously identified as of interest was 19 to 11C, or 
# 66 to 54F. 

# Overview 2019 data
p3 <- ggplot(alltemps19, aes(x = Date_time, y = degf_avg)) +
  geom_line() +
  facet_grid(site ~ .)

p3

ggsave(filename = "seaon_long_temperature_2019.jpg", 
       plot = p3, 
       device = "jpg", 
       path = "./results", 
       dpi = 300, width = 5.83, height = 5.83, units = "in")

  # Temperature data itself covers a broad part of June-Nov for 2019

# Overview 2020 data--identify and remove breaks
p4 <- ggplot(alltemps20, aes(x = Date_time, y = degf_avg)) +
  geom_line() +
  facet_grid(site ~ .)

p4

ggsave(filename = "seaon_long_temperature_2020.jpg", 
       plot = p4, 
       device = "jpg", 
       path = "./results", 
       dpi = 300, width = 5.83, height = 5.83, units = "in")

  # Temperature data for 2020 stop short of October


# To do--compare temperature data with periods for which there are photos at
# 30 minute intervals

## Next item--look at daily variation
#p5 <- 
ggplot(alltemps19, aes(x = Date_time, y = degf_avg)) +
  geom_line() +
  facet_grid(site ~ .) +
  scale_x_datetime(
    breaks = "1 hour",
    limits = c(as.POSIXct("2019-06-01 00:00:00"),as.POSIXct("2019-06-01 23:59:59"))
  ) +
  theme(axis.text.x = element_text(color = "black", size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))


p5