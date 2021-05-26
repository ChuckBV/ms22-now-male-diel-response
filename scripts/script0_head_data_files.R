#===========================================================================#
# script0_head_data_files.R
#
# Show that file structures are equivalent
# 1. Load 2019 and 2020 data sets
# 2. Observations, time period and time interval
#
#===========================================================================#

library(tidyverse)

##### LOAD SEASON-LONG DATA SETS ############################################

# Load season-long count data sets for five traps in 2019 and 2020

allsites19 <- readr::read_csv("./data/allsites_y19.csv")
allsites20 <- readr::read_csv("./data/allsites_y20.csv")

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

alltemps19 <- read_csv("./data/trapview_temps_degf_y19.csv")
alltemps20 <- read_csv("./data/trapview_temps_degf_y20.csv")

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


##### FURTHER CHARACTERIZE SEASON-LONG DATA SETS ############################

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

# drop event which is 95% NA and not essential. Use Base R to drop, then select
# only complete records

allsites19 <- allsites19[ ,-5] 
allsites19 <- allsites19[complete.cases(allsites19), ] 

# retains 95% of obs. Now making site into a factor to use it with the split function
# Looks like x has to be a factor for this to work

allsites19$site <- factor(allsites19$site, levels = unique(allsites19$site))

allsites19_L <- split(allsites19, f = allsites19$site)
allsites19_L[[1]][1:6,] 
allsites19_L[[2]][1:6,] 
allsites19_L[[3]][1:6,] 
allsites19_L[[4]][1:6,] 
allsites19_L[[5]][1:6,] 

# split made this into a list containing 5 data frames. Notation above
# is a klunky way of heading each. Subset back out entire data sets

UCKearney  <- allsites19_L[[1]]
MWoolf_east  <- allsites19_L[[2]]
MWoolf_west  <- allsites19_L[[3]]
Perez  <- allsites19_L[[4]]
usda <- allsites19_L[[5]]

# Pull all times out of UCKearney and place in a vector to examine getting time 
# differences. First verify length and show the first 10 values of this vector.
# Note that times are Pacific Daylight, but R pressumes universal time (UTC)

dtimes1 <- UCKearney$datetime  # 8244 values
length(dtimes1[!is.na(dtimes1)]) 
dtimes1[1:10]
# [1] "2019-04-26 13:56:00 UTC" "2019-04-26 14:56:00 UTC" "2019-04-26 15:57:00 UTC" "2019-04-26 16:56:00 UTC"
# [5] "2019-04-26 17:57:00 UTC" "2019-04-26 18:57:00 UTC" "2019-04-26 19:58:00 UTC" "2019-04-26 20:58:00 UTC"
# [9] "2019-04-26 21:58:00 UTC" "2019-04-26 22:58:00 UTC"

# One approach it to get the interval between the times in "dtimes1".
# Use diff() with units()<- to control unit

b <- diff(dtimes1)
units(b) <- "mins"
length(b) 

# 8243 obs, 1 less than dtimes. Show first 10 values

b[1:10]
# Time differences in mins
# [1] 60 61 59 61 60 61 60 60 60 60

# Show unique values

unique(b)


### Try using diff within the dataframe
MWoolf_west$dif_time[2:nrow(MWoolf_west)] <- diff(MWoolf_west$datetime)
MWoolf_west$dif_time <- MWoolf_west$dif_time/60 
    # manually convert from seconds to minutes

### What time intervals are frequent?
MWoolf_west %>% 
  group_by(dif_time) %>% 
  summarise(nObs = n()) %>% 
  filter(nObs > 10)
# A tibble: 9 x 2
# dif_time  nObs
#     <dbl> <int>
# 1   0.0167    49
# 2  25        118
# 3  29         30
# 4  30.0       41
# 5  30       6261
# 6  31        749
# 7  55         36
# 8  60        131
# 9  61         32
    # Various time intervals, hard to say why without comments column

## How many observations per day? This is simpler and cruder approach

### Starting with 2019 data

MWoolf_west$caldat <- as.Date(MWoolf_west$datetime)

Mwwest2 <- MWoolf_west %>% 
  group_by(caldat) %>% 
  summarise(nObs = n())
   # Before 2019-05-30 one image per hour; after 2
Mwwest2
Mwwest2[Mwwest2$nObs > 25, ]
  # First date with >25 is 5/31


MWoolf_east$caldat <- as.Date(MWoolf_east$datetime)

Mweast2 <- MWoolf_east %>% 
  group_by(caldat) %>% 
  summarise(nObs = n())

Mweast2
Mweast2[Mweast2$nObs > 25, ]
  # First date with >25 is 6/14

Perez$caldat <- as.Date(Perez$datetime)

Perez2 <- Perez %>% 
  group_by(caldat) %>% 
  summarise(nObs = n())

Perez2
Perez2[Perez2$nObs > 25, ]
  # First date with >25 is 6/04. More consistent after 6/14

UCKearney$caldat <- as.Date(UCKearney$datetime)

UCKearney2 <- UCKearney %>% 
  group_by(caldat) %>% 
  summarise(nObs = n())

UCKearney2
UCKearney2[UCKearney2$nObs > 25, ]
   # Consistently ~48/day from 5/31 on

usda$caldat <- as.Date(usda$datetime)

usda2 <- usda %>% 
  group_by(caldat) %>% 
  summarise(nObs = n())

usda2
usda2[usda2$nObs > 25, ]
   # Consistently ~48/day from 4/27 on

### Continuing with 2020 dat

allsites20 <- allsites20[ ,-5] 
allsites20 <- allsites20[complete.cases(allsites20), ]
allsites20

# Create temporary data frame containing readings per day

x <- allsites20 %>% 
  mutate(caldate = as.Date(datetime)) %>% # group into days
  group_by(site, caldate) %>% 
  summarise(nObs = n())

# Plot readings per day by site

ggplot(x, aes(x = caldate, y = nObs)) +
  geom_line() + 
  facet_grid(site ~ .)

# When needed filter x for 



