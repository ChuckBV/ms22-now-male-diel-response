#===========================================================================#
# script0_head_data_files.R
#
# Show that file structures are equivalent
# 1. Load 2019 and 2020 data sets
# 2. Observations, time period and time interval
#
#===========================================================================#

library(tidyverse)

#-- 1. Load 2019 and 2020 data sets ------------------------------------------

allsites19 <- read_csv("./data/allsites_y19.csv")
allsites19
# A tibble: 38,202 x 6
# datetime            pest_nmbr pest_dif reviewed event site     
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>    
# 1 2019-04-26 13:56:00         7        7 Yes      NA    UCKearney
# 2 2019-04-26 14:56:00         7        0 Yes      NA    UCKearney

allsites20 <- read_csv("./data/allsites_y20.csv")
allsites20
# A tibble: 31,726 x 6
#   datetime            pest_nmbr pest_dif reviewed event site      
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>     
# 1 2020-04-22 03:18:00         5        4 Yes      NA    mikewoolf1
# 2 2020-04-22 05:02:00         7        2 Yes      NA    mikewoolf1


alltemps19 <- read_csv("./data/trapview_temps_degf_y19.csv")
alltemps19
# A tibble: 18,969 x 6
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7
# 3 UCKearney 2019-05-16 19:00:00     55.0    53.8    56.8   90.4

alltemps20 <- read_csv("./data/trapview_temps_degf_y20.csv")
alltemps20
# A tibble: 17,580 x 6
#   Date_time           degf_avg degf_lo degf_hi rh_avg site 
#   <dttm>                 <dbl>   <dbl>   <dbl>  <dbl> <chr>
# 1 2020-04-22 02:00:00     51.5    50.2    52.5   90.3 MWT1 
# 2 2020-04-22 03:00:00     49.5    48.9    49.8   93.8 MWT1 
# 3 2020-04-22 04:00:00     49.7    48.9    50.4   94.2 MWT1 

#-- 2. Observations, time period and time interval  ---------------------------

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

### Get intervals
#intervals_events19 <- 
x <- allsites19 # Pass allsites19 to temp object x
x <- x[ ,-5] # drop event which is 95% NA and not essential
x <- x[complete.cases(x), ] # retains 95% of obs
x$site <- factor(x$site, levels = unique(x$site))
   # Looks like x has to be a factor for this to work
y <- split(x, f = x$site)
y[[1]][1:6,] 
y[[2]][1:6,] 
y[[3]][1:6,] 
y[[4]][1:6,] 
y[[5]][1:6,] 
   # split made this into a list containting 5 data frames. Notation above
   # is a klunky way of heading each

### Subset back out entire data sets
UCKearney  <- y[[1]] 
MWoolf_east  <- y[[2]] 
MWoolf_west  <- y[[3]] 
Perez  <- y[[4]] 
usda <- y[[5]]

### Pull all times out of y[[1]] (UCKearney) and place in a vector to examine
### getting time differences
dtimes1 <- y[[1]][,1]  # 8244 values
dtimes1 <- dtimes1[!is.na(dtimes1)] 
length(dtimes1) # 8244, no loss
z <- dtimes1[1:10] # first 10 values

### diff functions
### https://www.talkstats.com/threads/differences-between-elements-of-a-vector.13341/
### https://stackoverflow.com/questions/30510044/how-to-make-time-difference-in-same-units-when-subtracting-posixct

a <- seq(1,10) # diff() with an integer variable
diff(a)
# [1] 1 1 1 1 1 1 1 1 1

diff(z) # diff function with times from y$UCKearney
# Time differences in mins
# [1] 60 61 59 61 60 61 60 60 60

### Use diff with the entire string and units()<- to control unit
b <- diff(dtimes1)
str(b)
units(b) <- "mins"
length(b) #8243 obs, 1 less than dtimes
unique(b)

difs1 <- as.integer(diff(y$UCKearney[,1])) # y$UCKearney is 8254 rows
unique(difs1)

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

### How many observations per day?
MWoolf_west$caldat <- as.Date(MWoolf_west$datetime)

Mwwest2 <- MWoolf_west %>% 
  group_by(caldat) %>% 
  summarise(nObs = n())
   # Before 2019-05-30 one image per hour; after 2


