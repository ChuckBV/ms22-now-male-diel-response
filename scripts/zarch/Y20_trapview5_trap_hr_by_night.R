#===========================================================================#
# Y20_trapview5_trap_hr_by_night.R
#
# Explore 2020 field season data at half hour intervals from five TrapView
# stations
# 1. Load 2020 trapdata, get hour of night, Julian date, and epiweeks 
#  (line 17)
# 2.x  
#  
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(timechange)


#-- 1. Load 2020 trapdata, get hour of night, Julian date, and epiweek ------
allsites_y20 <- read_csv("./data/allsites_y20.csv")
allsites_y20
# A tibble: 31,726 x 6
#   datetime            pest_nmbr pest_dif reviewed event site      
#   <dttm>                  <dbl>    <dbl> <chr>    <chr> <chr>     
# 1 2020-04-22 03:18:00         5        4 Yes      NA    mikewoolf1
# 2 2020-04-22 05:02:00         7        2 Yes      NA    mikewoolf1

### Store local time zone in memory for subsequent steps
realtz  <- "America/Los_Angeles"
faketz <- "Atlantic/Cape_Verde" # UCT -1

### Get Date and Minutes from sunset

### Save real-time date, California time
allsites_y20$caldate <- as.Date(allsites_y20$datetime)

### Create a column with the datetime value for midnight for each date Pacific Time
allsites_y20$time_los_angeles <- timechange::time_force_tz(allsites_y20$datetime, tz = realtz)
  # declares that the datetime is in fact PDT
allsites_y20$time_cverde <- timechange::time_at_tz(allsites_y20$time_los_angeles, tz = faketz)
  # determines hour at Uct -1 (cabo verde)
allsites_y20$mnight_cverde <- lubridate::make_datetime(year(allsites_y20$time_cverde),
                                              month(allsites_y20$time_cverde),
                                              day(allsites_y20$time_cverde),
                                              0,0,0,faketz)
  # creates the midnight value for -1 uct
allsites_y20$since_night <- difftime(allsites_y20$time_cverde,allsites_y20$mnight_cverde, units = "hours")
  # finds time since midnight
allsites_y20$since_night2 <- as.numeric(allsites_y20$since_night)
  # makes the time unit numeric

glimpse(allsites_y20)

### Use View() to compare times
x <- allsites_y20[,c(1,7:11)]

x <- x[wday(x$caldate) != wday(x$time_cverde), ]
select(x,c(caldate,time_cverde))

### Get Julian date (days since January 1)
allsites_y20$julian <- yday(allsites_y20$caldate)

### Get CDC Epidemiological weeks (weeks since Jan 1 as done in the Americas)
allsites_y20$wk <- epiweek(allsites_y20$caldate)


#-- 2. Initial examination observations per hour  ---------------------------

### How many observations per hours (should be every 30 min). Create an integer
### for hr and ask how many obs

allsites_y20 %>% 
  mutate(int_hr = trunc(since_night2)) %>% 
  group_by(site,mnight_cverde,int_hr) %>% 
  summarise(nObs = n()) %>% 
  group_by(int_hr,nObs) %>% 
  summarise(nRec = n())
# A tibble: 87 x 3
# Groups:   int_hr [24]
#   int_hr  nObs  nRec
#    <dbl> <int> <int>
# 1      0     1    59
# 2      0     2   581
# 3      0     3    39
# 4      1     1    52
# 5      1     2   597
# 6      1     3    27
# 7      1     4     1
# 8      2     1    55
# 9      2     2   582
# 10      2     3    33
# ... with 77 more rows

### So usually 2 obs, but sometimes 1 or 3

#-- 3. Day-to-day variation in observations ---------------------------------

### Before getting to timing, ask how consistent was night-to-night capture
### and if it varied between traps 10 rows apart
x1 <- allsites_y20 %>% 
  mutate(cverde_date = as.Date(mnight_cverde)) %>% 
  select(site,cverde_date,pest_dif) %>% 
  group_by(site,cverde_date) %>% 
  summarise(nNOW = sum(pest_dif, na.rm = TRUE)) %>% 
  pivot_wider(names_from = site, values_from = nNOW) %>% 
  arrange(cverde_date)

### mikewoolf5 was out of commission fpr much of season, so compare first 4
x1a <- x1 %>% 
  select(1:5) %>% 
  filter(complete.cases(x1[,2:5]))
x1a
# A tibble: 118 x 5
#   cverde_date mikewoolf1 mikewoolf2 mikewoolf3 mikewoolf4
#   <date>           <dbl>      <dbl>      <dbl>      <dbl>
# 1 2020-04-22           8          9         14          2
# 2 2020-04-23           0          1          0          0

### creat correlation matrix
cor(as.matrix(x1a[,2:5]))
#            mikewoolf1 mikewoolf2 mikewoolf3 mikewoolf4
# mikewoolf1  1.0000000  0.4612622  0.4648556  0.4371512
# mikewoolf2  0.4612622  1.0000000  0.6420122  0.7683296
# mikewoolf3  0.4648556  0.6420122  1.0000000  0.6118048
# mikewoolf4  0.4371512  0.7683296  0.6118048  1.0000000

   # So traps were fairly correlated, so we can probably pool them

### Flip x1a back to indexed (but use x1a, because complete.cases
### provided a balanced data set)
x1b <- x1a %>% 
  pivot_longer(cols = 2:5, names_to = "site", values_to = "nNOW") %>% 
  group_by(cverde_date) %>% 
  summarise(nNOW = sum(nNOW))
x1b

p1 <- ggplot(x1b, aes(x = cverde_date, y = nNOW)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("") +
  ylab("Sum of NOW in four traps") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p1

ggsave(filename = "Y20_trapview_nightly.jpg", p1, path = "./output",
       width = 5.83, height = 4.5, dpi = 300, units = "in", device='jpg')

  # Should compare this to temperature data
  # Wind might also be a factor, but those data more difficult to find

#-- 4. Traps by hour of night ---------------------------------

### We want to know when traps started captured moths (hour of night), and
### how much this varied over the season and with temperature.
### Next step, break hours down into half-hour increments
x2 <- allsites_y20 %>% 
  mutate(date_cverde = as.Date(mnight_cverde),
         epwk = epiweek(date_cverde),
         hlf_hr = since_night2%/%0.5) %>% 
  select(site,epwk,date_cverde,time_cverde,pest_dif,since_night2,hlf_hr) %>% 
  filter(pest_dif > 0)
x2
# A tibble: 693 x 7
#   site        epwk date_cverde time_cverde         pest_dif since_night2 hlf_hr
#   <chr>      <dbl> <date>      <dttm>                 <dbl>        <dbl>  <dbl>
# 1 mikewoolf1    17 2020-04-22  2020-04-22 09:18:00        4         9.3      18
# 2 mikewoolf1    17 2020-04-22  2020-04-22 11:02:00        2        11.0      22
# 3 mikewoolf1    17 2020-04-22  2020-04-22 12:35:00        2        12.6      25

### Arrange by week of year and hour of night
x2a <- x2 %>% 
  group_by(epwk,hlf_hr) %>% 
  summarise(nNOW = sum(pest_dif, na.rm = TRUE))
x2a
# Groups:   epwk [25]
#    epwk hlf_hr  nNOW
#   <dbl>  <dbl> <dbl>
# 1    11     19     5
# 2    13     14     1
# 3    15     17     1
# 4    15     20     1
# 5    16      7     1
# 6    16      8     1
# 7    16     11     1

### How many weeks
length(unique(x2a$epwk))
# 25



### How many hlf_hr?
length(unique(x2a$hlf_hr))
# 45