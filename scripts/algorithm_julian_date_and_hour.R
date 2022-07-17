# algorithm_julian_date_and_hour_algorithm.R

### Demonstrates code used to shift the start of the Julian day from midnight
### to 1800 local time (nominal sundow, in fact long before sundown for much
### of the field season for NOW)

library(tidyverse)

### Create a fake data set. Note that, for brevity, this only examines the
### first 7 and last 6 hours of the day

hr <- c(seq(0,7),seq(18,23))
hr
# [1]  0  1  2  3  4  5  6  7 18 19 20 21 22 23
hr <- rep(hr, 5)

jday <- seq(151,155)
jday <- rep(jday,each = 14)

df1 <- data.frame(jday,hr)
head(df1)
#   jday hr
# 1  151  0
# 2  151  1
# 3  151  2
# 4  151  3
# 5  151  4
# 6  151  5

## Shift hour and day. Hours 0-5 should be with the same Julian date
## as the preceding hours 18-23. Hour 18 becomes 0, hour 23 becomes 5, and hours
## 0 to 5 become hours 6 to 14. If the new hours is > 5 then Julian is set
## back 1

df2 <- df1 %>% 
  mutate(jday2 = ifelse(hr <= 7, jday - 1, jday),
         hr2 = ifelse(hr > 7, hr - 18, hr + 6)) %>% 
  filter(jday2 == 151) # select just 1 day for clarity

df2
#    jday hr jday2 hr2
# 1   151 18   151   0
# 2   151 19   151   1
# 3   151 20   151   2
# 4   151 21   151   3
# 5   151 22   151   4
# 6   151 23   151   5
# 7   152  0   151   6
# 8   152  1   151   7
# 9   152  2   151   8
# 10  152  3   151   9
# 11  152  4   151  10
# 12  152  5   151  11
# 13  152  6   151  12
# 14  152  7   151  13

# Used a slightly different definition in script11--try again
df3 <- df1 %>% 
  mutate(hr2 = ifelse(hr >= 18, hr - 18, hr + 6),
         jday2 = ifelse(hr >= 18, jday, jday - 1)) %>% 
  arrange(jday,hr)
df3
# jday hr hr2 jday2
# 1   151  0   6   150
# 2   151  1   7   150
# 3   151  2   8   150
# 4   151  3   9   150
# 5   151  4  10   150
# 6   151  5  11   150
# 7   151  6  12   150
# 8   151  7  13   150
# 9   151 18   0   151
# 10  151 19   1   151
# 11  151 20   2   151
# 12  151 21   3   151
# 13  151 22   4   151
# 14  151 23   5   151

#-- Gets the job done