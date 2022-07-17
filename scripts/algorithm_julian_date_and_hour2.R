# algorithm_julian_date_and_hour_algorithm2.R

### Demonstrates code used to shift the start of the Julian day from midnight
### to 1800 local time (nominal sundow, in fact long before sundown for much
### of the field season for NOW)

library(tidyverse)

### Create a fake data set. Note that, for brevity, this only examines the
### first 7 and last 6 hours of the day

hr <- seq(0,23)
hr
# [1]  0  1  2  3  4  5  6  7 18 19 20 21 22 23
hr <- rep(hr, 5)

julian_day <- seq(151,155)
julian_day <- rep(julian_day,each = 24)

df1 <- data.frame(julian_day,hr)
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


# Used a slightly different definition in script11--try again
df2 <- df1 %>% 
  mutate(hr2 = ifelse(hr >= 18, hr - 18, hr + 6),
         julian_day2 = ifelse(hr >= 18, julian_day +1, julian_day)) %>% 
  select(julian_day,hr,julian_day2,hr2)
df2
#     julian_day hr julian_day2 hr2
# 1          151  0         151   6
# 2          151  1         151   7
# 3          151  2         151   8
# 4          151  3         151   9
# 5          151  4         151  10
# 6          151  5         151  11
# 7          151  6         151  12
# 8          151  7         151  13
# 9          151  8         151  14
# 10         151  9         151  15
# 11         151 10         151  16
# 12         151 11         151  17
# 13         151 12         151  18
# 14         151 13         151  19
# 15         151 14         151  20
# 16         151 15         151  21
# 17         151 16         151  22
# 18         151 17         151  23
# 19         151 18         152   0
# 20         151 19         152   1
# 21         151 20         152   2
# 22         151 21         152   3
# 23         151 22         152   4
# 24         151 23         152   5
# 25         152  0         152   6

