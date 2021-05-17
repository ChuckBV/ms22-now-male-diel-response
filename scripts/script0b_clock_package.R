# script0b_clock_package

library(tidyverse)
library(clock)

### See the vignette 
# https://cran.r-project.org/web/packages/clock/vignettes/clock.html

?date_build

date_build(2019, 2, 1:5)
# [1] "2019-02-01" "2019-02-02" "2019-02-03" "2019-02-04" "2019-02-05"

?date_time_build  # generates POSIXct

# When performing time-series related data analysis, you often need to summarize 
# your series at a less precise precision. There are many different ways to do 
# this, and the differences between them are subtle, but meaningful. clock offers 
# three different sets of functions for summarization:
#  - date_group()
#  - date_floor(), date_ceiling(), and date_round()
#  - date_shift()

### https://clock.r-lib.org/articles/recipes.html#generate-sequences-of-dates-and-date-times-1
### Use copy and past from above vignette to figure out this new toy
from <- as_naive_time(year_month_day(2019, 1, 1, 2, 30, 00))
from
# <time_point<naive><second>[1]>
# [1] "2019-01-01 02:30:00"

to <- as_naive_time(year_month_day(2019, 1, 1, 12, 30, 00))
to
# <time_point<naive><second>[1]>
# [1] "2019-01-01 12:30:00"

### Get a vector of datetime values at 30 minut intervals
x <- seq(from, to, by = duration_minutes(30))
# <time_point<naive><second>[21]>
#   [1] "2019-01-01 02:30:00" "2019-01-01 03:00:00" "2019-01-01 03:30:00"
# [4] "2019-01-01 04:00:00" "2019-01-01 04:30:00" "2019-01-01 05:00:00"
# [7] "2019-01-01 05:30:00" "2019-01-01 06:00:00" "2019-01-01 06:30:00"
# [10] "2019-01-01 07:00:00" "2019-01-01 07:30:00" "2019-01-01 08:00:00"
# [13] "2019-01-01 08:30:00" "2019-01-01 09:00:00" "2019-01-01 09:30:00"
# [16] "2019-01-01 10:00:00" "2019-01-01 10:30:00" "2019-01-01 11:00:00"
# [19] "2019-01-01 11:30:00" "2019-01-01 12:00:00" "2019-01-01 12:30:00"

### Make into POSIXct (more generic R)
x <- as.POSIXct(x)
str(x)
# POSIXct[1:21], format: "2019-01-01 02:30:00" "2019-01-01 03:00:00" "2019-01-01 03:30:00" ...

### Try diff function with current vector x
y <- diff(x)
y
# Time differences in mins
# [1] 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30

length(y)
# [1] 20
    # 1 less than x, as should be