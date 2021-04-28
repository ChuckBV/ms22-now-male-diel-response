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

