#===========================================================================#
# script0_head_data_files.R
#
# Show that file structures are equivalent
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
# datetime            pest_nmbr pest_dif reviewed event site      
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

