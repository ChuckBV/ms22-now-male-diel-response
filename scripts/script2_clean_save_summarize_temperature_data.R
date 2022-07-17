#===========================================================================#
# script2_clean_save_summarize_temperature_data.R
# 
# PARTS
# 1. Load combined 2019 and 2020 temperature data files (line 19)
# 2. Add useful descriptors and save 2-year temperature data set (line 103)
# 3. Merge all count and temperature data then save count data (line 133)
#    - p1: Daily count by site (line graph) for 2019
#    - p2: Daily count by site (line graph) for 2020
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(scales)
library(ggpubr)

#---------------------------------------------------------------------------#
#-- 1. Load and clean 2019 and 2020 temperature data files       ------------
#---------------------------------------------------------------------------#

### Obtain and tweak 2019 temperatures

alltemps19 <- readr::read_csv("./data/trapview_temps_degf_y19.csv")
alltemps19
# # A tibble: 18,969 x 6
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7
# 3 UCKearney 2019-05-16 19:00:00     55.0    53.8    56.8   90.4

### Get uniform site names

unique(alltemps19$site)
# [1] "UCKearney"   "usda"        "Perez"       "MWoolf_west" "MWoolf_east"

trapcodes <- read.csv("./data/trapview_trap_codes.csv")
trapcodes
#    Year        site TrapCode site2
# 1  2019       Perez    tv19a     E
# 2  2019 MWoolf_west    tv19b     A
# 3  2019 MWoolf_east    tv19c     B
# 4  2019        usda    tv19d     C
# 5  2019   UCKearney    tv19e     D
# 6  2020  mikewoolf1    tv20a    A1
# 7  2020  mikewoolf2    tv20b    A2
# 8  2020  mikewoolf3    tv20c    A3
# 9  2020  mikewoolf4    tv20d    A4
# 10 2020  mikewoolf5    tv20e    A5

#   2019 names match

trapcodes19 <- trapcodes[trapcodes$Year == 2019,c(2,4)]
trapcodes19

alltemps19 <- left_join(trapcodes19,alltemps19) # parallel with line 44
head(alltemps19,2)
#    site site2           Date_time degf_avg degf_lo degf_hi rh_avg
# 1 Perez     E 2019-06-04 16:00:00     88.7    86.2    90.5  59.84
# 2 Perez     E 2019-06-04 17:00:00     84.0    82.4    85.6  69.78


### Obtain and tweak 2020 temperatures
alltemps20 <- readr::read_csv("./data/trapview_temps_degf_y20.csv")
unique(alltemps20$site) #17,580 obs
# [1] "MWT1" "MWT2" "MWT3" "MWT4" "MWT5"

#  Change to names found in the trapcodes table
alltemps20$site[alltemps20$site == "MWT1"] <- "mikewoolf1"
alltemps20$site[alltemps20$site == "MWT2"] <- "mikewoolf2"
alltemps20$site[alltemps20$site == "MWT3"] <- "mikewoolf3"
alltemps20$site[alltemps20$site == "MWT4"] <- "mikewoolf4"
alltemps20$site[alltemps20$site == "MWT5"] <- "mikewoolf5"

trapcodes20 <- trapcodes[trapcodes$Year == 2020,c(2,4)]
trapcodes20
#          site site2
# 6  mikewoolf1    A1
# 7  mikewoolf2    A2
# 8  mikewoolf3    A3
# 9  mikewoolf4    A4
# 10 mikewoolf5    A5

alltemps20 <- left_join(trapcodes20,alltemps20) # parallel with line 44
head(alltemps20,4)
#   site site2           Date_time degf_avg degf_lo degf_hi rh_avg
# 1 mikewoolf1    A1 2020-04-22 02:00:00    51.50    50.2    52.5  90.27
# 2 mikewoolf1    A1 2020-04-22 03:00:00    49.53    48.9    49.8  93.83
# 3 mikewoolf1    A1 2020-04-22 04:00:00    49.68    48.9    50.4  94.21
# 4 mikewoolf1    A1 2020-04-22 05:00:00    49.52    49.1    49.8  94.05

# combine the temperature data sets
temps <- rbind(alltemps19,alltemps20)
temps
# A tibble: 36,549 x 6
# site      datetime            degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7

#---------------------------------------------------------------------------#
#-- 2. Add useful descriptors and save 2-year temperature data set ---------
#---------------------------------------------------------------------------#

### Set time zone
tzone <- "America/Los_Angeles"
lubridate::tz(temps$Date_time) <- tzone
attr(temps$Date_time,"tzone")

### Get new variable w degrees C
temps <- temps %>% 
  mutate(degc_avg = (degf_avg - 32)/1.8)

plot(temps$degf_avg,temps$degc_avg) # verify

# Add Yr, Mnth, Julian, and Hr to the temps data set
temps <- temps %>% 
  mutate(Yr = year(Date_time),
         Mnth = month(Date_time, label = TRUE, abbr = TRUE),
         Julian = yday(Date_time),
         Hr = hour(Date_time))
head(temps,3)
#    site site2           Date_time degf_avg degf_lo degf_hi rh_avg degc_avg   Yr Mnth Julian Hr
# 1 Perez     E 2019-06-04 16:00:00    88.70    86.2    90.5  59.84 31.50000 2019  Jun    155 16
# 2 Perez     E 2019-06-04 17:00:00    84.00    82.4    85.6  69.78 28.88889 2019  Jun    155 17
# 3 Perez     E 2019-06-04 18:00:00    80.67    79.0    82.2  74.41 27.03889 2019  Jun    155 18

### Save modified 2-year temperature data set
write.csv(temps,"temps_all.csv", row.names = FALSE)

#---------------------------------------------------------------------------#
#-- 3. Merge all Count and temperature data then save                 -------
#---------------------------------------------------------------------------#

#  Load 2-year counts data set
counts_y19y20 <- read_csv("./data-intermediate/counts_all.csv")
counts_y19y20
# A tibble: 69,390 x 7
#   site  site2 datetime            pest_nmbr pest_dif reviewed event
#   <chr> <chr> <dttm>                  <dbl>    <dbl> <chr>    <chr>
# 1 Perez E     2019-06-03 16:56:00         0        0 No       NA   
# 2 Perez E     2019-06-03 17:56:00         0        0 No       NA   
# 3 Perez E     2019-06-03 18:56:00         0        0 No       NA   

counts_y19y20 <- counts_y19y20 %>% 
  #  Create Mnth Julian and Hr variables
  mutate(Yr = year(datetime),
         Mnth = month(datetime, label = TRUE, abbr = TRUE),
         Julian = yday(datetime),
         Hr = hour(datetime)) %>% 
  # Pool by site2, Yr,Mnth,Julian,Hr
  group_by(site2,Yr,Mnth,Julian,Hr) %>% 
  summarise(count = sum(pest_dif, na.rm = TRUE))

counts_y19y20
# A tibble: 36,055 x 6
# Groups:   site2, Yr, Mnth, Julian [1,581]
#   site2    Yr Mnth  Julian    Hr count
#   <chr> <dbl> <ord>  <dbl> <int> <dbl>
# 1 A      2019 May      142    15     0
# 2 A      2019 May      142    16     0

temps <- temps %>% 
  select(site2,Yr,Mnth,Julian,Hr,degc_avg)

head(temps,3)
#   site2   Yr Mnth Julian Hr degc_avg
# 1     E 2019  Jun    155 16 31.50000
# 2     E 2019  Jun    155 17 28.88889
# 3     E 2019  Jun    155 18 27.03889

combined <- left_join(counts_y19y20, temps, by = c("Yr","site2","Julian","Hr"))
head(combined)
#   pest_dif      site Yr.x Mnth.x Julian Hr degf_avg degf_lo degf_hi rh_avg Yr.y Mnth.y
# 1        7 UCKearney 2019    Apr    116 13       NA      NA      NA     NA   NA   <NA>
# 2        1 UCKearney 2019    May    122  1       NA      NA      NA     NA   NA   <NA>
# 3        1 UCKearney 2019    May    123  2       NA      NA      NA     NA   NA   <NA>
# 4        1 UCKearney 2019    May    123 20       NA      NA      NA     NA   NA   <NA>
# 5        3 UCKearney 2019    May    136  9       NA      NA      NA     NA   NA   <NA>
# 6        1 UCKearney 2019    Jun    153  5    60.17    59.7    61.2  89.83 2019    Jun

### Save 2 year combined count and temperature data file
write.csv(combined,"./data-intermediate/combined_count_temp_all.csv", row.names = FALSE)
