#===========================================================================#
# script1_temperature_variation.R
# 
# Examine variation in nightly temperatures, and determine the number of
# observations in a range relevant to shifting mating activity earler
#
#===========================================================================#


library(tidyverse)
library(lubridate)

#---------------------------------------------------------------------------
## Variation in 2019 data

alltemps19 <- readr::read_csv("./data/trapview_temps_degf_y19.csv")

ggplot(alltemps19, aes(x = Date_time, y = degf_avg)) +
  geom_line() +
  facet_grid(site ~ .)

# May is in 2 of the 6 data sets, nothing earlier

# May is in 2 of the 6 data sets, nothing earlier

# Examine by month in order to contrast and to expand the x-scale. In order
# to do that, get month and day of month as variables

alltemps19
# A tibble: 18,969 x 6
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7

nightly_19 <- alltemps19 %>% # 18969 to 798
  mutate(mnth = month(Date_time, label = TRUE, abbr = TRUE),
         day_of_mnth = mday(Date_time)) %>% 
  group_by(site, mnth, day_of_mnth) %>% 
  summarise(degf_lo = min(degf_lo)) %>% 
  mutate(lt66 = ifelse(degf_lo < 66,1,0),
         lt52 = ifelse(degf_lo <= 52,1,0))

nightly_19 <- nightly_19[complete.cases(nightly_19), ]
  # drop a pesky row of NAs
unique(nightly_19$mnth)


monthly19 <- nightly_19 %>% 
  group_by(site,mnth) %>% 
  summarise(lt66 = sum(lt66),
            lt52 = sum(lt52),
            nObs = n(),
            pct_lt66 = 100*lt66/nObs,
            pct_lt52 = 100*lt52/nObs)

head(monthly19)
# A tibble: 6 x 7
# Groups:   site [2]
#   site        mnth   lt66  lt52  nObs pct_lt66 pct_lt52
#   <chr>       <ord> <dbl> <dbl> <int>    <dbl>    <dbl>
# 1 MWoolf_east Jun      19     0    27     70.4      0  
# 2 MWoolf_east Jul      23     0    31     74.2      0  
# 3 MWoolf_east Aug      22     0    31     71.0      0  

unique(monthly19$mnth)

tbl_lt66 <- monthly19 %>% 
  select(site,mnth,pct_lt66) %>% 
  pivot_wider(id_cols = site, 
              names_from = mnth, 
              values_from = pct_lt66,
              values_fill = 0)

tbl_lt66 <- tbl_lt66[,c(1,7,2:6,8)]
tbl_lt66
# A tibble: 5 x 8
# Groups:   site [5]
#   site          May   Jun   Jul   Aug   Sep   Oct   Nov
#   <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 MWoolf_east     0  70.4  74.2  71.0  96.7   100     0
# 2 MWoolf_west   100  90    83.9 100    96.7   100     0
# 3 Perez           0  92.6  96.8  74.2  93.3   100   100
# 4 UCKearney     100  86.7  80.6  80.6  96.7   100   100
# 5 usda          100  76.7  71.0  74.2  90     100     0

   # Entered top of range most nights, most sites

tbl_lt52 <- monthly19 %>% 
  select(site,mnth,pct_lt52) %>% 
  pivot_wider(id_cols = site, 
              names_from = mnth, 
              values_from = pct_lt52,
              values_fill = 0)

tbl_lt52 <- tbl_lt52[,c(1,7,2:6,8)]
tbl_lt52
# A tibble: 5 x 8
# Groups:   site [5]
#   site          May   Jun   Jul   Aug   Sep   Oct   Nov
#   <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 MWoolf_east   0    0     0        0  10    77.4     0
# 2 MWoolf_west  40    3.33  0        0  20    96.8     0
# 3 Perez         0   14.8   9.68     0  16.7  96.8   100
# 4 UCKearney    56.2  3.33  0        0  23.3 100     100
# 5 usda          0    3.33  0        0  26.7 100       0

# At lower end variable, cooler before June and after Aug

# Visualization of nighttime tempratures
alltemps19$hr = hour(alltemps19$Date_time)
alltemps19$mnth = month(alltemps19$Date_time, label = TRUE, abbr = TRUE)
alltemps19

ggplot(alltemps19, aes(x = as.factor(hr), y = degf_lo)) +
  geom_point() + 
  geom_boxplot() +
  facet_grid(mnth ~ .) 

#---------------------------------------------------------------------------
## Variation in 2020 data

alltemps20 <- readr::read_csv("./data/trapview_temps_degf_y20.csv")
head(alltemps20,2)
# A tibble: 2 x 6
#   Date_time           degf_avg degf_lo degf_hi rh_avg site 
#   <dttm>                 <dbl>   <dbl>   <dbl>  <dbl> <chr>
# 1 2020-04-22 02:00:00     51.5    50.2    52.5   90.3 MWT1 
# 2 2020-04-22 03:00:00     49.5    48.9    49.8   93.8 MWT1 

ggplot(alltemps20, aes(x = Date_time, y = degf_avg)) +
  geom_line() +
  facet_grid(site ~ .)
    # More holes in the data compared to 2018

alltemps20
# A tibble: 17,580 x 6
#   Date_time           degf_avg degf_lo degf_hi rh_avg site 
#   <dttm>                 <dbl>   <dbl>   <dbl>  <dbl> <chr>
# 1 2020-04-22 02:00:00     51.5    50.2    52.5   90.3 MWT1 
# 2 2020-04-22 03:00:00     49.5    48.9    49.8   93.8 MWT1

nightly_20 <- alltemps20 %>% # 18969 to 798
  mutate(mnth = month(Date_time, label = TRUE, abbr = TRUE),
         day_of_mnth = mday(Date_time)) %>% 
  group_by(site, mnth, day_of_mnth) %>% 
  summarise(degf_lo = min(degf_lo)) %>% 
  mutate(lt66 = ifelse(degf_lo < 66,1,0),
         lt52 = ifelse(degf_lo <= 52,1,0))

nightly_20 <- nightly_20[complete.cases(nightly_20), ]
# drop a pesky row of NAs
unique(nightly_20$mnth)


monthly20 <- nightly_20 %>% 
  group_by(site,mnth) %>% 
  summarise(lt66 = sum(lt66),
            lt52 = sum(lt52),
            nObs = n(),
            pct_lt66 = 100*lt66/nObs,
            pct_lt52 = 100*lt52/nObs)

head(monthly20)
# A tibble: 6 x 7
# Groups:   site [2]
#   site        mnth   lt66  lt52  nObs pct_lt66 pct_lt52
#   <chr>       <ord> <dbl> <dbl> <int>    <dbl>    <dbl>
# 1 MWoolf_east Jun      19     0    27     70.4      0  
# 2 MWoolf_east Jul      23     0    31     74.2      0  
# 3 MWoolf_east Aug      22     0    31     71.0      0  

unique(monthly20$mnth)

tbl_lt66 <- monthly20 %>% 
  select(site,mnth,pct_lt66) %>% 
  pivot_wider(id_cols = site, 
              names_from = mnth, 
              values_from = pct_lt66,
              values_fill = 0)

tbl_lt66 <- tbl_lt66[,c(1,7,2:6,8)]
tbl_lt66
# A tibble: 5 x 8
# Groups:   site [5]
#   site    Sep   Apr   May   Jun   Jul   Aug   Mar
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 MWT1   81.8   100  88.9  80    96.8  61.3     0
# 2 MWT2   95.5   100 100    86.7 100    74.2   100
# 3 MWT3   95.5   100  96.8  95.7  96.7  77.4     0
# 4 MWT4  100     100 100    96.3  96.3  77.4   100
# 5 MWT5   95.5     0   0     0    94.7  77.4     0

# Entered top of range most nights, most sites

tbl_lt52 <- monthly20 %>% 
  select(site,mnth,pct_lt52) %>% 
  pivot_wider(id_cols = site, 
              names_from = mnth, 
              values_from = pct_lt52,
              values_fill = 0)

tbl_lt52 <- tbl_lt52[,c(1:7)]
tbl_lt52
# A tibble: 5 x 7
# Groups:   site [5]
#   site    Apr   May   Jun   Jul   Aug   Sep
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 MWT1   22.2  48.1  20       0  0        0
# 2 MWT2   70    48    16.7     0  0        0
# 3 MWT3   20    41.9  21.7     0  3.23     0
# 4 MWT4   70    48.1  18.5     0  0        0
# 5 MWT5    0     0     0       0  0        0

# At lower end variable, cooler before June and after Aug

# Visualization of nighttime tempratures
alltemps20$hr = hour(alltemps20$Date_time)
alltemps20$mnth = month(alltemps20$Date_time, label = TRUE, abbr = TRUE)
alltemps20

ggplot(alltemps20, aes(x = as.factor(hr), y = degf_lo)) +
  geom_point() + 
  geom_boxplot() +
  facet_grid(mnth ~ .) 



