#===========================================================================#
# script11_explore_count_and_temperature.R
# 
# Uses the "combined" data set created in script10 to explore relationsips 
#
# PARTS
# 1. Basic questions about combined data set (multiple obs/night?)(line 14)  
# 2.  Examine captures after 7AM (before midnight) (line 64)  
#
# combined--imported as created in script10
# combined2--adds categorical variable for off hours
#
#===========================================================================#

library(tidyverse)
library(lubridate)

#-- 1. Basic questions about combined data set (multiple obs/night?) --------

combined <- read_csv("./data/merged_count_and_temp_data.csv")
combined
# A tibble: 1,428 x 11
#   pest_dif site         Yr Mnth.x Julian    Hr degf_avg degf_lo degf_hi rh_avg Mnth.y
#      <dbl> <chr>     <dbl> <chr>   <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <chr> 
# 1        1 UCKearney  2019 Jun       153     5     60.2    59.7    61.2   89.8 Jun   
# 2        1 UCKearney  2019 Jul       191     4     61.2    60.3    62.2   83.6 Jul   
# 3        1 UCKearney  2019 Jul       194     4     64.2    63.9    65.1   93.2 Jul 

length(combined$pest_dif)
# [1] 1428
length(combined$pest_dif[is.na(combined$pest_dif)])
# [1] 0

#-- First confirms r object and syntax. Second confirms that NAs have already
#-- been filtered out, and can be assumed not to exist for subsequent code

### How many cases of multiple entries per day?
combined %>%  
  group_by(site,Yr,Julian) %>% 
  summarise(moths = sum(pest_dif),
            nObs = n())
# A tibble: 577 x 5
# Groups:   site, Yr [10]
#   site          Yr Julian moths  nObs
#   <chr>      <dbl>  <dbl> <dbl> <int>
# 1 mikewoolf1  2019    142     1     1
# 2 mikewoolf1  2019    143     3     2
# 3 mikewoolf1  2019    144     2     1

combined %>% 
  group_by(site,Yr,Julian) %>% 
  summarise(moths = sum(pest_dif),
            nObs = n()) %>% 
  filter(nObs > 1) 

# A tibble: 331 x 5
# Groups:   site, Yr [10]
#   site          Yr Julian moths  nObs
#   <chr>      <dbl>  <dbl> <dbl> <int>
# 1 mikewoolf1  2019    143     3     2
# 2 mikewoolf1  2019    145    60     3
# 3 mikewoolf1  2019    148    66     4

#-- Multiple obs per night in ca. 60% of cases, even though
#-- data are in hour increments

#-- 2. Examine captures after 7AM (before midnight) -------------------------

# Create variable marking offhours 
combined2 <- combined %>% 
  mutate(offhrs = ifelse(Hr <= 7,"No","Yes"))

# Examine other categorical variables individual
combined2 %>% 
  group_by(Yr,offhrs) %>% 
  summarise(nObs = n())
# A tibble: 4 x 3
# Groups:   Yr [2]
# Yr offhrs  nObs
#   <dbl> <chr>  <int>
# 1  2019 No       646
# 2  2019 Yes      114
# 3  2020 No       586
# 4  2020 Yes       82

#-- R has table functions that would apply a chi square test. Such a test
#-- might find a significant different between the years, but it would not
#-- be important when comparing 12 vs. 15%

combined2 %>% 
  group_by(site,offhrs) %>% 
  summarise(nObs = n()) %>% 
  pivot_wider(names_from = site, values_from = nObs)
# A tibble: 2 x 10
#   offhrs mikewoolf1 mikewoolf2 mikewoolf3 mikewoolf4 mikewoolf5 MWoolf_east Perez UCKearney  usda
#   <chr>       <int>      <int>      <int>      <int>      <int>       <int> <int>     <int> <int>
# 1 No            406        137        133        171         24          95    96        66   104
# 2 Yes            88         13         20         21         10          16     7        10    11

# somewhat consistent when there were many captures, more random when there
# were fewer

### Now we can examine month and temperature (next time)

# How many "bins" do we have for month?
combined2 %>% 
  filter(Mnth.x != Mnth.y) 
  #-- Same thing, retatined from both sources in merge

combined2 %>% 
  group_by(Mnth.x) %>% 
  summarise(nObs = n())

# Make Mnth.x into a factor to conserve order
combined2$Mnth.x <- factor(combined2$Mnth.x, levels = c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"))
combined2

combined2 %>% 
  group_by(Mnth.x,offhrs) %>% 
  summarise(nObs = n()) 
# Mnth.x offhrs  nObs
# <fct>  <chr>  <int>
#  1 Mar    No         2
#  2 Apr    No        73
#  3 Apr    Yes       13
#  4 May    No        40
#  5 May    Yes       15
#  6 Jun    No        86
#  7 Jun    Yes       18
#  8 Jul    No       240
#  9 Jul    Yes       19
# 10 Aug    No       400
# 11 Aug    Yes       26
# 12 Sep    No       349
# 13 Sep    Yes       57
# 14 Oct    No        42
# 15 Oct    Yes       48

#-- We can ignore March. Suggests similar trends until October. Can be
#-- Processed with some combination of stacked bar chart and a table 
#-- with proportions. Will be necessary to re-arrange the data, particularly
#-- for the latter 

x <- combined2 %>% 
  group_by(Mnth.x,offhrs) %>%
  filter(Mnth.x != "Mar") %>% 
  summarise(nObs = n()) 
x # as expected

x$Mnth.x <- droplevels(x$Mnth.x)

levels(x$Mnth.x)
# [1] "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct"

# Use pivot_wider to get "No" and "Yes" on same line
x <- x %>% 
  pivot_wider(names_prefix = "Offhours_", names_from = offhrs, values_from = nObs)

x %>%
  mutate(pct_offhours = 100*(Offhours_Yes/(Offhours_No + Offhours_Yes)))
#   Mnth.x Offhours_No Offhours_Yes pct_offhours
#   <fct>        <int>        <int>        <dbl>
# 1 Apr             73           13        15.1 
# 2 May             40           15        27.3 
# 3 Jun             86           18        17.3 
# 4 Jul            240           19         7.34
# 5 Aug            400           26         6.10
# 6 Sep            349           57        14.0 
# 7 Oct             42           48        53.3 

# Go back to combined2 and examine other trends
combined2 <- combined2 %>% 
  filter(Mnth.y != "Mar")
#-- drops two observations
combined2$Mnth.x <- droplevels(combined2$Mnth.x)

# Plot of observations by hour with vertically aligned panels for month
y <- combined2 %>% 
  group_by(Mnth.x,Hr) %>% 
  summarise(nObs = n())

ggplot(data = y, aes(x = Hr, y = nObs)) +
  geom_col() +
  facet_grid(Mnth.x ~ ., scales = "free_y")

# Add temperature
ggplot(data = combined2, aes(x = Hr, y = degf_avg)) +
  geom_point() +
  facet_grid(Mnth.x ~ .)


  

