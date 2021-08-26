#===========================================================================#
# script12_explore_count_and_temperature2.R
# 
# Uses the "combined" data set created in script10 to explore relationsips 
#
# PARTS
# 1. Basic questions about combined data set (multiple obs/night?)(line 14)  
# 2. Examine captures after 7AM (before midnight) (line 64)  
#
# combined--imported as created in script10
# combined3
#  - Night only
#  - Only first obs of night for that site
#  - Additional variables adjust 0 hour to sundown and keep next sun-up in
#    the same Julian day
# combined4 (not yet done)
#  - All captures per night
#  - Additional variables to adjust 0 hour to sundown and keep next sun-up 
#    in the same Julian day (as in 3)
#  - Used to examine whether captures are less likely overall on hotter
#    nights
#  
#===========================================================================#

library(tidyverse)
library(lubridate)
#library(timechange)

#-- 1. (Heading to be determined) --------

combined <- read_csv("./data/merged_count_and_temp_data.csv")
combined
# A tibble: 1,428 x 11
#   pest_dif site         Yr Mnth.x Julian    Hr degf_avg degf_lo degf_hi rh_avg Mnth.y
#      <dbl> <chr>     <dbl> <chr>   <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <chr> 
# 1        1 UCKearney  2019 Jun       153     5     60.2    59.7    61.2   89.8 Jun   
# 2        1 UCKearney  2019 Jul       191     4     61.2    60.3    62.2   83.6 Jul   
# 3        1 UCKearney  2019 Jul       194     4     64.2    63.9    65.1   93.2 Jul 

#####################################################################
### Reduce to night observations and only the first observation (hour)

combined3 <- combined %>% 
  filter(Hr <= 7 | Hr >= 18) %>%       # only during dark
  group_by(Yr,site,Mnth.x,Julian) %>%  # <=1 obs per site per night
  summarise(hr_obs1 = min(Hr),         # first obs (except...)
            degf = mean(degf_avg))     # temp during first obs

combined3
# A tibble: 556 x 6
# Groups:   Yr, site, Mnth.x [52]
#       Yr site       Mnth.x Julian hr_obs1  degf
#   <dbl> <chr>      <chr>   <dbl>   <dbl> <dbl>
# 1  2019 mikewoolf1 Aug       213       5  59.2
# 2  2019 mikewoolf1 Aug       215       5  62.7

hrs <- sort(unique(combined3$hr_obs1))
hrs
# [1]  0  1  2  3  4  5  6  7 18 19 20 21 22 23

#-- all possible hours represented

## Temp of first obs vs. Julian day. This works without any further processing
## because hour is not a factor

p1 <- ggplot(combined3, aes(x = Julian, y = degf)) +
  geom_point() +
  theme_bw() +
  xlab("Julian Day") +
  ylab("Temperature (F) at time of first capture") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p1 

ggsave(filename = "fig_temperature_of_first_captr_by_julian.jpg", 
       plot = p1, device = "jpg", path = "./results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  

## Get Julian date reference points for months

# Make Mnth.x into a factor to conserve order
combined3[combined3$Mnth.x == "Mar", ] # 2 observations, drop them
combined3 <- combined3[combined3$Mnth.x != "Mar", ]

combined3$Mnth.x <- factor(combined3$Mnth.x, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct"))
combined3
levels(combined3$Mnth.x)

sort(unique(combined3$Julian))
# 102 to 131, about 25 days missing

combined3 %>% 
  group_by(Mnth.x) %>% 
  summarise(frst_Jul = min(Julian))
# A tibble: 7 x 2
# Mnth.x frst_Jul
#   <fct>     <dbl>
# 1 Apr         102
# 2 May         122
# 3 Jun         152
# 4 Jul         182
# 5 Aug         213
# 6 Sep         244
# 7 Oct         275

#-- Rough idea of how month corresponds to Julian Day


# Algorithm to shift time and date
combined3 <- combined3 %>% 
  mutate(Julian2 = ifelse(hr_obs1 <= 7, Julian - 1, Julian),
         hr2 = ifelse(hr_obs1 > 7, hr_obs1 - 18, hr_obs1 + 6))
combined3

########################################################
### Vertical bar chart of first counts by time of night

x <- combined3 %>% 
  group_by(hr2) %>% 
  summarise(nObs = n())

p2 <- ggplot(x, aes(x = hr2, y = nObs)) +
  geom_col() +
  theme_bw() +
  xlab("Hours after sundown") +
  ylab("Observations of first capture") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p2 

ggsave(filename = "fig_vbar_captures_by_time_of_night.jpg", 
       plot = p2, device = "jpg", path = "./results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  

########################################################
### Vertical bar chart of first counts by temperature

combined3$deg <- as.integer(combined3$degf) 
#  mutate(deg = integer(degf))
combined3

y <- combined3 %>% 
  group_by(deg) %>% 
  summarise(nObs = n())

p3 <- ggplot(y, aes(x = deg, y = nObs)) +
  geom_col() +
  theme_bw() +
  xlab("Degrees Farhenheit") +
  ylab("Observations of first capture") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p3

ggsave(filename = "fig_vbar_first_captures_by_temperature.jpg", 
       plot = p3, device = "jpg", path = "./results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  

###########################################################
### Does seasonality (photoperiod) affect relationship?

p4 <-  ggplot(combined3, aes(x = degf, y = hr2)) +
  geom_point() +
  theme_bw() +
  facet_grid(Mnth.x ~.) +
  xlab("Degrees Fahrenheit") +
  ylab("First capture (hour after sunset)") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p4

ggsave(filename = "first_cap_vs_temp_by_month.jpg", 
       plot = p4, device = "jpg", path = "./results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  


###########################################################
### Are there fewer captured overall on hot nights?

