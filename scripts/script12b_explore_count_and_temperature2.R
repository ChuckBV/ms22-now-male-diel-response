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

#-- 1. (Heading to be determined) --------

combined <- read_csv("./data/merged_count_and_temp_data.csv")
combined
# A tibble: 1,428 x 11
#   pest_dif site         Yr Mnth.x Julian    Hr degf_avg degf_lo degf_hi rh_avg Mnth.y
#      <dbl> <chr>     <dbl> <chr>   <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <chr> 
# 1        1 UCKearney  2019 Jun       153     5     60.2    59.7    61.2   89.8 Jun   
# 2        1 UCKearney  2019 Jul       191     4     61.2    60.3    62.2   83.6 Jul   
# 3        1 UCKearney  2019 Jul       194     4     64.2    63.9    65.1   93.2 Jul 

combined[!complete.cases(combined), ]
#-- Tibble of 0, all complete cases

## Get Julian date reference points for months

# Make Mnth.x into a factor to conserve order
combined[combined$Mnth.x == "Mar", ] # 2 observations, drop them
combined <- combined[combined$Mnth.x != "Mar", ]

combined$Mnth.x <- factor(combined$Mnth.x, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct"))
combined
levels(combined$Mnth.x)

### Algorithm to shift time and date. This is how we did the time shift in
### script11. See also "julian_date_and_hour_scratch.R". Note that numbering
### of "combined" data sets in this script is different from script11--don't
### switch back and forth.

### Anything after midnight and before sundown (6PM) has the Julian date from 
### the previous day

combined2 <- combined %>% 
  dplyr::mutate(Hr2 = ifelse(Hr >= 18, Hr - 18, Hr + 6),
                Julian2 = ifelse(Hr >= 18, Julian, Julian - 1)) %>% 
  dplyr::arrange(Julian,Hr)
combined2

combined2[!complete.cases(combined2), ]
#-- Tibble of 0, all complete cases

### Get median observation for Julian2. In order to be certain that median
### functions correctly, go from frequency table form (moths per hour of)
### observations) to case format (one observation for each moth)

combined3 <- vcdExtra::expand.dft(combined2, freq = "pest_dif")
#-- goes from 1426 to 3495 observations

combined3[!complete.cases(combined3), ]
#-- Tibble of 0, all complete cases

combined3 <- combined3 %>% 
  group_by(Yr,Julian2,site) %>% 
  dplyr::summarise(Hr2 = quantile(Hr2, probs = 0.25, na.rm = TRUE))
combined3
# A tibble: 560 x 4
# Groups:   Yr, Julian2 [242]
# Yr Julian2 site         Hr2
#   <int>   <int> <fct>      <dbl>
# 1  2019     142 mikewoolf1   8  
# 2  2019     143 mikewoolf1  17  
# 3  2019     144 mikewoolf1  11   

combined3[!complete.cases(combined3), ]
#-- Tibble of 0, all complete cases

# Lost temperature data in summary step above, so merge to source data set
# to get that back
combined4 <- left_join(combined3,combined2)
combined4
# A tibble: 551 x 13
# Groups:   Yr [2]
#      Yr Julian2   Hr2 pest_dif site       Mnth.x Julian    Hr degf_avg degf_lo degf_hi rh_avg Mnth.y
#   <dbl>   <dbl> <dbl>    <dbl> <chr>      <fct>   <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <chr> 
# 1  2019     142   8          2 mikewoolf1 May       143     2     53.9    52.7    55     89.3 May   
# 2  2019     143  17          2 mikewoolf1 May       144    11     75.5    72.3    77.2   54.6 May   
# 3  2019     144  11         56 mikewoolf1 May       145     5     54.4    54.1    55     96.2 May   
# 4  2019     147   8         35 mikewoolf1 May       148     2     54.3    53.8    54.9   82.8 May   
# 5  2019     148  11         27 mikewoolf1 May       149     5     58.0    57.2    58.6   94.2 May   
# 6  2019     149  11         22 mikewoolf1 May       150     5     59.4    59.2    59.9   89.3 May   
# 7  2019     150  11          2 mikewoolf1 May       151     5     58.4    57.7    59.2   84.7 May   
# 8  2019     151  11          7 mikewoolf1 Jun       152     5     59.1    58.6    60.3   84.1 Jun   
# 9  2019     152  11          1 UCKearney  Jun       153     5     60.2    59.7    61.2   89.8 Jun   
# 10  2019     154  10.5       NA NA         NA         NA    NA     NA      NA      NA     NA   NA 

#-- Some NAs because in some cases the algorith for median gives a value of x.5

x <- combined3$Hr2[combined3$Hr2%%1 != 0]
x
# [1] 10.5 10.5 10.5 10.5  9.5 10.5 10.5 10.5 10.5 10.5 10.5 11.5 10.5 11.5  8.5  1.5  3.5  4.5 12.5  1.5 10.5 11.5  5.5  1.5  5.5 10.5 10.5 13.5 11.5
#-- Confirms the remark above
length(x)
# [1] 63

combined3 %>% 
  mutate(Hr3 = ifelse(Hr2%%1 != 0,Hr2 - 0.5,Hr2)) %>% 
  filter(Hr3 != Hr2)
# A tibble: 63 x 5
# Groups:   Yr, Julian2 [56]
# Yr Julian2 site          Hr2   Hr3
# <int>   <int> <fct>       <dbl> <dbl>
# 1  2019     154 mikewoolf1   10.5    10
# 2  2019     163 Perez        10.5    10
# 3  2019     179 usda          9.5     9

#-- Confirms proposed fix, no need for yet another variable

# combined3 <- combined3 %>% 
#   mutate(Hr2 = ifelse(Hr2%%1 != 0,Hr2 - 0.5,Hr2))
# combined3

combined3[!complete.cases(combined3), ]
#-- Tibble of 0, all complete cases

combined4 <- left_join(combined3,combined2)
combined4
# A tibble: 714 x 13
# Groups:   Yr, Julian2 [242]
# Yr Julian2 site         Hr2 pest_dif Mnth.x Julian    Hr degf_avg degf_lo degf_hi rh_avg Mnth.y
#   <dbl>   <dbl> <chr>      <dbl>    <dbl> <fct>   <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <chr> 
# 1  2019     142 mikewoolf1     8        2 May       143     2     53.9    52.7    55     89.3 May   
# 2  2019     143 mikewoolf1    17        2 May       144    11     75.5    72.3    77.2   54.6 May   
# 3  2019     144 mikewoolf1    11       56 May       145     5     54.4    54.1    55     96.2 May 

combined4[!complete.cases(combined4), ]
# A tibble: 30 x 13
# Groups:   Yr, Julian2 [29]
# Yr Julian2 site          Hr2 pest_dif Mnth.x Julian    Hr degf_avg degf_lo degf_hi rh_avg Mnth.y
#   <dbl>   <dbl> <chr>       <dbl>    <dbl> <fct>   <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <chr> 
# 1  2019     184 mikewoolf1     10       NA NA         NA    NA       NA      NA      NA     NA NA    
# 2  2019     233 UCKearney      10       NA NA         NA    NA       NA      NA      NA     NA NA    
# 3  2019     234 Perez          10       NA NA         NA    NA       NA      NA      NA     NA NA    
# 4  2019     235 usda           10       NA NA         NA    NA       NA      NA      NA     NA NA 

#-- In 30 cases the algorithm gives a median value for which there was no 
#-- count in the input data set. This is out of 714 observations--acceptable
#-- loss

combined4 <- combined4[complete.cases(combined4), ]

### Now plot and determine correlations
p1 <- ggplot(combined4, aes(x = degf_avg, y = Hr2)) +
  geom_point() +
  facet_wrap(vars(Mnth.x), ncol = 3, nrow = 4) +
  theme_bw() +
  xlab("Degrees F at time of median capture") +
  ylab("Median capture time (Hours after sunset)") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p1

ggsave(filename = "Temp_v_time_med_capture_by_month.jpg", plot = p1, device = "jpg", path = "./results",
       dpi = 300, width = 5.83, height = 5.83, units = "in")

### Examine correlations (if any). The sophisticated way to do this would be
### to use the tidyverse package purr to get subsets into a list and apply
### correlation to each in list. Will use the simple method of subsetting into
### 7 1-mont data sets and using the correlation function 7 times.

# ?subset
# Warning
# This is a convenience function intended for use interactively. For programming 
# it is better to use the standard subsetting functions like [, and in particular
# the non-standard evaluation of argument subset can have unanticipated consequences.

Apr <- combined4[combined4$Mnth.x == "Apr", ]
May <- combined4[combined4$Mnth.x == "May", ]
Jun <- combined4[combined4$Mnth.x == "Jun", ]
Jul <- combined4[combined4$Mnth.x == "Jul", ]
Aug <- combined4[combined4$Mnth.x == "Aug", ]
Sep <- combined4[combined4$Mnth.x == "Sep", ]
Oct <- combined4[combined4$Mnth.x == "Oct", ]

### Correlatons for Apr
cor.test(Apr$degf_avg,Apr$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Apr$degf_avg and Apr$Hr2
# S = 4732, p-value = 0.2426
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2092195 
nrow(Apr) # 33

### Correlatons for May
cor.test(May$degf_avg,May$Hr2, method = "spearman")
#      Spearman's rank correlation rho
# 
# Spearman's rank correlation rho
# 
# data:  May$degf_avg and May$Hr2
# S = 2741.1, p-value = 0.08554
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3248532 
nrow(May) #29

### Correlatons for Jun
cor.test(Jun$degf_avg,Jun$Hr2, method = "spearman")
#
# Spearman's rank correlation rho
# 
# data:  Jun$degf_avg and Jun$Hr2
# S = 19879, p-value = 0.07757
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2422598 
nrow(Jun) #54

### Correlatons for Jul
cor.test(Jul$degf_avg,Jul$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Jul$degf_avg and Jul$Hr2
# S = 681819, p-value = 0.4771
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#         rho 
# -0.05715582 
nrow(Jul) #157

### Correlatons for Aug
cor.test(Aug$degf_avg,Aug$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Aug$degf_avg and Aug$Hr2
# S = 2140442, p-value = 0.4672
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.04735413  
nrow(Aug) #238

### Correlatons for Sep
cor.test(Sep$degf_avg,Sep$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Sep$degf_avg and Sep$Hr2
# S = 431802, p-value = 0.07148
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1501308 
nrow(Sep) #145

### Correlatons for Oct
cor.test(Oct$degf_avg,Oct$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Oct$degf_avg and Oct$Hr2
# S = 3098.3, p-value = 0.4398
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1520793 
nrow(Oct) #28


