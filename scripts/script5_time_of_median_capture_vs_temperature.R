#===========================================================================#
# script5_time_of_median_capture_vs_temperature.R
# 
# PARTS
# 1. df "all" -- Pool non-zero readings by month and hour of day(line 26)  
# 2. df "all2" shift 0 hour back 6 hrs fom 2400 to 1800 (line 64)  
# 3. df "all3" Expand from frequency to case form and saves (line 105)
# 4. df "all4", hour of median count by night (lin3 130)
# 5. df "all5" reads back temperature date and time shifts (line 167)
# 6. df "all6", left_join merge all4,all5 (line 211)
# 7. Scatter plot of hr median capture vs temperature (line 232)
# 8. Spearman correlation of hour of median capture with temperature
#
# all--imported as created in script2
# combined3
#  - Night only
#  - Only first obs of night for that site
#  - Additional variables adjust 0 hour to sundown and keep next sun-up in
#    the same Julian day
# combined4
#  - All captures per night
#  - Additional variables to adjust 0 hour to sundown and keep next sun-up 
#    in the same Julian day (as in 3)
#  - Used to examine whether captures are less likely overall on hotter
#    nights
#
# Fig 5, Median capture vs. tempature by month
#  
#===========================================================================#

library(tidyverse)
library(lubridate)

#---------------------------------------------------------------------------#
#-- 1. df "all" -- Pool non-zero readings by month and hour of day ---------
#---------------------------------------------------------------------------#

# Input is all readings taken over 2 years, including many reports of 
# zero moths captured. Reduces from 36,055 to 168 observations.

all <- read_csv("./data-intermediate/combined_count_temp_all.csv")
all
# A tibble: 36,055 x 8
#   site2    Yr Mnth.x Julian    Hr count Mnth.y degc_avg
#   <chr> <dbl> <chr>   <dbl> <dbl> <dbl> <chr>     <dbl>
# 1 A      2019 May       142    15     0 NA         NA  
# 2 A      2019 May       142    16     0 May        21.6
# 3 A      2019 May       142    17     0 May        19.8

# Pools all moths captured in 5 traps over 2 years by month and hour of capture

all <- all %>% 
  group_by(Yr,Mnth.x,Julian,Hr,site2) %>% 
  summarize(count = sum(count, na.rm = TRUE)) %>% 
  filter(Mnth.x %in% c("Apr","May","Jun","Jul","Aug","Sep","Oct")) %>% 
  rename(Mnth = Mnth.x) %>% 
  filter(count > 0)
all
# A tibble: 1,224 x 6
# Groups:   Yr, Mnth, Julian, Hr [833]
#      Yr Mnth  Julian    Hr site2 count
#   <dbl> <chr>  <dbl> <dbl> <chr> <dbl>
# 1  2019 Apr      114     5 C        12
# 2  2019 Apr      115     5 C         2
# 3  2019 Apr      115     6 C         1
# 4  2019 Apr      116    13 D         7

# Make Mnth into a factor to conserve order

all$Mnth <- factor(all$Mnth, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct"))
all
levels(all$Mnth)

#---------------------------------------------------------------------------#
#-- 2. df "all2" shift 0 hour back 6 hrs fom 2400 to 1800 ------------------
#---------------------------------------------------------------------------#

### Algorithm to shift time and date. This is how we did the time shift in
### script11. See also "julian_date_and_hour_scratch.R". Note that numbering
### of "combined" data sets in this script is different from script11--don't
### switch back and forth.

### Anything after midnight and before sundown (6PM) has the Julian date from 
### the previous day. See "algorithm_julian_date_and_hour.R"
### Anything after midnight and before sundown (6PM) has the Julian date from 
### the previous day

all2 <- all %>% 
  dplyr::mutate(Hr2 = ifelse(Hr >= 18, Hr - 18, Hr + 6),
                Julian2 = ifelse(Hr >= 18, Julian, Julian - 1)) %>% 
  dplyr::arrange(Julian,Hr)
all2

all2[!complete.cases(all2), ]
#-- Tibble of 0, all complete cases

all2
# A tibble: 1,224 x 8
# Groups:   Yr, Mnth, Julian, Hr [833]
#      Yr Mnth  Julian    Hr site2 count   Hr2 Julian2
#   <dbl> <fct>  <dbl> <dbl> <chr> <dbl> <dbl>   <dbl>
# 1  2020 Apr      102     2 A4        1     8     101
# 2  2020 Apr      102     4 A4        1    10     101
# 3  2020 Apr      103     3 A4        1     9     102
# 4  2020 Apr      105     4 A2        1    10     104

#---------------------------------------------------------------------------#
#-- 3. df "all3" Expand from frequency to case form  -----------------------
#---------------------------------------------------------------------------#

### Get median observation for Julian2. In order to be certain that median
### functions correctly, go from frequency table form (moths per hour of)
### observations) to case format (one observation for each moth)

all3 <- vcdExtra::expand.dft(all2, freq = "count")
#-- goes from 1224 to 3588 observations
all3[!complete.cases(all3), ]
#-- Tibble of 0, all complete cases
all3
# A tibble: 3,588 x 7
# Groups:   Yr, Mnth, Julian, Hr [833]
#       Yr Mnth  Julian    Hr site2   Hr2 Julian2
#   <int> <chr>  <int> <int> <chr> <int>   <int>
# 1  2020 Apr      102     2 A4        8     101
# 2  2020 Apr      102     4 A4       10     101
# 3  2020 Apr      103     3 A4        9     102

### vcdExtra cranks awhile to get this, so export this
write.csv(all3,"./data-intermediate/counts_all_case_form.csv",
          row.names = FALSE)

#---------------------------------------------------------------------------#
#-- 4. df "all4" Collapses back to nightly median & add back temp dat -------
#---------------------------------------------------------------------------#

all4 <- all3 %>% 
  group_by(Yr,Julian2,site2) %>% 
  dplyr::summarise(Hr2 = median(Hr2))
all4
#      Yr Julian2 site2   Hr2
#   <int>   <int> <chr> <dbl>
# 1  2019     113 C      11  
# 2  2019     114 C      11  
# 3  2019     115 D      19  
# 4  2019     116 C       9  
# 5  2019     117 C       9  
# 6  2019     119 C      11  

all4[!complete.cases(all4), ]
#-- Tibble of 0, all complete cases

# all4$Hr2_flr <- floor(all4$Hr2)
# all4[all4$Hr2 != all4$Hr2_flr, ]
# A tibble: 64 x 5
# Groups:   Yr, Julian2 [58]
#      Yr Julian2 site2   Hr2 Hr2_flr
#   <int>   <int> <chr> <dbl>   <dbl>
# 1  2019     121 C       8.5       8
# 2  2019     128 C       8.5       8
# 3  2019     147 C      16.5      16

#   In 12% of cases, the median function returned a 0.5 value

all4$Hr2 <- floor(all4$Hr2)

#  All median values rounded down

#---------------------------------------------------------------------------#
#-- 5. df "all5" reads back temperature date and time shifts ---------------
#---------------------------------------------------------------------------#

### Need time shift all again and get temperature data to merge

all <- read_csv("./data-intermediate/combined_count_temp_all.csv")
all
# A tibble: 36,055 x 8
#   site2    Yr Mnth.x Julian    Hr count Mnth.y degc_avg
#   <chr> <dbl> <chr>   <dbl> <dbl> <dbl> <chr>     <dbl>
# 1 A      2019 May       142    15     0 NA         NA  
# 2 A      2019 May       142    16     0 May        21.6
# 3 A      2019 May       142    17     0 May        19.8


all5 <- all %>%
  select(Yr,Mnth.x,Julian,Hr,site2,degc_avg) %>% 
  filter(Mnth.x %in% c("Apr","May","Jun","Jul","Aug","Sep","Oct")) %>% 
  rename(Mnth = Mnth.x)
all5

all5 <- all5 %>% 
  dplyr::mutate(Hr2 = ifelse(Hr >= 18, Hr - 18, Hr + 6),
                Julian2 = ifelse(Hr >= 18, Julian, Julian - 1)) %>% 
  dplyr::arrange(Yr,Julian,Hr) %>% 
  filter(!is.na(degc_avg)) %>% 
  select(-c(Julian,Hr))
all5
# A tibble: 33,376 x 6
#      Yr Mnth  site2 degc_avg   Hr2 Julian2
#   <dbl> <chr> <chr>    <dbl> <dbl>   <dbl>
# 1  2019 May   D         13.4    23     135
# 2  2019 May   D         14.2     0     136

all4
# A tibble: 584 x 4
# Groups:   Yr, Julian2 [261]
#     Yr Julian2 site2   Hr2
#   <int>   <int> <chr> <dbl>
# 1  2019     113 C      11  
# 2  2019     114 C      11  
# 3  2019     115 D      19  

#---------------------------------------------------------------------------#
#-- 6. df "all6" merges temperature on hr of median capture ---------------
#---------------------------------------------------------------------------#

all6 <- left_join(all4,all5)

all6 <- all6[complete.cases(all6), ] 
#   Mnth and degc_avg missing in some cases. 584 to 558 observations 

all6
# A tibble: 558 x 6
# Groups:   Yr, Julian2 [242]
#      Yr Julian2 site2   Hr2 Mnth  degc_avg
#   <dbl>   <dbl> <chr> <dbl> <chr>    <dbl>
# 1  2019     142 A         8 May       12.2
# 2  2019     143 A        17 May       24.2
# 3  2019     144 A        11 May       12.5
# 4  2019     147 A         8 May       12.4


#---------------------------------------------------------------------------#
#-- 7. Scatter plot of hr median capture vs temperature       ---------------
#---------------------------------------------------------------------------#

### Make Mnth an ordered factor
all6$Mnth <- factor(all6$Mnth, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct"))

### Now plot and determine correlations
p1 <- ggplot(all6, aes(x = degc_avg, y = Hr2)) +
  geom_point() +
  facet_wrap(vars(Mnth), ncol = 3, nrow = 4) +
  theme_bw() +
  xlab("Degrees C at time of median capture") +
  ylab("Median capture time (Hours after sunset)") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p1

ggsave(filename = "Fig5.jpg", plot = p1, device = "jpg", path = "./results",
       dpi = 300, width = 5.83, height = 5.83, units = "in")

#---------------------------------------------------------------------------#
#-- 8. Determine Sprearman correlation by month             ---------------
#---------------------------------------------------------------------------#

### Examine correlations (if any). The sophisticated way to do this would be
### to use the tidyverse package purr to get subsets into a list and apply
### correlation to each in list. Will use the simple method of subsetting into
### 7 1-mont data sets and using the correlation function 7 times.

# ?subset
# Warning
# This is a convenience function intended for use interactively. For programming 
# it is better to use the standard subsetting functions like [, and in particular
# the non-standard evaluation of argument subset can have unanticipated consequences.

Apr <- all6[all6$Mnth == "Apr", ]
May <- all6[all6$Mnth == "May", ]
Jun <- all6[all6$Mnth == "Jun", ]
Jul <- all6[all6$Mnth == "Jul", ]
Aug <- all6[all6$Mnth == "Aug", ]
Sep <- all6[all6$Mnth == "Sep", ]
Oct <- all6[all6$Mnth == "Oct", ]

### Correlations for Apr
cor.test(Apr$degc_avg,Apr$Hr2, method = "spearman")
# Spearman's rank correlation rho
# 
# data:  Apr$degc_avg and Apr$Hr2
# S = 3678, p-value = 0.3364
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1817484 
nrow(Apr) # 30

### Correlations for May
cor.test(May$degc_avg,May$Hr2, method = "spearman")
# Spearman's rank correlation rho
# 
# data:  May$degc_avg and May$Hr2
# S = 2917.6, p-value = 0.05726
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3509161 
nrow(May) #30

### Correlations for Jun
cor.test(Jun$degc_avg,Jun$Hr2, method = "spearman")
# Spearman's rank correlation rho
# 
# data:  Jun$degc_avg and Jun$Hr2
# S = 16001, p-value = 0.1055
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2316619 
nrow(Jun) #50

### Correlations for Jul
# cor.test(Jul$degc_avg,Jul$Hr2, method = "spearman")
# Spearman's rank correlation rho
# 
# data:  Jul$degc_avg and Jul$Hr2
# S = 341525, p-value = 0.1582
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1285577 
nrow(Jul) #122

### Correlations for Aug
cor.test(Aug$degc_avg,Aug$Hr2, method = "spearman")
# Spearman's rank correlation rho
# 
# data:  Aug$degc_avg and Aug$Hr2
# S = 1057035, p-value = 0.8457
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.0143686 
nrow(Aug) #186

### Correlations for Sep
cor.test(Sep$degc_avg,Sep$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Sep$degc_avg and Sep$Hr2
# S = 190925, p-value = 0.02859
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2060131 
nrow(Sep) #113

### Correlatons for Oct
cor.test(Oct$degc_avg,Oct$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Oct$degc_avg and Oct$Hr2
# S = 3672.8, p-value = 0.5473
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1211338 
nrow(Oct) #27


