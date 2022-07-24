#===========================================================================#
# script6_time_of_p25_capture_vs_temperature.R
#
# Same thing as script5, except examining hour of p25 capture rather than
# hour of p50
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
#===========================================================================#

library(tidyverse)
library(lubridate)
library(scales)

#---------------------------------------------------------------------------#
#-- 1. df "all" -- Pool non-zero readings by month and hour of day ---------#
#-- 2. df "all2" shift 0 hour back 6 hrs fom 2400 to 1800 ------------------#
#-- 3. df "all3" Expand from frequency to case form  -----------------------#

#   Expanded case form saved in previous iteration, so start from here

all3 <- read_csv("./data-intermediate/counts_all_case_form.csv")

#---------------------------------------------------------------------------#
#-- 4. df "all4" Collapses back to nightly median & add back temp dat -------
#---------------------------------------------------------------------------#

all4 <- all3 %>% 
  group_by(Yr,Julian2,site2) %>% 
  dplyr::summarise(Hr2 = quantile(Hr2, 0.25))
all4
#      Yr Julian2 site2   Hr2
#   <int>   <int> <chr> <dbl>
# 1  2019     113 C      11  
# 2  2019     114 C      11  
# 3  2019     115 D      19  
# 4  2019     116 C       9  
# 5  2019     117 C       9  
# 6  2019     119 C      11  

# all4$Hr2_flr <- floor(all4$Hr2)
# all4[all4$Hr2 != all4$Hr2_flr, ]
# A tibble: 127 x 5
# Groups:   Yr, Julian2 [99]
#      Yr Julian2 site2   Hr2 Hr2_flr
#   <dbl>   <dbl> <chr> <dbl>   <dbl>
# 1  2019     116 C      5.5        5
# 2  2019     123 C      8.25       8
# 3  2019     128 C      8.25       8
# 4  2019     142 A      7.25       7

#   In 22% of cases, the quantile(x, 0.25) function returned a 0.5 value

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
# A tibble: 557 x 6
# Groups:   Yr, Julian2 [242]
#      Yr Julian2 site2   Hr2 Mnth  degc_avg
#   <dbl>   <dbl> <chr> <dbl> <chr>    <dbl>
# 1  2019     142 A         7 May       11.5
# 2  2019     143 A        17 May       24.2


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

ggsave(filename = "suppl_fig1.jpg", plot = p1, device = "jpg", path = "./results",
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
# 
# Spearman's rank correlation rho
# 
# data:  Apr$degc_avg and Apr$Hr2
# S = 3831.9, p-value = 0.2185
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2274432 
nrow(Apr) # 30

### Correlations for May
cor.test(May$degc_avg,May$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  May$degc_avg and May$Hr2
# S = 3155.6, p-value = 0.1097
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2979796 
nrow(May) #30

### Correlations for Jun
cor.test(Jun$degc_avg,Jun$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Jun$degc_avg and Jun$Hr2
# S = 18655, p-value = 0.4715
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1041864 
nrow(Jun) #50

### Correlations for Jul
cor.test(Jul$degc_avg,Jul$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Jul$degc_avg and Jul$Hr2
# S = 348072, p-value = 0.0987
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1501906 
nrow(Jul) #122

### Correlations for Aug
cor.test(Aug$degc_avg,Aug$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Aug$degc_avg and Aug$Hr2
# S = 1178584, p-value = 0.06728
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# -0.135197 
nrow(Aug) #186

### Correlations for Sep
cor.test(Sep$degc_avg,Sep$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Sep$degc_avg and Sep$Hr2
# S = 203709, p-value = 0.106
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1528517 
nrow(Sep) #113

### Correlatons for Oct
cor.test(Oct$degc_avg,Oct$Hr2, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  Oct$degc_avg and Oct$Hr2
# S = 3508.8, p-value = 0.7246
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#         rho 
# -0.07106818 
nrow(Oct) #27


