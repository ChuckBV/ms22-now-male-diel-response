#===========================================================================#
# script4_capture_by_time_of_day.R
# 
# Uses the "combined" data set created in script10 to explore relationships 
#
# PARTS
# 1. Load combined count and temperature data into memory (line 25)  
# 2. Examine captures after 7AM (before midnight) (line 64)  
# 3. Captures by hour compared between months (line 108)
#     Fig4.jpg
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(DescTools)
library(FSA)
library(rcompanion)
library(vcdExtra)

#---------------------------------------------------------------------------#
#-- 1. Load combined count and temperature data into memory --------
#---------------------------------------------------------------------------#

all <- read_csv("./data-intermediate/combined_count_temp_all.csv")
all
# A tibble: 36,055 x 8
#   site2    Yr Mnth.x Julian    Hr count Mnth.y degc_avg
#   <chr> <dbl> <chr>   <dbl> <dbl> <dbl> <chr>     <dbl>
# 1 A      2019 May       142    15     0 NA         NA  
# 2 A      2019 May       142    16     0 May        21.6
# 3 A      2019 May       142    17     0 May        19.8
# 4 A      2019 May       142    18     0 May        18.1

#---------------------------------------------------------------------------#
#-- 2. Compare daytime fliers between years         -------------------------
#---------------------------------------------------------------------------#

# Create variable marking off-hours 
all2 <- all %>% 
  mutate(offhrs = ifelse(Hr <= 7 | Hr >= 1800,"No","Yes"))
all2
# A tibble: 36,055 x 9
#   site2    Yr Mnth.x Julian    Hr count Mnth.y degc_avg offhrs
#   <chr> <dbl> <fct>   <dbl> <dbl> <dbl> <chr>     <dbl> <chr> 
# 1 A      2019 May       142    15     0 NA         NA   Yes   
# 2 A      2019 May       142    16     0 May        21.6 Yes   
# 3 A      2019 May       142    17     0 May        19.8 Yes   
# 4 A      2019 May       142    18     0 May        18.1 Yes 

# Examine time of flight be year (2 x 2 matrix)
x <- all2 %>% 
  group_by(Yr,offhrs) %>% 
  summarize(count = sum(count, na.rm = TRUE)) %>% 
  pivot_wider(names_from = offhrs, values_from = count)
x


y <- as.matrix(x[,2:3])
y
#        No Yes
# [1,] 2065 195
# [2,] 1223 111

prop.table(y,1) # 1 gives row percentages
# [1,] 0.9137168 0.08628319
# [2,] 0.9167916 0.08320840

chisq.test(y)
# 
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  y
# X-squared = 0.066168, df = 1, p-value = 0.797

#-- R has table functions that would apply a chi square test. Such a test
#-- might find a significant different between the years, but it would not
#-- be important when comparing 12 vs. 15%

all2 %>% 
  group_by(site2,offhrs) %>% 
  dplyr::summarize(count = sum(count, na.rm = TRUE)) %>% 
  pivot_wider(names_from = site2, values_from = count)
# A tibble: 2 x 11
#   offhrs     A    A1    A2    A3    A4    A5     B     C     D     E
#   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 No      1177   241   368   216   353    45   361   217    87   223
# 2 Yes      109    37    13    25    23    13    29    19    26    12

# somewhat consistent when there were many captures, more random when there
# were fewer

#---------------------------------------------------------------------------#
#-- 3. Captures by hour compared between months ----------------------------
#---------------------------------------------------------------------------#

# How many "bins" do we have for month?
all2 %>% 
  filter(Mnth.x != Mnth.y) 
  #-- Same thing, retained from both sources in merge

# Make Mnth.x into a factor to conserve order
all2$Mnth.x <- factor(all2$Mnth.x, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct"))

all2 %>% 
  group_by(Mnth.x) %>% 
  filter(Mnth.x %in% c("Apr","May","Jun","Jul","Aug","Sep","Oct")) %>% 
  dplyr::summarize(count = sum(count, na.rm = TRUE))
# A tibble: 7 x 2
#   Mnth.x count
#   <fct>  <dbl>
# 1 Apr      152
# 2 May      293
# 3 Jun      190
# 4 Jul      771
# 5 Aug      906
# 6 Sep     1133
# 7 Oct      143

# Zero hour moved from midnight to sundown. Since these are 
# pooled dates, it is unnecessary to adjust the date
all2$Hour <- ifelse(all2$Hr >= 18,all2$Hr - 18,all2$Hr + 6)
all2$offhrs <- NULL # outlived its usefulness
all2 <- all2[complete.cases(all2), ]

### For median and kruskal-wallis, I need 1 record per moth

# Examine number of moths associated with records
sort(unique(all2$count))
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 16 17 19 20 21 22 23 24 25 26 27 32 35 56

all2_case_frm <- vcdExtra::expand.dft(all2, freq = "count") # 3501, 0s dropped

kruskal.test(Hour ~ Mnth.x, data = all2_case_frm)
# data:  Hour by Month
# Kruskal-Wallis chi-squared = 262.83, df = 6, p-value < 2.2e-16

Desc(Hour ~ Mnth.x, data = all2_case_frm)
# Hour ~ Mnth.x (all2_case_frm)
# 
# Summary: 
#   n pairs: 3'501, valid: 3'501 (100.0%), missings: 0 (0.0%), groups: 7
# 
# 
# Apr      Aug      Jul      Jun      May      Oct      Sep
# mean      9.496   10.810   10.372   11.300    9.812    6.469   10.452
# median   10.000   11.000   11.000   11.000   10.000    6.000   11.000
# sd        2.838    1.967    1.517    3.816    2.178    3.851    2.516
# IQR       3.000    1.000    1.000    1.000    2.500    5.000    2.000
# n           123      906      751      190      255      143    1'133
# np       3.513%  25.878%  21.451%   5.427%   7.284%   4.085%  32.362%
# NAs           0        0        0        0        0        0        0
# 0s            0        2        1        0        0        3        1
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 276, df = 6, p-value < 2.2e-16

PT = FSA::dunnTest(Hour ~ Mnth.x,
                   data = all2_case_frm,
                   method = "bonferroni")

PT = PT$res

PT
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Bonferroni method.
# 
#    Comparison           Z      P.unadj        P.adj
# 1   Apr - Aug  -7.5521613 4.280939e-14 8.989971e-13
# 2   Apr - Jul  -5.3471024 8.937344e-08 1.876842e-06
# 3   Aug - Jul   4.1660705 3.098949e-05 6.507794e-04
# 4   Apr - Jun  -5.5490496 2.872266e-08 6.031760e-07
# 5   Aug - Jun   1.0467315 2.952234e-01 1.000000e+00
# 6   Jul - Jun  -1.5031686 1.327956e-01 1.000000e+00
# 7   Apr - May  -2.5377331 1.115730e-02 2.343034e-01
# 8   Aug - May   6.3072284 2.840764e-10 5.965605e-09
# 9   Jul - May   3.3323753 8.610804e-04 1.808269e-02
# 10  Jun - May   3.7938854 1.483082e-04 3.114471e-03
# 11  Apr - Oct   4.9597116 7.059793e-07 1.482556e-05
# 12  Aug - Oct  14.8433465 7.682586e-50 1.613343e-48
# 13  Jul - Oct  12.3855197 3.130504e-35 6.574059e-34
# 14  Jun - Oct  11.3100870 1.169735e-29 2.456443e-28
# 15  May - Oct   8.5047714 1.819537e-17 3.821029e-16
# 16  Apr - Sep  -6.1150894 9.650287e-10 2.026560e-08
# 17  Aug - Sep   3.2572520 1.124965e-03 2.362427e-02
# 18  Jul - Sep  -1.2840196 1.991351e-01 1.000000e+00
# 19  Jun - Sep   0.7863973 4.316348e-01 1.000000e+00
# 20  May - Sep  -4.3563027 1.322778e-05 2.777834e-04
# 21  Oct - Sep -13.4144662 4.975026e-41 1.044755e-39

x <- rcompanion::cldList(comparison = PT$Comparison,
                         p.value = PT$P.adj,
                         threshold = 0.05)


x
# Group Letter MonoLetter
# 1   Apr      a       a   
# 5   May      a       a   
# 4   Jun     bc        bc 
# 3   Jul      c         c 
# 2   Aug      b        b  
# 7   Sep      c         c 
# 6   Oct      d          d


#---------------------------------------------------------------------------#
#-- 4. Generate boxplot to compare by month      ----------------------------
#---------------------------------------------------------------------------#

all2_case_frm$Mnth.x <- factor(all2_case_frm$Mnth.x, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct"))

all2_case_frm

p0 <- ggplot(all2_case_frm, aes(x = Mnth.x, y = Hour)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Month") +
  ylab("Hour from 18:00") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p0

ggsave(filename = "Fig4.jpg", plot = p0, device = "jpg", path = "./results",
       dpi = 300, width = 5.83, height = 4.5, units = "in")




  

