#===========================================================================#
# script11_explore_count_and_temperature.R
# 
# Uses the "combined" data set created in script10 to explore relationsips 
#
# PARTS
# 1. Basic questions about combined data set (multiple obs/night?)(line 14)  
# 2. Examine captures after 7AM (before midnight) (line 64)  
# 3. Captures by hour compared between months (line 108)
#     Fig4.jpg
#
# combined--imported as created in script10
# combined2--adds categorical variable for off hours
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
all2 <- combined %>% 
  mutate(offhrs = ifelse(Hr <= 7,"No","Yes"))

# Examine time of flight be year (2 x 2 matrix)
x <- combined2 %>% 
  group_by(Yr,offhrs) %>% 
  summarise(nObs = n())

x <- table(combined2$Yr,combined2$offhrs)
x

#       No Yes
# 2019 646 114
# 2020 586  82

prop.table(x,1) # 1 gives row percentages

#       No       Yes
# 2019 0.8500000 0.1500000
# 2020 0.8772455 0.1227545

#-- R has table functions that would apply a chi square test. Such a test
#-- might find a significant different between the years, but it would not
#-- be important when comparing 12 vs. 15%

combined2 %>% 
  group_by(site,offhrs) %>% 
  dplyr::summarise(nObs = n()) %>% 
  pivot_wider(names_from = site, values_from = nObs)
# A tibble: 2 x 10
#   offhrs mikewoolf1 mikewoolf2 mikewoolf3 mikewoolf4 mikewoolf5 MWoolf_east Perez UCKearney  usda
#   <chr>       <int>      <int>      <int>      <int>      <int>       <int> <int>     <int> <int>
# 1 No            406        137        133        171         24          95    96        66   104
# 2 Yes            88         13         20         21         10          16     7        10    11

# somewhat consistent when there were many captures, more random when there
# were fewer

#-- 3. Captures by hour compared between months ----------------------------

# How many "bins" do we have for month?
combined2 %>% 
  filter(Mnth.x != Mnth.y) 
  #-- Same thing, retained from both sources in merge

combined2 %>% 
  group_by(Mnth.x) %>% 
  dplyr::summarise(nObs = n())

# Make Mnth.x into a factor to conserve order
combined2$Mnth.x <- factor(combined2$Mnth.x, levels = c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"))
combined2

### Problem: for median and kruskal-wallis, I need 1 record per moth

# Examine number of moths associated with records
sort(unique(combined2$pest_dif))
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 16 17 19 20 21 22 23 24 25 26 27 32 35 56

# Two moths in March, so drop these obs and that category
combined2 <- combined2[combined2$Mnth.x != "Mar", ]
droplevels(combined2$Mnth.x)

# Zero hour moved from midnight to sundown. Since these are 
# pooled dates, it is unecessary to adjust the date
combined2$Hour <- ifelse(combined2$Hr >= 18,combined2$Hr - 18,combined2$Hr + 6)

combined3 <- combined2 %>% 
  select(Mnth.x,Hour,pest_dif) %>% 
  rename(Month = Mnth.x,
         Frequency = pest_dif)
# Exapanding the data from a frequency table (# of obs by caetogory) to case form
# (1 observation per moth)
combined3 <- expand.dft(combined3, freq = "Frequency")

combined3$Month <- factor(combined3$Month, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct"))

Desc(Hour ~ Month, data = combined3)

PT = FSA::dunnTest(Hour ~ Month,
                   data = combined3,
                   method = "bonferroni")

PT

PT = PT$res

rcompanion::cldList(comparison = PT$Comparison,
        p.value = PT$P.adj,
        threshold = 0.05)
# Group Letter MonoLetter
# 1   Apr      a       a   
# 2   Aug      b        b  
# 3   Jul      c         c 
# 4   Jun     bc        bc 
# 5   May      a       a   
# 6   Oct      d          d
# 7   Sep      c         c 

p0 <- ggplot(combined2, aes(x = Mnth.x, y = Hour)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Month") +
  ylab("Hour from sunset") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p0

ggsave(filename = "Fig4.jpg", plot = p0, device = "jpg", path = "./results",
       dpi = 300, width = 5.83, height = 4.5, units = "in")




  

