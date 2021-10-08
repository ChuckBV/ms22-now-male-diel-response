#===========================================================================#
# script11_explore_count_and_temperature.R
# 
# Uses the "combined" data set created in script10 to explore relationsips 
#
# PARTS
# 1. Basic questions about combined data set (multiple obs/night?)(line 14)  
# 2. Examine captures after 7AM (before midnight) (line 64)  
# 3. Captures by hour compared between months (line 108)
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

# Create variable marking off-hours 
combined2 <- combined %>% 
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
  summarise(nObs = n()) %>% 
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
  summarise(nObs = n())

# Make Mnth.x into a factor to conserve order
combined2$Mnth.x <- factor(combined2$Mnth.x, levels = c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"))
combined2

### Problem: for median and kruskal-wallis, I need 1 record per moth

# Examine number of moths associated with records
sort(unique(combined2$pest_dif))
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 16 17 19 20 21 22 23 24 25 26 27 32 35 56

combined2 <- combined2[combined2$Mnth.x != "Mar", ]
droplevels(combined2$Mnth.x)

combined2$Hour <- ifelse(combined2$Hr >= 18,combined2$Hr - 18,combined2$Hr + 6)

# simplify frequency table then expand
combined3 <- combined2 %>% 
  select(Mnth.x,Hour,pest_dif) %>% 
  rename(Month = Mnth.x,
         Frequency = pest_dif)

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

ggsave(filename = "boxplot_hour_vs_month.jpg", plot = p0, device = "jpg", path = "./results",
       dpi = 300, width = 5.83, height = 4.5, units = "in")



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
  summarise(counts = sum(pest_dif))

sort(unique(y$Hr))
# [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

# Shift start time of clock
y <- y %>% 
  mutate(Hour = ifelse(Hr >= 18,Hr - 18,Hr + 6))
  # now verify
test <- y %>% 
  group_by(Hr,Hour) %>% 
  summarise(nObs = n())



p1 <- ggplot(data = y, aes(x = Hour, y = counts)) +
  geom_col() +
  facet_grid(Mnth.x ~ ., scales = "free_y") +
  theme_bw() +
  xlab("Hour of day") +
  ylab("Adults captured") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p1

ggsave(filename = "Fig_cap_v_time_of_day.jpg", plot = p1, device = "jpg", path = "./results",
       dpi = 300, width = 5.83, height = 9.4, units = "in")

# ggsave(filename = "Fig_cap_v_time_of_day.eps", plot = p1, device = "eps", path = "./results", 
#        dpi = 300, width = 5.83, height = 9.4, units = "in")
#-- EntSoc maximum is 9.4 inches high. For jpg saved for ease of use in draft,
#-- but eps preferable for production


# Add temperature
ggplot(data = combined2, aes(x = Hour, y = degf_avg)) +
  geom_point() +
  facet_grid(Mnth.x ~ .)

### Compare median hour per day vs temperature by month
combined4 <- combined2 %>% 
  select(Yr,Julian,Mnth.x,Hour,degf_avg,pest_dif)
  # now in frequency table form

combined5 <- vcdExtra::expand.dft(combined4, freq = "pest_dif")

combined5 <- combined5 %>% 
  group_by(Yr,Julian) %>% 
  dplyr::summarise(Hour = median(Hour))

# Merge temperature dat back in
combined5 <- left_join(combined5,combined4)

combined5 <- combined5[complete.cases(combined5), ]

p2 <- ggplot(combined5, aes(x = Hour, y = degf_avg)) +
  geom_point() +
  facet_wrap(vars(Mnth.x), ncol = 3, nrow = 4) +
  theme_bw() +
  xlab("Median capture time (Hours after sunset)") +
  ylab("Degrees F at time of median capture") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p2

ggsave(filename = "Temp_v_time_med_capture_by_month.jpg", plot = p2, device = "jpg", path = "./results",
       dpi = 300, width = 5.83, height = 5.83, units = "in")

#-- Need to use purr or similar to examine correlation by month

### Ask if there is correlation between temperature and captures during 
### daylight hours (not a useful question)

daylt <- combined2 %>% 
  filter(Hr > 7 & Hr < 18)

p2 <- ggplot(data = daylt) +
  geom_bar(aes(x = Mnth.x)) +
  theme_bw() +
  xlab("Month") +
  ylab("Adults captured during daylight hours") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p2

ggsave(filename = "fig_daytime_fliers_by_month.jpg", 
      plot = p2, device = "jpg", path = "./results", 
      dpi = 300, width = 5.83, height = 3.9, units = "in")  


ggplot(daylt, aes(x = degf_avg, y = Hr)) +
  geom_point(aes(colour = Mnth.x))
#-- color does not clarify

p3 <- ggplot(daylt, aes(x = Hr, y = degf_avg)) +
  geom_point() +
  facet_grid(Mnth.x ~ .) +
  theme_bw() +
  xlab("Hour of day") +
  ylab("Degrees Fahrenheit") +
  theme(axis.text.x = element_text(color = "black", size = 8),# angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8))

p3

ggsave(filename = "fig_daytime_fliers_by_hour_and_temp.jpg", 
       plot = p3, device = "jpg", path = "./results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  
#-- On average, gets hotter as hours since sunup increases.
#-- Given this factor, this graphic suggests no relationship
#-- between temperature and hour of capture for daytime fliers


  

