#===========================================================================#
# script13_low_temp_and_capture.R
# 
# Examine the hypothesis that, above a certain nightly low temperature,
# the capture per night drops off
#
#===========================================================================#

library(tidyverse)
library(lubridate)

# Need to examine data by night using the counts and temperature data merged
# in script10, but need to re-define night as done with Julian2 and Hour2
# in script12

#-- 1. Get daily summary of July and August count data ----------------------

# Load original count data files into memory
allsites19 <- read.csv("./data/allsites_y19_scrubbed.csv") 
allsites20 <- read.csv("./data//allsites_y20_scrubbed.csv")

allsites19$site[allsites19$site == "MWoolf_west"] <- "mikewoolf1"

counts_y19y20 <- rbind(allsites19,allsites20)
counts_y19y20$datetime <- as.POSIXct(counts_y19y20$datetime)
counts_y19y20 <- counts_y19y20 %>% 
  filter(reviewed != "No") %>% 
  select(datetime,pest_dif,site) %>% 
  mutate(Yr = year(datetime),
         Mnth = month(datetime, label = TRUE, abbr = TRUE),
         Julian = yday(datetime),
         Hr = hour(datetime))

# Reduce to July and Aug
jul_aug <- counts_y19y20 %>% 
  filter(Mnth %in% c("Jul","Aug"))

# Incorporate modified Julian date and hour
jul_aug <- jul_aug %>% 
  mutate(Julian2 = ifelse(Hr < 18, Julian - 1, Julian),
         Hr2 = ifelse(Hr < 18, Hr + 6, Hr - 18))
# different from script12 because all hours included
head(jul_aug)
#                datetime pest_dif      site   Yr Mnth Julian Hr Julian2 Hr2
# 1   2019-07-01 00:29:00        0 UCKearney 2019  Jul    182  0     181   6
# 2   2019-07-01 06:00:00        0 UCKearney 2019  Jul    182  6     181  12
# 3   2019-07-01 06:30:00        0 UCKearney 2019  Jul    182  6     181  12
# 4   2019-07-01 07:00:00        0 UCKearney 2019  Jul    182  7     181  13
# 5   2019-07-01 07:31:00        0 UCKearney 2019  Jul    182  7     181  13
# 6   2019-07-01 07:56:00        0 UCKearney 2019  Jul    182  7     181  13

# take count from hour to daily
jul_aug <- jul_aug %>% 
  group_by(Yr,Mnth,site,Julian2) %>% 
  summarise(now_pr_day = sum(pest_dif, na.rm = TRUE))
head(jul_aug)
# A tibble: 6 x 5
# Groups:   Yr, Mnth, site [1]
#      Yr Mnth  site        Julian2 now_pr_day
#   <dbl> <ord> <chr>         <dbl>      <int>
# 1  2019 Jul   MWoolf_east     181          0
# 2  2019 Jul   MWoolf_east     182          0
# 3  2019 Jul   MWoolf_east     183          0
# 4  2019 Jul   MWoolf_east     184          2
# 5  2019 Jul   MWoolf_east     185         18
# 6  2019 Jul   MWoolf_east     186         28

jul_aug19 <- jul_aug %>% 
  filter(Yr == 2019)

ggplot(jul_aug19, aes(x = Julian2, y = now_pr_day)) +
  geom_line() +
  facet_grid(site ~ .)

jul_aug20 <- jul_aug %>% 
  filter(Yr == 2020)

ggplot(jul_aug20, aes(x = Julian2, y = now_pr_day)) +
  geom_line() +
  facet_grid(site ~ .)


#-- 2. Get daily summary of low temperature for merge onto count data -------

# Get temps  using script10
alltemps19 <- readr::read_csv("./data/trapview_temps_degf_y19.csv")
alltemps20 <- readr::read_csv("./data/trapview_temps_degf_y20.csv")

alltemps20$site[alltemps20$site == "MWT1"] <- "mikewoolf1"
alltemps20$site[alltemps20$site == "MWT2"] <- "mikewoolf2"
alltemps20$site[alltemps20$site == "MWT3"] <- "mikewoolf3"
alltemps20$site[alltemps20$site == "MWT4"] <- "mikewoolf4"
alltemps20$site[alltemps20$site == "MWT5"] <- "mikewoolf5"

alltemps19 <- alltemps19 %>% 
  dplyr::rename(datetime = Date_time)
alltemps20 <- alltemps20 %>% 
  dplyr::rename(datetime = Date_time)

alltemps19$site[alltemps19$site == "MWoolf_west"] <- "mikewoolf1"

temps <- rbind(alltemps19,alltemps20)

# Get Yr, Julian, Hr, Julian2, and Hr2 in temps data set
temps <- temps %>% 
  mutate(Yr = year(datetime),
         Mnth = month(datetime, label = TRUE, abbr = TRUE),
         Julian = yday(datetime),
         Hr = hour(datetime))

temps <- temps %>% 
  mutate(Julian2 = ifelse(Hr < 18, Julian - 1, Julian),
         Hr2 = ifelse(Hr < 18, Hr + 6, Hr - 18))
  # different from script12 because all hours included
temps

# Question: What time of day had the most low temperatures?
lo_temp <- temps %>% 
  group_by(Yr,site,Mnth,Julian2) %>% 
  summarise(min_temp = min(degf_lo))

lo_temp2 <- left_join(lo_temp,temps) %>% 
  filter(min_temp == degf_lo)
lo_temp2
#      Yr site       Mnth  Julian2 min_temp datetime            degf_avg degf_lo degf_hi rh_avg Julian    Hr   hr2
#   <dbl> <chr>      <ord>   <dbl>    <dbl> <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>  <dbl> <int> <dbl>
# 1  2019 mikewoolf1 May       141     64.9 2019-05-22 17:00:00     67.6    64.9    70     57.1    142    17    23
# 2  2019 mikewoolf1 May       142     51.1 2019-05-23 05:00:00     51.4    51.1    51.8   97.5    143     5    11
# 3  2019 mikewoolf1 May       143     51.1 2019-05-24 05:00:00     52.0    51.1    53.4   90.9    144     5    11

p1 <- ggplot(lo_temp2) +
  geom_bar(aes(x = Hr)) +
  theme_bw() +
  xlab("Hour from midnight")

p1

ggsave(filename = "temp_by_hr_histo.jpg", 
       plot = p1, device = "jpg", path = "./results", 
       dpi = 300, width = 2.83, height = 1.7, units = "in")  
#-- Answer: Usually 5-6AM, but with some variation

lo_temp2 <- lo_temp2 %>% 
  mutate(Date = as.Date(datetime)) %>% 
  select(Yr,site,Mnth,Date,Julian2,degf_lo)
lo_temp2

jul_aug_comb <- left_join(jul_aug,lo_temp2)
jul_aug_comb
# A tibble: 701 x 7
# Groups:   Yr, Mnth, site [20]
#      Yr Mnth  site       Julian2 now_pr_day Date       degf_lo
#   <dbl> <ord> <chr>        <dbl>      <int> <date>       <dbl>
# 1  2019 Jul   mikewoolf1     181          0 2019-07-01    56.5
# 2  2019 Jul   mikewoolf1     182          0 2019-07-02    55.4
# 3  2019 Jul   mikewoolf1     183          0 2019-07-03    55.4

p2 <- ggplot(jul_aug_comb, aes(x = degf_lo, y = now_pr_day)) +
  geom_point() +
  theme_bw() +
  xlab("Min. nightly temp. (F)") +
  ylab("Adults captured \nper night") +
  theme_bw() 
  

p2

ggsave(filename = "now_per_night_by_min_temp_jul_aug.jpg", 
       plot = p2, device = "jpg", path = "./results", 
       dpi = 300, width = 5.83, height = 3.5, units = "in")  


cor.test(jul_aug_comb$now_pr_day,jul_aug_comb$degf_lo,
         method = "spearman") 
# Spearman's rank correlation rho
# 
# data:  jul_aug_comb$now_pr_day and jul_aug_comb$degf_lo
# S = 53793941, p-value = 0.3658
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.03437811 