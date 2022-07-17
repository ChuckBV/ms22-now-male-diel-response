#===========================================================================#
# script10_moth_captures_by_hour.R
# 
# PARTS
# 1. Load combined 2019 and 2020 temperature data files (line 19)
# 2. Merge and 2019 and 2020 count data (line 25)
# 3. Seasonal overview using just count data (line 82)
#    - p1: Daily count by site (line graph) for 2019
#    - p2: Daily count by site (line graph) for 2020
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(scales)
library(ggpubr)

#---------------------------------------------------------------------------#
#-- 1. Load and clean 2019 and 2020 data files (same as script3) ------------
#---------------------------------------------------------------------------#

alltemps19 <- readr::read_csv("./data/trapview_temps_degf_y19.csv")
alltemps20 <- readr::read_csv("./data/trapview_temps_degf_y20.csv")

### Merge TrapCode into 2020 data

unique(alltemps19$site)
# [1] "UCKearney"   "usda"        "Perez"       "MWoolf_west" "MWoolf_east"

alltemps19 <- left_join(trapcodes19,alltemps19) # parallel with line 44
head(alltemps19,2)
#    site TrapCode           Date_time degf_avg degf_lo degf_hi rh_avg
# 1 Perez    tv19a 2019-06-04 16:00:00     88.7    86.2    90.5  59.84
# 2 Perez    tv19a 2019-06-04 17:00:00     84.0    82.4    85.6  69.78


unique(alltemps20$site) #17,580 obs
# [1] "MWT1" "MWT2" "MWT3" "MWT4" "MWT5"

alltemps20$site[alltemps20$site == "MWT1"] <- "mikewoolf1"
alltemps20$site[alltemps20$site == "MWT2"] <- "mikewoolf2"
alltemps20$site[alltemps20$site == "MWT3"] <- "mikewoolf3"
alltemps20$site[alltemps20$site == "MWT4"] <- "mikewoolf4"
alltemps20$site[alltemps20$site == "MWT5"] <- "mikewoolf5"

alltemps20 <- left_join(trapcodes20,alltemps20) # parallel with line 44
head(alltemps20,2)
#         site TrapCode           Date_time degf_avg degf_lo degf_hi rh_avg
# 1 mikewoolf1    tv20a 2020-04-22 02:00:00    51.50    50.2    52.5  90.27
# 2 mikewoolf1    tv20a 2020-04-22 03:00:00    49.53    48.9    49.8  93.83

# Modify for merge compatibility (now not used)
# alltemps19 <- alltemps19 %>% 
#   dplyr::rename(datetime = Date_time)
# alltemps20 <- alltemps20 %>% 
#   dplyr::rename(datetime = Date_time)

### Make variables in alltemps19 and alltemps20 compatible with each
### other and with the counts data set

unique(counts_y19y20$TrapCode)
# [1] "tv19a" "tv19b" "tv19c" "tv19d" "tv19e" "tv20a" "tv20b" "tv20c" "tv20d" "tv20e" 

# combine the temperature data sets
temps <- rbind(alltemps19,alltemps20)
temps
# A tibble: 36,549 x 6
# site      datetime            degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7

tzone <- "America/Los_Angeles"
lubridate::tz(temps$Date_time) <- tzone
attr(temps$Date_time,"tzone")

#### Get new variable w degrees C
temps <- temps %>% 
  mutate(degc_avg = (degf_avg - 32)/1.8)

plot(temps$degf_avg,temps$degc_avg) # verify

# Add Yr, Mnth, Julian, and Hr to the temps data set
temps <- temps %>% 
  mutate(Yr = year(Date_time),
         Mnth = month(Date_time, label = TRUE, abbr = TRUE),
         Julian = yday(Date_time),
         Hr = hour(Date_time))
head(temps,3)
#      site TrapCode           Date_time degf_avg degf_lo degf_hi rh_avg
# 1   Perez    tv19a 2019-06-04 16:00:00    88.70    86.2    90.5  59.84
# 2   Perez    tv19a 2019-06-04 17:00:00    84.00    82.4    85.6  69.78
# 3   Perez    tv19a 2019-06-04 18:00:00    80.67    79.0    82.2  74.41


### Temperatures plot for 2019
temps19 <- temps %>% 
  filter(year(Date_time) == 2019)
str(temps19)

p3 <- ggplot(temps19, aes(x = Date_time, y = degc_avg)) +
  geom_line() +
  facet_grid(TrapCode ~ .) +
  theme_bw() +
  #scale_x_datetime(breaks = as.Date.POSIXct(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01"))) +
  scale_x_datetime(breaks = date_breaks("1 month")) +
  xlab("") +
  ylab("Temperature (degrees Celcius)") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p3

# ggsave(filename = "Y19_trapview_temps_daily_by_site.jpg", p3, path = "./results",
#        width = 2.83, height = 5.83, dpi = 300, units = "in", device='jpg')


# Temperatures plot for 2020
temps20 <- temps %>% 
  filter(year(Date_time) == 2020)

# Rename variable for prettier graph
temps20$site[temps20$site == "mikewoolf1"] <- "West 1"
temps20$site[temps20$site == "mikewoolf2"] <- "West 2"
temps20$site[temps20$site == "mikewoolf3"] <- "West 3"
temps20$site[temps20$site == "mikewoolf4"] <- "West 4"
temps20$site[temps20$site == "mikewoolf5"] <- "West 5"


p4 <- ggplot(temps20, aes(x = Date_time, y = degc_avg)) +
  geom_line() +
  facet_grid(TrapCode ~ .) +
  theme_bw() +
  scale_x_datetime(breaks = date_breaks("1 month")) +
  xlab("") +
  ylab("Temperature (degrees Celcius)") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p4

# ggsave(filename = "Y20_trapview_temps_daily_by_site.jpg", p4, path = "./results",
#        width = 2.83, height = 5.83, dpi = 300, units = "in", device='jpg')

### Output combined 2019 plots

p1p3 <- ggarrange(p1,p3, ncol = 2)
p1p3

ggsave(filename = "Fig2.jpg", p1p3, path = "./results",
       width = 5.83, height = 5.83, dpi = 300, units = "in", device='jpg')

### Output combined 2020 plots

p2p4 <- ggarrange(p2,p4, ncol = 2)
p2p4

ggsave(filename = "Fig3.jpg", p2p4, path = "./results",
       width = 5.83, height = 5.83, dpi = 300, units = "in", device='jpg')


# Merge temps to counts w left join, so that records in temps are retained only
# if they match a record in counts
length(temps$degf_avg[is.na(temps$degf_avg)])
# [1] 1
#temps <- 
temps <- temps[!is.na(temps$Date_time),] # drop the 1 NA in case that is a problem

head(temps,2)

#-- 5. Merge non-zero Count data to corresponding temperature records -------

counts_y19y20$datetime <- NULL
temps$datetime <- NULL
### For counts, the hour function makes the reading more approximate. For 
### temps, data were sent every hour on the hour and were a summary of the 
### previous hour, so the hour function gives us something that matches the
### counts data

combined <- left_join(counts_y19y20, temps, by = c("Yr","site","Julian","Hr"))
head(combined)
#   pest_dif      site Yr.x Mnth.x Julian Hr degf_avg degf_lo degf_hi rh_avg Yr.y Mnth.y
# 1        7 UCKearney 2019    Apr    116 13       NA      NA      NA     NA   NA   <NA>
# 2        1 UCKearney 2019    May    122  1       NA      NA      NA     NA   NA   <NA>
# 3        1 UCKearney 2019    May    123  2       NA      NA      NA     NA   NA   <NA>
# 4        1 UCKearney 2019    May    123 20       NA      NA      NA     NA   NA   <NA>
# 5        3 UCKearney 2019    May    136  9       NA      NA      NA     NA   NA   <NA>
# 6        1 UCKearney 2019    Jun    153  5    60.17    59.7    61.2  89.83 2019    Jun

nrow(combined)
# [1] 1479
nrow(combined[complete.cases(combined), ])
# [1] 1428
### lose some records, but retain most

combined[!complete.cases(combined), ]
## offenders were in certain times and places

combined <- combined[complete.cases(combined), ]
combined

write.csv(combined,"./data/merged_count_and_temp_data.csv",row.names = FALSE)

combined19 <-combined %>% 
  filter(Yr == 2019) %>% 
  ggplot(aes(x = ))

#-- 6. Explore high and low temps by month  ----------------------
 
Hi_lo <- temps %>%
  group_by(Mnth,Julian) %>%
  summarise(Hi = max(degf_hi),
            Lo = min(degf_lo))

DescTools::Desc(Hi ~ Mnth, data = Hi_lo)

DescTools:Desc(Lo ~ Mnth, data = Hi_lo)


### Boxplots of high and low temperature by month
ggplot(Hi_lo, aes(x = Mnth, y = Hi)) +
         geom_boxplot()

ggplot(Hi_lo, aes(x = Mnth, y = Lo)) +
  geom_boxplot()

### Bloxplots of temperautre by hour, facet_wrap month
### Crude, but demonstrates that temperature continues to increase until 
### the last hour of daylight

x <- c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")

temps2 <- temps  %>%
  filter(Mnth %in% x)

ggplot(temps2, aes(x = Hr, y = degf_avg, group = Hr)) +
  geom_boxplot() +
  facet_wrap(vars(Mnth))
