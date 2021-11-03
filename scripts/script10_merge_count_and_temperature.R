#===========================================================================#
# script10_moth_captures_by_hour.R
# 
# PARTS
# 1. Load combined 2019 and 2020 count and temperature data files (line 15)
# 2. Merge and 2019 and 2020 count data (line 25)
# 3. Seasonal overview using just count data (line 82)
#    - p1: Daily count by site (line graph) for 2019
#    - p2: Daily count by site (line graph) for 2020
# 4. Merge 2019 and 2020 temperature data (line 232)
# 5. Merge non-zero count data and temperature data (line 319)
#    - p3: Hourly temperature by site (line graph) for 2019
#    - p4: Hourly temperature by site (line graph) for 2020
#    - p1p3: p1 and p3 side by side
#    - p2p4: p2 and p4 side by side
# Output file "combined" saved as...
#   merged_count_and_temp_data.csv
# 6. Explore high and low temps by month
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(scales)
library(ggpubr)
library(DescTools)

#---------------------------------------------------------------------------#
#-- 1. Load and clean 2019 and 2020 data files (same as script3) ------------
#---------------------------------------------------------------------------#

### Load original data files into memory

allsites19 <- read.csv("./data/allsites_y19_scrubbed.csv") 
allsites20 <- read.csv("./data//allsites_y20_scrubbed.csv")

### Provide a trap_code variable
# upload
trapcodes <- read.csv("./data/trapview_trap_codes.csv")
# isolate by year
trapcodes19 <- trapcodes[trapcodes$Year == 2019,2:3]
trapcodes20 <- trapcodes[trapcodes$Year == 2020,2:3]
# merge w present data
allsites19 <- left_join(trapcodes19,allsites19)
allsites20 <- left_join(trapcodes20,allsites20)



# Examine and fix discrepancy in site names in 2020 capture and temperature data
#unique(allsites20$site)
# [1] "mikewoolf1" "mikewoolf2" "mikewoolf3" "mikewoolf4" "mikewoolf5"

#---------------------------------------------------------------------------#
#-- 2. Merge and 2019 and 2020 count data -----------------------------------
#---------------------------------------------------------------------------#

### Modify for merge compatibility)

# Drop counts of zero
allsites19 <- allsites19 %>% 
  filter(pest_dif > 0)
#-- 802 observations

allsites20 <- allsites20 %>% 
  filter(pest_dif > 0)
#-- 681 observations

### Combine 2019 and 2020

counts_y19y20 <- rbind(allsites19,allsites20)

### Set datetime format

counts_y19y20$datetime <- as.POSIXct(counts_y19y20$datetime)
str(counts_y19y20)
attr(counts_y19y20$datetime,"tzone")
# ""
### Get system timezone attribute (takes daylight saving time into account)

(x <- Sys.timezone())
# [1] "America/Los_Angeles"

lubridate::tz(counts_y19y20$datetime) <- x

attr(counts_y19y20$datetime,"tzone")
# [1] "America/Los_Angeles"

### Final QC, expunge records not reviewed

counts_y19y20 %>% 
  filter(reviewed == "No")
#   site TrapCode            datetime pest_nmbr pest_dif reviewed event
# 1 Perez    tv19a 2019-08-16 15:56:00         3        3       No  <NA>
# 2 Perez    tv19a 2019-08-19 04:28:00         1        1       No  <NA>
# 3  usda    tv19d 2019-08-19 00:28:00         1        1       No  <NA>
# 4  usda    tv19d 2019-08-31 05:27:00         1        1       No  <NA>

counts_y19y20 <- counts_y19y20 %>% 
  filter(reviewed != "No") %>% 
  select(datetime,pest_dif,site,TrapCode) %>% 
  mutate(Yr = year(datetime),
         Mnth = month(datetime, label = TRUE, abbr = TRUE),
         Julian = yday(datetime),
         Hr = hour(datetime))

head(counts_y19y20,2)
#               datetime pest_dif  site TrapCode   Yr Mnth Julian Hr
# 1 2019-06-06 03:59:00        1 Perez    tv19a 2019  Jun    157  3
# 2 2019-06-06 05:59:00        2 Perez    tv19a 2019  Jun    157  5

#---------------------------------------------------------------------------#
#-- 3. Seasonal overview using just the count data from both years  ---------
#---------------------------------------------------------------------------#

### temporary data frame allsites for current figure

allsites <- counts_y19y20 %>% 
  mutate(wk = epiweek(datetime))
allsites$caldate <- as.Date(allsites$datetime)
head(allsites,2)
#              datetime pest_dif  site TrapCode   Yr Mnth Julian Hr wk    caldate
# 1 2019-06-06 03:59:00        1 Perez    tv19a 2019  Jun    157  3 23 2019-06-06
# 2 2019-06-06 05:59:00        2 Perez    tv19a 2019  Jun    157  5 23 2019-06-06

### sumarize counts by day

daily <- allsites %>% 
  group_by(Yr,wk,caldate,TrapCode) %>% 
  summarize(orangeworm = sum(pest_dif)) %>% 
  rename(Date = caldate)
daily
# A tibble: 595 x 5
# Groups:   Yr, wk, Date [275]
#      Yr    wk Date       TrapCode orangeworm
#   <dbl> <dbl> <date>     <chr>         <int>
# 1  2019    17 2019-04-24 tv19d            12
# 2  2019    17 2019-04-25 tv19d             3
# 3  2019    17 2019-04-26 tv19e             7

### Manipulate 2019 data file to improve graph appearance and to get 
### geom_line to return to 0 for dates with no captures

daily19 <- daily %>%
  filter(Yr == 2019)
(begin19 <- min(daily19$Date))
#[1] "2019-04-24"
(end19 <- max(daily19$Date))
# [1] "2019-10-30"

### NB The earliest temperature data we have from Trapview units in 2019
### is May 16, and that is for the tv19e (MWoolf_east). Do not plot trap
### data prior to May 16
begin19 <- as.Date("2019-05-16")


# Rename variable for prettier graph (now not used)
# daily19$site[daily19$site == "MWoolf_east"] <- "MW East"
# daily19$site[daily19$site == "MWoolf_west"] <- "MW West"
# daily19$site[daily19$site == "usda"] <- "USDA"

TrapCode <- unique(daily19$TrapCode)
TrapCode
# [1] "tv19d" "tv19e" "tv19b" "tv19a" "tv19c" 

# rename site to Site (now not used)
# daily19$Site <- daily19$site
# daily19$site <- NULL
#>>> indirect base method used because dplyr complained about ambiguity


Dates19 <- (Date = seq(begin19,end19, by = "1 day"))
length(Dates19)
# 168

TrapCode <- rep(TrapCode, each = 168)
Date <- rep(Dates19, times = 5)

Dates <- data.frame(TrapCode,Date)
head(Dates)
head(daily19)
daily19b <- left_join(Dates,daily19)
head(daily19b,2)
#   Site       Date   Yr wk orangeworm
# 1 USDA 2019-04-24 2019 17         12
# 2 USDA 2019-04-25 2019 17          3
daily19b$orangeworm[is.na(daily19b$orangeworm)] <- 0


p1 <- ggplot(daily19b, aes(x = Date,y = orangeworm)) +
  geom_line() +
  facet_grid(TrapCode ~ .) +
  theme_bw() +
  scale_x_date(breaks = as.Date(c("2019-05-01","2019-06-01","2019-07-01","2019-08-01","2019-09-01","2019-10-01","2019-11-01"))) +
  xlab("") +
  ylab("NOW per day") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p1

ggsave(filename = "Y19_trapview_daily_by_site.jpg", p1, path = "./results",
       width = 2.83, height = 5.83, dpi = 300, units = "in", device='jpg')

### Get plot for 2020 count data to match the one for 2019 
### count data
daily20 <- daily %>% 
  filter(Yr == 2020)
(begin20 <- min(daily20$Date))
#[1] "2019-03-09"
(end20 <- max(daily20$Date))
# [1] "2019-09-22"

# Rename variable for prettier graph (not not used)
# daily20$site[daily20$site == "mikewoolf1"] <- "West 1"
# daily20$site[daily20$site == "mikewoolf2"] <- "West 2"
# daily20$site[daily20$site == "mikewoolf3"] <- "West 3"
# daily20$site[daily20$site == "mikewoolf4"] <- "West 4"
# daily20$site[daily20$site == "mikewoolf5"] <- "West 5"

TrapCode <- unique(daily20$TrapCode)
TrapCode
# [1] "tv20d" "tv20b" "tv20a" "tv20c" "tv20e"

# rename site to Site (now not used)
# daily20$Site <- daily20$site
# daily20$site <- NULL

Dates20 <- (Date = seq(begin20,end20, by = "1 day"))
# length = 198

TrapCode <- rep(TrapCode, each = 198)
Date <- rep(Dates20, times = 5)

Dates20 <- data.frame(TrapCode,Date)
head(Dates20)
head(daily20)
daily20b <- left_join(Dates20,daily20)
head(daily20b,2)
#   Site       Date   Yr wk orangeworm
# 1 USDA 2019-04-24 2019 17         12
# 2 USDA 2019-04-25 2019 17          3
daily20b$orangeworm[is.na(daily20b$orangeworm)] <- 0


p2 <- ggplot(daily20b, aes(x = Date,y = orangeworm)) +
  geom_line( ) +
  facet_grid(TrapCode ~ .) +
  theme_bw() +
  scale_x_date(breaks = as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01"))) +
    xlab("") +
  ylab("NOW per day") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p2

ggsave(filename = "Y20_trapview_wkly_by_site.jpg", p2, path = "./results",
       width = 2.83, height = 5.83, dpi = 300, units = "in", device='jpg')

#---------------------------------------------------------------------------#
#-- 4. Merge 2019 and 2020 temperature data ---------------------------------
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

lubridate::tz(temps$datetime) <- x
attr(temps$datetime,"tzone")

# Add Yr, Mnth, Julian, and Hr to the temps data set
temps <- temps %>% 
  mutate(Yr = year(datetime),
         Mnth = month(datetime, label = TRUE, abbr = TRUE),
         Julian = yday(datetime),
         Hr = hour(datetime))
head(temps,3)
#      site TrapCode           Date_time degf_avg degf_lo degf_hi rh_avg
# 1   Perez    tv19a 2019-06-04 16:00:00    88.70    86.2    90.5  59.84
# 2   Perez    tv19a 2019-06-04 17:00:00    84.00    82.4    85.6  69.78
# 3   Perez    tv19a 2019-06-04 18:00:00    80.67    79.0    82.2  74.41


### Temperatures plot for 2019
temps19 <- temps %>% 
  filter(year(Date_time) == 2019)
str(temps19)

### 


p3 <- ggplot(temps19, aes(x = Date_time, y = degf_avg)) +
  geom_line() +
  facet_grid(TrapCode ~ .) +
  theme_bw() +
  #scale_x_datetime(breaks = as.Date.POSIXct(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01"))) +
  scale_x_datetime(breaks = date_breaks("1 month")) +
  xlab("") +
  ylab("Temperature (degrees Fahrenheit)") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p3

ggsave(filename = "Y19_trapview_temps_daily_by_site.jpg", p3, path = "./results",
       width = 2.83, height = 5.83, dpi = 300, units = "in", device='jpg')


# Temperatures plot for 2020
temps20 <- temps %>% 
  filter(year(Date_time) == 2020)

# Rename variable for prettier graph
temps20$site[temps20$site == "mikewoolf1"] <- "West 1"
temps20$site[temps20$site == "mikewoolf2"] <- "West 2"
temps20$site[temps20$site == "mikewoolf3"] <- "West 3"
temps20$site[temps20$site == "mikewoolf4"] <- "West 4"
temps20$site[temps20$site == "mikewoolf5"] <- "West 5"


p4 <- ggplot(temps20, aes(x = datetime, y = degf_avg)) +
  geom_line() +
  facet_grid(site ~ .) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month") +
  xlab("") +
  ylab("Temperature (degrees Fahrenheit)") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p4

ggsave(filename = "Y20_trapview_temps_daily_by_site.jpg", p4, path = "./results",
       width = 2.83, height = 5.83, dpi = 300, units = "in", device='jpg')

### Output combined 2019 plots

p1p3 <- ggarrange(p1,p3, ncol = 2)
p1p3

ggsave(filename = "Y19_counts_temps_daily_by_site.jpg", p1p3, path = "./results",
       width = 5.83, height = 5.83, dpi = 300, units = "in", device='jpg')

### Output combined 2020 plots

p2p4 <- ggarrange(p2,p4, ncol = 2)
p2p4

ggsave(filename = "Y20_counts_temps_daily_by_site.jpg", p2p4, path = "./results",
       width = 5.83, height = 5.83, dpi = 300, units = "in", device='jpg')


# Merge temps to counts w left join, so that records in temps are retained only
# if they match a record in counts
length(temps$degf_avg[is.na(temps$degf_avg)])
# [1] 1
temps <- temps[!is.na(temps$datetime),] # drop the 1 NA in case that is a problem

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
# pest_dif       site   Yr Mnth.x Julian Hr degf_avg degf_lo degf_hi rh_avg Mnth.y
#   1           7  UCKearney 2019    Apr    116 13       NA      NA      NA     NA   <NA>
#   2           1  UCKearney 2019    May    122  1       NA      NA      NA     NA   <NA>
#   3           1  UCKearney 2019    May    123  2       NA      NA      NA     NA   <NA>
#   4           1  UCKearney 2019    May    123 20       NA      NA      NA     NA   <NA>
#   5           3  UCKearney 2019    May    136  9       NA      NA      NA     NA   <NA>
#   651        12       usda 2019    Apr    114  5       NA      NA      NA     NA   <NA>
#   652         2       usda 2019    Apr    115  5       NA      NA      NA     NA   <NA>
#   653         1       usda 2019    Apr    115  6       NA      NA      NA     NA   <NA>
#   654         1       usda 2019    Apr    116 20       NA      NA      NA     NA   <NA>
#   655         1       usda 2019    Apr    117  3       NA      NA      NA     NA   <NA>
#   656         1       usda 2019    Apr    117 17       NA      NA      NA     NA   <NA>
#   657         2       usda 2019    Apr    118  3       NA      NA      NA     NA   <NA>
#   658         1       usda 2019    Apr    120  5       NA      NA      NA     NA   <NA>
#   659         1       usda 2019    Apr    120  5       NA      NA      NA     NA   <NA>
#   660         1       usda 2019    May    121  4       NA      NA      NA     NA   <NA>
#   661         2       usda 2019    May    122  1       NA      NA      NA     NA   <NA>
#   662         2       usda 2019    May    122  2       NA      NA      NA     NA   <NA>
#   663         2       usda 2019    May    122  2       NA      NA      NA     NA   <NA>
#   664         3       usda 2019    May    122  3       NA      NA      NA     NA   <NA>
#   665         1       usda 2019    May    122  3       NA      NA      NA     NA   <NA>
#   666         1       usda 2019    May    122  5       NA      NA      NA     NA   <NA>
#   667         1       usda 2019    May    122  6       NA      NA      NA     NA   <NA>
#   668         1       usda 2019    May    122 19       NA      NA      NA     NA   <NA>
#   669         2       usda 2019    May    123  2       NA      NA      NA     NA   <NA>
#   670         1       usda 2019    May    123  2       NA      NA      NA     NA   <NA>
#   671         1       usda 2019    May    123  3       NA      NA      NA     NA   <NA>
#   672         1       usda 2019    May    123  5       NA      NA      NA     NA   <NA>
#   673         1       usda 2019    May    124  1       NA      NA      NA     NA   <NA>
#   674         1       usda 2019    May    124  2       NA      NA      NA     NA   <NA>
#   675         3       usda 2019    May    124  3       NA      NA      NA     NA   <NA>
#   676         1       usda 2019    May    124  4       NA      NA      NA     NA   <NA>
#   677         1       usda 2019    May    127  9       NA      NA      NA     NA   <NA>
#   678         1       usda 2019    May    128  4       NA      NA      NA     NA   <NA>
#   679         1       usda 2019    May    129  2       NA      NA      NA     NA   <NA>
#   680         1       usda 2019    May    129  3       NA      NA      NA     NA   <NA>
#   681         1       usda 2019    May    138  6       NA      NA      NA     NA   <NA>
#   682         1       usda 2019    May    148 10       NA      NA      NA     NA   <NA>
#   683         1       usda 2019    May    148 11       NA      NA      NA     NA   <NA>
#   1433        2 mikewoolf5 2020    Jul    190  3       NA      NA      NA     NA   <NA>
#   1434        1 mikewoolf5 2020    Jul    190  5       NA      NA      NA     NA   <NA>
#   1435        1 mikewoolf5 2020    Jul    191  4       NA      NA      NA     NA   <NA>
#   1436        3 mikewoolf5 2020    Jul    191  5       NA      NA      NA     NA   <NA>
#   1437        2 mikewoolf5 2020    Jul    191  5       NA      NA      NA     NA   <NA>
#   1438        2 mikewoolf5 2020    Jul    191  6       NA      NA      NA     NA   <NA>
#   1439        1 mikewoolf5 2020    Jul    192  4       NA      NA      NA     NA   <NA>
#   1440        3 mikewoolf5 2020    Jul    192  5       NA      NA      NA     NA   <NA>
#   1441        1 mikewoolf5 2020    Jul    193  5       NA      NA      NA     NA   <NA>
#   1442        1 mikewoolf5 2020    Jul    193  7       NA      NA      NA     NA   <NA>
#   1443        1 mikewoolf5 2020    Jul    195  5       NA      NA      NA     NA   <NA>
#   1444        1 mikewoolf5 2020    Jul    195  8       NA      NA      NA     NA   <NA>
#   1445        1 mikewoolf5 2020    Jul    195 12       NA      NA      NA     NA   <NA>
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

Desc(Hi ~ Mnth, data = Hi_lo)

Desc(Lo ~ Mnth, data = Hi_lo)


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
