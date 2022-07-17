#===========================================================================#
# script3_plot_phenology_and_temperature.R
# 
# PARTS
# 1. Load combined 2019 and 2020 count and temperature data files (line 15)
# 2. Merge and 2019 and 2020 count data (line 25)
# 3. Seasonal overview using just count data (line 82)
#    - p1: Daily count by site (line graph) for 2019
#    - p2: Daily count by site (line graph) for 2020
# 4. Merge 2019 and 2020 temperature data (line 232)
#    - p3: Hourly temperature by site (line graph) for 2019
#    - p4: Hourly temperature by site (line graph) for 2020
# 5. Merge non-zero count data and temperature data (line 319)
#    - p1p3: p1 and p3 side by side [Fig2.jpg]
#    - p2p4: p2 and p4 side by side [Fig3.jpg]
# Output file "combined" saved as...
#   merged_count_and_temp_data.csv
# 6. Explore high and low temps by month
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(scales)
library(ggpubr)

#---------------------------------------------------------------------------#
#-- 1. Load and clean 2019 and 2020 data files (same as script3) ------------
#---------------------------------------------------------------------------#

### Load original data files into memory

allsites19 <- read.csv("./data/allsites_y19_scrubbed.csv") #37,916 obs
allsites20 <- read.csv("./data//allsites_y20_scrubbed.csv") #31,474 obs

### Provide a trap_code variable. Contains orignial site names, names from
### the Hengst thesis, and identifiers to be use for the manuscript

# upload
trapcodes <- read.csv("./data/trapview_trap_codes.csv")
trapcodes
#    Year        site TrapCode site2
# 1  2019       Perez    tv19a     E
# 2  2019 MWoolf_west    tv19b     A
# 3  2019 MWoolf_east    tv19c     B
# 4  2019        usda    tv19d     C
# 5  2019   UCKearney    tv19e     D
# 6  2020  mikewoolf1    tv20a    A1
# 7  2020  mikewoolf2    tv20b    A2
# 8  2020  mikewoolf3    tv20c    A3
# 9  2020  mikewoolf4    tv20d    A4
# 10 2020  mikewoolf5    tv20e    A5

# isolate by year
trapcodes19 <- trapcodes[trapcodes$Year == 2019,c(2,4)]
trapcodes20 <- trapcodes[trapcodes$Year == 2020,c(2,4)]

# merge w present data
allsites19 <- left_join(trapcodes19,allsites19)
allsites20 <- left_join(trapcodes20,allsites20)

#---------------------------------------------------------------------------#
#-- 2. Pool and 2019 and 2020 count data -----------------------------------
#---------------------------------------------------------------------------#

### Pool and save all count observarions (including 0)
counts_y19y20 <- rbind(allsites19,allsites20)

write.csv(counts_y19y20,"./data-intermediate/counts_all.csv", row.names = FALSE)

### Modify for merge compatibility)

# # Drop counts of zero
# allsites19 <- allsites19 %>% 
#   filter(pest_dif > 0)
# #-- 802 observations
# 
# allsites20 <- allsites20 %>% 
#   filter(pest_dif > 0)
# #-- 681 observations

### Set datetime format
counts_y19y20$datetime <- as.POSIXct(counts_y19y20$datetime)
str(counts_y19y20)
attr(counts_y19y20$datetime,"tzone")
# [1] ""

### Attach appropriate timezone attribute (takes daylight saving time into account)
lubridate::tz(counts_y19y20$datetime) <- "America/Los_Angeles"
attr(counts_y19y20$datetime,"tzone") # confirmation
# [1] "America/Los_Angeles"

# ### Final QC, expunge records not reviewed
# 
# counts_y19y20 %>% 
#   filter(reviewed == "No")
# #   site TrapCode            datetime pest_nmbr pest_dif reviewed event
# # 1 Perez    tv19a 2019-08-16 15:56:00         3        3       No  <NA>
# # 2 Perez    tv19a 2019-08-19 04:28:00         1        1       No  <NA>
# # 3  usda    tv19d 2019-08-19 00:28:00         1        1       No  <NA>
# # 4  usda    tv19d 2019-08-31 05:27:00         1        1       No  <NA>

counts_y19y20 <- counts_y19y20 %>% 
  #filter(reviewed != "No") %>% 
  select(datetime,pest_dif,site,site2) %>% 
  mutate(Yr = year(datetime),
         Mnth = month(datetime, label = TRUE, abbr = TRUE),
         Julian = yday(datetime),
         Hr = hour(datetime),
         Minute = minute(datetime))

head(counts_y19y20,2)
#              datetime pest_dif  site site2   Yr Mnth Julian Hr Minute
# 1 2019-06-06 03:59:00        1 Perez     E 2019  Jun    157  3     59
# 2 2019-06-06 05:59:00        2 Perez     E 2019  Jun    157  5     59

# Use base r histogram to confirm that most counts are close to 30 and 60 
# minutes of the hour
hist(counts_y19y20$Minute)

#---------------------------------------------------------------------------#
#-- 3. Seasonal overview using just the count data from both years  ---------
#---------------------------------------------------------------------------#

### temporary data frame allsites for current figure

allsites <- counts_y19y20 %>% 
  mutate(wk = epiweek(datetime))

allsites$caldate <- as.Date(allsites$datetime)
head(allsites,2)
#   datetime pest_dif  site site2   Yr Mnth Julian Hr Minute wk    caldate
# 1 2019-06-06 03:59:00        1 Perez     E 2019  Jun    157  3     59 23 2019-06-06
# 2 2019-06-06 05:59:00        2 Perez     E 2019  Jun    157  5     59 23 2019-06-06



### sumarize counts by day

daily <- allsites %>% 
  group_by(Yr,wk,caldate,site2) %>% 
  summarize(orangeworm = sum(pest_dif)) %>% 
  rename(Date = caldate)
daily
# A tibble: 1,808 x 5
# Groups:   Yr, wk, Date [453]
#      Yr    wk Date       site2 orangeworm
#   <dbl> <dbl> <date>     <chr>      <int>
# 1  2019    17 2019-04-24 C             12
# 2  2019    17 2019-04-25 C              3
# 3  2019    17 2019-04-26 C              0
# 4  2019    17 2019-04-26 D              7

### Manipulate 2019 data file to improve graph appearance and to get 
### geom_line to return to 0 for dates with no captures

daily19 <- daily %>%
  filter(Yr == 2019)
(begin19 <- min(daily19$Date))
#[1] "2019-04-24"
(end19 <- max(daily19$Date))
# [1] "2019-10-30"
daily19
# A tibble: 973 x 5
# Groups:   Yr, wk, Date [223]
#      Yr    wk Date       site2 orangeworm
#   <dbl> <dbl> <date>     <chr>      <int>
# 1  2019    17 2019-04-24 C             12
# 2  2019    17 2019-04-25 C              3
# 3  2019    17 2019-04-26 C              0
# 4  2019    17 2019-04-26 D              7
# 5  2019    17 2019-04-27 C              2

### NB The earliest temperature data we have from Trapview units in 2019
### is May 16, and that is for the tv19e (MWoolf_east). Do not plot trap
### data prior to May 16
begin19 <- as.Date("2019-05-16")


# Rename variable for prettier graph (now not used)
# daily19$site[daily19$site == "MWoolf_east"] <- "MW East"
# daily19$site[daily19$site == "MWoolf_west"] <- "MW West"
# daily19$site[daily19$site == "usda"] <- "USDA"

site2 <- sort(unique(daily19$site2))
site2
# [1] "A" "B" "C" "D" "E"

# rename site to Site (now not used)
# daily19$Site <- daily19$site
# daily19$site <- NULL
# indirect base method used because dplyr complained about ambiguity

Dates19 <- seq(begin19,end19, by = "1 day")
length(Dates19)
# 173

site2 <- rep(site2, each = 173)
Date <- rep(Dates19, times = 5)

Dates <- data.frame(site2,Date)
head(Dates)
head(daily19)
daily19b <- left_join(Dates,daily19)
head(daily19b,2)
#   site2       Date Yr wk orangeworm
# 1     A 2019-05-16 NA NA         NA
# 2     A 2019-05-17 NA NA         NA

# Replace NA with zero in this data frame
#daily19b$orangeworm[is.na(daily19b$orangeworm)] <- 0

# Pretty up var name for plotting
y19b <- daily19b %>% 
  rename(Site = site2)

head(y19b,2)
#   Site       Date Yr wk orangeworm
# 1    A 2019-05-16 NA NA         NA
# 2    A 2019-05-17 NA NA         NA

str(y19b)
# 'data.frame':	980 obs. of  5 variables:
# $ Site      : chr  "A" "A" "A" "A" ...
# $ Date      : Date, format: "2019-05-16" "2019-05-17" "2019-05-18" "2019-05-19" ...
# $ Yr        : num  NA NA NA NA NA ...
# $ wk        : num  NA NA NA NA NA NA 21 21 21 21 ...
# $ orangeworm: int  NA NA NA NA NA NA 0 4 2 60 ...

# keep getting errors--messed up by NA?
x <- y19b[complete.cases(y19b), ]
x

p1 <- ggplot(x, aes(x = Date,y = orangeworm)) +
  geom_line() +
  facet_grid(Site ~ .) +
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

# ggsave(filename = "Y19_trapview_daily_by_site.jpg", p1, path = "./results",
#        width = 2.83, height = 5.83, dpi = 300, units = "in", device='jpg')

### Get plot for 2020 count data to match the one for 2019 
### count data
daily20 <- daily %>% 
  filter(Yr == 2020)
(begin20 <- min(daily20$Date))
#[1] "2019-03-09"
(end20 <- max(daily20$Date))
# [1] "2019-09-22"
daily20
# A tibble: 835 x 5
# Groups:   Yr, wk, Date [230]
#      Yr    wk Date       site2 orangeworm
#   <dbl> <dbl> <date>     <chr>      <int>
# 1  2020    10 2020-03-06 A4             0
# 2  2020    10 2020-03-07 A4             0


# Rename variable for prettier graph (not not used)
# daily20$site[daily20$site == "mikewoolf1"] <- "West 1"
# daily20$site[daily20$site == "mikewoolf2"] <- "West 2"
# daily20$site[daily20$site == "mikewoolf3"] <- "West 3"
# daily20$site[daily20$site == "mikewoolf4"] <- "West 4"
# daily20$site[daily20$site == "mikewoolf5"] <- "West 5"

site2 <- sort(unique(daily20$site2))
site2
# [1] "A1" "A2" "A3" "A4" "A5"

Dates20 <- (Date = seq(begin20,end20, by = "1 day"))
# length = 201

site2 <- rep(site2, each = 201)
Date <- rep(Dates20, times = 5)

Dates20 <- data.frame(site2,Date)
head(Dates20)
daily20
# Groups:   Yr, wk, Date [230]
#      Yr    wk Date       site2 orangeworm
#   <dbl> <dbl> <date>     <chr>      <int>
# 1  2020    10 2020-03-06 A4             0
# 2  2020    10 2020-03-07 A4             0
# 3  2020    10 2020-03-08 A4             0

daily20b <- left_join(Dates20,daily20)
head(daily20b,2)
#   site2       Date Yr wk orangeworm
# 1    A1 2020-03-06 NA NA         NA
# 2    A1 2020-03-07 NA NA         NA

daily20b <- daily20b[complete.cases(daily20b), ]
daily20b <- daily20b %>% 
  rename(Site = site2)

p2 <- ggplot(daily20b, aes(x = Date,y = orangeworm)) +
  geom_line( ) +
  facet_grid(Site ~ .) +
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

# ggsave(filename = "Y20_trapview_wkly_by_site.jpg", p2, path = "./results",
#        width = 2.83, height = 5.83, dpi = 300, units = "in", device='jpg')

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
