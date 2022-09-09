#===========================================================================#
# script3_plot_phenology_and_temperature.R
# 
# PARTS
# 1. Load 2019 and 2020 count data (line 25)
# 2. Seasonal overview of count data (line 66)
#    - p1: Daily count by site (line graph) for 2019
#    - p2: Daily count by site (line graph) for 2020
# 3. Load 2019 and 2020 temperature data (line 232)
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
#-- 1. Pool and 2019 and 2020 count data -----------------------------------
#---------------------------------------------------------------------------#

### Pool and save all count observarions (including 0)
counts_y19y20 <- read_csv("./data-intermediate/counts_all.csv")
counts_y19y20
# A tibble: 69,390 x 7
#   site  site2 datetime            pest_nmbr pest_dif reviewed event
#   <chr> <chr> <dttm>                  <dbl>    <dbl> <chr>    <chr>
# 1 Perez E     2019-06-03 16:56:00         0        0 No       NA   
# 2 Perez E     2019-06-03 17:56:00         0        0 No       NA   
# 3 Perez E     2019-06-03 18:56:00         0        0 No       NA   

### Set datetime format
counts_y19y20$datetime <- as.POSIXct(counts_y19y20$datetime)
str(counts_y19y20)
attr(counts_y19y20$datetime,"tzone")
# [1] ""

### Attach appropriate timezone attribute (takes daylight saving time into account)
lubridate::tz(counts_y19y20$datetime) <- "America/Los_Angeles"
attr(counts_y19y20$datetime,"tzone") # confirmation
# [1] "America/Los_Angeles"


counts_y19y20 <- counts_y19y20 %>% 
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

#---------------------------------------------------------------------------#
#-- 2. Plots for daily count data for 2019 and 2020                 ---------
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

site2 <- sort(unique(daily19$site2))
site2
# [1] "A" "B" "C" "D" "E"

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
y19b <- y19b[complete.cases(y19b), ]
y19b

p1 <- ggplot(y19b, aes(x = Date,y = orangeworm)) +
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
#-- 3. Plots for hourly temperature  data for 2019 and 2020 ----------------
#---------------------------------------------------------------------------#

# Load 2 year temperature data from data-intermediate
temps <- read_csv("./data-intermediate/temps_all.csv")
temps

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
temps
# A tibble: 36,549 x 12
#   site  site2 Date_time           degf_avg degf_lo degf_hi rh_avg degc_avg    Yr Mnth  Julian    Hr
#   <chr> <chr> <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>    <dbl> <dbl> <ord>  <dbl> <int>
# 1 Perez E     2019-06-04 16:00:00     88.7    86.2    90.5   59.8     31.5  2019 Jun      155    16
# 2 Perez E     2019-06-04 17:00:00     84      82.4    85.6   69.8     28.9  2019 Jun      155    17
# 3 Perez E     2019-06-04 18:00:00     80.7    79      82.2   74.4     27.0  2019 Jun      155    18

# Pretty and consistent names for plots
temps <- temps %>% 
  rename(Site = site2)

### Temperatures plot for 2019
temps19 <- temps %>% 
  filter(year(Date_time) == 2019)
str(temps19)


p3 <- ggplot(temps19, aes(x = Date_time, y = degc_avg)) +
  geom_line() +
  facet_grid(Site ~ .) +
  theme_bw() +
  #scale_x_datetime(breaks = as.Date.POSIXct(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01"))) +
  scale_x_datetime(breaks = date_breaks("1 month")) +
  xlab("") +
  ylab("Temperature (degrees Celsius)") +
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

p4 <- ggplot(temps20, aes(x = Date_time, y = degc_avg)) +
  geom_line() +
  facet_grid(Site ~ .) +
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

#---------------------------------------------------------------------------#
#-- 4. Combined plots for 2019 and 2020                      ----------------
#---------------------------------------------------------------------------#

### Output combined 2020 plots

p1p3 <- ggarrange(p1,p3, ncol = 2)
p1p3

ggsave(filename = "Fig2.jpg", p1p3, path = "./results",
       width = 5.83, height = 5.83, dpi = 300, units = "in", device='jpg')

### Output combined 2020 plots

p2p4 <- ggarrange(p2,p4, ncol = 2)
p2p4

ggsave(filename = "Fig3.jpg", p2p4, path = "./results",
       width = 5.83, height = 5.83, dpi = 300, units = "in", device='jpg')


