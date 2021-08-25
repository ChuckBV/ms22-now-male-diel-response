#===========================================================================#
# script10_moth_captures_by_hour.R
# 
# PARTS
# 1. Load combined 2019 and 2020 count and temperature data files (line 15)
# 2. Merge and 2019 and 2020 count data (line 25)
# 3. Merge 2019 and 2020 temperature data(line 74)
# 4. Merge non-zero count data and temperature data (line 161)
# 
# Output file "combined" saved as...
#
#===========================================================================#

library(tidyverse)
library(lubridate)

#-- 1. Load and clean 2019 and 2020 data files (same as script3) ------------

# Load original data files into memory
allsites19 <- read.csv("./data/allsites_y19_scrubbed.csv") 
allsites20 <- read.csv("./data//allsites_y20_scrubbed.csv")

# Examine and fix discrepancy in site names in 2020 capture and temperature data
unique(allsites20$site)
# [1] "mikewoolf1" "mikewoolf2" "mikewoolf3" "mikewoolf4" "mikewoolf5"

#-- 2. Merge and 2019 and 2020 count data --------------------------

# Modify for merge compatibility)
head(allsites19)
head(allsites20)

allsites19 <- allsites19 %>% 
  filter(pest_dif > 0)
#-- 802 observations

allsites20 <- allsites20 %>% 
  filter(pest_dif > 0)
#-- 681 observations

counts_y19y20 <- rbind(allsites19,allsites20)

counts_y19y20$datetime <- as.POSIXct(counts_y19y20$datetime)
str(counts_y19y20)

counts_y19y20 %>% 
  filter(reviewed == "No")
#              datetime pest_nmbr pest_dif reviewed event  site
# 1 2019-08-16 15:56:00         3        3       No  <NA> Perez
# 2 2019-08-19 04:28:00         1        1       No  <NA> Perez
# 3 2019-08-19 00:28:00         1        1       No  <NA>  usda
# 4 2019-08-31 05:27:00         1        1       No  <NA>  usda

counts_y19y20 <- counts_y19y20 %>% 
  filter(reviewed != "No") %>% 
  select(datetime,pest_dif,site) %>% 
  mutate(Yr = year(datetime),
         Mnth = month(datetime, label = TRUE, abbr = TRUE),
         Julian = yday(datetime),
         Hr = hour(datetime))

head(counts_y19y20,2)
#              datetime pest_dif      site   Yr Mnth Julian Hr
# 1 2019-04-26 13:56:00        7 UCKearney 2019  Apr    116 13
# 2 2019-05-02 01:57:00        1 UCKearney 2019  May    122  1

str(counts_y19y20)

#-- 1479 obs

offhrs <- counts_y19y20 %>% 
  filter(Hr > 7)
### 201 obs, or 13%. For next pass, create a factor within "counts" rather 
### than subdivide the data set. That allows a more direct examination of 
### proportion by month

#-- 3. Seasonal overview using just the count data from both years  ---------

# temporary data frame allsites for current figure
allsites <- counts_y19y20 %>% 
  mutate(wk = epiweek(datetime))
head(allsites,2)
#              datetime pest_dif      site   Yr Mnth Julian Hr wk
# 1 2019-04-26 13:56:00        7 UCKearney 2019  Apr    116 13 17
# 2 2019-05-02 01:57:00        1 UCKearney 2019  May    122  1 18

allsites$caldate <- as.Date(allsites$datetime)


daily <- allsites %>% 
  group_by(Yr,wk,caldate,site) %>% 
  summarize(Date = min(caldate),
            orangeworm = sum(pest_dif))
daily
# A tibble: 595 x 6
# Groups:   Yr, wk, caldate [275]
#      Yr    wk caldate    site      Date       orangeworm
#   <dbl> <dbl> <date>     <chr>     <date>          <int>
# 1  2019    17 2019-04-24 usda      2019-04-24         12
# 2  2019    17 2019-04-25 usda      2019-04-25          3
# 3  2019    17 2019-04-26 UCKearney 2019-04-26          7

wkly19 <- daily %>% 
  filter(Yr == 2019) %>% 
  group_by(Yr,wk,site) %>% 
  summarize(Date = min(Date),
            orangeworm = sum(orangeworm))
wkly19
# A tibble: 87 x 5
# Groups:   Yr, wk [28]
#      Yr    wk site        Date       orangeworm
#   <dbl> <dbl> <chr>       <date>          <int>
# 1  2019    17 UCKearney   2019-04-26          7
# 2  2019    17 usda        2019-04-24         18
# 3  2019    18 UCKearney   2019-05-02          3

wkly20 <- daily %>% 
  filter(Yr == 2020) %>% 
  group_by(Yr,wk,site) %>% 
  summarize(Date = min(Date),
            orangeworm = sum(orangeworm))
wkly20
# A tibble: 90 x 5
# Groups:   Yr, wk [25]
#      Yr    wk site       Date       orangeworm
#   <dbl> <dbl> <chr>      <date>          <int>
# 1  2020    11 mikewoolf4 2020-03-09          5
# 2  2020    13 mikewoolf4 2020-03-24          1
# 3  2020    15 mikewoolf4 2020-04-11          2

p19 <- ggplot(wkly19, aes(x = Date,y = orangeworm)) +
  geom_col(width = 10) +
  facet_grid(site ~ .) +
  # any width over 3.0 gives a warning about non-overlapping x intervals
  # I think it looks better with width = 5.0
  theme_bw() +
  xlab("") +
  ylab("NOW per week") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p19

ggsave(filename = "Y19_trapview_wkly_by_site.jpg", p19, path = "./results",
       width = 2.83, height = 5.83, dpi = 300, units = "in", device='jpg')

p20 <- ggplot(wkly20, aes(x = Date,y = orangeworm)) +
  geom_col(width = 10) +
  facet_grid(site ~ .) +
  # any width over 3.0 gives a warning about non-overlapping x intervals
  # I think it looks better with width = 5.0
  theme_bw() +
  xlab("") +
  ylab("NOW per week") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p20

ggsave(filename = "Y20_trapview_wkly_by_site.jpg", p20, path = "./results",
       width = 2.83, height = 5.83, dpi = 300, units = "in", device='jpg')


#-- 4. Merge 2019 and 2020 temperature data ---------------------------------

alltemps19 <- readr::read_csv("./data/trapview_temps_degf_y19.csv")
alltemps20 <- readr::read_csv("./data/trapview_temps_degf_y20.csv")

# Examine and fix discrepancy in site names in 2020 capture and temperature data
unique(allsites20$site)
# [1] "mikewoolf1" "mikewoolf2" "mikewoolf3" "mikewoolf4" "mikewoolf5"
unique(alltemps20$site)
# [1] "MWT1" "MWT2" "MWT3" "MWT4" "MWT5"

alltemps20$site[alltemps20$site == "MWT1"] <- "mikewoolf1"
alltemps20$site[alltemps20$site == "MWT2"] <- "mikewoolf2"
alltemps20$site[alltemps20$site == "MWT3"] <- "mikewoolf3"
alltemps20$site[alltemps20$site == "MWT4"] <- "mikewoolf4"
alltemps20$site[alltemps20$site == "MWT5"] <- "mikewoolf5"

# Modify for merge compatibility)
alltemps19 <- alltemps19 %>% 
  dplyr::rename(datetime = Date_time)
alltemps20 <- alltemps20 %>% 
  dplyr::rename(datetime = Date_time)

### Make variables in alltemps19 and alltemps20 compatible with each
### other and with the counts data set

# Change MWoolf_west site name for consistency w 2020
alltemps19$site[alltemps19$site == "MWoolf_west"] <- "mikewoolf1"
alltemps19
# A tibble: 18,969 x 6
# site      datetime            degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7
# 3 UCKearney 2019-05-16 19:00:00     55.0    53.8    56.8   90.4

# reorder consist w 2019
alltemps20 <- alltemps20[,c(6,1:5)] 

alltemps20$site[alltemps20$site == "MWT1"] <- "mikewoolf1"
alltemps20$site[alltemps20$site == "MWT2"] <- "mikewoolf2"
alltemps20$site[alltemps20$site == "MWT3"] <- "mikewoolf3"
alltemps20$site[alltemps20$site == "MWT4"] <- "mikewoolf4"
alltemps20$site[alltemps20$site == "MWT5"] <- "mikewoolf5"

unique(alltemps19$site)
# [1] [1] "UCKearney"   "usda"        "Perez"       "mikewoolf1"  "MWoolf_east"

unique(alltemps20$site)
# [1] "mikewoolf1" "mikewoolf2" "mikewoolf3" "mikewoolf4" "mikewoolf5"

# Also change MWoolf_west in the counts data set
counts_y19y20$site[counts_y19y20$site == "MWoolf_west"]  <- "mikewoolf1"
unique(counts_y19y20$site)
# [1] "UCKearney"   "MWoolf_east" "mikewoolf1"  "Perez"       "usda"        "mikewoolf2"  "mikewoolf3"  "mikewoolf4"  "mikewoolf5"  

# combine the temperature data sets
temps <- rbind(alltemps19,alltemps20)
temps
# A tibble: 36,549 x 6
# site      datetime            degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7

# Add Yr, Mnth, Julian, and Hr to the temps data set
temps <- temps %>% 
  mutate(Yr = year(datetime),
         Mnth = month(datetime, label = TRUE, abbr = TRUE),
         Julian = yday(datetime),
         Hr = hour(datetime))
temps
# A tibble: 36,549 x 10
#   site      datetime            degf_avg degf_lo degf_hi rh_avg    Yr Mnth  Julian    Hr
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl> <dbl> <ord>  <dbl> <int>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0  2019 May      136    17
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7  2019 May      136    18

# Merge temps to counts w left join, so that records in temps are retained only
# if they match a record in counts
length(temps$degf_avg[is.na(temps$degf_avg)])
# [1] 1
temps <- temps[!is.na(temps$datetime),] # drop the 1 NA in case that is a problem

#-- 4. Merge non-zero Count data to corresponding temperature records -------

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
