#===========================================================================#
# script1_clean_save_summarize_count_data.R
# 
# PARTS
# 1. Load combined 2019 and 2020 count and temperature data files (line 15)
# 2. Merge and 2019 and 2020 count data (line 62)
#    - output "counts_all.csv" to ./data-intermediate
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
#-- 2. Pool and 2019 and 2020 count data and save -----------------------------------
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
#-- 3. Overview of count trends for 2019 and 20202                  ---------
#---------------------------------------------------------------------------#

counts_y19y20 %>% 
  filter(Mnth %in% c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")) %>% 
  group_by(Yr,Mnth) %>%
  summarise(Total = sum(pest_dif, na.rm = TRUE)) %>%
  pivot_wider(names_from = Mnth, values_from = Total)
# A tibble: 2 x 9
# Groups:   Yr [2]
#      Yr   Apr   May   Jun   Jul   Aug   Sep   Oct   Mar
#   <dbl> <int> <int> <int> <int> <int> <int> <int> <int>
# 1  2019    29   249    94   507   529   709   143    NA
# 2  2020   123    44    96   264   377   424    NA     6

