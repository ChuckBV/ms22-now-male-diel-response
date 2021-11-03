#===========================================================================#
# scratch_characterize_script10_datasets.R
#---------------------------------------------------------------------------#
#  Shows/confirms that counts precede temperature data in the Kearney and
#  USDA traps in 2019, but not in other cases
#===========================================================================#

### First run script10 to obtain dataframes allsites, counts_y19y20,
### alltemps19 and alltemps20

library(tidyverse)
library(lubridate)

#----------------------------------------------------------------------------#
################ Examine allsites count data sets ############################
#----------------------------------------------------------------------------#


head(allsites)
#              datetime pest_dif      site   Yr Mnth Julian Hr wk    caldate
# 1 2019-04-26 13:56:00        7 UCKearney 2019  Apr    116 13 17 2019-04-26
# 2 2019-05-02 01:57:00        1 UCKearney 2019  May    122  1 18 2019-05-02
#    2 of 1479 records


head(counts_y19y20)
#               datetime pest_dif      site   Yr Mnth Julian Hr
# 1 2019-04-26 13:56:00        7 UCKearney 2019  Apr    116 13
# 2 2019-05-02 01:57:00        1 UCKearney 2019  May    122  1
#    2 of 1479 records

#----------------------------------------------------------------------------#
#  The only difference is that allsites have additional variables for 
#  epiweek and with date as.Date() in addition to as.datetime()
#----------------------------------------------------------------------------#

### Examine first dates by site for 2019

allsites19 %>%
  group_by(site) %>%
  summarise(nObs = n())
# A tibble: 5 x 2
#   site         nObs
#   <chr>       <int>
# 1 MWoolf_east   111
# 2 MWoolf_west   355
# 3 Perez         103
# 4 UCKearney      81
# 5 usda          148

# [1] "UCKearney"   "usda"        "Perez"       "MWoolf_west" "MWoolf_east"

a1 <- allsites19 %>%
  filter(site == "UCKearney") %>%
  arrange(datetime) %>%
  head(72)

b1 <- allsites19 %>%
  filter(site == "usda") %>%
  arrange(datetime) %>%
  head(72)

c1 <- allsites19 %>%
  filter(site == "Perez") %>%
  arrange(datetime) %>%
  head(72)

d1 <- allsites19 %>%
  filter(site == "MWoolf_west") %>%
  arrange(datetime) %>%
  head(72)

e1 <- allsites19 %>%
  filter(site == "MWoolf_east") %>%
  arrange(datetime) %>%
  head(72)

  
#----------------------------------------------------------------------------#
################ Examine temperature data sets ###############################
#----------------------------------------------------------------------------#

### Look at the temperature data set for 2019

alltemps19
# A tibble: 18,969 x 6
#   site      Date_time           degf_avg degf_lo degf_hi rh_avg
#   <chr>     <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
# 1 UCKearney 2019-05-16 17:00:00     56.1    55.4    56.8   83.0
# 2 UCKearney 2019-05-16 18:00:00     57.5    56.1    58.6   81.7
# 3 UCKearney 2019-05-16 19:00:00     55.0    53.8    56.8   90.4

### Verify site names

unique(alltemps19$site)
# [1] "UCKearney"   "usda"        "Perez"       "MWoolf_west" "MWoolf_east"

### Examine first 1-3 days for each site/trap

a2 <- alltemps19 %>%
  filter(site == "UCKearney") %>%
  arrange(Date_time) %>%
  head(72)

b2 <- alltemps19 %>%
  filter(site == "usda") %>%
  arrange(Date_time) %>%
  head(72)

c2 <- alltemps19 %>%
  filter(site == "Perez") %>%
  arrange(Date_time) %>%
  head(72)

d2 <- alltemps19 %>%
  filter(site == "MWoolf_west") %>%
  arrange(Date_time) %>%
  head(72)

e2 <- alltemps19 %>%
  filter(site == "MWoolf_east") %>%
  arrange(Date_time) %>%
  head(72)


#----------------------------------------------------------------------------#
########### Compare count and temperature sets ###############################
#----------------------------------------------------------------------------#

head(a1,6)
head(a2,6)
#  For Kearney we show counts for 4/26, 5/2, 5/3 and 5/16, but
#  not temperature data until 5/16

head(b1,6)
head(b2,6)
#  For USDA we show counts in April but no temperature data until May 28

head(c1,6)
head(c2,6)
#  For Perez temperature and count data lined up

head(d1,6)
head(d2,6)
#  For MWoolf_west temperature and count data lined up

head(e1,6)
head(e2,6)
#  For MWoolf_west temperature and count data lined up


