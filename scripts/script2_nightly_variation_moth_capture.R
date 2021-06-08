#===========================================================================#
# script2_nightly_variation_moth_capture
# 
# Examine variation in nightly temperatures, and determine the number of
# observations in a range relevant to shiftying mating activity earler
#
#===========================================================================#

library(tidyverse)
library(lubridate)


allsites19 <- readr::read_csv("./allsites_y19.csv")
allsites20 <- readr::read_csv("./allsites_y20.csv")
