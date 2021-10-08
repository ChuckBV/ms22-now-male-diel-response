# scratch_frequency_tables.R

library(vcdExtra)

### Convert a table into individual records

# Create test table

FrequencyTable <- data.frame(Gender = c("Male","Male","Female","Female"),
                             Enrollment = c(0,1,0,1),
                             Frequency = c(50,25,50,15))
FrequencyTable
#   Gender Enrollment Frequency
# 1   Male          0        50
# 2   Male          1        25
# 3 Female          0        50
# 4 Female          1        15

sum(FrequencyTable$Frequency)
# [1] 140

### See https://stats.stackexchange.com/questions/15574/how-to-convert-a-frequency-table-into-a-vector-of-values/15575
x <- rep(FrequencyTable$Gender, FrequencyTable$Frequency)
y <- rep(FrequencyTable$Enrollment, FrequencyTable$Frequency)

expanded <- data.frame(x,y)

### See https://opensourceconnections.com/blog/2016/09/17/expanding-data-frequency-table-r-stata/
expanded2 <- vcdExtra::expand.dft(FrequencyTable, freq = "Frequency")
expanded2
