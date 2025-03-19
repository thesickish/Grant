
library(tidyverse)

# read data into tibble
petitions <- read_csv("Updated/Master_edited.csv")

# create identifier
petitions$ID <- 1:nrow(petitions)

# rename relevant columns
names(petitions)[9] <- 'FCV_owner'
names(petitions)[10] <- 'LPV_owner'
names(petitions)[11] <- 'FCV_notice'
names(petitions)[12] <- 'LPV_notice'
names(petitions)[13] <- 'FCV_decision'
names(petitions)[14] <- 'LPV_decision'
names(petitions)[17] <- 'outcome'
names(petitions)[18] <- 'outcome_details'
names(petitions)[19] <- 'FCV_change'
names(petitions)[20] <- 'LPV_change'

# extract year from date
names(petitions)[2] <- 'date'
petitions = separate_wider_delim(petitions, cols = date, delim = "/", names = c("month", "day","year"),too_few = "align_start")

# plot of LPV changes across the years
p1a <- ggplot(petitions, aes(x=year, y=FCV_change)) + geom_boxplot() + theme_minimal()
p1a <- p1a + geom_point(color = "blue",stat = "sum", aes(size = after_stat(n))) 

# plot of FCV changes across the years
p1b <- ggplot(petitions, aes(x=year, y=LPV_change)) + geom_boxplot() + theme_minimal()
p1b <- p1b + geom_point(color = "blue",stat = "sum", aes(size = after_stat(n))) 
