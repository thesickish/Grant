
library(tidyverse)

# read data into tibble
petitions <- read_csv("Updated/Master_edited_new.csv")

# create identifier
petitions$ID <- 1:nrow(petitions)

# rename relevant columns
names(petitions)[1] <- 'include'
names(petitions)[3] <- 'tax_year'
names(petitions)[11] <- 'FCV_owner'
names(petitions)[12] <- 'LPV_owner'
names(petitions)[13] <- 'FCV_notice'
names(petitions)[14] <- 'LPV_notice'
names(petitions)[15] <- 'FCV_decision'
names(petitions)[16] <- 'LPV_decision'
names(petitions)[19] <- 'outcome'
names(petitions)[20] <- 'outcome_details'
names(petitions)[21] <- 'FCV_change'
names(petitions)[22] <- 'LPV_change'

# plot of LPV changes across the years
p1a <- ggplot(petitions, aes(x=tax_year, y=FCV_change)) + geom_boxplot() + theme_minimal()
p1a <- p1a + geom_point(color = "blue",stat = "sum", aes(size = after_stat(n))) 

# plot of FCV changes across the years
p1b <- ggplot(petitions, aes(x=tax_year, y=LPV_change)) + geom_boxplot() + theme_minimal()
p1b <- p1b + geom_point(color = "blue",stat = "sum", aes(size = after_stat(n))) 
