
library(tidyverse)
library(scales)

# read data into tibble
petitions <- read_csv("Updated2/master_edited_4.28.25.csv")

# create identifier
petitions$ID <- 1:nrow(petitions)

# rename relevant columns
names(petitions)[2] <- 'include'
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

petitions$tax_year = as.character(petitions$tax_year)
pet = subset(petitions,include == 1)

pet$FCV_change = as.numeric(pet$FCV_change)
pet$LPV_change = as.numeric(pet$LPV_change)

# plot of LPV changes across the years
p1a <- ggplot(pet, aes(x=tax_year, y=FCV_change)) + geom_boxplot() + theme_minimal()
p1a <- p1a + geom_point(color = "blue",stat = "sum", aes(size = after_stat(n)))
p1a <- p1a + scale_size_continuous(name = "Number of Petitions",breaks = c(2,4,6,8,10))
p1a <- p1a + labs(x = "Tax Year", y = "Percent Change in Full Cash Value (FCV)")
p1a <- p1a + scale_y_continuous(labels = percent)

# plot of FCV changes across the years
p1b <- ggplot(pet, aes(x=tax_year, y=LPV_change)) + geom_boxplot() + theme_minimal()
p1b <- p1b + geom_point(color = "blue",stat = "sum", aes(size = after_stat(n)))
p1b <- p1b + scale_size_continuous(name = "Number of Petitions",breaks = c(2,4,6,8,10))
p1b <- p1b + labs(x = "Tax Year", y = "Percent Change in Limited Property Value (LPV)")
p1b <- p1b + scale_y_continuous(labels = percent)

