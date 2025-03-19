
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

p3a_noNA <- petitions %>% drop_na(FCV_owner, FCV_notice, FCV_decision, year)

p3a <- ggplot(p3a_noNA %>% pivot_longer(cols = c(FCV_owner, FCV_notice, FCV_decision), names_to = "variable", values_to = "value"), 
       aes(x = year, y = value, fill = variable)) + 
       geom_boxplot() + scale_fill_manual(values = c("red", "blue", "green")) + theme_minimal()

p3b_noNA <- petitions %>% drop_na(LPV_owner, LPV_notice, LPV_decision, year)

p3b <- ggplot(p3b_noNA %>% pivot_longer(cols = c(LPV_owner, LPV_notice, LPV_decision), names_to = "variable", values_to = "value"), 
              aes(x = year, y = value, fill = variable)) + 
  geom_boxplot() + scale_fill_manual(values = c("red", "blue", "green")) + theme_minimal()
