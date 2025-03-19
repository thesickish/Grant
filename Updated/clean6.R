
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

petitions$FCV_owner_gain = (petitions$FCV_notice - petitions$FCV_decision)/(petitions$FCV_notice - petitions$FCV_owner)

p6a_noNA <- petitions %>% drop_na(FCV_owner_gain, year)

p6a <- ggplot(p6a_noNA %>% pivot_longer(cols = c(FCV_owner_gain), names_to = "variable", values_to = "value"), 
               aes(x = year, y = value)) + 
  geom_boxplot() + theme_minimal()

p6a <- p6a + geom_point(color = "blue",stat = "sum", aes(size = after_stat(n))) 


petitions$LPV_owner_gain = (petitions$LPV_notice - petitions$LPV_decision)/(petitions$LPV_notice - petitions$LPV_owner)

p6b_noNA <- petitions %>% drop_na(LPV_owner_gain, year)

p6b <- ggplot(p6b_noNA %>% pivot_longer(cols = c(LPV_owner_gain), names_to = "variable", values_to = "value"), 
              aes(x = year, y = value)) + 
  geom_boxplot() + theme_minimal()

p6b <- p6b + geom_point(color = "blue",stat = "sum", aes(size = after_stat(n))) 

