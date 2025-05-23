
library(tidyverse)

# read data into tibble
petitions <- read_csv("fwforyotam/Master_edited.csv")

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

p4a_noNA <- petitions %>% filter(!(is.na(outcome_FCV)|(is.na(year))))

p4a <- ggplot(p4a_noNA %>% count(year, outcome_FCV), 
               aes(x = year, y = n, color = outcome_FCV, group = outcome_FCV)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("No Change" = "darkgreen", 
                                "Granted in Full" = "black", 
                                "Granted in Part" = "darkgrey")) + 
  theme_minimal() + 
  labs(title = "Trends in Outcomes Over Time", x = "Year", y = "Count", color = "Outcome")


p4b_noNA <- petitions %>% filter(!(is.na(outcome_FCV)|(is.na(year))))

p4b <- ggplot(p4b_noNA %>% count(year, outcome_LPV), 
              aes(x = year, y = n, color = outcome_LPV, group = outcome_LPV)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("No Change" = "darkgreen", 
                                "Granted in Full" = "black", 
                                "Granted in Part" = "darkgrey")) + 
  theme_minimal() + 
  labs(title = "Trends in Outcomes Over Time", x = "Year", y = "Count", color = "Outcome")
