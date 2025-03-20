
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

petitions$tax_year = as.character(petitions$tax_year)
pet = subset(petitions,include == 1)

p4a_noNA <- petitions %>% filter(!(is.na(outcome_FCV)|(is.na(tax_year))))

p4a <- ggplot(p4a_noNA %>% count(year, outcome_FCV), 
               aes(x = tax_year, y = n, color = outcome_FCV, group = outcome_FCV)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("No Change" = "darkgreen", 
                                "Granted in Full" = "black", 
                                "Granted in Part" = "darkgrey")) + 
  theme_minimal() + 
  labs(title = "Trends in Outcomes Over Time", x = "Year", y = "Count", color = "Outcome")


p4b_noNA <- petitions %>% filter(!(is.na(outcome_FCV)|(is.na(year))))

p4b <- ggplot(p4b_noNA %>% count(year, outcome_LPV), 
              aes(x = tax_year, y = n, color = outcome_LPV, group = outcome_LPV)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("No Change" = "darkgreen", 
                                "Granted in Full" = "black", 
                                "Granted in Part" = "darkgrey")) + 
  theme_minimal() + 
  labs(title = "Trends in Outcomes Over Time", x = "Year", y = "Count", color = "Outcome")
