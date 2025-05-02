
library(tidyverse)

# read data into tibble
petitions <- read_csv("Updated2/master_edited_5.2.25.csv")

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

p4a <- ggplot(pet %>% count(tax_year, outcome_FCV), 
               aes(x = tax_year, y = n, color = outcome_FCV, group = outcome_FCV)) + 
  geom_line(size = 1) + geom_point(size = 2) + theme_minimal() + 
  scale_color_manual(values = c("No Change or Worse" = "darkgreen", 
                                "Granted in Full" = "blue", 
                                "Granted in Part" = "red")) +
  scale_y_continuous(sec.axis = dup_axis(name = ""),limits = c(1,14), breaks = seq(0, 14, by = 2)) +
  labs(title = "Outcomes Over Time", x = "Tax Year", y = "Number of Petitions", color = "Outcome for Full Cash Value (FCV)")
  

p4b <- ggplot(pet %>% count(tax_year, outcome_LPV), 
              aes(x = tax_year, y = n, color = outcome_LPV, group = outcome_LPV)) + 
  geom_line(size = 1) + geom_point(size = 2) + theme_minimal() + 
  scale_color_manual(values = c("No Change or Worse" = "darkgreen", 
                                "Granted in Full" = "blue", 
                                "Granted in Part" = "red")) + 
  scale_y_continuous(sec.axis = dup_axis(name = ""),limits = c(1,14), breaks = seq(0, 14, by = 2)) +
  labs(title = "Outcomes Over Time", x = "Tax Year", y = "Number of Petitions", color = "Outcome for Limited Property Value (LPV)")
