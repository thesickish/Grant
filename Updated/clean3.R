
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

p3a_noNA <- petitions %>% drop_na(FCV_owner, FCV_notice, FCV_decision, tax_year)

p3a <- ggplot(p3a_noNA %>% pivot_longer(cols = c(FCV_owner, FCV_notice, FCV_decision), names_to = "variable", values_to = "value"), 
       aes(x = tax_year, y = value, fill = variable)) + 
       geom_boxplot() + scale_fill_manual(values = c("red", "blue", "green")) + theme_minimal()

p3b_noNA <- petitions %>% drop_na(LPV_owner, LPV_notice, LPV_decision, year)

p3b <- ggplot(p3b_noNA %>% pivot_longer(cols = c(LPV_owner, LPV_notice, LPV_decision), names_to = "variable", values_to = "value"), 
              aes(x = tax_year, y = value, fill = variable)) + 
  geom_boxplot() + scale_fill_manual(values = c("red", "blue", "green")) + theme_minimal()
