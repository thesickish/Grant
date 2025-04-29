
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

pet$FCV_owner = as.numeric(pet$FCV_owner)
pet$LPV_owner = as.numeric(pet$LPV_owner)
pet$FCV_notice = as.numeric(pet$FCV_notice)
pet$LPV_notice = as.numeric(pet$LPV_notice)
pet$FCV_decision = as.numeric(pet$FCV_decision)
pet$LPV_decision = as.numeric(pet$LPV_decision)

pet$FCV_owner_gain = (pet$FCV_notice - pet$FCV_decision)/(pet$FCV_notice - pet$FCV_owner)

p6a <- ggplot(pet %>% pivot_longer(cols = c(FCV_owner_gain), names_to = "variable", values_to = "value"), 
               aes(x = tax_year, y = value)) + 
  geom_boxplot() + theme_minimal()

p6a <- p6a + geom_point(color = "blue",stat = "sum", aes(size = after_stat(n))) +
  labs(title = "Full Cash Value (FCV) % of Range Granted", x = "Tax Year", y = "Percent") +
  scale_size_continuous(name = "Number of Petitions",breaks = c(2,4,6,8)) +
  scale_y_continuous(labels = percent)


pet$LPV_owner_gain = (pet$LPV_notice - pet$LPV_decision)/(pet$LPV_notice - pet$LPV_owner)

p6b <- ggplot(pet %>% pivot_longer(cols = c(LPV_owner_gain), names_to = "variable", values_to = "value"), 
              aes(x = tax_year, y = value)) + 
  geom_boxplot() + theme_minimal()

p6b <- p6b + geom_point(color = "blue",stat = "sum", aes(size = after_stat(n))) +
  labs(title = "Limited Property Value (LPV) % of Range Granted", x = "Tax Year", y = "Percent") +
scale_size_continuous(name = "Number of Petitions",breaks = c(2,4,6,8)) +
  scale_y_continuous(labels = percent)


