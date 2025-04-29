
library(tidyverse)

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

pet$FCV_owner_adv = pet$FCV_notice - pet$FCV_decision
pet$FCV_owner_disadv = pet$FCV_decision - pet$FCV_owner

p5a <- ggplot(pet %>% pivot_longer(cols = c(FCV_owner_adv, FCV_owner_disadv), names_to = "variable", values_to = "value"), 
              aes(x = tax_year, y = value, fill = variable)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_manual(values = c("blue", "red")) + theme_minimal()

p5a <- p5a + ylim(-125000, 1250000)

pet$LPV_owner_adv = pet$LPV_notice - pet$LPV_decision
pet$LPV_owner_disadv = pet$LPV_decision - pet$LPV_owner

p5b <- ggplot(pet %>% pivot_longer(cols = c(LPV_owner_adv, LPV_owner_disadv), names_to = "variable", values_to = "value"), 
              aes(x = tax_year, y = value, fill = variable)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_manual(values = c("blue", "red")) + theme_minimal()

p5b <- p5b + ylim(-100000, 1000000)

pet$FCV_owner_adv2 = pet$FCV_owner_adv - pet$FCV_owner_disadv

p5a2 <- ggplot(pet %>% pivot_longer(cols = c(FCV_owner_adv2), names_to = "variable", values_to = "value"), 
              aes(x = tax_year, y = value, fill = variable)) + 
  geom_boxplot() + scale_fill_manual(values = c("blue")) + theme_minimal() + 
  labs(title = "FCV Advantage minus Disadvantage", x = "Year", y = "", color = "Outcome")

pet$LPV_owner_adv2 = pet$LPV_owner_adv - pet$LPV_owner_disadv

p5b2 <- ggplot(pet %>% pivot_longer(cols = c(LPV_owner_adv2), names_to = "variable", values_to = "value"), 
               aes(x = tax_year, y = value, fill = variable)) + 
  geom_boxplot() + scale_fill_manual(values = c("blue")) + theme_minimal() +
  labs(title = "LPV Advantage minus Disadvantage", x = "Year", y = "", color = "Outcome")



