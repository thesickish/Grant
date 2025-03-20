
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

petitions$FCV_owner_adv = petitions$FCV_notice - petitions$FCV_decision
petitions$FCV_owner_disadv = petitions$FCV_decision - petitions$FCV_owner

p5a_noNA <- petitions %>% drop_na(FCV_owner_adv, FCV_owner_disadv, tax_year)

p5a <- ggplot(p5a_noNA %>% pivot_longer(cols = c(FCV_owner_adv, FCV_owner_disadv), names_to = "variable", values_to = "value"), 
              aes(x = tax_year, y = value, fill = variable)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_manual(values = c("blue", "red")) + theme_minimal()

petitions$LPV_owner_adv = petitions$LPV_notice - petitions$LPV_decision
petitions$LPV_owner_disadv = petitions$LPV_decision - petitions$LPV_owner

p5b_noNA <- petitions %>% drop_na(LPV_owner_adv, LPV_owner_disadv, year)

p5b <- ggplot(p5b_noNA %>% pivot_longer(cols = c(LPV_owner_adv, LPV_owner_disadv), names_to = "variable", values_to = "value"), 
              aes(x = tax_year, y = value, fill = variable)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_manual(values = c("blue", "red")) + theme_minimal()

p5b <- p5b + ylim(-1000000, 1000000)


petitions$FCV_owner_adv2 = petitions$FCV_owner_adv - petitions$FCV_owner_disadv

p5a2_noNA <- petitions %>% drop_na(FCV_owner_adv2, year)

p5a2 <- ggplot(p5a2_noNA %>% pivot_longer(cols = c(FCV_owner_adv2), names_to = "variable", values_to = "value"), 
              aes(x = tax_year, y = value, fill = variable)) + 
  geom_boxplot() + scale_fill_manual(values = c("blue")) + theme_minimal()


petitions$LPV_owner_adv2 = petitions$LPV_owner_adv - petitions$LPV_owner_disadv

p5b2_noNA <- petitions %>% drop_na(LPV_owner_adv2, year)

p5b2 <- ggplot(p5b2_noNA %>% pivot_longer(cols = c(LPV_owner_adv2), names_to = "variable", values_to = "value"), 
               aes(x = tax_year, y = value, fill = variable)) + 
  geom_boxplot() + scale_fill_manual(values = c("blue")) + theme_minimal()


