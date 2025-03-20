
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

petitions$FCV_owner_adv = petitions$FCV_notice - petitions$FCV_decision
petitions$FCV_owner_disadv = petitions$FCV_decision - petitions$FCV_owner

petitions$LPV_owner_adv = petitions$LPV_notice - petitions$LPV_decision
petitions$LPV_owner_disadv = petitions$LPV_decision - petitions$LPV_owner

p7a <- ggplot(petitions, aes(x=tax_year, y=FCV_owner_adv)) + theme_minimal()
p7a <- p7a + stat_summary(fun = sum, geom = "bar", aes(group = 1))

p7b <- ggplot(petitions, aes(x=tax_year, y=LPV_owner_adv)) + theme_minimal()
p7b <- p7b + stat_summary(fun = sum, geom = "bar", aes(group = 1))

p7a2 <- ggplot(petitions) + theme_minimal()
p7a2 <- p7a2 + stat_summary(aes(x = tax_year, y = FCV_owner_adv, color = "FCV Owner Adv", group = 1), fun = sum, geom = "line")
p7a2 <- p7a2 + stat_summary(aes(x = tax_year, y = FCV_owner_disadv, color = "FCV Owner Disadv", group = 1), fun = sum, geom = "line")
p7a2 <- p7a2 + scale_color_manual(values = c("FCV Owner Adv" = "blue", "FCV Owner Disadv" = "red"))
