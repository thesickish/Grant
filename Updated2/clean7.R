
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

pet$LPV_owner_adv = pet$LPV_notice - pet$LPV_decision
pet$LPV_owner_disadv = pet$LPV_decision - pet$LPV_owner

p7a <- ggplot(pet, aes(x=tax_year, y=FCV_owner_adv)) + theme_minimal()
p7a <- p7a + stat_summary(fun = sum, geom = "bar", aes(group = 1)) +
  labs(title = "Total $ Value Granted (Full Cash Value)", x = "Tax Year", y = "Dollars (in Millions)", color = "Outcome") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))


p7b <- ggplot(pet, aes(x=tax_year, y=LPV_owner_adv)) + theme_minimal()
p7b <- p7b + stat_summary(fun = sum, geom = "bar", aes(group = 1)) +
  labs(title = "Total $ Value Granted (Limited Property Value)", x = "Tax Year", y = "Dollars (in Millions)", color = "Outcome") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))

p7a2 <- ggplot(pet) + theme_minimal()
p7a2 <- p7a2 + stat_summary(aes(x = tax_year, y = FCV_owner_adv, color = "FCV Owner Adv", group = 1), fun = sum, geom = "line")
p7a2 <- p7a2 + stat_summary(aes(x = tax_year, y = FCV_owner_disadv, color = "FCV Owner Disadv", group = 1), fun = sum, geom = "line")
p7a2 <- p7a2 + scale_color_manual(values = c("FCV Owner Adv" = "blue", "FCV Owner Disadv" = "red"),
  name = "", labels = c("Granted", "Not Granted")) +
  labs(title = "Total $ Value Granted vs Not Granted (Full Cash Value)", x = "Tax Year", y = "Dollars (in Millions)", color = "Outcome") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))

p7b2 <- ggplot(pet) + theme_minimal()
p7b2 <- p7b2 + stat_summary(aes(x = tax_year, y = LPV_owner_adv, color = "LPV Owner Adv", group = 1), fun = sum, geom = "line")
p7b2 <- p7b2 + stat_summary(aes(x = tax_year, y = LPV_owner_disadv, color = "LPV Owner Disadv", group = 1), fun = sum, geom = "line")
p7b2 <- p7b2 + scale_color_manual(values = c("LPV Owner Adv" = "blue", "LPV Owner Disadv" = "red"),
    name = "", labels = c("Granted", "Not Granted")) +
  labs(title = "Total $ Value Granted vs Not Granted (Limited Property Value)", x = "Tax Year", y = "Dollars (in Millions)", color = "Outcome") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))

