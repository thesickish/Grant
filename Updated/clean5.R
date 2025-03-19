
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

petitions$FCV_owner_adv = petitions$FCV_notice - petitions$FCV_decision
petitions$FCV_owner_disadv = petitions$FCV_decision - petitions$FCV_owner

p5a_noNA <- petitions %>% drop_na(FCV_owner_adv, FCV_owner_disadv, year)

p5a <- ggplot(p5a_noNA %>% pivot_longer(cols = c(FCV_owner_adv, FCV_owner_disadv), names_to = "variable", values_to = "value"), 
              aes(x = year, y = value, fill = variable)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_manual(values = c("blue", "red")) + theme_minimal()

petitions$LPV_owner_adv = petitions$LPV_notice - petitions$LPV_decision
petitions$LPV_owner_disadv = petitions$LPV_decision - petitions$LPV_owner

p5b_noNA <- petitions %>% drop_na(LPV_owner_adv, LPV_owner_disadv, year)

p5b <- ggplot(p5b_noNA %>% pivot_longer(cols = c(LPV_owner_adv, LPV_owner_disadv), names_to = "variable", values_to = "value"), 
              aes(x = year, y = value, fill = variable)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_manual(values = c("blue", "red")) + theme_minimal()

p5b <- p5b + ylim(-1000000, 1000000)


petitions$FCV_owner_adv2 = petitions$FCV_owner_adv - petitions$FCV_owner_disadv

p5a2_noNA <- petitions %>% drop_na(FCV_owner_adv2, year)

p5a2 <- ggplot(p5a2_noNA %>% pivot_longer(cols = c(FCV_owner_adv2), names_to = "variable", values_to = "value"), 
              aes(x = year, y = value, fill = variable)) + 
  geom_boxplot() + scale_fill_manual(values = c("blue")) + theme_minimal()


petitions$LPV_owner_adv2 = petitions$LPV_owner_adv - petitions$LPV_owner_disadv

p5b2_noNA <- petitions %>% drop_na(LPV_owner_adv2, year)

p5b2 <- ggplot(p5b2_noNA %>% pivot_longer(cols = c(LPV_owner_adv2), names_to = "variable", values_to = "value"), 
               aes(x = year, y = value, fill = variable)) + 
  geom_boxplot() + scale_fill_manual(values = c("blue")) + theme_minimal()


