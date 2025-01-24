
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

p2a_noNA <- petitions %>% filter(!(is.na(outcome_FCV)|(is.na(year))))

p2a <- ggplot(p2a_noNA, aes(x = year, fill = outcome_FCV)) + geom_bar(position = "stack") + theme_minimal()  
p2a <- p2a + labs(title = "Number of Observations per Year by Outcome",x = "Year",y = "Total Count",fill = "Outcome")

p2b_noNA <- petitions %>% filter(!(is.na(outcome_LPV)|(is.na(year))))

p2b <- ggplot(p2b_noNA, aes(x = year, fill = outcome_LPV)) + geom_bar(position = "stack") + theme_minimal()  
p2b <- p2b + labs(title = "Number of Observations per Year by Outcome",x = "Year",y = "Total Count",fill = "Outcome")

p2a2 <- ggplot(p2a_noNA, aes(x = year, fill = outcome_FCV)) + geom_bar(position = "stack")
p2a2 <- p2a2 + geom_text(stat = "count", 
    aes(label = paste0(round(after_stat(count / tapply(..count.., ..x.., sum)[as.character(..x..)] * 100), 1), "%")),
    position = position_stack(vjust = 0.5), color = "white", size = 2.5)
p2a2 <- p2a2 + scale_fill_manual(values = c("No Change" = "darkgreen","Granted in Full" = "black","Granted in Part" = "darkgrey"))

p2b2 <- ggplot(p2b_noNA, aes(x = year, fill = outcome_LPV)) + geom_bar(position = "stack")
p2b2 <- p2b2 + geom_text(stat = "count", 
                         aes(label = paste0(round(after_stat(count / tapply(..count.., ..x.., sum)[as.character(..x..)] * 100), 1), "%")),
                         position = position_stack(vjust = 0.5), color = "white", size = 2.5)
p2b2 <- p2b2 + scale_fill_manual(values = c("No Change" = "darkgreen","Granted in Full" = "black","Granted in Part" = "darkgrey"))

