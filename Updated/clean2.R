
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

p2a_noNA <- petitions %>% filter(!(is.na(outcome_FCV)|(is.na(year))))

p2a <- ggplot(p2a_noNA, aes(x = tax_year, fill = outcome_FCV)) + geom_bar(position = "stack") + theme_minimal()  
p2a <- p2a + labs(title = "Number of Observations per Year by Outcome",x = "Year",y = "Total Count",fill = "Outcome")

p2b_noNA <- petitions %>% filter(!(is.na(outcome_LPV)|(is.na(year))))

p2b <- ggplot(p2b_noNA, aes(x = tax_year, fill = outcome_LPV)) + geom_bar(position = "stack") + theme_minimal()  
p2b <- p2b + labs(title = "Number of Observations per Year by Outcome",x = "Year",y = "Total Count",fill = "Outcome")

p2a2 <- ggplot(p2a_noNA, aes(x = tax_year, fill = outcome_FCV)) + geom_bar(position = "stack")
p2a2 <- p2a2 + geom_text(stat = "count", 
    aes(label = paste0(round(after_stat(count / tapply(..count.., ..x.., sum)[as.character(..x..)] * 100), 1), "%")),
    position = position_stack(vjust = 0.5), color = "white", size = 2.5)
p2a2 <- p2a2 + scale_fill_manual(values = c("No Change" = "darkgreen","Granted in Full" = "black","Granted in Part" = "darkgrey"))

p2b2 <- ggplot(p2b_noNA, aes(x = tax_year, fill = outcome_LPV)) + geom_bar(position = "stack")
p2b2 <- p2b2 + geom_text(stat = "count", 
                         aes(label = paste0(round(after_stat(count / tapply(..count.., ..x.., sum)[as.character(..x..)] * 100), 1), "%")),
                         position = position_stack(vjust = 0.5), color = "white", size = 2.5)
p2b2 <- p2b2 + scale_fill_manual(values = c("No Change" = "darkgreen","Granted in Full" = "black","Granted in Part" = "darkgrey"))

