
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

p2a <- ggplot(pet, aes(x = tax_year, fill = outcome_FCV)) + geom_bar(position = "stack") + theme_minimal()  
p2a <- p2a + labs(title = "Number of Observations per Year by Outcome",x = "Year",y = "Total Count",fill = "Outcome")
p2a <- p2a + scale_y_continuous(sec.axis = dup_axis(name = ""))

p2b <- ggplot(pet, aes(x = tax_year, fill = outcome_LPV)) + geom_bar(position = "stack") + theme_minimal()  
p2b <- p2b + labs(title = "Number of Observations per Year by Outcome",x = "Year",y = "Total Count",fill = "Outcome")
p2b <- p2b + scale_y_continuous(sec.axis = dup_axis(name = ""))

p2a2 <- ggplot(pet, aes(x = tax_year, fill = outcome_FCV)) + geom_bar(position = "stack") + theme_minimal() 
p2a2 <- p2a2 + geom_text(stat = "count", 
    aes(label = paste0(round(after_stat(count / tapply(..count.., ..x.., sum)[as.character(..x..)] * 100), 1), "%")),
    position = position_stack(vjust = 0.5), color = "white", size = 2.5)
p2a2 <- p2a2 + scale_fill_manual(values = c("No Change or Worse" = "darkgreen","Granted in Full" = "blue","Granted in Part" = "red"))
p2a2 <- p2a2 + labs(fill = "Outcome for Full Cash Value (FCV)")
p2a2 <- p2a2 + labs(x = "Tax Year", y = "Number of Petitions")
p2a2 <- p2a2 + scale_y_continuous(sec.axis = dup_axis(name = ""))

p2b2 <- ggplot(pet, aes(x = tax_year, fill = outcome_LPV)) + geom_bar(position = "stack") + theme_minimal() 
p2b2 <- p2b2 + geom_text(stat = "count", 
                         aes(label = paste0(round(after_stat(count / tapply(..count.., ..x.., sum)[as.character(..x..)] * 100), 1), "%")),
                         position = position_stack(vjust = 0.5), color = "white", size = 2.5)
p2b2 <- p2b2 + scale_fill_manual(values = c("No Change or Worse" = "darkgreen","Granted in Full" = "blue","Granted in Part" = "red"))
p2b2 <- p2b2 + labs(fill = "Outcome for Limited Property Value (LPV)")
p2b2 <- p2b2 + labs(x = "Tax Year", y = "Number of Petitions")
p2b2 <- p2b2 + scale_y_continuous(sec.axis = dup_axis(name = ""))
