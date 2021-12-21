#estimate lengths for fish fragments from AR43 MOC10
setwd("/Users/juliacox/Desktop/OTZ Fish Biomass/Data")
library(readxl)
library(tidyverse)
library(dplyr)
assigned <- read_excel("AR43 Assigned Lengths.xlsx")
whole <- read_excel("AR43 Whole Lengths.xlsx")
assigned$assigned_type
assigned$tow_net <- paste(assigned$Tow, assigned$Net, sep = "-")
whole$tow_net <- paste(whole$Tow, whole$Net, sep = "-")

#split assigned by net
split(assigned, with(assigned, interaction()), drop = TRUE)

#if statements not working 
if (str_count(assigned$`Family (larvae only counted)` %in% assigned$Net < 5))  {
  #Boolean_Expression 1 result is TRUE then, it will check for Boolean_Expression 2
  if (str_count(assigned$`Family (larvae only counted)` %in% assigned$Tow < 5))  {
    #Boolean_Expression 2 result is TRUE, then it will check for Boolean_Expression 3
    if (str_count(assigned$`Family (larvae only counted)` %in% assigned$Cruise < 5)) {
      assigned$`Family (larvae only counted)` #do nothing
    } else {
      #Boolean_Expression 2 result is FALSE then, these statements will be executed
      "tow" %>% assigned$assign_type #add the word tow to a new column at rows
    } else  {
      #If the Boolean_Expression 1 result is FALSE, these statements will be executed
      "cruise" %>% assigned$assigned_type
    } else { 
      "net" %>% assigned$assigned_type 
    }
  }
}