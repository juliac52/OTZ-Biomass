#estimate lengths for fish fragments from AR43 MOC10
setwd("/Users/juliacox/Desktop/OTZ Fish Biomass/Data")
library(readxl)
library(tidyverse)
library(dplyr)
assigned <- read_excel("AR43 Assigned Lengths.xlsx")
assigned <- assigned %>% rename( Family = `Family (larvae only counted)`)
whole <- read_excel("AR43 Whole Lengths.xlsx")
#make columns with net, tow, and cruise identifiers for each family
assigned$tow_net <- paste(assigned$Tow, assigned$Net, sep = "-")
whole$tow_net <- paste(whole$Tow, whole$Net, sep = "-") 
#make column to use to count how many fish are in each net per tow 
assigned$`Fish Net` <- substring(assigned$`Fish ID`,1,13)
whole$`Fish Net` <- substring(whole$`Fish ID`,1,13)
#make column to use to count how many fish are in each tow
assigned$`Fish Tow` <- paste(assigned$Tow, assigned$Family, sep = "-")
whole$`Fish Tow` <- paste(whole$Tow, whole$Family, sep = "-")
#count frequencies of families caught in net, tow, and cruise 
net_freq <-table(whole$`Fish Net`)
net_freq <- as.data.frame(net_freq)
tow_freq <- table(whole$`Fish Tow`)
tow_freq <- as.data.frame(tow_freq)
cruise_freq <- table(whole$Family)
cruise_freq <- as.data.frame(cruise_freq)
net_freq <- net_freq %>% rename(`Fish Net` = Var1, frequency = Freq)
tow_freq <- tow_freq %>% rename(`Fish Tow` = Var1, frequency = Freq)
cruise_freq <- cruise_freq %>% rename(`Family` = Var1, frequency = Freq)
#merge whole and assigned with net_freq, tow_freq, and cruise_freq
whole <- merge(whole,net_freq,by="Fish Net")
whole <- merge(whole,tow_freq,by="Fish Tow")
whole <- merge(whole,cruise_freq,by="Family")
whole <- whole %>% rename(fish_per_net = frequency.x, fish_per_tow = frequency.y, fish_per_cruise = frequency)
assigned <- merge(assigned,net_freq,by="Fish Net")
assigned <- merge(assigned,tow_freq,by="Fish Tow")
assigned <- merge(assigned,cruise_freq,by="Family")
assigned <- assigned %>% rename(fish_per_net = frequency.x, fish_per_tow = frequency.y, fish_per_cruise = frequency)
#combine whole and assigned dataframes 
AppendMe <- function(dfNames) {
  do.call(rbind, lapply(dfNames, function(x) {
    cbind(get(x), source = x)
  }))
}
total_AR43 <- AppendMe(c("whole", "assigned"))
#
total_AR43$mean_level <- ifelse(total_AR43$fish_per_net >= 5, 'net',
                    ifelse(total_AR43$fish_per_tow >= 5, 'tow',
                           ifelse(total_AR43$fish_per_cruise >= 5, 'cruise', 'N/A')))
#find known length means for all families 
net_means <- whole %>% group_by(`Fish Net`) %>%
  summarise(mean=mean(`Length (cm)`))
net_means <- net_means %>% rename(net_mean = mean)
tow_means <- whole %>% group_by(`Fish Tow`) %>%
  summarise(mean=mean(`Length (cm)`))
tow_means <- tow_means %>% rename(tow_mean = mean)
cruise_means <- whole %>% group_by(Family) %>%
  summarise(mean=mean(`Length (cm)`))
cruise_means <- cruise_means %>% rename(cruise_mean = mean)
#combine mean tables with total_AR43 
total_AR43 <- merge(total_AR43,net_means,by="Fish Net")
total_AR43 <- merge(total_AR43,tow_means,by="Fish Tow")
total_AR43 <- merge(total_AR43,cruise_means,by="Family")
#assign lenths based on mean_level column
total_AR43$assigned_length_cm <- ifelse(total_AR43$mean_level == 'net', total_AR43$net_mean,
                                ifelse(total_AR43$mean_level == 'tow', total_AR43$tow_mean,
                                       ifelse(total_AR43$mean_level == 'cruise', total_AR43$cruise_mean, 'N/A')))
#"filter" data into two dataframes based on 'source'
assigned_length_AR43 <- total_AR43[total_AR43$source == 'assigned',]
whole_length_AR43 <- total_AR43[total_AR43$source == 'whole',]
#remove column with broken lengths in assigned   
assigned_length_AR43 = subset(assigned_length_AR43, select = -c(`Length (cm)`) )
#remove column with assigned lengths in whole 
whole_length_AR43 = subset(whole_length_AR43, select = -c(assigned_length_cm) )
#change col names to combine whole and assigned lenght dataframes 
assigned_length_AR43 <- assigned_length_AR43 %>% rename(`Length (cm)` = assigned_length_cm, 
                                                        length_type = source)
whole_length_AR43 <- whole_length_AR43 %>% rename(length_type = source)
#merge dataframes 
AR43_final <- AppendMe(c("assigned_length_AR43", "whole_length_AR43"))
AR43_final$`Length (cm)` <- as.numeric(AR43_final$`Length (cm)`)
#write to excel spreadsheet 
library(xlsx)
write.xlsx(AR43_final, file = "AR43_final_fish_measurements.xlsx",
           sheetName = "Sheet 1", append = FALSE)

##########sandbox vvv###########
ifelse(whole$fish_per_net >= 5, mean_net <- whole %>% group_by(`Fish Net`) %>%
         summarise(mean=mean(`Length (cm)`)), 
       ifelse(whole$fish_per_tow >= 5, mean_tow <- whole %>% group_by(`Fish Tow`) %>%
                summarise(mean=mean(`Length (cm)`)), 
              ifelse(whole$fish_per_cruise >= 5, mean_cruise <- whole %>% group_by(Family) %>%
                       summarise(mean=mean(`Length (cm)`)),
                     whole$Family == whole$Family)))
       
if (net_freq$frequency >= 5) {
  net_means <- whole %>% group_by(`Fish Net`) %>%
    summarise(mean=mean(`Length (cm)`))
} else if (whole$fish_per_tow >= 5) {
  tow_means <- whole %>% group_by(`Fish Tow`) %>%
    summarise(mean=mean(`Length (cm)`))
} else if (whole$fish_per_cruise >= 5) {
  cruise_means <- whole %>% group_by(`Family`) %>%
    summarise(mean=mean(`Length (cm)`))
} else {
  whole$Family = whole$Family
}
#dumb way vv
#split assigned by net
whole_split <- split(whole, whole$tow_net)
whole_MOC1_Net1 <- whole_split$`MOC-1-1`
whole_MOC1_Net2 <- whole_split$`MOC-1-2`
whole_MOC1_Net3 <- whole_split$`MOC-1-3`
whole_MOC1_Net4 <- whole_split$`MOC-1-4`
whole_MOC2_Net1 <- whole_split$`MOC-2-1`
whole_MOC2_Net2 <- whole_split$`MOC-2-2`
whole_MOC2_Net3 <- whole_split$`MOC-2-3`
whole_MOC2_Net4 <- whole_split$`MOC-2-4`
whole_MOC3_Net1 <- whole_split$`MOC-3-1`
whole_MOC3_Net2 <- whole_split$`MOC-3-2`
whole_MOC3_Net3 <- whole_split$`MOC-3-3`
whole_MOC3_Net4 <- whole_split$`MOC-3-4`
whole_MOC4_Net1 <- whole_split$`MOC-4-1`
whole_MOC4_Net2 <- whole_split$`MOC-4-2`
whole_MOC4_Net3 <- whole_split$`MOC-4-3`
whole_MOC4_Net4 <- whole_split$`MOC-4-4`
whole_MOC5_Net1 <- whole_split$`MOC-5-1`
whole_MOC5_Net2 <- whole_split$`MOC-5-2`
whole_MOC5_Net3 <- whole_split$`MOC-5-3`
M1_N1_tot <- table(whole_MOC1_Net1$Family)
M1_N2_tot <- table(whole_MOC1_Net2$Family)
M1_N3_tot <- table(whole_MOC1_Net3$Family)
M1_N4_tot <- table(whole_MOC1_Net4$Family)
M2_N1_tot <- table(whole_MOC2_Net1$Family)
M2_N2_tot <- table(whole_MOC2_Net2$Family)
M2_N3_tot <- table(whole_MOC2_Net3$Family)
M2_N4_tot <- table(whole_MOC2_Net4$Family)
M3_N1_tot <- table(whole_MOC3_Net1$Family)
M3_N2_tot <- table(whole_MOC3_Net2$Family)
M3_N3_tot <- table(whole_MOC3_Net3$Family)
M3_N4_tot <- table(whole_MOC3_Net4$Family)
M4_N1_tot <- table(whole_MOC4_Net1$Family)
M4_N2_tot <- table(whole_MOC4_Net2$Family)
M4_N3_tot <- table(whole_MOC4_Net3$Family)
M4_N4_tot <- table(whole_MOC4_Net4$Family)
M5_N1_tot <- table(whole_MOC5_Net1$Family)
M5_N2_tot <- table(whole_MOC5_Net2$Family)
M5_N3_tot <- table(whole_MOC5_Net3$Family)

AppendMe <- function(dfNames) {
  do.call(rbind, lapply(dfNames, function(x) {
    cbind(get(x), source = x)
  }))
}

net_totals <- AppendMe(c("M1_N1_tot","M1_N2_tot","M1_N3_tot","M1_N4_tot",
                         "M2_N1_tot","M2_N2_tot","M2_N3_tot","M2_N4_tot",
                         "M3_N1_tot","M3_N2_tot","M3_N3_tot","M3_N4_tot",
                         "M4_N1_tot","M4_N2_tot","M4_N3_tot","M4_N4_tot",
                         "M5_N1_tot","M5_N2_tot","M5_N3_tot"))
test<-data.frame(net_totals)
names(test)=c("Family","Count","Net")

test.df <- as.data.frame.table(test)
test.df$Family <- as.character(test.df$Var1)
test.df$Family <- gsub('[[:digit:]]+', '', test.df$Family)
test.df$Family <- gsub('.', '', test.df$Family)
test.df$Family<-gsub(".","",as.character(test.df$Family))

test.df$trim <- startsWith(test.df$Family, 'Gonostomatidae', trim=TRUE) 

assigned$test <- "test"
assigned$assigned_type <- ""
#try this if statement structure 
if (fishPerNet >= 5) {
  #// calculate fishPerNet average...
  #// ...loop or function call...
} else if (fishPerTow >= 5) {
  #// calculate fishPerTow average...
  #// ...loop or function call...
} else if (fishPerCruise >= 5) {
  #// calculate fishPerCruise average...
  #// ...loop or function call...
} else {
  #// Do Nothing...
}

#if statements not working 
if (str_count(assigned$`Family (larvae only counted)` %in% assigned$Net < 5))  {
  #Boolean_Expression 1 result is TRUE then, it will check for Boolean_Expression 2
  if (str_count(assigned$`Family (larvae only counted)` %in% assigned$Tow < 5))  {
    #Boolean_Expression 2 result is TRUE, then it will check for Boolean_Expression 3
    if (str_count(assigned$`Family (larvae only counted)` %in% assigned$Cruise < 5)) {
      assigned$`Family (larvae only counted)` #do nothing
    } else {
      #Boolean_Expression 2 result is FALSE then, these statements will be executed
      assigned$assigned_type <- "tow" #add the word tow to a new column at rows
    } else {
      #If the Boolean_Expression 1 result is FALSE, these statements will be executed
      assigned$assigned_type <- "cruise"
    } else { 
      assigned$assigned_type <- "net"
    } else {
      assigned$assigned_type <- "N/A"
    }
  }
} 
