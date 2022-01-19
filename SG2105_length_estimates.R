#estimate lengths for fish fragments from SG2105 MOC10
setwd("/Users/juliacox/Desktop/OTZ Fish Biomass/Data")
library(readxl)
library(tidyverse)
library(dplyr)
assigned <- read_excel("SG2105 Assigned Lengths.xlsx")
assigned <- assigned %>% rename( Family = `Family (larvae only counted)`)
whole <- read_excel("SG2105 Whole Lengths.xlsx")
whole <- whole %>% rename( Family = `Family (larvae only counted)`)
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
total_SG2105 <- AppendMe(c("whole", "assigned"))
#
total_SG2105$mean_level <- ifelse(total_SG2105$fish_per_net >= 5, 'net',
                                ifelse(total_SG2105$fish_per_tow >= 5, 'tow',
                                       ifelse(total_SG2105$fish_per_cruise >= 5, 'cruise', 'N/A')))
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
#combine mean tables with total_SG2105 
total_SG2105 <- merge(total_SG2105,net_means,by="Fish Net")
total_SG2105 <- merge(total_SG2105,tow_means,by="Fish Tow")
total_SG2105 <- merge(total_SG2105,cruise_means,by="Family")
#assign lenths based on mean_level column
total_SG2105$assigned_length_cm <- ifelse(total_SG2105$mean_level == 'net', total_SG2105$net_mean,
                                        ifelse(total_SG2105$mean_level == 'tow', total_SG2105$tow_mean,
                                               ifelse(total_SG2105$mean_level == 'cruise', total_SG2105$cruise_mean, 'N/A')))
#"filter" data into two dataframes based on 'source'
assigned_length_SG2105 <- total_SG2105[total_SG2105$source == 'assigned',]
whole_length_SG2105 <- total_SG2105[total_SG2105$source == 'whole',]
#remove column with broken lengths in assigned   
assigned_length_SG2105 = subset(assigned_length_SG2105, select = -c(`Length (cm)`) )
#remove column with assigned lengths in whole 
whole_length_SG2105 = subset(whole_length_SG2105, select = -c(assigned_length_cm) )
#change col names to combine whole and assigned lenght dataframes 
assigned_length_SG2105 <- assigned_length_SG2105 %>% rename(`Length (cm)` = assigned_length_cm, 
                                                        length_type = source)
whole_length_SG2105 <- whole_length_SG2105 %>% rename(length_type = source)
#merge dataframes 
SG2105_final <- AppendMe(c("assigned_length_SG2105", "whole_length_SG2105"))
SG2105_final$`Length (cm)` <- as.numeric(SG2105_final$`Length (cm)`)
#write to excel spreadsheet 
library(xlsx)
write.xlsx(SG2105_final, file = "SG2105_final_fish_measurements.xlsx",
           sheetName = "Sheet 1", append = FALSE)

