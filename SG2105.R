#GitHub access token: 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
#Calculate EtOH fish biomass using length weight relationships from literature. 
#Fish ID'ed to family
#SL measured in ImageJ
#Use frozen fish IDs to choose which references to use in analysis 
#
#Cruise: SG2105
#Calculating weighted means for a and b values: 
#Exclude frozen fish only ID'd to family, unless family 
#is the only ID available for that family (i.e. Alepocephalidae)
#If a reference is not available at the genus level, the genus will be excluded
#altogether. 
#
#Calulate % of genera within each family. 
#Match frozen fish genera with WW a and b values from literature.
#Make sure units are compatable, all measurements should be converted 
#to mm and mg. Adjust a value accordingly (b value is not affected).  
#
#Calulate weighted means based on genus % of each family.
#Apply calculated a and b values to EtOH fish lengths in
#equation y = ax^b

setwd("/Users/juliacox/Desktop/OTZ Fish Biomass/Data")
library(readxl)
library(tidyverse)
library(dplyr)
#import data
lit.rev <- read_excel("Collated Length Weight relationships.xlsx")
froz.fish.EA <- read_excel("Julia_RegionalFishCounts_4biomass.xlsx", sheet = "E. Atlantic")
SG2105.data <- read_excel("SG2105_final_fish_measurements.xlsx")
SG2105.data = subset(SG2105.data, select = -c(`...1`) )
#narrow down families in froz.fish.EA to ones represented in SG2105.data
froz.fish.EA <- rename(froz.fish.EA, Family = family)
froz.fish.use <- subset(froz.fish.EA, (Family %in% SG2105.data$Family))
#separate "lowest_taxa" col. in froz.fish.use into genus and species cols. 
froz.fish.EA.separated <-separate(froz.fish.use, lowest_taxa, c("genus","species"), sep = " ") %>% 
  replace_na(list(species = "sp."))

#remove rows only identified to family, unless there are no members of family ID'd to genus  
froz.fish.EA.separated$use <- ifelse(froz.fish.EA.separated$genus == froz.fish.EA.separated$Family,
                                     !duplicated(froz.fish.EA.separated$Family),
                                     froz.fish.EA.separated$genus)
froz.fish.EA.use <- froz.fish.EA.separated[!grepl("FALSE", froz.fish.EA.separated$use),]

#make sure all units are in mm and mg by converting a values 
#convert all a values so y unit = mg 
lit.rev$a_value_mg<-ifelse(lit.rev$`Weight unit`== "g",
                           lit.rev$`a value (WW)`*1000, 
                           lit.rev$`a value (WW)`) 

#convert all a values so x unit = mm - does this work? 
lit.rev$ycm <- lit.rev$a_value_mg*(10^lit.rev$`b value (WW)`)

lit.rev$a_corrected <- ifelse(lit.rev$`Length unit`== "cm",
                              lit.rev$ycm/(100^lit.rev$`b value (WW)`), 
                              lit.rev$a_value_mg) 
#remove spp. measured in TL
lit.rev.use <- lit.rev[!grepl("TL", lit.rev$`Length type`),]

#take genus a and b means of lit.rev.use
lit.rev.separated <-separate(lit.rev.use, `Species`, c("genus","species"), sep = " ") %>% 
  replace_na(list(species = NA))
#since we don't have a genus for Alepocephalidae, insert family name in genus spot 
#for lit.rev
lit.rev.final <- lit.rev.separated %>%
  mutate(genus = replace(genus, genus == "Alepocephalus", Family[genus=="Alepocephalus"]))

genus.a.mean <- tapply(lit.rev.final$a_corrected,lit.rev.final$genus,mean)
genus.a.mean.df <- as.data.frame.table(genus.a.mean)
colnames(genus.a.mean.df) <- c("genus", "a_mean")
genus.b.mean <- tapply(lit.rev.final$`b value (WW)`, lit.rev.final$genus,mean)
genus.b.mean.df <- as.data.frame.table(genus.b.mean)
colnames(genus.b.mean.df) <- c("genus", "b_mean")

#get genus totals for froz.fish.EA.use
genus.totals <- data.frame(aggregate(Count~Family+genus, data = froz.fish.EA.use, FUN = sum)) 

#find which genera are missing in genus.a.mean.df and genus.b.mean.df 
missing.genera <- subset(genus.totals,!(genus%in%genus.a.mean.df$genus))
sum(missing.genera$Count)/sum(genus.totals$Count)*100
#4.7% of individuals missing in missing genera 
#use missing.genera list to search for missing genera 

#combine genus.totals and lit.rev dataframes by $genus
total <- Reduce(merge, list(genus.totals, genus.a.mean.df, genus.b.mean.df))
#get summary statistics for a and b 
library(data.table)
long <- melt(total(wide), id.vars = c("Code","Country"), variable.name = "year")
reshape(total, idvar = "genus", timevar = "a_mean", direction = "wide")

#take weighted means for a and b 
family.totals <-data.frame(aggregate(Count~Family, data = total, FUN = sum))
colnames(family.totals) <- c("Family", "total_count_fam")
total <- merge(total, family.totals, by.x="Family", by.y="Family")
total$proportion_fam <- total$Count/total$total_count
total$proportion_a <- total$a_mean*total$proportion_fam
total$proportion_b <- total$b_mean*total$proportion_fam

a <- data.frame(aggregate(proportion_a~Family, data = total, FUN = sum)) 
b <- aggregate(proportion_b~Family, data = total, FUN = sum)
weighted.growth.fact <- merge(a, b)

#apply a and b values to measured SG2105 fish 
SG2105.gf <- merge(SG2105.data, weighted.growth.fact, by.x="Family", by.y="Family")
SG2105.gf$`Length (mm)` <- SG2105.gf$`Length (cm)`*10
SG2105.gf$`Weight (mg)` <- SG2105.gf$proportion_a*(SG2105.gf$`Length (mm)`^SG2105.gf$proportion_b)

#calculate biomass per volume filtered *change for Sarmiento*
otz_nets <- read_excel("OTZ_NETS.xlsx")
#only include SG2105 cruise 
otz_nets <- otz_nets[otz_nets$cruise_no == 'SG2105',]
#make a column in common between otz_nets and SG2105.gf, tow-net ID
otz_nets$tow_ID <- paste(otz_nets$tow_type, otz_nets$tow_no, otz_nets$net_no, sep = "-")
SG2105.gf$tow_ID <- paste(SG2105.gf$Tow, SG2105.gf$Net, sep = "-")
#merge otz_nets and SG2105.gf by tow_ID
SG2105_flow <- merge(SG2105.gf,otz_nets,by="tow_ID")
#calculate "biomass"/m^3 filtered 
SG2105_flow$net_volume_filtered <- as.numeric(SG2105_flow$net_volume_filtered)
SG2105_flow$biomass_per_filtered <- SG2105_flow$`Weight (mg)`/SG2105_flow$net_volume_filtered  

#add date and time data to MOC tows 
otz_tows <- read_excel("OTZ_TOWS.xlsx")
SG2105_tows <- subset(otz_tows, cruise_no == "SG2105")
SG2105_total <- merge(SG2105_tows, SG2105_flow, by.x="tow_no", by.y="tow_no")

SG2105_total$day_night <- " "
SG2105_total$deci_start_time_ET <- 
  sapply(strsplit(SG2105_total$tow_start_time_EST,":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         })
SG2105_total$day_night <- cut((SG2105_total$deci_start_time_ET), c(0,5,18,24),
                            c("night", "day", "night"))

#vv sandbox vv
#helpful code for sp. and spp. 
#https://github.com/marytoner/WoRMS_matching/blob/master/demo_modified_forLaurenM.Rmd#L56

