#SG2105 plots 
library(ggplot2)
#length and weight plot by family 
LW_plot <- ggplot(SG2105.gf, aes(x = `Length (mm)`, y = `Weight (mg)`, color = Family)) +
  scale_color_brewer(palette="PRGn") + geom_point() + theme_bw()

#try stacked and grouped barplot, found an issue with dodged barplots 
stacked_totals <- ggplot(SG2105_flow, aes(x = Net, y = `mg/m^3_filtered`,fill = Family)) + 
  geom_bar(stat = "identity", position = "stack") + facet_grid(~ Tow)  + theme_bw() + 
  scale_fill_brewer(palette="PRGn")

#day vs night stacked barplot 
day_v_night <- ggplot(SG2105_total, aes(x = Net, y = `mg/m^3_filtered`,fill = Family)) + 
  geom_bar(stat = "identity", position = "stack") + facet_grid(~ day_night)  + theme_bw() + 
  scale_fill_brewer(palette="PRGn")


#biomass per fish per family boxplot
biomass_boxplot <- ggplot(SG2105_flow, aes(x=Family, y=`mg/m^3_filtered`, fill=Family)) +
  geom_boxplot()+
  theme_bw()+
  scale_fill_brewer(palette="PRGn")

#"biomass" per net *does not work*
biomass_by_net <- ggplot(SG2105_flow, aes(fill=Family, y=`mg/m^3_filtered`, x=Net)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw()

#biomass per family *does not work*
biomass_per_fam <- ggplot(SG2105_flow, aes(fill=Family, y=`mg/m^3_filtered`, x=Family)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw()

#biomass per tow *does not work* 
biomass_by_tow <- ggplot(SG2105_flow, aes(fill=Family, y=`mg/m^3_filtered`, x=Tow)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw()

#biomass per tow and net *does not work*
biomass_by_nettow <- ggplot(SG2105_flow, aes(fill=Family, y=`mg/m^3_filtered`, x=tow_ID)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw()



