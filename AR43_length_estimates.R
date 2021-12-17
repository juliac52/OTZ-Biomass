#estimate lengths for fish fragments from AR43 MOC10
setwd("/Users/juliacox/Desktop/OTZ Fish Biomass/Data")
library(readxl)
library(tidyverse)
library(dplyr)
assigned <- read_excel("AR43 Assigned Lengths.xlsx")
whole <- read_excel("AR43 Whole Lengths.xlsx")
