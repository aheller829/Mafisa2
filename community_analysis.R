## Community analysis with L1.1 data 

library(tidyverse)
library(ggplot2)

# Set working directory
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/L1/"
setwd(dir)

# Read in L1.1 data
soilbiomass <- read.csv("Mafisa2_SoilBiomass_L1.1.csv")
dbh <- read.csv("Mafisa_2_dbh_20250415.csv")

# Count trees with dbh > 5 per plot and join to soil table
dbh_summary <- dbh %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(TreeCount = n())
soilbiomass <- dplyr::left_join(soilbiomass, dbh_summary)
soilbiomass <- dplyr::mutate(soilbiomass, TreeCount = ifelse(is.na(TreeCount), 0, TreeCount))
