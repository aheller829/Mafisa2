## Visualizing ingested L1.1 data 

library(tidyverse)
library(ggplot2)

# Set working directory
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/L1/"
setwd(dir)

# Read in L1.1 data
soilbiomass <- read.csv("Mafisa2_SoilBiomass_L1.1.csv")

# Visualize data 
# Bulk density
names(soilbiomass)
bd_wetmass <- ggplot(soilbiomass, aes(x = BD_wet_mass_total)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("BD wet mass total (g)") +
  geom_vline(xintercept = 5000, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 15000, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 16000, 2000), limits = c(0, 16000))
bd_wetmass

bd_wtd <- ggplot(soilbiomass, aes(x = WTD_mass)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("WTD sample mass (g)") +
  geom_vline(xintercept = 250, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 600, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 600, 50), limits = c(0,600))
bd_wtd


# SOC
# Combine SOC1 and SOC2 samples
soc1 <- dplyr::select(soilbiomass, SoilPlot, SOC_total = SOC1_mass_total)
soc2 <- dplyr::select(soilbiomass, SoilPlot, SOC_total = SOC2_mass_total)
soc <- rbind(soc1, soc2)

soc_mass <- ggplot(soc, aes(x = SOC_total)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("SOC 1 & 2 mass total (g)") +
  geom_vline(xintercept = 5000, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 15000, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 16000, 2000), limits = c(0, 16000))
soc_mass


# PCT cover
pct <- ggplot(soilbiomass, aes(x = TotalCover)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Total (any hit) canopy cover (%)") +
  geom_vline(xintercept = 120, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 100, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 300))
pct

