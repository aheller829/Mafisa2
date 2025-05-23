# Calculations and figures for Mafisa 2 report

library(tidyverse)
library(ggplot2)


# Read in data
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/"
setwd(dir)

labdata <- read.csv("L3/Mafisa2_LabData_joined_L3.csv")

cover <- read.csv("L1/plot_cover_photos.csv")
cover <- dplyr::select(cover, SoilPlot = Plot.Name, Class = GoogleEarthClass.2)

labdata <- dplyr::left_join(labdata, cover)


dbh <- read.csv("L2/Mafisa2_DBH_L2.csv")

soilbiomass <- read.csv("L2/Mafisa2_SoilBiomass_L2.csv")

district <- dplyr::select(labdata, SoilPlot, District)

soilbiomass <- dplyr::left_join(soilbiomass, district)


# Combine savanna/open woodland and open woodland
labdata <- dplyr::mutate(labdata, Class = ifelse(Class == "Savanna/open woodland", "Open woodland", Class))

# Reorder class
labdata$Class <- factor(labdata$Class, levels = c("Dambo", "Savanna", "Open woodland", "Dense woodland"))

### Boxplot of SOC densities by habitat type
names(labdata)
ggplot(labdata, aes(y = SOC_density, x = Class, fill = Class)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("SOC densities across ecosystem types") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("SOC density") +
  facet_wrap(~District)


### Env across districts
env <- snap %>%
  dplyr::group_by(Community) %>%
  dplyr::summarise(MeanTemp = mean(MeanAnnualTemperature_C),
                   MeanPrecip = mean(MeanAnnualRainfall_mm))


### Indicator species table
ind <- read.csv("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2 Report\\IndicatorSp.csv")
library(gtsummary)
library(gt)

tab <- ind %>%
  dplyr::rename("Functional group" = Functional.Group, "P-value" = P.value) %>%
  gt() %>%
  tab_header(title = md("**Indicator species by habitat**")) %>%
  cols_align(align = c("center"), columns = everything())
tab

gtsave(tab, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2 Report\\Figures\\indsp.png")









### Woody biomass analysis
dbh <- dplyr::mutate(dbh, AGB = 0.1232*Tree_dbh^2.3586, # Calculate biomass
                     BGB = 0.1259*Tree_dbh^2.0488)

# Sum biomass per plot
biomass_sum <- dbh %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(AGB_mass_kg = sum(AGB),
                   BGB_mass_kg = sum(BGB))



dbh <- dplyr::mutate(dbh, AGB_c = AGB*0.5, # Apply carbon factor
                     BGB_c = BGB*0.5)
# Calculate density
treecount <- dbh %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(TreeCount = n())

dbh <- dplyr::left_join(dbh, treecount)
# Plot area
plotarea <- pi*20^2
# Upscale density to hectares by multiplying tree count by 7.955449
dbh <- dplyr::mutate(dbh, Density_ha = TreeCount*7.955449)

# C stocks at stand level
dbh <- dplyr::mutate(dbh, agb_cstocks = (AGB*Density_ha)/1000) 

# Tree species per district
district <- dplyr::select(labdata, SoilPlot, District)
treesp <- dbh %>%
  dplyr::left_join(district) %>%
  dplyr::group_by(District, Tree_ID) %>%
  dplyr::summarise(TreeCount = n())
spcount <- treesp %>%
  dplyr::group_by(District) %>%
  dplyr::summarise(Count = n())
