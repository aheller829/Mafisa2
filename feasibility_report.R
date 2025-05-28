# Calculations and figures for Mafisa 2 report

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggsoiltexture)
library(randomForest)
library(rpart)
library(rpart.plot)
library(PerformanceAnalytics)
library(ggridges)

# Read in data
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/"
setwd(dir)

labdata <- read.csv("L3/Mafisa2_LabData_joined_L3.csv")

cover <- read.csv("L1/plot_cover_photos.csv")

dbh <- read.csv("L2/Mafisa2_DBH_L2.csv")

soilbiomass <- read.csv("L2/Mafisa2_SoilBiomass_L2.csv")

biomass <- read.csv("L2/Mafisa2_Biomass_recalc.csv")

snap <- read.csv("L3/Mafisa2_SNAPGRAZE_QuantVegClass.csv")

veg <- read.csv("L2/Mafisa2_Veg2x2_BD_L2.csv")


# Join cover data
cover <- dplyr::rename(cover, SoilPlot = Plot.Name)

labdata <- dplyr::left_join(labdata, cover)

# Add district to biomass data
district <- dplyr::select(labdata, SoilPlot, District)

soilbiomass <- dplyr::left_join(soilbiomass, district)

# Order district classes
labdata$District <- factor(labdata$District, levels = c("Mutala", "Mombola", "Senanga", "Kanguya"))

# Combine savanna/open woodland and open woodland
unique(labdata$GoogleEarthClass.2)
labdata <- dplyr::mutate(labdata, GoogleEarthClass.2 = ifelse(GoogleEarthClass.2 == "Savanna/open woodland", "Open woodland", GoogleEarthClass.2))
# Rename grassland dambo
labdata <- dplyr::mutate(labdata, GoogleEarthClass.1 = ifelse(GoogleEarthClass.1 == "Grassland", "Dambo", GoogleEarthClass.1))

# Reorder class
labdata$GoogleEarthClass.2 <- factor(labdata$GoogleEarthClass.2, levels = c("Dambo", "Savanna", "Open woodland", "Dense woodland"))
labdata$GoogleEarthClass.1 <- factor(labdata$GoogleEarthClass.1, levels = c("Dambo", "Savanna", "Woodland"))

### Boxplot of SOC densities by habitat type and district
names(labdata)
ggplot(labdata, aes(y = SOC_density, x = GoogleEarthClass.1, fill = GoogleEarthClass.1)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("SOC densities across habitats and project areas") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("SOC density (t C/ha)") +
  facet_wrap(~District) +
  coord_cartesian(ylim = c(0, 200)) +
  ggpubr::stat_compare_means(size = 3)

table(labdata$GoogleEarthClass.1)

# SOC density by habitat type
ggplot(labdata, aes(y = SOC_density, x = GoogleEarthClass.1, fill = GoogleEarthClass.1)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("SOC densities across habitats") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("SOC density (t C/ha)") +
 coord_cartesian(ylim = c(0, 200)) +
  ggpubr::stat_compare_means()


# SOC density by district
ggplot(labdata, aes(y = SOC_density, x = District, fill = District)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("SOC densities across project areas") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("SOC density (t C/ha)") +
  coord_cartesian(ylim = c(0, 200)) +
  ggpubr::stat_compare_means()



# Ridgeplots of SOC densities
ggplot(labdata, aes(x = SOC_density, y = District, fill = District)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme_minimal() +
  ggtitle("SOC densities across project areas") +
  ggpubr::stat_compare_means()


min(labdata$SOC_density, na.rm = TRUE)
max(labdata$SOC_density, na.rm = TRUE)


# Plot within texture triangle
txtplot <- dplyr::select(labdata, District, SoilPlot, sand = Sand_pct, clay = Clay_pct, silt = Silt_pct, OC, SOC_density, TXT_class)
txtplot <- dplyr::left_join(txtplot, cover)
txtplot <- dplyr::filter(txtplot, !is.na(sand))

ggsoiltexture::ggsoiltexture(txtplot, class = "USDA")

# Carbon by texture
ggsoiltexture::ggsoiltexture(txtplot, class = "USDA") +
  geom_point(aes(color = SOC_density, size = SOC_density)) +
  scale_color_continuous(type = "viridis", direction = -1) +
  scale_size_continuous(range = c(1, 5), guide = "none") +
  labs(color = "organic carbon (%)") +
  theme(legend.title = element_text(face = "bold"),
        legend.position = "bottom") 


# Boxplot of density by soil texture
ggplot(txtplot, aes(y = SOC_density, x = TXT_class, fill = TXT_class)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("SOC densities across soil texture classes") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("SOC density (t C/ha)") +
  coord_cartesian(ylim = c(0, 200)) +
  ggpubr::stat_compare_means()


# Hist of soc density
ggplot(labdata, aes(x = SOC_density)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("SOC density (t/C ha)") +
  geom_vline(xintercept = 5000, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 15000, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 200, 10), limits = c(0, 200)) 






# LM/Random forest of SOC density 
set.seed(222)
names(snap)
# Choose variables for RF
data <- dplyr::select(snap, SoilPlot = SiteLabel, SOC_density, VegClass, SoilTextureClass, EstimatedPercentSand, MeanAnnualRainfall_mm, MeanAnnualTemperature_C)
names(labdata)
data.2 <- dplyr::select(labdata, SoilPlot, GoogleEarthClass.1)
names(biomass)
data.3 <- dplyr::select(biomass, SoilPlot, HerbDryWeightSum, HerbDryMatterPct, WoodyDryWeightSum, WoodyDryMatterPct)
names(soilbiomass)
data.4 <- dplyr::select(soilbiomass, SoilPlot, TREE_pct:TotalCover)
names(veg)
data.5 <- dplyr::select(veg, SoilPlot, AnnualForb:TotalFoliar, LargeGaps)
# Join
data <- data %>%
  dplyr::left_join(data.2) %>%
  dplyr::left_join(data.3) %>%
  dplyr::left_join(data.4) %>%
  dplyr::left_join(data.5) %>%
  dplyr::select(-SoilPlot) %>%
  na.omit()
str(data)
data$VegClass <- as.factor(data$VegClass)
data$SoilTextureClass <- as.factor(data$SoilTextureClass)
data$GoogleEarthClass.1 <- as.factor(data$GoogleEarthClass.1)


# Cor plot
names(data)
num.data <- dplyr::select(data, -VegClass, -SoilTextureClass, -GoogleEarthClass.1)
PerformanceAnalytics::chart.Correlation(num.data, histogram=TRUE, pch=19)




# lm
names(data)
model <- SOC_density~VegClass+SoilTextureClass+EstimatedPercentSand+MeanAnnualRainfall_mm+
  MeanAnnualTemperature_C+GoogleEarthClass.1+HerbDryWeightSum+HerbDryMatterPct+WoodyDryWeightSum+
  WoodyDryMatterPct+TREE_pct+DRH_pct+SRH_pct+SHRUB_pct+BARE_pct+AnnualForb+AnnualGrass+Carex+Moss+PerennialForb+
  PerennialGrass+Shrub+Subshrub+Tree+SpeciesRichness+TotalFoliar+LargeGaps

lrm <- lm(model, data = data)
summary(lrm)


# CART
cart <- rpart::rpart(model, data = data)
rpart.plot::rpart.plot(cart, cex = 0.75)
cart




# RF
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3)) 
train <- data[ind == 1,]
test <- data[ind == 2,]

rf <- randomForest(x = train[-1],
                   y = train$SOC_density, 
                   ntree = 500,
                   importance = TRUE)

rf

# Predicting the Test set results
y_pred = predict(rf, newdata = test[-1])

# Confusion Matrix
confusion_mtx = table(test[, 1], y_pred)
confusion_mtx

# Plotting model
plot(rf)

# Importance plot
importance(rf)

# Variable importance plot
varImpPlot(rf)







### Env across districts
env <- snap %>%
  dplyr::group_by(Community) %>%
  dplyr::summarise(MeanTemp = mean(MeanAnnualTemperature_C),
                   MeanPrecip = mean(MeanAnnualRainfall_mm))
# Boxplot of rainfall and precip
ggplot(snap, aes(y = MeanAnnualTemperature_C, x = Community, fill = Community)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("Mean annual temperature (C) across project areas") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("C") 
ggplot(snap, aes(y = MeanAnnualRainfall_mm, x = Community, fill = Community)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("Mean annual rainfall (mm) across project areas") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("Mean annual rainfall (mm)") 


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


# Points per district
plots <- labdata %>%
  dplyr::group_by(District) %>%
  dplyr::summarise(Count = n())










### Woody biomass analysis
dbh <- dplyr::mutate(dbh, AGB = 0.1232*Tree_dbh^2.3586, # Calculate biomass
                     BGB = 0.1259*Tree_dbh^2.0488)

dbh <- dplyr::mutate(dbh, AGB_c_pertree = AGB*0.5, # Apply carbon factor per tree
                     BGB_c_pertree = BGB*0.5)


# Calculate density
treecount <- dbh %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(TreeCount = n())

dbh <- dplyr::left_join(dbh, treecount)
# Plot area
plotarea <- pi*20^2
# Upscale density to hectares by multiplying tree count by 7.955449
dbh <- dplyr::mutate(dbh, Density_ha = TreeCount*7.955449)


# Sum biomass per plot
biomass_sum <- dbh %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(AGB_mass_kg_per_plot = sum(AGB),
                   BGB_mass_kg_per_plot = sum(BGB),
                   AGB_c_per_plot = sum(AGB_c_pertree),
                   BGB_c_per_plot = sum(BGB_c_pertree))

# Scale up from plot to ha
biomass_sum <- dplyr::mutate(biomass_sum, 
                             AGB_mass_tons_ha = (AGB_mass_kg_per_plot*7.957747)/1000,
                             BGB_mass_tons_ha = (BGB_mass_kg_per_plot*7.957747)/1000,
                             AGB_C_tons_ha = (AGB_c_per_plot*7.957747)/1000,
                             BGB_C_tons_ha = (BGB_c_per_plot*7.957747)/1000)


# Join with district
biomass_sum <- dplyr::left_join(biomass_sum, district)
# Summarize by district
biomass.summary.district <- biomass_sum %>%
  dplyr::group_by(District) %>%
  dplyr::summarise(
                   Mean_AGBstocks_tons_ha = mean(AGB_mass_tons_ha),
                   Mean_BGBstocks_tons_ha = mean(BGB_mass_tons_ha),
                   Mean_AGB_c_tons_ha = mean(AGB_C_tons_ha),
                   Mean_BGB_c_tons_ha = mean(BGB_C_tons_ha)) %>%
  dplyr::mutate_if((is.numeric), ~ round(., 3))
  




# Tree species per district
district <- dplyr::select(labdata, SoilPlot, District)
treesp <- dbh %>%
  dplyr::left_join(district) %>%
  dplyr::group_by(District, Tree_ID) %>%
  dplyr::summarise(TreeCount = n())
spcount <- treesp %>%
  dplyr::group_by(District) %>%
  dplyr::summarise(Count = n())
