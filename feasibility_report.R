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
library(gt)
library(gtsummary)
library(cooccur)

# Read in data
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/"
setwd(dir)

labdata <- read.csv("L3/Mafisa2_LabData_joined_L3.csv")

cover <- read.csv("L1/plot_cover_photos.csv")

# Force as utf-8 and convert to columns
dbh <- read.csv2("L2/Mafisa2_DBH_L2.csv")
dbh <- tidyr::separate_wider_delim(dbh, cols = ParentGlobalID.SoilPlot.Tree_ID.Tree_dbh.SurveyDate.x.y,
                                   delim = ",", names = c("ParentGlobalID", "SoilPlot", "Tree_ID", "Tree_dbh", "SurveyDate", "x", "y"))

soilbiomass <- read.csv("L2/Mafisa2_SoilBiomass_L2.csv")

biomass <- read.csv("L2/Mafisa2_Biomass_recalc.csv")

snap <- read.csv("L3/Mafisa2_SNAPGRAZE_format_csv.csv")

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




#### Bio plots
joinedweights <- dplyr::left_join(joinedweights, district)
joinedweights <- dplyr::left_join(joinedweights, cover)

joinedweights %>%
  ggplot(aes(y = HerbDryWeight_kg_ha, x = GoogleEarthClass.1, fill = GoogleEarthClass.1)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("Herbaceous biomass across ecosystem types") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("Herbaceous biomass (kg/ha)") 


joinedweights %>%
  ggplot(aes(y = WoodyDryWeight_kg_ha, x = GoogleEarthClass.1, fill = GoogleEarthClass.1)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("Woody biomass across ecosystem types") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("Woody biomass (kg/ha)") 

# Make tall table and do it paired
weightstall <- joinedweights %>%
  dplyr::select(SoilPlot, District, GoogleEarthClass.1, HerbDryWeight_kg_ha, WoodyDryWeight_kg_ha, ) %>%
  tidyr::gather(Measure, Value, 4:5)

weightstall <- dplyr::mutate(weightstall, GoogleEarthClass.1 = ifelse(GoogleEarthClass.1 == "Grassland", 
                                                                      "Dambo", GoogleEarthClass.1))


# Paired boxplot, tons of biomass per plot
weightstall  %>%
  ggplot(aes(y = Value, x = GoogleEarthClass.1, fill = Measure)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("Clipped biomass across project areas") +
  theme_minimal() +
  scale_fill_hue(labels = c("Herbaceous biomass", "Woody biomass")) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("Biomass (kg/ha)") 

# Scatterplot of SOC density and biomass
names(snap)
snap$ClippedBiomass_kg_ha <- snap$HerbDryWeight_kg_ha + snap$WoodyDryWeight_kg_ha

snap %>%
  ggplot(aes(x = SOC_density, y = ClippedBiomass_kg_ha)) + 
  geom_point()



# Check tree species in savanna
dbh <- dplyr::left_join(dbh, cover)
savanna <- dplyr::filter(dbh, GoogleEarthClass.1 == "Savanna")

savanna_trees <- savanna %>%
  dplyr::group_by(Tree_ID) %>%
  dplyr::summarise(SpeciesCount = n())

# How many plots did species occur at
savanna_tree_plots <- savanna %>%
  dplyr::select(Tree_ID, SoilPlot) %>%
  dplyr::distinct() %>%
  dplyr::group_by(Tree_ID) %>%
  dplyr::summarise(PlotCount = n())


# Check for tree species cooccurance
# First make dbh table wide
dbh_wide <- dbh %>%
  dplyr::select(Tree_ID, SoilPlot) %>%
  dplyr::mutate(Presence = 1) %>%
  dplyr::distinct() %>%
  tidyr::spread(key = SoilPlot, value = Presence) %>%
  dplyr::mutate_all(~replace(., is.na(.), 0)) %>%
  tibble::remove_rownames() %>%
  tibble::column_to_rownames(var = "Tree_ID")



cooccur_result <- cooccur::cooccur(dbh_wide, type = "spp_site", thresh = TRUE,
                                   spp_names = TRUE)
summary(cooccur_result)
plot(cooccur_result)


# Canopy cover histograms by project area
canopycover <- dplyr::select(soilbiomass, SoilPlot, DensiCanopyCover)
canopycover <- dplyr::left_join(canopycover, district)
canopycover$District <- factor(canopycover$District, levels = c("Mutala", "Mombola", "Senanga", "Kanguya"))
canopycover <- dplyr::mutate(canopycover, Level = ifelse(DensiCanopyCover < 50, "Low", "High"))
# Histograms
ggplot(canopycover, aes(x = DensiCanopyCover)) +
  geom_histogram(binwidth = 10, bins = 7, fill = "darkblue") +
  theme_minimal() +
  theme(legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) + 
  facet_wrap(~District) +
  ylab("") + xlab("Tree canopy cover (%)") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "red")









# LM/Random forest of SOC density 
set.seed(222)
names(snap)
# Choose variables for RF
data <- dplyr::select(snap, SoilPlot = SiteLabel, SOC_density, VegClass, SoilTextureClass, EstimatedPercentSand, MeanAnnualRainfall_mm, MeanAnnualTemperature_C)
names(labdata)
data.2 <- dplyr::select(labdata, SoilPlot, GoogleEarthClass.1)
names(joinedweights)
data.3 <- dplyr::select(joinedweights, SoilPlot, HerbDryWeight_kg_ha, WoodyDryWeight_kg_ha)
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
  MeanAnnualTemperature_C+GoogleEarthClass.1+HerbDryWeight_kg_ha+WoodyDryWeight_kg_ha+TREE_pct+DRH_pct+SRH_pct+SHRUB_pct+BARE_pct+AnnualForb+AnnualGrass+Carex+Moss+PerennialForb+
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
                   BGB_c_per_plot = sum(BGB_c_pertree),
                   Density_ha = mean(Density_ha))

# Scale up from plot to ha
biomass_sum <- dplyr::mutate(biomass_sum, 
                             AGB_mass_tons_plot = AGB_mass_kg_per_plot/1000,
                             BGB_mass_tons_plot = BGB_mass_kg_per_plot/1000,
                             AGB_mass_tons_ha = (AGB_mass_kg_per_plot*7.957747)/1000,
                             BGB_mass_tons_ha = (BGB_mass_kg_per_plot*7.957747)/1000,
                             AGB_C_tons_ha = (AGB_c_per_plot*7.957747)/1000,
                             BGB_C_tons_ha = (BGB_c_per_plot*7.957747)/1000)


# Join with district
biomass_sum <- dplyr::left_join(biomass_sum, district)
# Summarize by district
biomass.summary.district <- biomass_sum %>%
  dplyr::group_by(District) %>%
  dplyr::summarise(Mean_tree_density_ha = mean(Density_ha),
                   SD_tree_density_ha = sd(Density_ha),
                   Mean_AGBstocks_tons_ha = mean(AGB_mass_tons_ha),
                   SD_AGBstocks_tons_ha = sd(AGB_mass_tons_ha),
                   Mean_BGBstocks_tons_ha = mean(BGB_mass_tons_ha),
                   SD_BGBstocks_tons_ha = sd(BGB_mass_tons_ha),
                   Mean_AGB_c_tons_ha = mean(AGB_C_tons_ha),
                   SD_AGB_c_tons_ha = sd(AGB_C_tons_ha),
                   Mean_BGB_c_tons_ha = mean(BGB_C_tons_ha),
                   SD_BGB_c_tons_ha = sd(BGB_C_tons_ha)) %>%
  dplyr::mutate_if((is.numeric), ~ round(., 3))
  
# Make GT Table
biomass_tab <- dplyr::select(biomass_sum, -SoilPlot, -AGB_mass_kg_per_plot, -BGB_mass_kg_per_plot,
                             -AGB_mass_tons_plot, -BGB_mass_tons_plot,
                             "Tree density (ha)" = Density_ha,
                             -AGB_c_per_plot, -BGB_c_per_plot, "AGB (t/ha)" = AGB_mass_tons_ha, "BGB (t/ha)" = BGB_mass_tons_ha,
                             "AGB carbon stocks (t/ha)" = AGB_C_tons_ha, "BGB carbon stocks (t/ha)" = BGB_C_tons_ha)

biomass_tab$District <- factor(biomass_tab$District, levels = c("Mutala", "Mombola", "Senanga", "Kanguya"))

tab <- gtsummary::tbl_summary(biomass_tab, by = District,
                              type = where(is.numeric) ~ "continuous",
                              statistic = all_continuous() ~ "{mean} ± {sd}",
                              digits = everything() ~ 1,
                              label = "Tree density (ha)" ~ "Tree density (ha<sup>-1</sup>)")
tab
tab <- gtsummary::modify_header(tab, label = "**Measurement**")
tab <- tab %>%
  gtsummary::as_gt() %>%
  gt::fmt_markdown(columns = vars(label))
tab
gt::gtsave(tab, path = "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2 Report\\Figures", filename = "biomass.png", vwidth = 1500, vheight = 1000)


# Create plots
names(biomass_sum)
ggplot(biomass_sum, aes(y = BGB_C_tons_ha, x = District, fill = District)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("Aboveground woody biomass across project areas") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("Tons/ha")  + 
  ggpubr::stat_compare_means(size = 3)


# Kruskal test
kruskal.test(AGB_C_tons_ha ~ District, data = biomass_sum)







# Gather for paired boxplot
names(biomass_sum)
biomass_tall <- biomass_sum %>%
  tidyr::gather(Measurement, Value, 2:11) %>%
  dplyr::select(SoilPlot, District, Measurement, Value)
biomass_tall$District <- factor(biomass_tall$District, levels = c("Mutala", "Mombola", "Senanga", "Kanguya"))
# Paired boxplot, tons of biomass per plot
biomass_tall %>%
  dplyr::filter(Measurement == "AGB_mass_tons_plot" | Measurement == "BGB_mass_tons_plot") %>%
  ggplot(aes(y = Value, x = District, fill = Measurement)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("Woody biomass across project areas") +
  theme_minimal() +
  scale_fill_hue(labels = c("Aboveground woody biomass", "Belowground woody biomass")) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("Biomass (tons) per plot") + 
  ggpubr::stat_compare_means(size = 3)

# Paired boxplot, biomass tons per hectare
biomass_tall %>%
  dplyr::filter(Measurement == "AGB_mass_tons_ha" | Measurement == "BGB_mass_tons_ha") %>%
  ggplot(aes(y = Value, x = District, fill = Measurement)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("Woody biomass across project areas") +
  theme_minimal() +
  scale_fill_hue(labels = c("Aboveground woody biomass", "Belowground woody biomass")) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("Biomass (tons/ha)") 
# Paired boxplot, c tons per hectare
biomass_tall %>%
  dplyr::filter(Measurement == "AGB_C_tons_ha" | Measurement == "BGB_C_tons_ha") %>%
  ggplot(aes(y = Value, x = District, fill = Measurement)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("Woody carbon stocks across project areas") +
  theme_minimal() +
  scale_fill_hue(labels = c("Aboveground biomass C stocks", "Belowground biomass C stocks")) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("C (tons/ha)") 








# Tree species per district
district <- dplyr::select(labdata, SoilPlot, District)
treesp <- dbh %>%
  dplyr::left_join(district) %>%
  dplyr::group_by(District, Tree_ID) %>%
  dplyr::summarise(TreeCount = n())
spcount <- treesp %>%
  dplyr::group_by(District) %>%
  dplyr::summarise(Count = n())
