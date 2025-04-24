## Visualizing ingested L1.1 data 

library(tidyverse)
library(ggplot2)
library(rgl)
library(PerformanceAnalytics)

# Set working directory
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/L1/"
setwd(dir)

# Read in L1.1 data
soilbiomass <- read.csv("Mafisa2_SoilBiomass_L1.1.csv")
gctable <- read.csv("Mafisa2_GC_L1.1.csv")
vegtable <- read.csv("Mafisa2_Veg2x2_BD_L1.1.csv")


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


# Trees
trees <- ggplot(soilbiomass, aes(x = TreeCount)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Number of trees per plot with dbh > 5 cm") +
  scale_x_continuous(breaks = seq(-10, 200, 10), limits = c(-10, 200))
trees


# Bare ground
bg <- ggplot(cover_wide, aes(x = bare_ground)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Bare ground (%)") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
bg


# Tree cover
treecover <- ggplot(cover_wide, aes(x = tree_leaf_or_stem)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Tree (%)") +
  scale_x_continuous(breaks = seq(-10, 100, 10), limits = c(-10, 100))
treecover




# Biodiversity data
# Compare functional group cover from GC method to 2x2 method
names(gctable)
names(vegtable)
cover_comp <- gctable %>%
  dplyr::left_join(vegtable) %>%
  dplyr::select(SoilPlot, bare_ground,
                grass_leaf_or_stem, AnnualGrass, PerennialGrass, Carex, AnnualForb, PerennialForb,
                shrub_leaf_or_stem, Subshrub, Shrub, tree_leaf_or_stem, Tree) %>%
  dplyr::mutate(Woody2x2 = Shrub + Subshrub + Tree,
                WoodyGC = shrub_leaf_or_stem+ tree_leaf_or_stem,
                Herb2x2 = AnnualForb + AnnualGrass + Carex + PerennialForb + PerennialGrass,
                HerbGC = grass_leaf_or_stem,
                Grass2x2 = AnnualGrass + PerennialGrass + Carex) %>%
  dplyr::select(SoilPlot, bare_ground, Woody2x2, WoodyGC, Herb2x2, HerbGC,
                grass_leaf_or_stem, Grass2x2, AnnualGrass, PerennialGrass, Carex, AnnualForb, PerennialForb,
                shrub_leaf_or_stem, Subshrub, Shrub, tree_leaf_or_stem, Tree)
# Plot differences in functional groups
names(cover_comp)
ggplot(cover_comp, aes(x = Woody2x2, y = WoodyGC)) + 
  geom_point(size=4, color="#69b3a2") +
  ggtitle("Total woody cover (%) across methods") +
  xlab("Quadrat") + ylab("Point intercept")

ggplot(cover_comp, aes(x = Herb2x2, y = HerbGC)) + 
  geom_point(size=4, color="#69b3a2") +
  ggtitle("Total herbaceous cover (%) across methods") +
  xlab("Quadrat") + ylab("Point intercept")

ggplot(cover_comp, aes(x = Grass2x2, y = grass_leaf_or_stem)) + 
  geom_point(size=4, color="#69b3a2") +
  ggtitle("Grass cover (%) across methods") +
  xlab("Quadrat") + ylab("Point intercept")


# Join densiometer measurements for third method comparison of tree cover
densi <- dplyr::select(soilbiomass, SoilPlot, DensiCanopyCover)
cover_comp <- dplyr::left_join(cover_comp, densi)
cover_comp <- dplyr::mutate(cover_comp , DensiCanopyCover = ifelse(is.na(DensiCanopyCover), 0, DensiCanopyCover))


# Quadrat vs. point intercept
ggplot(cover_comp, aes(x = Tree, y = tree_leaf_or_stem)) + 
  geom_point(size=4, color="#69b3a2") +
  ggtitle("Tree cover (%) across methods") +
  xlab("Quadrat") + ylab("Point intercept")

# Quadrat vs. point intercept vs. densiometer
rgl::plot3d(x = cover_comp$Tree,
            y = cover_comp$tree_leaf_or_stem,
            z = cover_comp$DensiCanopyCover,
            col = "#69b3a2",
            xlab = "Point intercept", ylab = "Quadrat", zlab = "Densiometer",
            radius = 0.1)

# Correlation plot
trees <- dplyr::select(cover_comp, Tree_PointIntercept = tree_leaf_or_stem, Tree_Quadrat = Tree, Tree_Densiometer = DensiCanopyCover)
PerformanceAnalytics::chart.Correlation(trees, histogram=TRUE, pch=19)


# Grass correlation plot
grasses <- dplyr::select(cover_comp, SoilPlot, Grass_PointIntercept = grass_leaf_or_stem, Grass_Quadrat = Grass2x2)

grasses <- soilbiomass %>%
  dplyr::select(SoilPlot, DRH_pct, SRH_pct) %>%
  dplyr::mutate(ocularest, Grass_Ocular = DRH_pct + SRH_pct) %>%
  dplyr::left_join(grasses) %>%
  dplyr::select(Grass_Ocular, Grass_PointIntercept, Grass_Quadrat) 

PerformanceAnalytics::chart.Correlation(ocularest, histogram=TRUE, pch=19)



