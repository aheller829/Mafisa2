## Community analysis with L1.1 data 

library(tidyverse)
library(ggplot2)
library(vegan)

# Set working directory
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/L1/"
setwd(dir)

# Read in L1.1 data
soilbiomass <- read.csv("Mafisa2_SoilBiomass_L1.1.csv")
dbh <- read.csv("Mafisa_2_dbh_20250415.csv")
gctable <- read.csv("Mafisa2_GC_L1.1.csv")
vegtable <- read.csv("Mafisa2_Veg2x2_BD_L1.1.csv")

# Count trees with dbh > 5 per plot and join to soil table
dbh_summary <- dbh %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(TreeCount = n())
soilbiomass <- dplyr::left_join(soilbiomass, dbh_summary)
soilbiomass <- dplyr::mutate(soilbiomass, TreeCount = ifelse(is.na(TreeCount), 0, TreeCount))


# Create analysis table (with plant functional groups rather than species)
names(soilbiomass)
names(gctable)
names(vegtable)


analysis.table <- soilbiomass %>%
  dplyr::left_join(gctable) %>%
  dplyr::left_join(vegtable) %>%
  dplyr::select(litter, AnnualForb:Tree, TotalFoliar) %>%
  na.omit()
# Give each row an ID column (rownumber)
analysis.table <- dplyr::mutate(analysis.table, ID = rownames(analysis.table))
analysis.table$ID <- as.numeric(analysis.table$ID)


fg.foliar <- dplyr::select(analysis.table, -ID)
# Convert to a bray-curtis dissimilarity matrix
fg.foliar.dist <- vegan::vegdist(fg.foliar, method = "bray", binary = FALSE)
# Run a PCoA on functional groups and structural indicators
vegPCA <- cmdscale(fg.foliar.dist, k = 2)
# View ordination
ordiplot(vegPCA, type = "text")
# Look at functional group correlation with ordination axes and plot over ordination
fit <- envfit(vegPCA, analysis.table, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
fit
plot(fit, p.max = 0.05, col = "red")



# Test cluster number metrics
library(labdsv)
fg.KM.cascade <- cascadeKM(fg.foliar.dist, inf.gr = 3, sup.gr = 20, iter = 100, criterion = "ssi")
plot(fg.KM.cascade, sortg = TRUE)
library(NbClust)
NbClust(analysis.table, diss = fg.foliar.dist, distance = NULL, min.nc = 3, max.nc = 20, method = "kmeans", index = "all")
par(mfrow=c(1,1))


# Fuzzy clustering
# Start with number of clusters indicated by cluster metrics (k)
# Adjust fuzziness/crispness with membership exponent approaching 2 for fuzzier classification
library(cluster)
veg.fanny <- fanny(fg.foliar.dist, k = 3, memb.exp = 1.01, maxit = 1000, keep.diss = TRUE)
# Display's Dunn's partition coefficient (low coeff = very fuzzy, near 1 = crisp)
veg.fanny$coeff
# Build a dataframe of membership values
fanny.mems <- as.data.frame(veg.fanny$membership)
fanny.mems <- fanny.mems %>%
  mutate_if(is.numeric, round, digits = 3)
# Ordination of clusters
clusplot(veg.fanny, lines = 0, labels = 3, plotchar = FALSE, col.text = "black", cex.text = 0.9)
# Or, with ordiplot
colors <- c("darkseagreen4", "palevioletred1", "steelblue2", "tan3")
# Ordinate
ordiplot(vegPCA, main = "Fuzzy Clusters - Plant Abundance")
stars(veg.fanny$membership, locatio = vegPCA, draw.segm = TRUE, add = TRUE, scale = FALSE, len = 0.08, 
      col.segments = colors)
ordihull(vegPCA, veg.fanny$clustering, col = "black")
ordispider(vegPCA, veg.fanny$clustering, col = "gray", label = T)
# Vectors
fit <- envfit(vegPCA, analysis.table, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
plot(fit, p.max = 0.05, col = "blue")


# Assign plots to clusters by top membership value
topmems <- fanny.mems %>%
  mutate(ID = rownames(fanny.mems)) %>%
  gather(Cluster, MemVal, V1:V3) %>%
  group_by(ID) %>%
  arrange(MemVal) %>%
  slice(which.max(MemVal)) %>%
  ungroup() %>%
  arrange(Cluster, MemVal)
str(analysis.table)
topmems$ID <- as.numeric(topmems$ID)
topmems<- inner_join(topmems, analysis.table)



# Rename clusters#Edited cluster numbers to use for spiders
names(topmems)
topmem.summary <- topmems %>%
  dplyr::select(Abies:Yucca, S, Gap2, Cluster) %>%
  gather(Species, MeanCover, Abies:Gap2, factor_key = FALSE) %>%
  group_by(Cluster, Species) %>%
  select_if(is.numeric) %>%
  summarise_all(funs(mean)) %>%
  filter(MeanCover > 0)
# Sort by highest
topmem.summary <- topmem.summary %>%
  group_by(Cluster) %>%
  arrange(desc(MeanCover), .by_group = TRUE)
# Use lookup table to rename
clusternames <- read.csv("clusternames.csv")
# Join
topmems <- dplyr::left_join(topmems, clusternames, by = "Cluster")

