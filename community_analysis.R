## Community analysis with L1.1 data 

library(tidyverse)
library(ggplot2)
library(vegan)
library(indicspecies)

# Set working directory
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/L1/"
setwd(dir)

# Read in L1.1 data
soilbiomass <- read.csv("Mafisa2_SoilBiomass_L1.1.csv")
dbh <- read.csv("Mafisa_2_dbh_20250505.csv")
gctable <- read.csv("Mafisa2_GC_L1.1.csv")
vegtable <- read.csv("Mafisa2_Veg2x2_BD_L1.1.csv")
presence <- read.csv("Mafisa2_SpeciesPresence_L1.1.csv")
spwide <- read.csv("Mafisa2_topspecieswide_L1.1.csv")

# Replace densi NA with 0
soilbiomass <- dplyr::mutate(soilbiomass , DensiCanopyCover = ifelse(is.na(DensiCanopyCover), 0, DensiCanopyCover))

# Rosewood
rosewood <- vegtable %>%
  dplyr::select(SoilPlot, Guibourtia.coleosperma, Pterocarpus.angolensis) %>%
  dplyr::filter(Guibourtia.coleosperma > 1 | Pterocarpus.angolensis > 1)
rosewood <- dplyr::left_join(rosewood, cover)

classes <- cover %>%
  dplyr::mutate(Community = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Luampa",
                                   ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Nalolo",
                                          ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                 ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Sioma", NA))))) %>%
  dplyr::group_by(Community, GoogleEarthClass.1) %>%
  dplyr::summarize(Count = n())

# Read in plot cover class
cover <- read.csv("plot_cover_photos.csv")
cover <- dplyr::rename(cover, SoilPlot = Plot.Name)



# Count trees with dbh > 5 per plot and join to soil table
dbh_summary <- dbh %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(TreeCount = n(),
                   MeanDBH = mean(Tree_dbh))
soilbiomass <- dplyr::left_join(soilbiomass, dbh_summary)
soilbiomass <- dplyr::mutate(soilbiomass, TreeCount = ifelse(is.na(TreeCount), 0, TreeCount),
                             MeanDBH = ifelse(is.na(MeanDBH), 0, MeanDBH))



# Create analysis table 
names(soilbiomass)
names(gctable)
names(vegtable)


varjoin <- soilbiomass %>%
  dplyr::left_join(gctable) %>%
  dplyr::left_join(vegtable) %>%
  dplyr::left_join(cover) 
# Give each row an ID column (rownumber)
varjoin <- dplyr::mutate(varjoin, ID = rownames(varjoin))
varjoin$ID <- as.numeric(varjoin$ID)

# Estimate missing values
varjoin <- dplyr::mutate(varjoin, bare_ground = ifelse(SoilPlot == "SS05", 40, bare_ground),
                         MeanHeight = ifelse(SoilPlot == "SS05", 350, MeanHeight))











# Subset for ordination
names(varjoin)
analysis.table <- varjoin %>%
  dplyr::select(SoilPlot, ID, AnnualForb:Shrub, DensiCanopyCover, TotalFoliar, bare_ground,
                TreeCount, MeanDBH, LargeGaps, MeanHeight, Class = GoogleEarthClass.2) 

# Remove nominal/categorical variables
fg.foliar <- dplyr::select(analysis.table, -SoilPlot, -ID, -Class)

# Look for NAs
nas <- dplyr::filter_all(fg.foliar, any_vars(is.na(.)))


# Remove NA values
fg.foliar <- na.omit(fg.foliar)
str(fg.foliar)
# Convert to a bray-curtis dissimilarity matrix
fg.foliar.dist <- vegan::vegdist(fg.foliar, method = "gower")
# Run a PCoA on functional groups and structural indicators
vegPCA <- cmdscale(fg.foliar.dist, k = 2)
# View ordination
ordiplot(vegPCA, type = "text")
# Look at functional group correlation with ordination axes and plot over ordination
fit <- envfit(vegPCA, fg.foliar, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
fit
plot(fit, p.max = 0.05, col = "red")
# Plot vegclass labels over ordination
score <- vegan::scores(vegPCA)[, 1:2]
vegclass <- dplyr::select(analysis.table, SoilPlot, Class)
score <- cbind(score, vegclass)  
ordiplot(vegPCA, type = "points")
orditorp(vegPCA, display = "sp", label = score$Class, scaling = scl, font = 2, fill = "white")
fgs <- dplyr::select(vegtable, AnnualForb:TotalFoliar)
# Vectors
fit <- envfit(vegPCA, fgs, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
plot(fit, p.max = 0.05, col = "blue")







str(fg.foliar.dist)
# Test cluster number metrics
library(labdsv)
fg.KM.cascade <- cascadeKM(fg.foliar.dist, inf.gr = 3, sup.gr = 20, iter = 100, criterion = "ssi")
plot(fg.KM.cascade, sortg = TRUE)
library(NbClust)
NbClust(data = fg.foliar, diss = fg.foliar.dist, distance = NULL, min.nc = 3, max.nc = 10, method = "kmeans", index = "all")
par(mfrow=c(1,1))

# Fuzzy clustering
# Start with number of clusters indicated by cluster metrics (k)
# Adjust fuzziness/crispness with membership exponent approaching 2 for fuzzier classification
library(cluster)
veg.fanny <- fanny(fg.foliar.dist, k = 3, memb.exp = 1.2, maxit = 1000, keep.diss = TRUE)
# Display's Dunn's partition coefficient (low coeff = very fuzzy, near 1 = crisp)
veg.fanny$coeff
# Build a dataframe of membership values
fanny.mems <- as.data.frame(veg.fanny$membership)
fanny.mems <- fanny.mems %>%
  mutate_if(is.numeric, round, digits = 3)
# Ordination of clusters
clusplot(veg.fanny, lines = 0, labels = 3, plotchar = FALSE, col.text = "black", cex.text = 0.9)
# Or, with ordiplot
colors <- c("darkseagreen4", "palevioletred1", "steelblue2", "tan3", "gray")

# Ordinate
ordiplot(vegPCA, main = "Fuzzy Clusters")
stars(veg.fanny$membership, locatio = vegPCA, draw.segm = TRUE, add = TRUE, scale = FALSE, len = 0.03, 
      col.segments = colors)
ordihull(vegPCA, veg.fanny$clustering, col = "black")
ordispider(vegPCA, veg.fanny$clustering, col = "gray", label = T)
# Vectors
fit <- envfit(vegPCA, fg.foliar, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
plot(fit, p.max = 0.05, col = "blue")


# Plot known veg classes onto clusters
score <- vegan::scores(vegPCA)[, 1:2]
vegclass <- dplyr::select(analysis.table, SoilPlot, Class)
score <- cbind(score, vegclass)                

ordiplot(vegPCA, main = "Fuzzy Clusters", type = "n")
stars(veg.fanny$membership, locatio = vegPCA, draw.segm = TRUE, add = TRUE, scale = FALSE, len = 0.03, 
      col.segments = colors, labels = NULL)
ordihull(vegPCA, veg.fanny$clustering, col = "black")
ordispider(vegPCA, veg.fanny$clustering, col = "gray", label = T)
orditorp(vegPCA, display = "site", label = score$Class, scaling = scl, font = 2, fill = "white")
fit <- envfit(vegPCA, fg.foliar, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
fit
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
names(topmems)
topmems <- dplyr::select(topmems, ID, Cluster, MemVal, Class, SoilPlot, AnnualForb:MeanHeight)


# Rename clusters
#Edited cluster numbers to use for spiders
names(topmems)
topmem.summary <- topmems %>%
  dplyr::select(AnnualForb:MeanHeight, Cluster, Class) %>%
  gather(Species, MeanCover, AnnualForb:MeanHeight, factor_key = FALSE) %>%
  group_by(Cluster, Species) %>%
  select_if(is.numeric) %>%
  summarise_all(funs(mean)) %>%
  filter(MeanCover > 0)
# Sort by highest
topmem.summary <- topmem.summary %>%
  group_by(Cluster) %>%
  arrange(desc(MeanCover), .by_group = TRUE) %>%
  dplyr::mutate_if(is.numeric, round, 1)



# Boxplots of indicators by cluster
names(topmems)
topmems %>%
  ggplot(aes(x = Cluster, y = Tree_densi, fill = Cluster)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("")






# CART
library(rpart)
library(rpart.plot)
# Join fgs to topmems
names(analysis.table)
treevars <- dplyr::select(analysis.table, Class, AnnualForb:PerennialGrass, TotalFoliar, bare_ground)
treevars <- dplyr::rename(treevars, TreeCover = DensiCanopyCover)

tree <- rpart::rpart(Class ~., data = treevars, method = "class")
tree

rpart.plot::rpart.plot(tree, extra = 101)



# Indicator species analysis
names(varjoin)

sp <- varjoin %>%
  dplyr::filter(SoilPlot != "SS04") %>%
  dplyr::select(Abrus.pulchellus.:Xylopia.odoratissima)

class <- varjoin %>%
  dplyr::filter(SoilPlot != "SS04") %>%
  dplyr::select(GoogleEarthClass.2)

inv <- indicspecies::multipatt(sp, class$GoogleEarthClass.2)

summary(inv)








# Turn into a kml for google earth
topmems <- topmems[-c(50), ]
xy <- dplyr::select(soilbiomass, SoilPlot, x, y)
topmems <- dplyr::left_join(topmems, xy)

library(sf)
# Convert to sf object
topmems.sf <- sf::st_as_sf(topmems, coords = c("x", "y"), crs = 4326)
# Plot
plot(topmems.sf["Class"])
# Project
topmems.sf <- sf::st_transform(topmems.sf, st_crs("+proj=longlat +datum=WGS84"))
# Write it as kml file
sf::st_write(topmems.sf, "topmems20250506.kml", driver="KML") 


# Separate files by classes
# Convert to sf object
dambo <- dplyr::filter(topmems.sf, Class == "Dambo")
# Write it as kml file
sf::st_write(dambo, "Spatial/Dambo.kml", driver="KML") 

sav <- dplyr::filter(topmems.sf, Class == "Savanna" | Class == "Shrubland")
# Write it as kml file
sf::st_write(sav, "Spatial/Savanna.kml", driver="KML") 


ywoodland <- dplyr::filter(topmems.sf, Class == "Immature woodland")
# Write it as kml file
sf::st_write(ywoodland, "Spatial/ImmatureWoodland.kml", driver="KML")  


woodland <- dplyr::filter(topmems.sf, Class == "Woodland")
# Write it as kml file
sf::st_write(woodland, "Spatial/woodland.kml", driver="KML")                            




# Add x,y data to veg classes
xy <- dplyr::select(soilbiomass, SoilPlot, x, y)
ss16 <- MAFISA_2_General_Site_BD_Fo_0 %>%
  dplyr::filter(SoilPlot == "SS16") %>%
  dplyr::select(SoilPlot, x, y)
xy <- rbind(xy, ss16)
cover <- dplyr::left_join(cover, xy)

write.csv(cover, "plot_cover_xy.csv", row.names = FALSE)
# Save to kml
cover.sp <- sf::st_as_sf(cover, coords = c("x", "y"), crs = 4326)
# Plot
plot(cover.sp["Class.1"])
# Project
cover.sp <- sf::st_transform(cover.sp, st_crs("+proj=longlat +datum=WGS84"))
# Write it as kml file
sf::st_write(cover.sp, "Spatial/plot_cover_xy.kml", driver="KML") 
