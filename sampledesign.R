library(tidyverse)
library(sf)
library(raster)
library(stars)
library(spsurvey)
library(igraph)
library(geosphere)


# Redistribute sample points from both years across Limulunga project areas
# Read in Mombola points
mom_points <- sf::st_read(dsn = "T:\\Projects\\Carbon and Biodiversity Projects\\Mafisa-2\\Projects\\Mafisa-2_SampleDesign\\Mafisa-2_SampleDesign.gdb",
                          layer = "mom_tsu_final_withstrata")
# Read in area 
mom_project_area <- sf::st_read(dsn = "T:\\Projects\\Carbon and Biodiversity Projects\\Mafisa-2\\Projects\\Mafisa-2_SampleDesign\\Mafisa-2_SampleDesign.gdb",
                                layer = "Mombola_Project_Area_revised")
# Find differences in areas (old and added)
unique(mom_project_area$Name)
limulunga <- c("Limulunga 2", "Limulunga 3", "Limulunga West")
point_areas <- subset(mom_project_area, !(mom_project_area$Name %in% limulunga)) 
nopoint_areas <- subset(mom_project_area, mom_project_area$Name %in% limulunga)
# Sum areas 
pointareas_total <- point_areas %>%
  summarise(TotalArea = sum(Area))

nopointareas_total <- nopoint_areas %>%
  summarise(TotalArea = sum(Area))
# 8019 km2 have points
# 2896 km2 don't have points
# Distribute points proportionally 
# What percent of 8019 is 2896
(2896*100)/8019
# 36% of the total project area doesn't have points
# What is 36% of 538 (ssus)?
(36*538)/100
# 194 SSUs
# 581 points from overall sample design should be moved to Limulunga
# 290 points from each year, 97 SSUs from each year
# Need to cluster points so that clusters remain intact
adj <- sf::st_distance(mom_points) # Calculate distances
adj <- matrix(as.numeric(as.numeric(adj)) < 600, nrow = nrow(adj)) # Binary matrix teling us whether each plot is within 600 m
g <- graph_from_adjacency_matrix(adj) # Plot
plot(g)
# Add back to dataframe
mom_points$Cluster <- factor(components(g)$membership)
# Add new plot names so that plots within a cluster are close in name
mom_points <- mom_points %>%
  dplyr::group_by(Cluster) %>%
  dplyr::mutate(ID = row_number())
mom_points <- tidyr::unite(mom_points, col = "NewPlotName", c("Project", "Cluster", "ID"), sep = "", remove = FALSE)
# Tidy column names
names(mom_points)
mom_points <- dplyr::select(mom_points, PlotName = NewPlotName, SSU = Cluster, FID = FID_1, SampleYear:Y, Project, Shape)
# Create list of 97 SSUs to remove from each year (exclude SSU 467 from 2025 which was sampled already)
ssus_2025 <- mom_points %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(SampleYear == 2025) %>%
  dplyr::select(SSU) %>%
  dplyr::distinct()
ssu_2025_rm <- dplyr::filter(ssus_2025, SSU != 467)
ssu_2025_rm <- ssu_2025_rm[sample(1:nrow(ssu_2025_rm), size = 97), ]
# Repeat for 2026 (removing 290 points)
ssus_2026 <- mom_points %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(SampleYear == 2026) %>%
  dplyr::select(SSU) %>%
  dplyr::distinct()
ssu_2026_rm <-ssus_2026[sample(1:nrow(ssus_2026), size = 97), ]
# Remove this random selection from total points and save as new shapefile
ssu_rm <- rbind(ssu_2025_rm, ssu_2026_rm)
mom_points <- subset(mom_points, !(mom_points$SSU %in% ssu_rm$SSU))
# Now will have to remake 580 points for new polygons
st_write(mom_points, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Baseline\\Spatial\\mom_final_tsu_nolimulunga.shp", append = FALSE)




# Subset Limulunga TSUs by sample year, add cluster, etc
limulunga_tsus <- sf::st_read(dsn = "T:\\Projects\\Carbon and Biodiversity Projects\\Mafisa-2\\Projects\\Mafisa-2_SampleDesign\\Mafisa-2_SampleDesign.gdb",
                          layer = "mom_lim_tsus_join")

adj <- sf::st_distance(limulunga_tsus) # Calculate distances
adj <- matrix(as.numeric(as.numeric(adj)) < 600, nrow = nrow(adj)) # Binary matrix teling us whether each plot is within 600 m
g <- graph_from_adjacency_matrix(adj) # Plot
plot(g)
# Add back to dataframe
limulunga_tsus$Cluster <- factor(components(g)$membership)
# Add new plot names so that plots within a cluster are close in name
limulunga_tsus$Project <- "Mombola"
limulunga_tsus$FID <- seq.int(nrow(limulunga_tsus))
limulunga_tsus <- limulunga_tsus %>%
  dplyr::group_by(Cluster) %>%
  dplyr::mutate(ID = row_number())
limulunga_tsus <- tidyr::unite(limulunga_tsus , col = "PlotName", c("Project", "Cluster", "ID"), sep = "", remove = FALSE)

st_write(limulunga_tsus, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Baseline\\Spatial\\lim_tsus.shp", append = FALSE)


# Add sample year
lim_tsus <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Baseline\\Spatial\\lim_tsus.shp")
table(lim_tsus$Cluster)


clusters <- lim_tsus %>%
  sf::st_drop_geometry() %>%
  dplyr::select(Cluster) %>%
  dplyr::distinct()

clusters2025 <- clusters[sample(1:nrow(clusters), size = 102), ]
lim_tsu_2025 <- subset(lim_tsus, lim_tsus$Cluster %in% clusters2025)
lim_tsu_2026 <- subset(lim_tsus, !(lim_tsus$Cluster %in% clusters2025))

lim_tsu_2026$SampleYear <- 2026
lim_tsu_2025$SampleYear <- 2025

lim_tsus <- rbind(lim_tsu_2025, lim_tsu_2026)
names(lim_tsus)
lim_tsus <- dplyr::select(lim_tsus, FID = FID_1, PlotName, SSU = Cluster, SampleYear, X, Y, Project)

st_write(lim_tsus, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Baseline\\Spatial\\mom_limulunga_tsus.shp", append = FALSE)

# Split whole Mombola project area by year










# Subset points by sample year
mom_ssu <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mom_ssu.shp")

mom_ssu_2025 <- mom_ssu[sample(1:nrow(mom_ssu), size = 269), ]
mom_ssu_2025$SampleYear <- 2025

mom_ssu <- subset(mom_ssu, !(mom_ssu$CID %in% mom_ssu_2025$CID)) 
mom_ssu$SampleYear <- 2026


st_write(mom_ssu, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mom_ssu_2026.shp", append = FALSE)
st_write(mom_ssu_2025, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mom_ssu_2025.shp", append = FALSE)



# Mombola point cleanup
mom_points <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mom_tsu_strata.shp")
# Remove duplicates
names(mom_points)
mom_points <- dplyr::select(mom_points, X, Y, CoverClass = gridcode, SampleYear, geometry)
mom_points <- dplyr::distinct(mom_points)
names(mom_points)
mom_points$FID <- seq.int(nrow(mom_points))
mom_points <- dplyr::mutate(mom_points, Cover = ifelse(CoverClass == 2, "Dambo",
                                                       ifelse(CoverClass == 3, "Woodland",
                                                              ifelse(CoverClass == 4, "Savanna", "Unknown"))))

mom_points <- dplyr::select(mom_points, FID, SampleYear, X, Y, CoverCode = CoverClass, Cover, geometry)

# Add preliminary names
mom_points$Project <- "Mombola"

mom_points <- tidyr::unite(mom_points, col = "PlotName", c("Project", "FID"), sep = "", remove = FALSE)


sf::st_write(mom_points, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mom_tsu_final_withstrata.shp", append = FALSE)






# Subset Mutala points by sample year
mut_ssu <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mut_ssu.shp")

mut_ssu_2025 <- mut_ssu[sample(1:nrow(mut_ssu), size = 269), ]
mut_ssu_2025$SampleYear <- 2025

mut_ssu <- subset(mut_ssu, !(mut_ssu$CID %in% mut_ssu_2025$CID)) 
mut_ssu$SampleYear <- 2026


st_write(mut_ssu, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mut_ssu_2026.shp", append = FALSE)
st_write(mut_ssu_2025, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mut_ssu_2025.shp", append = FALSE)



# Mutala point cleanup
mut_points <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mut_tsu_strata.shp")
# Remove duplicates
names(mut_points)
mut_points <- dplyr::select(mut_points, X, Y, CoverClass = gridcode, SampleYear, geometry)
mut_points <- dplyr::distinct(mut_points)
names(mut_points)
mut_points$FID <- seq.int(nrow(mut_points))
mut_points <- dplyr::mutate(mut_points, Cover = ifelse(CoverClass == 2, "Dambo",
                                                       ifelse(CoverClass == 3, "Woodland",
                                                              ifelse(CoverClass == 4, "Savanna", "Unknown"))))

mut_points <- dplyr::select(mut_points, FID, SampleYear, X, Y, CoverCode = CoverClass, Cover, geometry)

# Add preliminary names
mut_points$Project <- "Mutala"

mut_points <- tidyr::unite(mut_points, col = "PlotName", c("Project", "FID"), sep = "", remove = FALSE)

mut_points <- sf::st_zm(mut_points, drop = TRUE, what = "ZM")

sf::st_write(mut_points, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mut_tsu_final_withstrata.shp", append = FALSE)










# Calculate poly areas and look at sample points per strata
mut_lc <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mut_lc_polys.shp")
mut_lc <- sf::st_make_valid(mut_lc)
mut_lc$Area <- sf::st_area(mut_lc)
# Get total project area 
mut_pa <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\MutalaProjectArea.shp")
mut_pa$Area <- sf::st_area(mut_pa)
# Sum areas by strata
mut_lc_df <- sf::st_drop_geometry(mut_lc)
mut_area_sum <- mut_lc_df %>%
  dplyr::group_by(gridcode) %>%
  dplyr::summarise(StrataArea = sum(Area))
# Convert to km
mut_area_sum <- dplyr::mutate(mut_area_sum, AreaKm = StrataArea/1000000)
mut_pa <- dplyr::mutate(mut_pa, Area_km = Area/1000000)
# 13483 is the total project area km2
# Find percent areas
mut_area_sum <- dplyr::mutate(mut_area_sum, Percents = (AreaKm*100)/13483)
# Read in points
# Point calculation
mut_tsu <- sf::st_drop_geometry(mut_points)
mut_tsu_table <- mut_tsu %>%
  dplyr::group_by(CoverClass) %>%
  dplyr::summarise(Count = n())
# Sum of points with strata
104+636+786
# 1526 in our cover classes of interest
# 84 points in agriculture
# 5 points in water
# Percents
mut_tsu_table <- dplyr::mutate(mut_tsu_table, Percent = (Count*100)/1526)
# Very similar




# Repeat for Mombola
mom_lc <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mom_lc_polys.shp")
mom_lc <- sf::st_make_valid(mom_lc)
mom_lc$Area <- sf::st_area(mom_lc)
# Get total project area 
mom_pa <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\MombolaProjectArea.shp")
mom_pa$Area <- sf::st_area(mom_pa)
# Sum areas by strata
mom_lc_df <- sf::st_drop_geometry(mom_lc)
mom_area_sum <- mom_lc_df %>%
  dplyr::group_by(gridcode) %>%
  dplyr::summarise(StrataArea = sum(Area))
# Convert to km
mom_area_sum <- dplyr::mutate(mom_area_sum, AreaKm = StrataArea/1000000)
mom_pa <- dplyr::mutate(mom_pa, Area_km = Area/1000000)
mom_pa <- mom_pa %>%
  dplyr::summarise(Area_sum = sum(Area_km))
# 8046is the total project area km2
# Find percent areas
mom_area_sum <- dplyr::mutate(mom_area_sum, Percents = (AreaKm*100)/8046)
# Read in points
mom_tsu <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mom_tsu_final_withstrata.shp")
# Point calculation
mom_tsu <- sf::st_drop_geometry(mom_tsu)
mom_tsu_table <- mom_tsu %>%
  dplyr::group_by(CoverClass) %>%
  dplyr::summarise(Count = n())
# Sum of points with strata
478+835+194
# Percents
mom_tsu_table <- dplyr::mutate(mom_tsu_table, Percent = (Count*100)/1507)
# Very similar

# Join and calculate percent differences
mut_area_sum <- dplyr::select(mut_area_sum, CoverClass = gridcode, AreaKm, AreaPercent = Percents)
mut_area_sum$AreaPercent <- as.numeric(mut_area_sum$AreaPercent)
mut_area_sum$AreaKm <- as.numeric(mut_area_sum$AreaKm)
mut_tsu_table <- mut_tsu_table %>%
  dplyr::filter(CoverClass == 2 | CoverClass == 3 | CoverClass == 4) %>%
  dplyr::left_join(mut_area_sum)


mom_area_sum <- dplyr::select(mom_area_sum, CoverClass = gridcode, AreaKm, AreaPercent = Percents)
mom_area_sum$AreaPercent <- as.numeric(mom_area_sum$AreaPercent)
mom_area_sum$AreaKm <- as.numeric(mom_area_sum$AreaKm)
mom_tsu_table <- mom_tsu_table %>%
  dplyr::filter(CoverClass == 2 | CoverClass == 3 | CoverClass == 4) %>%
  dplyr::left_join(mom_area_sum)





# Add plot names
mut_tsu <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mut_tsu_final_withstrata.shp")

mut_tsu$Project <- "Mutala"

mut_tsu$Name <- "MTL"

# Add name of cover class
mut_tsu <- dplyr::mutate(mut_tsu, CoverClassName = ifelse(CoverClass == 2, "Dambo",
                                                          ifelse(CoverClass == 3, "Woodland",
                                                                 ifelse(CoverClass == 4, "Savanna", 
                                                                        ifelse(CoverClass == 1, "Agriculture",
                                                                               ifelse(CoverClass == 2, "Water", NA))))))

mut_tsu <- tidyr::unite(mut_tsu, col = "PlotName", c("Name", "FID_1"), sep = "", remove = FALSE)

mut_tsu <- dplyr::select(mut_tsu, FID = FID_1, Project, PlotName, 
                         CoverClass, CoverClassName, X = POINT_X, Y = POINT_Y, geometry)
sf::st_write(mut_tsu, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mut_tsu_final_withstrata.shp", append = FALSE)





# Calculate sample size if CI is 95%, SD is 4.1, mean difference is 0.4

((1.96*4.1)/(0.10*0.4))^2 # One year

((1.96*4.1)/(0.10*(0.4*2)))^2 # Two years

((1.96*4.1)/(0.10*(0.4*3)))^2 # Three years

((1.96*4.1)/(0.10*(0.4*4)))^2 # Four years

((1.96*4.1)/(0.10*(0.4*5)))^2 # Five years

((1.96*4.1)/(0.10*(0.4*6)))^2 # Six years

((1.96*4.1)/(0.10*(0.4*7)))^2 # Seven years

((1.96*4.1)/(0.10*(0.4*8)))^2 # Eight years

((1.96*4.1)/(0.10*(0.4*10)))^2


# Uncertainty
(1.96*4.1)/(sqrt(631)*0.4) # Eight years





# Read in project area polygons and lc rasters
mut_pa <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\MutalaProjectArea.shp")

mom_pa <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\MombolaProjectArea.shp")

plot(mut_pa)
plot(mom_pa)

# Remove z dimensions
mut_pa <- sf::st_zm(mut_pa)
mom_pa <- sf::st_zm(mom_pa)

lc <- raster::raster("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\lulc\\prj.adf")
plot(lc)

# Clip rasters to project areas
mut_lc <- raster::crop(lc, extent(mut_pa))
mut_lc <- raster::mask(mut_lc, mut_pa)

plot(mut_lc)
plot(mut_pa, add = TRUE)


mom_lc <- raster::crop(lc, extent(mom_pa))
mom_lc <- raster::mask(mom_lc, mom_pa)

plot(mom_lc)
plot(mom_pa, add = TRUE)

# Save raster clips
# raster::writeRaster(mut_lc,'C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mut_lc_clip.tif', options=c('TFW=YES'))
# raster::writeRaster(mom_lc,'C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mom_lc_clip.tif', options=c('TFW=YES'))



# Convert to polygons (convert to SpatRaster to use terra package)

mut_lc_poly <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(mut_lc), 
                                         as_points = FALSE, merge = TRUE)) 

plot(mut_lc_poly)

mom_lc_poly <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(mom_lc),
                                           as_points = FALSE, merge = TRUE))



