library(tidyverse)
library(sf)
library(raster)
library(stars)
library(spsurvey)


# Calculate points per stratum
mom_points <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mom_ssu_5_gridcode.shp")
sum1 <- mom_points %>%
  dplyr::group_by(gridcode) %>%
  dplyr::summarise(Count = n())


sum2 <- mom_points %>%
  dplyr::group_by(gridcode) %>%
  dplyr::summarise(Count = n())



# Mombola point cleanup
mom_points <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mombola_tsu_strata.shp")
# Remove duplicates
names(mom_points)
mom_points <- dplyr::select(mom_points, POINT_X, POINT_Y, RASTERVALU, geometry)
mom_points <- dplyr::distinct(mom_points)
names(mom_points)
mom_points$FID <- seq.int(nrow(mom_points))
mom_points <- dplyr::select(mom_points, FID, POINT_X, POINT_Y, CoverClass = RASTERVALU, geometry)



st_write(mom_points, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mom_tsu_final_withstrata.shp", append = FALSE)


# Mutala point cleanup
mut_points <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mutala_tsu_strata.shp")
# Remove duplicates
mut_points <- dplyr::select(mut_points, POINT_X, POINT_Y, RASTERVALU, geometry)
mut_points <- dplyr::distinct(mut_points)
names(mut_points)
mut_points$FID <- seq.int(nrow(mut_points))
mut_points <- dplyr::select(mut_points, FID, POINT_X, POINT_Y, CoverClass = RASTERVALU, geometry)

st_write(mut_points, "C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2\\Mafisa 2 Data\\Spatial\\mut_tsu_final_withstrata.shp", append = FALSE)


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



