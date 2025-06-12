library(tidyverse)
library(sf)
library(raster)
library(stars)
library(MBHdesign)






















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



