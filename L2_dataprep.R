### Preparing Mafisa 2 feasibility data for SNAPGRAZE pipeline

library(tidyverse)
library(sf)
library(geodata)
library(rworldmap)
library(terra)
library(ggplot2)
library(viridis)
library(sp)

### Point to data folder

dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/"
setwd(dir)

### Read in L2 Mafisa 2 data
soilbiomass <- read.csv("L2/Mafisa2_SoilBiomass_L2.csv")

soilbiomass <- soilbiomass[-c(42), ]

### Get polygon of Zambia country boundary
world <- rworldmap::getMap(resolution = "low")
class(world)
# Convert world data to sf object
world <- sf::st_as_sf(world)
class(world)
# Subset to Zambia 
zmb <- subset(world, ADMIN == "Zambia")
(zmap <- ggplot2::ggplot(data = zmb) +
    geom_sf(fill = "cornsilk")) 

# Save boundary as shapefile
sf::st_write(zmb, "L1\\Spatial\\zmb_boundary.shp", driver = 'ESRI Shapefile', append = FALSE)


### Get climate data from WorldClim download
# BIO1 = annual mean temperature
# BIO12 = annual precipitation 
zmb_wc <- geodata::worldclim_country("Zambia", var = "bio", path = tempdir())
plot(zmb_wc)

# Extract BIO1 and BIO12
bio1 <- zmb_wc[[1]]
bio12 <- zmb_wc[[12]]

# Convert soilbiomass data to x,y vector object
soilxy <- dplyr::select(soilbiomass, SoilPlot, x, y)
soilxy_v <- terra::vect(soilxy, geom = c("x", "y"),
                      crs = 4326)
# Visualize points
plot(soilxy_v)

# Extract raster values to points
temp <- terra::extract(bio1, soilxy_v)
temp <- cbind(soilxy, temp)
temp <- dplyr::select(temp, SoilPlot, temp_c = wc2.1_30s_bio_1, x, y)

precip <- terra::extract(bio12, soilxy_v)
precip <- cbind(soilxy, precip)
precip <- dplyr::select(precip, SoilPlot, precip_mm = wc2.1_30s_bio_12, x, y)


# Convert to sf objects
temp <- sf::st_as_sf(temp, coords = c("x", "y"), crs = 4326)
precip <- sf::st_as_sf(precip, coords = c("x", "y"), crs = 4326)

plot(temp["temp_c"])
plot(precip["precip_mm"])

# Plot over western Zambia
# Read in administrative boundaries
zmb_l1 <- sf::st_read("C:\\Users\\allie.heller\\OneDrive - Biodiversity Research Institute\\Desktop\\Mafisa 2 Data\\Spatial\\Zambia boundaries\\zmb_admbnda_adm1_dmmu_20201124.shp")
# Subset to western
westernprovince <- dplyr::filter(zmb_l1, ADM1_EN == "Western")


str(temp)
ggplot() +
  geom_sf(data = westernprovince, fill = "antiquewhite1", color = "gray") +
  geom_sf(data = temp, aes(color = temp_c)) +
  viridis::scale_color_viridis(name = "Annual mean temperature (c)") +
  theme(legend.title = element_text(hjust = 0.5))
  


ggplot() +
  geom_sf(data = westernprovince, fill = "antiquewhite1", color = "gray") +
  geom_sf(data = precip, aes(color = precip_mm)) +
  viridis::scale_color_viridis(name = "Average annual precipitation (mm)", direction = -1) +
  theme(legend.title = element_text(hjust = 0.5))






# Format data according to SNAPGRAZE workbook for KRCP
names(soilbiomass)

# Convert x, y data to UTM
soilxy_utm <- soilxy %>%
  sf::st_as_sf(coords = c("x", "y")) %>%
  sf::st_set_crs(4326) %>%
  sf::st_transform(32634) %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  dplyr::rename(UTM_E= X, UTM_S = Y) %>%
  cbind(soilxy) %>%
  dplyr::select(-x, -y)
# Transform to sf object and plot to make sure they projected correctly
soilxy_utm_sf <- sf::st_as_sf(soilxy_utm, coords = c("UTM_E", "UTM_S"), crs = 32634)
ggplot() +
  geom_sf(data = westernprovince, fill = "antiquewhite1", color = "gray") +
  geom_sf(data = soilxy_utm_sf) 
# Format data 
names(soilbiomass)

l3soil <- soilbiomass %>%
  dplyr::left_join(soilxy_utm) %>%
  dplyr::left_join(temp) %>%
  dplyr::left_join(precip) %>%
  dplyr::mutate(Project = "Mafisa 2 Feasibility",
                Community = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Luampa",
                                   ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Nalolo",
                                   ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                   ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Sioma", NA)))),
                PipeDiameter_cm = NA,
                CoreVolume_cm3 = NA,
                BD_g_cm3 = NA,
                SoilTextureClass = NA,
                EstimatedPercentSand = NA,
                VegClass = NA,
                photo_no_north = NA,
                photo_no_south = NA,
                SOC_percent = NA,
                SOC_density = NA) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(AverageDepth_cm = mean(c(SOC1_depth, SOC2_depth))) %>%
  dplyr::select(Project, SiteLabel = SoilPlot, Community, UTM_S, UTM_E, PipeDiameter_cm,
                SOC1_collected, SOC1_depth_cm = SOC1_depth, SOC2_collected, SOC2_depth_cm = SOC2_depth, 
                AverageDepth_cm, CoreVolume_cm3, BD_collected, BD_wet_mass_total_g = BD_wet_mass_total, BD_rock_volume_ml = BD_frag_vol,
                BD_g_cm3, SoilTextureClass, EstimatedPercentSand, VegClass, photo_no_north, photo_no_south, MeanAnnualRainfall_mm = precip_mm,
                MeanAnnualTemperature_C = temp_c, SOC_percent, SOC_density)
# Calculate average SOC sample depth
l3soil$AverageDepth_cm <- mean(c(l3soil$SOC1_depth_cm, l3soil$SOC2_depth_cm))
