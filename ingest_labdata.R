### Ingesting data from S123 output forms

library(tidyverse)
library(readxl)
library(ggplot2)
library(ggsoiltexture)
library(ggrepel)

### Point to data folder

dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/"
setwd(dir)


# Level one lab data
labdata <- excel_sheets("L1/20250513_LabSamples_L1.xlsx") # File path
labdata_list <- lapply(labdata, function(x) read_excel("L1/20250513_LabSamples_L1.xlsx", sheet = x))  # Read excel file into list of excel sheets
names(labdata_list) <- labdata # Pull sheet names
list2env(labdata_list, envir=.GlobalEnv) # Write each excel sheet to a separate dataframe 


# Bring in field data
soilbiomass <- read.csv("L2/Mafisa2_SoilBiomass_L2.csv")
cover <- read.csv("L1/plot_cover_photos.csv")
cover <- dplyr::select(cover, SoilPlot = Plot.Name, Class = GoogleEarthClass.1)


### Clean SOC table
SOC_clean <- OrganicCarbon %>%
  dplyr::mutate(SoilPlot = str_replace_all(SoilPlot, fixed(" "), ""), # Remove white spaces from plot names
                District = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Luampa",
                                   ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Nalolo",
                                          ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                 ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Sioma", NA))))) # Add district name
  


# Create histogram of C values
c <- ggplot(SOC_clean, aes(x = OC)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Soil organic carbon (%)") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1.7, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 2, 0.1), limits = c(0, 2)) 
c




### Clean texture table
TXT_clean <- SoilTexture %>%
  dplyr::mutate(SoilPlot = str_replace_all(SoilPlot, fixed(" "), ""), # Remove white spaces from plot names
                District = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Luampa",
                                  ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Nalolo",
                                         ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Sioma", NA))))) # Add district name

# Create histogram of C values
names(TXT_clean)
sand <- ggplot(TXT_clean, aes(x = Sand_pct)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Soil percent sand (%)") +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # geom_vline(xintercept = 1.7, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) 
sand

clay <- ggplot(TXT_clean, aes(x = Clay_pct)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Soil percent clay (%)") +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # geom_vline(xintercept = 1.7, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) 
clay

silt <- ggplot(TXT_clean, aes(x = Silt_pct)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Soil percent clay (%)") +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # geom_vline(xintercept = 1.7, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) 
silt


# Plot within texture triangle
txtplot <- dplyr::left_join(TXT_clean, SOC_clean)
txtplot <- dplyr::select(txtplot, District, SoilPlot, sand = Sand_pct, clay = Clay_pct, silt = Silt_pct, OC, TXT_class)
txtplot <- dplyr::left_join(txtplot, cover)

ggsoiltexture::ggsoiltexture(txtplot, class = "USDA")

# Carbon by texture
ggsoiltexture::ggsoiltexture(txtplot, class = "USDA") +
  geom_point(aes(color = OC, size = OC)) +
  scale_color_continuous(type = "viridis", direction = -1) +
  scale_size_continuous(range = c(1, 5))
  labs(color = "organic carbon (%)") +
  theme(legend.title = element_text(face = "bold"),
        legend.position = "bottom")

  
  
# Split by basecamp
txtplot %>%
  dplyr::filter(District == "Luampa") %>%
  ggsoiltexture::ggsoiltexture(class = "USDA") +
    geom_point(aes(color = OC, size = OC)) +
    scale_color_continuous(type = "viridis", direction = -1) +
    scale_size_continuous(range = c(1, 5)) +
    labs(color = "organic carbon (%)") +
    # ggrepel::geom_label_repel(aes(label = Class), box.padding = 0.5, max.overlaps = 40) +
    theme(legend.title = element_text(face = "bold"),
          legend.position = "bottom")
  

# Stacked barchart of soil texture by basecamp
txtstack <- txtplot %>%
  dplyr::select(District, TXT_class) %>%
  dplyr::group_by(District, TXT_class) %>%
  dplyr::summarise(Count = n())

ggplot(txtstack, aes(fill = TXT_class, y = Count, x = District)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("Proportion of samples") +
  scale_fill_discrete(name = "Soil texture classes")
  






### Clean BD table
bd_clean<- BulkDensity %>%
dplyr::mutate(SoilPlot = str_replace_all(SoilPlot, fixed(" "), ""), # Remove white spaces from plot names
                District = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Luampa",
                                  ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Nalolo",
                                         ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Sioma", NA))))) # Add district name
# Some plots are missing field weights, and depth for all is 100 cm
# Bring in soilbiomass field data to cross-reference
names(soilbiomass)
fieldbd <- soilbiomass %>%
  dplyr::select(SoilPlot, BD_depth_field = BD_depth, WTD_mass) %>%
  dplyr::left_join(bd_clean)
# Replace missing wtd weights and depths from soilbiomass field data
names(fieldbd)
bd_clean <- fieldbd %>%
  dplyr::mutate(BD_depth_lab = BD_depth_field, 
                BD_field_weight = ifelse(is.na(BD_field_weight), WTD_mass, BD_field_weight)) %>%
  dplyr::select(SoilPlot, District:BD_dry_weight) 


# Create histogram of BD values
bd <- ggplot(bd_clean, aes(x = BD_dry_weight)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("BD dry weight (g)") +
 # geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
 # geom_vline(xintercept = 1.7, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(100, 600, 20), limits = c(100, 600)) 
bd




### Clean herb bio table
names(BiomassHerbacious)
names(soilbiomass)
# Cross reference values with field data
herbbio_clean <- BiomassHerbacious %>%
  dplyr::mutate(SoilPlot = str_replace_all(SoilPlot, fixed(" "), ""), # Remove white spaces from plot names
                District = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Luampa",
                                  ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Nalolo",
                                         ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Sioma", NA))))) # Add district name
# Select QC columns from field data and join to table
fieldherb <- dplyr::select(soilbiomass, SoilPlot, Biomass_collected, HerbBio_length_field = HerbBio_length, HerbBio_samples,
                           HerbBio_weight_field = HerbBio_weight)
herbbio_qc <- dplyr::left_join(herbbio_clean, fieldherb)
# Run QC
samplecount <- herbbio_qc %>% 
  select(starts_with('HerbLab')) %>% 
  by_row(~ sum(!is.na(.)), .collate = 'cols')
names(herbbio_qc)
herbbio_qc <- herbbio_qc %>%
  dplyr::mutate(TotalFieldWeight_sum = HerbFieldWeight1 + HerbFieldWeight2 + HerbFieldWeight3) %>%
  dplyr::select(herbbio_qc, SoilPlot, HerbBio_samples, WeightCount, HerbDistance1, HerbBio_length_field,
                TotalFieldWeight, HerbBio_weight_field)
