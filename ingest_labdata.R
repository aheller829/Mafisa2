### Ingesting data from S123 output forms

library(tidyverse)
library(readxl)
library(ggplot2)
library(ggsoiltexture)
library(ggrepel)
library(ggridges)

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
                District = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Kanguya",
                                   ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Mombola",
                                          ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                 ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Mutala", NA))))) # Add district name
SOC_clean <- dplyr::select(SOC_clean, -SampleNo)
# Add corrected depths (from field data)
socdepths <- dplyr::select(soilbiomass, SoilPlot, SOC1_depth, SOC2_depth)
SOC_clean <- dplyr::left_join(SOC_clean, socdepths)  
SOC_clean <- dplyr::mutate(SOC_clean, SOC1_depth = ifelse(is.na(SOC1_depth), SOC_depth_lab, SOC1_depth),
                           SOC2_depth = ifelse(is.na(SOC2_depth), SOC_depth_lab, SOC2_depth),
                           SOC_average_depth = (SOC1_depth+SOC2_depth)/2)
SOC_clean <- dplyr::select(SOC_clean, -SOC_depth_lab)
SOC_clean <- dplyr::distinct(SOC_clean)
# Save to csv
write.csv(SOC_clean, "L1/Mafisa2_OrganicCarbon_L1.1.csv", row.names = FALSE)


# Create histogram of C values
c <- ggplot(SOC_clean, aes(x = OC)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Soil organic carbon (%)") +
  scale_x_continuous(breaks = seq(0, 2, 0.1), limits = c(0, 2)) 
c




### Clean texture table
TXT_clean <- SoilTexture %>%
  dplyr::mutate(SoilPlot = str_replace_all(SoilPlot, fixed(" "), ""), # Remove white spaces from plot names
                District = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Kanguya",
                                  ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Mombola",
                                         ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Mutala", NA))))) # Add district name
TXT_clean <- dplyr::select(TXT_clean, SoilPlot, Sand_pct, Clay_pct, Silt_pct, TXT_class)
TXT_clean <- dplyr::distinct(TXT_clean)
# Write to csv
write.csv(TXT_clean, "L1/Mafisa2_SoilTexture_L1.1.csv", row.names = FALSE)


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
  scale_size_continuous(range = c(1, 5), guide = "none") +
  labs(color = "organic carbon (%)") +
  theme(legend.title = element_text(face = "bold"),
        legend.position = "bottom")

  
  
# Split by basecamp
txtplot %>%
  dplyr::filter(District == "Senanga") %>%
  ggsoiltexture::ggsoiltexture(class = "USDA") +
    geom_point(aes(color = OC, size = OC)) +
    scale_color_continuous(type = "viridis", direction = -1) +
    scale_size_continuous(range = c(1, 5), guide = "none") +
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
                District = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Kanguya",
                                  ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Mombola",
                                         ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Mutala", NA))))) # Add district name
# Some plots are missing field weights, and depth for all is 100 cm
# Bring in soilbiomass field data to cross-reference
names(soilbiomass)
fieldbd <- soilbiomass %>%
  dplyr::select(SoilPlot, BD_depth, WTD_mass, BD_wet_mass_total) %>%
  dplyr::left_join(bd_clean)
# Check if any wtd weights are different by plot
wtd_qc <- dplyr::filter(fieldbd, WTD_mass != BD_field_weight)
# Five plots have mismatched weights: LU08, SS04, SS19, SS13, SSN10 

# Replace missing wtd weights and depths from soilbiomass field data
names(fieldbd)
bd_clean <- fieldbd %>%
  dplyr::mutate(BD_field_weight = ifelse(is.na(BD_field_weight), WTD_mass, BD_field_weight)) %>%
  dplyr::select(SoilPlot, District, BD_depth, BD_field_weight:BD_dry_weight) 

bd_clean <- dplyr::distinct(bd_clean)


write.csv(bd_clean, "L1/Mafisa2_BulkDensity_L1.1.csv", row.names = FALSE)


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
                District = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Kanguya",
                                  ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Mombola",
                                         ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Mutala", NA))))) # Add district name
# Select QC columns from field data and join to table
fieldherb <- dplyr::select(soilbiomass, SoilPlot, Biomass_collected, HerbBio_length, HerbBio_samples,
                           HerbBio_weight)
herbbio_qc <- dplyr::left_join(herbbio_clean, fieldherb)
# Run QC
samplecount <- herbbio_qc %>% 
  dplyr::select(SoilPlot, starts_with('HerbLabDry'), -HerbLabDryWeightAvg) %>%
  tidyr::gather(Instance, Weight, 2:5) %>%
  dplyr::mutate(Weight = ifelse(!is.na(Weight), 1, 0)) %>%
  dplyr::distinct() %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(SampleCount = sum(Weight))

names(herbbio_qc)
herbbio_qc <- herbbio_qc %>%
  dplyr::mutate(TotalFieldWeight_sum_fromlab = HerbFieldWeight1 + HerbFieldWeight2 + HerbFieldWeight3) %>%
  dplyr::left_join(samplecount) %>%
  dplyr::select(SoilPlot, HerbBio_samples, SampleCount, HerbDistance1, HerbBio_length,
                TotalFieldWeight_sum_fromlab, HerbBio_weight)
# Add columns to autoqc
names(herbbio_qc)
herbbio_sampleno_qc <- herbbio_qc %>%
  dplyr::select(SoilPlot, HerbBio_samples, SampleCount) %>%
  dplyr::filter(HerbBio_samples != SampleCount)

herbbio_weight_qc <- herbbio_qc %>%
  dplyr::select(SoilPlot, TotalFieldWeight_sum_fromlab, HerbBio_weight) %>%
  dplyr::filter(TotalFieldWeight_sum_fromlab != HerbBio_weight)


herbbio_transect_qc <- herbbio_qc %>%
  dplyr::select(SoilPlot, HerbDistance1, HerbBio_length) %>%
  dplyr::filter(HerbDistance1 !=HerbBio_length)

# Plots NLO07, NLO12, NLO13, SSN10, SSN14 have issues with field weights and/or sample number
names(herbbio_clean)
herbbio_clean <- herbbio_clean %>%
  dplyr::left_join(fieldherb) %>%
  dplyr::mutate(HerbDistance1 = HerbBio_length,
                HerbDistance2 = HerbBio_length, 
                HerbDistance3 = HerbBio_length,
                HerbDistance4 = HerbBio_length) %>%
  dplyr::select(SoilPlot, District, HerbDistance1:HerbBiomassDry)

herbbio_clean <- dplyr::distinct(herbbio_clean)
# Save to csv
write.csv(herbbio_clean, "L1/Mafisa2_HerbaceousBiomass_L1.1.csv", row.names = FALSE)









### Clean woody bio table
names(BiomassWoody)
names(soilbiomass)
# Cross reference values with field data
woodybio_clean <- BiomassWoody %>%
  dplyr::mutate(SoilPlot = str_replace_all(SoilPlot, fixed(" "), ""), # Remove white spaces from plot names
                District = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Kanguya",
                                  ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Mombola",
                                         ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Mutala", NA))))) # Add district name
# Select QC columns from field data and join to table
fieldwood <- dplyr::select(soilbiomass, SoilPlot, Biomass_collected, WoodyBio_length, WoodyBio_samples,
                           WoodyBio_weight)
woodbio_qc <- dplyr::left_join(woodybio_clean, fieldwood)
# Run QC
samplecount <- woodbio_qc %>% 
  dplyr::select(SoilPlot, starts_with('WoodyLabDry'), -WoodyLabDryWeightAvg) %>%
  tidyr::gather(Instance, Weight, 2:5) %>%
  dplyr::mutate(Weight = ifelse(!is.na(Weight), 1, 0)) %>%
  dplyr::distinct() %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(SampleCount = sum(Weight))

names(woodbio_qc)
woodbio_qc <- woodbio_qc %>%
  dplyr::mutate(TotalFieldWeight_sum_fromlab = WoodyFieldWeight1 + WoodyFieldWeight2 + WoodyFieldWeight3 + WoodyFieldWeight4 + WoodyFieldWeight5) %>%
  dplyr::left_join(samplecount) %>%
  dplyr::select(SoilPlot, WoodyBio_samples, SampleCount, WoodyDistance1, WoodyBio_length,
                TotalFieldWeight_sum_fromlab, WoodyBio_weight)
# Add columns to autoqc
names(woodbio_qc)
woodbio_sampleno_qc <- woodbio_qc %>%
  dplyr::select(SoilPlot, WoodyBio_samples, SampleCount) %>%
  dplyr::filter(WoodyBio_samples != SampleCount)

woodbio_weight_qc <- woodbio_qc %>%
  dplyr::select(SoilPlot, TotalFieldWeight_sum_fromlab, WoodyBio_weight) %>%
  dplyr::filter(TotalFieldWeight_sum_fromlab != WoodyBio_weight)


woodbio_transect_qc <- woodbio_qc %>%
  dplyr::select(SoilPlot, WoodyDistance1, WoodyBio_length) %>%
  dplyr::filter(WoodyDistance1 != WoodyBio_length)
# Join with herb bio transect length
woodbio_transect_qc <- dplyr::left_join(woodbio_transect_qc, herbbio_transect_qc)

# All woody weights match. LU19 was recorded as having no samples in the field, but two samples are present from lab. 
# A large number of plots have mismatched transect lengths 
woodybio_clean <- woodybio_clean %>%
  dplyr::left_join(fieldwood) %>%
  dplyr::mutate(WoodyDistance1 = WoodyBio_length,
                WoodyDistance2 = WoodyBio_length,
                WoodyDistance3 = WoodyBio_length,
                WoodyDistance4 = WoodyBio_length,
                WoodyDistance5 = WoodyBio_length) %>%
  dplyr::select(SoilPlot, District, WoodyDistance1:WoodyBiomassDry)

woodybio_clean <- dplyr::distinct(woodybio_clean)
# Save to csv
write.csv(woodybio_clean, "L1/Mafisa2_WoodyBiomass_L1.1.csv", row.names = FALSE)


# Derive bulk density
names(soilbiomass)
fullbd <- dplyr::select(soilbiomass, SoilPlot, BD_wet_mass_total, BD_frag_vol) # Extract weights for full BD cores
fullbd <- fullbd[-c(52), ]

bd_clean <- bd_clean %>%
  dplyr::left_join(fullbd) %>% # Join full core weights to lab bd
  dplyr::mutate(SoilWaterContent = (BD_field_weight - BD_dry_weight)/BD_dry_weight) %>% # Calculate soil water content
  dplyr::mutate(TotalBDDryWeight = BD_wet_mass_total/(1 + SoilWaterContent)) %>% # Calculate dry weight of full core
  dplyr::mutate(BDCoreVolume = 4.2^2*pi*BD_depth) %>% # Add core volume in cm ^ 3
  dplyr::mutate(BD_frag_vol = ifelse(is.na(BD_frag_vol), 0, BD_frag_vol)) %>% # Fill missing fragment measurements 
  dplyr::mutate(WholeCoreBD = TotalBDDryWeight/(BDCoreVolume - BD_frag_vol)) # Calculate bulk density
  
# Calculate SOC density
bd_clean <- bd_clean %>%
  dplyr::left_join(SOC_clean) %>%
  dplyr::mutate(SOC_density = OC*WholeCoreBD*SOC_average_depth)

bd_clean <- dplyr::distinct(bd_clean)

names(bd_clean)
bd_clean <- dplyr::select(bd_clean, SoilPlot, District, BD_depth, WTD_BD_field_weight = BD_field_weight,
                          WTD_BD_fresh_weight = BD_fresh_weight, WTD_BD_dry_weight = BD_dry_weight,
                          BDCoreVolume, SoilWaterContent, BD_fullcore_dry_weight = TotalBDDryWeight, 
                          BD_g_cm3 = WholeCoreBD, SOC_density)


### Join tables and write as L2
labdata <- bd_clean %>%
  dplyr::left_join(SOC_clean) %>%
  dplyr::left_join(TXT_clean) %>%
  dplyr::left_join(herbbio_clean) %>%
  dplyr::left_join(woodybio_clean)


write.csv(labdata, "L2/Mafisa2_LabData_L2.csv", row.names = FALSE)





# Create histogram of BD values
bd <- ggplot(bd_clean, aes(x = WholeCoreBD)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Bulk density (g/cm^3)") +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # geom_vline(xintercept = 1.7, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 5, 0.5), limits = c(0, 5))
bd




# Create histogram of BD values
socstocks <- ggplot(bd_clean, aes(x = SOC_density)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("SOC density") +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # geom_vline(xintercept = 1.7, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 300, 20), limits = c(0, 300))
socstocks 




# Join calculated vars to ecosystem types
bd_clean <- dplyr::select(bd_clean, -Class)
bd_clean <- dplyr::left_join(bd_clean, cover)

bd_clean %>%
  dplyr::filter(!is.na(Class)) %>%
  ggplot(aes(x = SOC_density, y = Class)) + 
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none") 

# Boxplots
bd_clean %>%
  dplyr::filter(!is.na(Class)) %>%
  ggplot(aes(x = Class, y = SOC_density, fill = Class)) + 
  geom_boxplot() +
  theme(legend.position = "none")


bd_clean %>%
  dplyr::filter(!is.na(District)) %>%
  ggplot(aes(x = District, y = SOC_density, fill = District)) + 
  geom_boxplot() +
  theme(legend.position = "none")

