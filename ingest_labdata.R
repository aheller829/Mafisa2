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
labdata <- readxl::excel_sheets("L1/20250519_LabSamples_L1.xlsx") # File path
labdata_list <- lapply(labdata, function(x) read_excel("L1/20250519_LabSamples_L1.xlsx", sheet = x))  # Read excel file into list of excel sheets
names(labdata_list) <- labdata # Pull sheet names
list2env(labdata_list, envir=.GlobalEnv) # Write each excel sheet to a separate dataframe 


# Bring in field data
soilbiomass <- read.csv("L2/Mafisa2_SoilBiomass_L2.csv")
cover <- read.csv("L1/plot_cover_photos.csv")
cover <- dplyr::select(cover, SoilPlot = Plot.Name, Class = Class.1)





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
# write.csv(SOC_clean, "L2/Mafisa2_OrganicCarbon_L2.csv", row.names = FALSE)


# Create histogram of C values
c <- ggplot(SOC_clean, aes(x = OC)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Soil organic carbon (%)") +
  scale_x_continuous(breaks = seq(0, 2, 0.1), limits = c(0, 2)) 
c


# Boxplots
soc_plot <- dplyr::left_join(SOC_clean, cover)

soc_plot %>%
  dplyr::mutate(Class = ifelse(Class == "Grassland", "Dambo", Class)) %>%
 dplyr::filter(!is.na(Class)) %>%
  ggplot(aes(x = Class, y = OC, fill = Class)) + 
  geom_boxplot() +
  theme(legend.position = "none")





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
# write.csv(TXT_clean, "L2/Mafisa2_SoilTexture_L2.csv", row.names = FALSE)


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
  dplyr::filter(District == "Kanguya") %>%
  ggsoiltexture::ggsoiltexture(class = "USDA") +
    geom_point(aes(color = OC, size = OC)) +
    scale_color_continuous(type = "viridis", direction = -1) +
    scale_size_continuous(range = c(1, 5), guide = "none") +
    labs(color = "organic carbon (%)") +
    # ggrepel::geom_label_repel(aes(label = Class), box.padding = 0.5, max.overlaps = 40) +
    theme(legend.title = element_text(face = "bold"),
          legend.position = "bottom") +
  ggtitle("Kanguya (Luampa/BC 4)")

  

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
# Four plots have mismatched weights: LU08, NLO08, NLO19, SS13

# Replace missing wtd weights and depths from soilbiomass field data
names(fieldbd)
bd_clean <- fieldbd %>%
  dplyr::mutate(BD_field_weight = ifelse(is.na(BD_field_weight), WTD_mass, BD_field_weight)) %>%
  dplyr::select(SoilPlot, District, BD_depth, BD_field_weight:BD_dry_weight) 

bd_clean <- dplyr::distinct(bd_clean)


# write.csv(bd_clean, "L2/Mafisa2_BulkDensity_L2.csv", row.names = FALSE)

names(bd_clean)
# Create histogram of BD values
bd <- ggplot(bd_clean, aes(x = BD_dry_weight)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("BD dry weight (g)") +
 # geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
 # geom_vline(xintercept = 1.7, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(100, 600, 20), limits = c(100, 600)) 
bd




### Clean herb bio table
names(BiomassHerbaceous)
names(soilbiomass)
# Cross reference values with field data
herbbio_clean <- BiomassHerbaceous %>%
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
# write.csv(herbbio_clean, "L2/Mafisa2_HerbaceousBiomass_L2.csv", row.names = FALSE)









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
# write.csv(woodybio_clean, "L2/Mafisa2_WoodyBiomass_L2.csv", row.names = FALSE)





# Derive bulk density
names(soilbiomass)
fullbd <- dplyr::select(soilbiomass, SoilPlot, BD_wet_mass_total, BD_frag_vol) # Extract weights for full BD cores
fullbd <- dplyr::mutate(fullbd, BD_frag_vol = ifelse(is.na(BD_frag_vol), 0, BD_frag_vol))

names(bd_clean)
names(fullbd)

# Calculate radius of auger based on volume
sqrt(7300/(100*pi))


bd_clean <- bd_clean %>%
  dplyr::left_join(fullbd) %>% # Join full core weights to lab bd
  dplyr::mutate(TotalBDDryWeight = (BD_dry_weight/BD_field_weight)*BD_wet_mass_total) %>% # Calculate dry weight of full core
  dplyr::mutate(BDCoreVolume = 4.82^2*pi*BD_depth) %>% # Add core volume in cm ^ 3
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
                          BDCoreVolume, BD_fullcore_dry_weight = TotalBDDryWeight, 
                          BD_g_cm3 = WholeCoreBD, SOC_density)



bd_compare <- labdata_ogSWC %>%
  dplyr::select(SoilPlot, BD_fullcore_dry_weight_OG = BD_fullcore_dry_weight, BD_g_cm3_OG = BD_g_cm3, SOC_density_OG = SOC_density) %>%
  dplyr::left_join(bd_clean) %>%
  dplyr::select(SoilPlot, BD_fullcore_dry_weight_OG, BD_fullcore_dry_weight, BD_g_cm3_OG, BD_g_cm3, SOC_density_OG, SOC_density)


# Standardize biomass
names(herbbio_clean)
# Recalculate weights as sums rather than averages
freshweights <- herbbio_clean %>%
  dplyr::select(SoilPlot, District, HerbLabFreshWeight1, 
                HerbLabFreshWeight2, HerbLabFreshWeight3, HerbLabFreshWeight4) %>%
  tidyr::gather(Instance, Weight, HerbLabFreshWeight1:HerbLabFreshWeight4) %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(FreshWeightSum = sum(Weight, na.rm = TRUE)) 

dryweights <- herbbio_clean %>%
  dplyr::select(SoilPlot, District, HerbLabDryWeight1, 
                HerbLabDryWeight2, HerbLabDryWeight3, HerbLabDryWeight4) %>%
  tidyr::gather(Instance, Weight, HerbLabDryWeight1:HerbLabDryWeight4) %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(DryWeightSum = sum(Weight, na.rm = TRUE)) 

joinedweights <- dplyr::left_join(freshweights, dryweights)
# Calculate moisture content
joinedweights$HerbMoistureContent <- ((joinedweights$FreshWeightSum-joinedweights$DryWeightSum)/joinedweights$DryWeightSum)*100
# Calculate percent dry matter
joinedweights$HerbDryMatterPct <- (joinedweights$DryWeightSum/joinedweights$FreshWeightSum)*100
# Rename
herbjoinedweights <- dplyr::select(joinedweights, SoilPlot, HerbFreshWeightSum = FreshWeightSum, HerbDryWeightSum = DryWeightSum, 
                                   HerbMoistureContent, HerbDryMatterPct)
# Repeat for woody
freshweights <- woodybio_clean %>%
  dplyr::select(SoilPlot, District, WoodyLabFreshWeight1, 
                WoodyLabFreshWeight2, WoodyLabFreshWeight3, WoodyLabFreshWeight4) %>%
  tidyr::gather(Instance, Weight, WoodyLabFreshWeight1:WoodyLabFreshWeight4) %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(FreshWeightSum = sum(Weight, na.rm = TRUE)) 

dryweights <- woodybio_clean %>%
  dplyr::select(SoilPlot, District, WoodyLabDryWeight1, 
                WoodyLabDryWeight2, WoodyLabDryWeight3, WoodyLabDryWeight4) %>%
  tidyr::gather(Instance, Weight, WoodyLabDryWeight1:WoodyLabDryWeight4) %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(DryWeightSum = sum(Weight, na.rm = TRUE)) 

joinedweights <- dplyr::left_join(freshweights, dryweights)
# Calculate moisture content
joinedweights$WoodyMoistureContent <- ((joinedweights$FreshWeightSum-joinedweights$DryWeightSum)/joinedweights$DryWeightSum)*100
# Calculate percent dry matter
joinedweights$WoodyDryMatterPct <- (joinedweights$DryWeightSum/joinedweights$FreshWeightSum)*100
# Rename
woodyjoinedweights <- dplyr::select(joinedweights, SoilPlot, WoodyFreshWeightSum = FreshWeightSum, WoodyDryWeightSum = DryWeightSum, 
                                    WoodyMoistureContent, WoodyDryMatterPct)
# Join herb and woody
joinedweights <- dplyr::left_join(herbjoinedweights, woodyjoinedweights)
# Replace woody NA with 0
joinedweights <- dplyr::mutate_all(joinedweights, ~replace(., is.na(.), 0))

# Area conversions
# First add transect lengths
tlength <- dplyr::select(soilbiomass, SoilPlot, WoodyBio_length, HerbBio_length)
# Convert to m area 
tlength <- dplyr::mutate(tlength, 
                         WoodyArea_m = WoodyBio_length*0.1,
                         HerbArea_m = HerbBio_length*0.1)

joinedweights <- dplyr::left_join(joinedweights, tlength)

# Divide fractional areas by fraction to upscale to 1 m
names(joinedweights)
joinedweights <- dplyr::mutate(joinedweights, HerbDryWeight_m = HerbDryWeightSum/HerbArea_m,
                               WoodyDryWeight_m = WoodyDryWeightSum/WoodyArea_m)

joinedweights <- dplyr::select(joinedweights, SoilPlot, HerbFreshWeightSum, HerbMoistureContent, HerbDryMatterPct,
                               WoodyFreshWeightSum, WoodyMoistureContent, WoodyDryMatterPct, HerbBio_length, WoodyBio_length,
                               HerbArea_m, WoodyArea_m, HerbDryWeightSum, HerbDryWeight_m, WoodyDryWeightSum, WoodyDryWeight_m)

joinedweights <- dplyr::mutate(joinedweights, HerbDryWeight_kg_ha = HerbDryWeight_m*10,
                               WoodyDryWeight_kg_ha = WoodyDryWeight_m*10)

# Add in tons per hectare
joinedweights <- dplyr::mutate(joinedweights, HerbDryWeight_t_ha = HerbDryWeight_kg_ha/1000,
                               WoodyDryWeight_t_ha = WoodyDryWeight_kg_ha/1000)



# Save to csv
write.csv(joinedweights, "L2/Mafisa2_Biomass_recalc.csv", row.names = FALSE)


### Join tables and write as L2
# RM district where present, so tables are only joining on SoilPlot
names(SOC_clean)
SOC_clean <- dplyr::select(SOC_clean, -District, -Date)
names(TXT_clean)
names(herbbio_clean)
herbbio_clean <- dplyr::select(herbbio_clean, -District)
names(woodybio_clean)
woodybio_clean <- dplyr::select(woodybio_clean, -District)

labdata <- bd_clean %>%
  dplyr::left_join(SOC_clean) %>%
  dplyr::left_join(TXT_clean) %>%
  dplyr::left_join(herbbio_clean) %>%
  dplyr::left_join(woodybio_clean)
labdata <- labdata %>%
  dplyr::mutate(
                District = ifelse(str_detect(SoilPlot, "LU[:digit:]"), "Kanguya",
                                  ifelse(str_detect(SoilPlot, "NLO[:digit:]"), "Mombola",
                                         ifelse(str_detect(SoilPlot, "SSN[:digit:]"), "Senanga",
                                                ifelse(str_detect(SoilPlot, "SS[:digit:]"), "Mutala", NA))))) # Add district name



write.csv(labdata, "L3/Mafisa2_LabData_joined_L3.csv", row.names = FALSE)




names(bd_clean)
# Create histogram of BD values
bd <- ggplot(bd_clean, aes(x = BD_g_cm3)) + 
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



# Label SOC density outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

dat <- bd_clean %>% 
  dplyr::filter(!is.na(SOC_density)) %>%
  tibble::rownames_to_column(var = "outlier") %>% 
  dplyr::group_by(District) %>% 
  dplyr::mutate(is_outlier = ifelse(is_outlier(SOC_density), SOC_density, NA)) 

dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)

dat <- dplyr::mutate(dat, outlier = ifelse(!is.na(outlier), SoilPlot, NA))

ggplot(dat, aes(y = SOC_density, x = District, fill = District)) + 
         geom_boxplot() + 
         geom_text(aes(label = outlier), na.rm = TRUE, nudge_y = 6)



# BD outliers, labeled
names(bd_clean)
dat <- bd_clean %>% 
  dplyr::filter(!is.na(BD_g_cm3)) %>%
  tibble::rownames_to_column(var = "outlier") %>% 
  dplyr::mutate(is_outlier = ifelse(is_outlier(BD_g_cm3), BD_g_cm3, NA)) 

dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)

dat <- dplyr::mutate(dat, outlier = ifelse(!is.na(outlier), SoilPlot, NA))

ggplot(dat, aes(y = BD_g_cm3, x = "")) + 
  geom_boxplot() + 
  geom_text(aes(label = outlier), na.rm = TRUE, nudge_y = 0.05) + 
  xlab("") + ggtitle("Bulk density across all plots")




# OC percent, labeled
dat <- labdata %>% 
  dplyr::filter(!is.na(OC)) %>%
  dplyr::group_by(TXT_class) %>%
  tibble::rownames_to_column(var = "outlier") %>% 
  dplyr::mutate(is_outlier = ifelse(is_outlier(OC), OC, NA)) 

dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)

dat <- dplyr::mutate(dat, outlier = ifelse(!is.na(outlier), SoilPlot, NA))

ggplot(dat, aes(y = OC, x = TXT_class, fill = TXT_class)) + 
  geom_boxplot() + 
  xlab("") + ggtitle("OC (%) across texture classes") +
  geom_text(aes(label = outlier), na.rm = TRUE, nudge_y = 0.05) 


labdata %>%
  ggplot(aes(x = TXT_class, y = OC, fill = TXT_class)) + 
  geom_boxplot()




