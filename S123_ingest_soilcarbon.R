### Ingesting data from S123 output forms

library(tidyverse)
library(readxl)

### Point to data folder
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2/Mafisa 2 Data/Baseline/"
setwd(dir)


# Read in the L1 data
mafisa2_soil_biomass <- excel_sheets("L1/20250721_BaselineMombola_L1.xlsx") # File path
mafisa2_soil_biomass_list <- lapply(mafisa2_soil_biomass, function(x) read_excel("L1/20250721_BaselineMombola_L1.xlsx", sheet = x))  # Read excel file into list of excel sheets
names(mafisa2_soil_biomass_list ) <- mafisa2_soil_biomass # Pull sheet names from the workbook
list2env(mafisa2_soil_biomass_list, envir=.GlobalEnv) # Write each excel sheet to a separate dataframe 

# Create a new dataframe of plot names linked to unique identifier 
# Rename GlobalID to ParentGlobalID - for subsequent tables generated from a single S123 form, ParentGlobalID is used to refer back to the original plot
plotnames <- dplyr::select(Mafisa_2_Soil_and_Biomass_S_0, ParentGlobalID = GlobalID, SoilPlot)

# Clean bulk density weight increment table
bulk_density_clean <- bulk_density_mass_increment_1 %>%
  dplyr::left_join(plotnames) %>% # Join with plot names
  dplyr::select(-BD_wet_mass_increm) %>% # Remove increment weight column 
  dplyr::group_by(SoilPlot) %>% # Group weight increments by plot
  dplyr::slice(which.max(BD_wet_mass_total)) %>% # Take total weight by plot (largest weight)
  dplyr::select(ParentGlobalID, BD_wet_mass_total) %>%
  dplyr::ungroup() 
# QA/QC check
bd_qc <- bulk_density_clean %>%
  dplyr::filter(BD_wet_mass_total > 15000 | # If any rows are above expected range
                  BD_wet_mass_total < 5000 | # If any rows are below expected range
                  is.na(BD_wet_mass_total)) # If any rows contain NA


# Clean SOC1 table
soc1_clean <- soc_sample_one_mass_increme_2 %>%
  dplyr::left_join(plotnames) %>% # Join with plot names
  dplyr::group_by(ParentGlobalID, SoilPlot) %>% # Group weight increments by plot, and include all variables to keep
  dplyr::summarise(SOC1_mass_total = sum(SOC1_mass_total)) %>% # Take sum of all mass increments within plot
  dplyr::ungroup()
# QA/QC check
soc1_qc <- soc1_clean %>%
  dplyr::filter(SOC1_mass_total > 15000 | # If any rows are above expected range
                  SOC1_mass_total < 5000 | # If any rows are below expected range
                  is.na(SOC1_mass_total)) # If any rows contain NA

# Clean SOC2 table
soc2_clean <- soc_sample_two_mass_increme_3 %>%
  dplyr::left_join(plotnames) %>% # Join with plot names
  dplyr::group_by(ParentGlobalID, SoilPlot) %>% # Group weight increments by plot, and include all variables to keep
  dplyr::summarise(SOC2_mass_total = sum(SOC2_mass_total)) %>% # Take sum of all mass increments within plot
  dplyr::ungroup()
# QA/QC check
soc2_qc <- soc2_clean %>%
  dplyr::filter(SOC2_mass_total > 15000 | # If any rows are above expected range
                  SOC2_mass_total < 5000 | # If any rows are below expected range
                  is.na(SOC2_mass_total)) # If any rows contain NA

# Clean densiometer table
densi_clean <- densiometer_reading_4 %>%
  dplyr::left_join(plotnames) %>% # Join with plot names
  dplyr::select(ParentGlobalID, Densi_points)
# QA/QC check
densi_qc <- densi_clean %>%
  dplyr::filter(Densi_points > 100 | # If any rows are above expected range
                  is.na(Densi_points)) # If any rows contain NA
# Find plot mean
densi_clean <- densi_clean %>%
  dplyr::group_by(ParentGlobalID) %>%
  dplyr::summarise(Densi_points = mean(Densi_points))


# Clean DbH table
dbh_clean <- plant_id_dbh_greater_5cm_5 %>%
  dplyr::left_join(plotnames) # Join with plot names
# QA/QC check
dbh_qc <- dbh_clean  %>%
  dplyr::filter(Tree_dbh < 5 | # If any rows are below expected range
                  is.na(Tree_dbh)) # If any dbh rows contain NA
                  # is.na(Tree_ID)) # If any species name rows contain NA - disable this check if not using species IDs
# Remove incorrect data if needed
dbh_clean <- dplyr::filter(dbh_clean, ObjectID != 118 & ObjectID != 1225)


# Clean woody biomass table
woodybio_clean <- woody_biomass_weight_increm_6 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::group_by(ParentGlobalID, SoilPlot) %>% # Group weight increments by plot, and include all variables to keep
  dplyr::summarise(WoodyBio_weight = sum(WoodyBio_weight)) %>% # Take sum of all mass increments within plot
  dplyr::ungroup()
# QA/QC check after join with full dataset to compare 0 readings with Y/N method collected

# Clean woody biomass table
herbbio_clean <- herb_biomass_weight_increme_7 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::group_by(ParentGlobalID, SoilPlot) %>% # Group weight increments by plot, and include all variables to keep
  dplyr::summarise(HerbBio_weight = sum(HerbBio_weight)) %>% # Take sum of all mass increments within plot
  dplyr::ungroup()
# QA/QC check after join with full dataset to compare 0 readings with Y/N method collected


# Combine all cleaned dataframes
Mafisa_2_data_join <- dplyr::rename(Mafisa_2_Soil_and_Biomass_S_0, ParentGlobalID = GlobalID) 

Mafisa_2_data_join <- Mafisa_2_data_join %>%
  dplyr::left_join(bulk_density_clean) %>%
  dplyr::left_join(soc1_clean) %>%
  dplyr::left_join(soc2_clean) %>%
  dplyr::left_join(densi_clean) %>%
  dplyr::left_join(woodybio_clean) %>%
  dplyr::left_join(herbbio_clean)

Mafisa_2_data_join <- dplyr::distinct(Mafisa_2_data_join) # Remove duplicate rows, if created during join




# QA/QC check of full data table
# Add total cover column by summing estimates of plant functional groups and bare ground
Mafisa_2_data_join <- dplyr::mutate(Mafisa_2_data_join, TotalCover = TREE_pct + DRH_pct + SRH_pct + SHRUB_pct + BARE_pct)
# Reorder variables - be sure that this list is correct based on any updates to the S123 form
names(Mafisa_2_data_join)
Mafisa_2_data_join <- dplyr::select(Mafisa_2_data_join, ObjectID:SoilPlot, SoilPlot_alt, LandCover, LandCover_alt, Names:BD_frag_vol, BD_wet_mass_total,
                                    WTD_mass:SOC1_rock_volume, SOC1_mass_total, SOC2_collected:SOC2_rock_volume, SOC2_mass_total,
                                    SOC_combined:Densi_collected, DensiCanopyCover = CanopyCover, Densi_mean = Densi_points, PCT_collected:BARE_pct, TotalCover, # Renaming canopy cover variable
                                    PCT_notes, DBH_collected:WoodyBio_samples, WoodyBio_weight, HerbBio_length:HerbBio_samples, HerbBio_weight, 
                                    Biomass_notes, CreationDate, Creator, EditDate, Editor, x, y)
# Add form version 
Mafisa_2_data_join <- dplyr::mutate(Mafisa_2_data_join, FormVersion = "Mafisa_2 Soil and Biomass Sampling")




# Create method-specific dataframes that are subsets of the whole dataset with potential errors
# Fix errors as appropriate, and document in QC tracking

# Plot characterization errors
plotchar_errors <- Mafisa_2_data_join %>%
  dplyr::select(ObjectID:PlotNotes, x, y) %>%
  dplyr::filter(!is.na(SoilPlot_alt) | # Did a plot name need to be modified or added?
                !is.na(LandCover_alt)| # Was an alternative land cover type identified?
                is.na(x) | # Are x data present?
                is.na(y)) # Are y data present?




# Bulk density QC
bd_errors <- Mafisa_2_data_join %>%
  dplyr::select(ObjectID:BD_notes) %>%
  dplyr::filter(BD_collected == "yes" & if_any(c(BD_frag_vol, BD_depth, BD_wet_mass_total, WTD_mass), ~ is.na(.)) | # Check for missing data - if BD method was collected, the selected columns should not contain NA values
                  BD_wet_mass_total > 17000 | # Upper expected BD value
                  BD_wet_mass_total < 4000 | # Lower expected BD value
                  WTD_mass < 40 | # Lower expected WTD value
                  WTD_mass > 140) # Upper expected WTD value

# If bd_errors returns a dataframe with > 0 observations, check returned plots to see why 
# Document missing data, outliers, etc. and connect with data collection crews if needed
# Document any changes made to correct dataset


# SOC/texture QC
soc_errors <- Mafisa_2_data_join %>%
  dplyr::select(ObjectID:PlotNotes, SOC1_collected:TXT_notes) %>%
  dplyr::filter(SOC1_collected == "yes" & if_any(c(SOC1_depth, SOC1_mass_total), ~ is.na(.)) | # If SOC1 was collected, depth and mass should not be NA
                SOC2_collected == "yes" & if_any(c(SOC2_depth, SOC2_mass_total), ~ is.na(.)) | # If SOC2 was collected, depth and mass should not be NA
                if_any(c(SOC1_mass_total, SOC2_mass_total), ~ . > 17000) | # Upper expected SOC mass value
                if_any(c(SOC1_mass_total, SOC2_mass_total), ~ . < 4000) | # Lower expected SOC mass value
                SOC_combined != "yes" | # If SOC samples were not combined, why?
                TXT_collected != "yes") # If texture sample was not collected, why?


# Rootpit QC
rootpit_qc <- Mafisa_2_data_join %>%
  dplyr::select(ObjectID:PlotNotes, RootPit_collected:RootPit_notes) %>%
  dplyr::filter(RootPit_collected == "yes" & is.na(RootPit_depth)) # If root pit was collected, depth should not be NA
                  


# Percent cover and densiometer QC
pct_qc <- Mafisa_2_data_join %>%
  dplyr::select(ObjectID:PlotNotes, Densi_collected:PCT_notes) %>%
  dplyr::mutate(LowerCanopyCover = DRH_pct + SRH_pct + SHRUB_pct + BARE_pct) %>% # Add a lower canopy column - this should not exceed 100
  dplyr::filter(Densi_collected == "yes" & is.na(DensiCanopyCover) | # If densiometer was used, canopy cover should not be NA
                DensiCanopyCover > 100 | # Densiometer canopy cover should not exceed 100
                PCT_collected == "yes" & if_any(c(TREE_pct, DRH_pct, SRH_pct, SHRUB_pct, BARE_pct), ~ is.na(.)) | # If percent cover was estimated, no funtional group should be NA
                TotalCover < 100 | # Total cover should not be below 100
                TotalCover > 200 | # Total cover should not exceed 200
                LowerCanopyCover > 100) # Lower canopy cover should not exceed 100, except perhaps in rare situations where shrub cover is high and shrubs have an unusual shape allowing for beneath shrub cover to be estimated
  

# Biomass QC
bio_qc <- Mafisa_2_data_join %>%
  dplyr::select(ObjectID:PlotNotes, Biomass_collected:Biomass_notes) %>%
  dplyr::filter(Biomass_collected == "yes" & if_any(c(WoodyBio_length, WoodyBio_samples, WoodyBio_weight,
                                                      HerbBio_length, HerbBio_samples, HerbBio_weight), ~ is.na(.)) | # If biomass was collected, transect length, sample number, and weights should not be NA
                WoodyBio_weight > 2000 | # Biomass weight should not exceed 2000
                HerbBio_weight > 2000 ) # Biomass weight should not exceed 2000
  




# Join baseline data collected in outdated S123 form (already QC'd to L2) to current data table
soilbiomass_l2 <- read.csv("L2/Baseline_Mafisa2_SoilBiomass_L2.csv")
names(soilbiomass_l2) # Check columns in soilbiomass table
names(Mafisa_2_data_join) # Check columns in working table
# Add variables to soilbiomass table so that tables can be joined
soilbiomass_l2$SoilPlot_alt <- NA
soilbiomass_l2$LandCover <- NA
soilbiomass_l2$LandCover_alt <- NA
soilbiomass_l2$SOC1_rock_volume <- NA
soilbiomass_l2$SOC2_rock_volume <- NA
# Check date structure
str(soilbiomass_l2)

# Rename/reorder variables from soilbiomass table so that it can be joined to current working table
soilbiomass_l2 <- dplyr::select(soilbiomass_l2, ObjectID:SoilPlot, SoilPlot_alt, LandCover, LandCover_alt,
                                Names, PlotNotes,
                                BD_collected, BD_depth, BD_frag_vol, BD_wet_mass_total:SOC1_depth,
                                SOC1_rock_volume, SOC1_mass_total:SOC2_depth, SOC2_rock_volume, SOC2_mass_total:FormVersion)
Mafisa_2_data_join <- dplyr::select(Mafisa_2_data_join, -Densi_mean)
# Do columns match?
names(soilbiomass_l2)
str(soilbiomass_l2)
names(Mafisa_2_data_join)
str(Mafisa_2_data_join)
# Change working table dates to character
Mafisa_2_data_join$SurveyDate <- as.character(Mafisa_2_data_join$SurveyDate)
Mafisa_2_data_join$EditDate <- as.character(Mafisa_2_data_join$EditDate)
Mafisa_2_data_join$CreationDate <- as.character(Mafisa_2_data_join$CreationDate)

# Join tables
Mafisa_2_data_join <- rbind(Mafisa_2_data_join, soilbiomass_l2)

# Save QC'd data 
write.csv(Mafisa_2_data_join, "L2/Baseline_Mafisa2_SoilBiomass_L2_07212025.csv", row.names = FALSE)


