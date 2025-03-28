### Ingesting data from S123 output forms
# Consolidating data from the original Mafisa_2 Soil Sampling S123 form and the updated Mafisa_2 Soil and Biomass form

library(tidyverse)
library(readxl)

### Point to data folder
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/L1/"
setwd(dir)

### Read in L1 data with updated variable names
# Mafisa_2 Soil Sampling data
og_mafisa2_soil <- excel_sheets("20250306_ZambiaSoilC_L1.xlsx") # File path
og_mafisa2_soil_list <- lapply(og_mafisa2_soil, function(x) read_excel("20250306_ZambiaSoilC_L1.xlsx", sheet = x)) # Read excel file into list of excel sheets
names(og_mafisa2_soil_list) <- og_mafisa2_soil # Pull sheet names
list2env(og_mafisa2_soil_list, envir=.GlobalEnv) # Write each excel sheet to a separate dataframe 

# Mafisa_2 Soil and Biomass data
mafisa2_soil_biomass <- excel_sheets("20250327_ZambiaSoilBiomass_L1.xlsx") # File path
mafisa2_soil_biomass_list <- lapply(mafisa2_soil_biomass, function(x) read_excel("20250327_ZambiaSoilBiomass_L1.xlsx", sheet = x))  # Read excel file into list of excel sheets
names(mafisa2_soil_biomass_list ) <- mafisa2_soil_biomass # Pull sheet names
list2env(mafisa2_soil_biomass_list, envir=.GlobalEnv) # Write each excel sheet to a separate dataframe 



### Merge data across S123 form versions
# Create a new dataframe of plot names linked to unique identifier (renamed ParentGlobalID)
names(Mafisa_2_Soil_and_Biomass_S_0)
plotnames <- dplyr::select(Mafisa_2_Soil_and_Biomass_S_0, ParentGlobalID = GlobalID, SoilPlot)

# Clean bulk density weight increment table
bulk_density_clean <- bulk_density_mass_increment_1 %>%
  dplyr::left_join(plotnames) %>% # Join with plot names
  dplyr::select(-BD_wet_mass_increm) %>% # Remove increment weight column 
  dplyr::group_by(SoilPlot) %>% # Group weight increments by plot
  dplyr::slice(which.max(BD_wet_mass_total)) %>% # Take total weight by plot (largest weight)
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


# Clean DbH table
dbh_clean <- plant_id_dbh_greater_5cm_5 %>%
  dplyr::left_join(plotnames) # Join with plot names
# QA/QC check
dbh_clean <- dbh_clean  %>%
  dplyr::filter(Tree_dbh < 5 | # If any rows are below expected range
                  is.na(Tree_dbh) | # If any dbh rows contain NA
                  is.na(Tree_ID)) # If any species name rows contain NA


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
Mafisa_2_data_join <- Mafisa_2_Soil_and_Biomass_S_0 %>%
  dplyr::rename(ParentGlobalID = GlobalID) %>%
  dplyr::left_join(bulk_density_clean) %>%
  dplyr::left_join(soc1_clean) %>%
  dplyr::left_join(soc2_clean) %>%
  dplyr::left_join(densi_clean) %>%
  dplyr::left_join(woodybio_clean) %>%
  dplyr::left_join(herbbio_clean)

# QA/QC check of full data table
# Reorder variables
names(Mafisa_2_data_join)
Mafisa_2_data_join <- dplyr::select(Mafisa_2_data_join, ObjectID:BD_frag_vol, BD_depth, BD_wet_mass_total,
                                    WTD_mass:SOC1_depth, SOC1_mass_total, SOC2_collected:SOC2_depth, SOC2_mass_total,
                                    SOC_combined:Densi_collected, Densi_points, CanopyCover:WoodyBio_samples, WoodyBio_weight,
                                    HerbBio_length:HerbBio_samples, HerbBio_weight, Biomass_notes, CreationDate, Creator, EditDate,
                                    Editor, x, y)



# First combine old S123 data sheets into single dataframe
print(og_mafisa2_soil) # Show dataframe names (one dataframe per excel sheet)
# Rename unique identifier columns so that dataframes can be joined
Mafisa_2_Soil_Sampling_0 <- dplyr::rename(Mafisa_2_Soil_Sampling_0, ParentGlobalID = GlobalID)
# bulk_density_weight_1 is an empty sheet - bulk density weights are recorded in 

# Compare column names between Soil Sampling and Soil and Biomass survey forms
og_cols <- names(og_Mafisa_2_Soil_Sampling_0) # Old survey
survey_cols <- names(Mafisa_2_Soil_and_Biomass_Survey_0) # New survey
setdiff(og_cols, survey_cols) # Shows columns present in og_cols, not in survey_cols
# Many of these columns are the same but use different cases
# Change column names to all lowercase
names(Mafisa_2_Soil_and_Biomass_Survey_0) <- base::tolower(names(Mafisa_2_Soil_and_Biomass_Survey_0))
names(og_Mafisa_2_Soil_Sampling_0) <- base::tolower(names(og_Mafisa_2_Soil_Sampling_0))
# Repeat column name comparison
og_cols <- names(og_Mafisa_2_Soil_Sampling_0) # Old survey
survey_cols <- names(Mafisa_2_Soil_and_Biomass_Survey_0) # New survey
setdiff(og_cols, survey_cols) # Shows columns present in og_cols, not in survey_cols




### dbh data merge
# Incorporate edits from Kondwani into data tables, and merge old and new S123 forms
dbh_edits <- read.csv("dbh_join_KondwaniEdits.csv")

# Get list of plot names linked to global ids



