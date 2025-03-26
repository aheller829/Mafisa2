### Ingesting data from S123 output forms
# Consolidating data from the original Mafisa_2 Soil Sampling S123 form and the updated Mafisa_2 Soil and Biomass form

library(tidyverse)

### Point to data folder
dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data"
setwd(dir)

### Read in data 
# Mafisa_2 Soil Sampling data
og_bulk_density_weight_1 <- read.csv("Mafisa_2 Soil Sampling/bulk_density_weight_1.csv") # Empty form
og_Mafisa_2_Soil_Sampling_0 <- read.csv("Mafisa_2 Soil Sampling/Mafisa_2_Soil_Sampling_0.csv")
og_plant_id_dbh_greater_5cm_2 <- read.csv("Mafisa_2 Soil Sampling/plant_id_dbh_greater_5cm_2.csv")

# Mafisa_2 Soil and Biomass data
bulk_density_mass_increments_1 <- read.csv("Mafisa_2 Soil and Biomass/bulk_density_mass_increments_1.csv")
densiometer_reading_4 <- read.csv("Mafisa_2 Soil and Biomass/densiometer_reading_4.csv")
herb_biomass_weight_increments_7 <- read.csv("Mafisa_2 Soil and Biomass/herb_biomass_weight_increments_7.csv")
Mafisa_2_Soil_and_Biomass_Survey_0 <- read.csv("Mafisa_2 Soil and Biomass/Mafisa_2_Soil_and_Biomass_Survey_0.csv")
plant_id_dbh_greater_5cm_5 <- read.csv("Mafisa_2 Soil and Biomass/plant_id_dbh_greater_5cm_5.csv")
soc_sample_one_mass_increments_2 <- read.csv("Mafisa_2 Soil and Biomass/soc_sample_one_mass_increments_2.csv")
soc_sample_two_mass_increments_3 <- read.csv("Mafisa_2 Soil and Biomass/soc_sample_two_mass_increments_3.csv")
woody_biomass_weight_increments_6 <- read.csv("Mafisa_2 Soil and Biomass/woody_biomass_weight_increments_6.csv")

### Merge data across S123 form versions
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



