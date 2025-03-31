### Standardizing data from the original Mafisa_2 Soil Sampling S123 form so it can be joined with the updated Mafisa_2 Soil and Biomass form


# Load libraries
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


# Structure data table so it can be joined with Mafisa_2_Soil and Biomass data
print(og_mafisa2_soil) # Show dataframe names (one dataframe per excel sheet)
# bulk_density_weight_1 is an empty sheet - bulk density weights are recorded in Mafisa_2_Soil_Sampling
# Standardize column names, adding columns as needed 
og_mafisa2_soil_clean <- Mafisa_2_Soil_Sampling %>%
  dplyr::mutate(Names = NA, PlotNotes = NA, RootPit_collected = "no", RootPit_depth = NA, RootPit_notes = NA,
                Densi_collected = "yes", DensiCanopyCover = TREE_pct, PCT_collected = "yes", 
                TotalCover = TREE_pct + DRH_pct + SRH_pct + SHRUB_pct + BARE_pct, Biomass_collected = "yes",
                DBH_collected = "yes", Sapling_collected = ifelse(Sapling_count == 0, "yes", "no"), WoodyBio_samples = NA, 
                HerbBio_samples = NA, FormVersion = "Mafisa_2 Soil Sampling") %>%
  dplyr::select(ObjectID, ParentGlobalID = GlobalID, SurveyDate, SoilPlot, Names, PlotNotes, BD_collected, BD_frag_vol, BD_depth, BD_wet_mass_total,
                WTD_mass, BD_notes, SOC1_collected, SOC1_depth, SOC1_mass_total, SOC2_collected, SOC2_depth, SOC2_mass_total, SOC_combined,
                SOC_notes, TXT_collected, TXT_notes, RootPit_collected, RootPit_depth, RootPit_notes, Densi_collected, DensiCanopyCover, PCT_collected,
                TREE_pct, DRH_pct, SRH_pct, SHRUB_pct, BARE_pct, TotalCover, PCT_notes, DBH_collected, DBH_radius, DBH_notes, Sapling_collected,
                Sapling_count, Biomass_collected, WoodyBio_length, WoodyBio_samples, WoodyBio_weight, HerbBio_length, HerbBio_samples, 
                HerbBio_weight, Biomass_notes, CreationDate, Creator, EditDate, Editor, x, y, FormVersion) 

# Check updated column names
names(og_mafisa2_soil_clean)




### QC DBH
# Add plot names
og_plotnames <- dplyr::select(og_mafisa2_soil_clean, ParentGlobalID, SoilPlot)
# Join plot names with dbh data
og_dbh_clean <- plant_id_dbh_greater_5cm_2 %>%
  dplyr::left_join(og_plotnames)
  


# Write soil and dbh data tables to csv
write.csv(og_mafisa2_soil_clean, "ZambiaSoilC_L1.1.csv", row.names = FALSE)
write.csv(og_dbh_clean, "ZambiaSoilC_dbh_L1.1.csv", row.names = FALSE)


### dbh data merge
# Incorporate edits from Kondwani into data tables, and merge old and new S123 forms
dbh_edits <- read.csv("dbh_join_KondwaniEdits.csv")

# Get list of plot names linked to global ids



