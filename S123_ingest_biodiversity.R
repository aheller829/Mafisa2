### Ingesting data from biodiversity S123 output forms

library(tidyverse)
library(readxl)

### Point to data folder

dir <- "C:/Users/allie.heller/OneDrive - Biodiversity Research Institute/Desktop/Mafisa 2 Data/"
setwd(dir)

# Mafisa 2 plot characterization data
plotchar <- excel_sheets("L1/20250418_PlotCharacterization_BD_L1.xlsx") # File path
plotchar_list <- lapply(plotchar, function(x) read_excel("L1/20250418_PlotCharacterization_BD_L1.xlsx", sheet = x))  # Read excel file into list of excel sheets
names(plotchar_list) <- plotchar # Pull sheet names
list2env(plotchar_list, envir = .GlobalEnv) # Write each excel sheet to a separate dataframe 


# Mafisa 2 veg 2x2 data
veg2x2 <- excel_sheets("L1/20250418_Veg2x2_BD_L1.xlsx") # File path
veg2x2_list <- lapply(veg2x2, function(x) read_excel("L1/20250418_Veg2x2_BD_L1.xlsx", sheet = x))  # Read excel file into list of excel sheets
names(veg2x2_list) <- veg2x2 # Pull sheet names
list2env(veg2x2_list, envir = .GlobalEnv) # Write each excel sheet to a separate dataframe 


# Mafisa 2 veg 2x2 data
gc <- excel_sheets("L1/20250418_GroundCover_BD_L1.xlsx") # File path
gc_list <- lapply(gc, function(x) read_excel("L1/20250418_GroundCover_BD_L1.xlsx", sheet = x))  # Read excel file into list of excel sheets
names(gc_list) <- gc # Pull sheet names
list2env(gc_list, envir = .GlobalEnv) # Write each excel sheet to a separate dataframe 



# Check variable names
names(MAFISA_2_General_Site_BD_Fo_0)

