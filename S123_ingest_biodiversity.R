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
# Produces MAFISA_2_General_Site_BD_Fo_0

# Mafisa 2 veg 2x2 data
veg2x2 <- excel_sheets("L1/20250418_Veg2x2_BD_L1.xlsx") # File path
veg2x2_list <- lapply(veg2x2, function(x) read_excel("L1/20250418_Veg2x2_BD_L1.xlsx", sheet = x))  # Read excel file into list of excel sheets
names(veg2x2_list) <- veg2x2 # Pull sheet names
list2env(veg2x2_list, envir = .GlobalEnv) # Write each excel sheet to a separate dataframe 
# Produces MAFISA_2_2x2_Veg_Plot_0, and plantlist dataframes 1-6

# Mafisa 2 ground cover data
gc <- excel_sheets("L1/20250418_GroundCover_BD_L1.xlsx") # File path
gc_list <- lapply(gc, function(x) read_excel("L1/20250418_GroundCover_BD_L1.xlsx", sheet = x))  # Read excel file into list of excel sheets
names(gc_list) <- gc # Pull sheet names
list2env(gc_list, envir = .GlobalEnv) # Write each excel sheet to a separate dataframe 
# Produces MAFISA_2_Ground_Cover_0, distance_1


# Check variable names
names(MAFISA_2_General_Site_BD_Fo_0)

### General Site BD ingestion
# General site BD QA/QC by variable group - enter missing data into QC tracking sheet
names(MAFISA_2_General_Site_BD_Fo_0)

equipment_qc <- MAFISA_2_General_Site_BD_Fo_0 %>%
  dplyr::filter(ARU == "Yes" & is.na(ARU_number) | # If ARUs were deployed but number is missing
                  Cameras == "Yes" & is.na(Camera_number)) # If cameras were deployed but number is missing

slope_qc <- dplyr::filter(MAFISA_2_General_Site_BD_Fo_0,
                  SlopeTaken == "Yes (flat ground)" & is.na(Slope))  # If slope method waas used but slope is missing

structure_qc <- dplyr::filter(MAFISA_2_General_Site_BD_Fo_0,
                  ShrubStructure != "None" & is.na(CommonShrubs), # If there are shrubs present but common species are not listed
                  TreeStructure != "None" & is.na(CommonTrees)) # If there are trees present but common species are not listed

animals_qc <- dplyr::filter(MAFISA_2_General_Site_BD_Fo_0,
                  GrazingEvidence != "Not at All" & is.na(GrazerSpecies), # If there is evidence of grazing but species are not listed
                  BrowsingEvidence != "Not at All" & is.na(BrowsingEvidence), # If there is evidence of browsing but species are not listed
                  AnimalsPresent == "Yes" & is.na(AnimalSpecies)) # Animals were present but species are not listed

featuredistance_qc <- dplyr::filter(MAFISA_2_General_Site_BD_Fo_0,
                  if_any(c(TempWaterDistance, PermWaterDistance, RecentSettlementDistance, RelictSettlementDistance), ~ is.na(.))) # Distance to features is missing



# Find where plots have mulitple rows
plot_qc <- MAFISA_2_General_Site_BD_Fo_0 %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::filter(count > 1) 
doubleplots <- subset(MAFISA_2_General_Site_BD_Fo_0, MAFISA_2_General_Site_BD_Fo_0$SoilPlot %in% plot_qc$SoilPlot)








### Veg 2x2 ingestion
# Veg 2x2 data organization - add unique identifier, split plant species list strings, add cover category
plotnames <- dplyr::select(MAFISA_2_2x2_Veg_Plot_0, ParentGlobalID = GlobalID, SoilPlot)
plant0_5 <- plantlist_0_5_6 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, PlantName) %>%
  dplyr::mutate(PlantName = strsplit(as.character(PlantName), ",")) %>%
  unnest(PlantName) %>%
  dplyr::mutate(Cover = "0_5") 
plant0_5$PlantName <- trimws(plant0_5$PlantName)

plant5_25 <- plantlist_5_25_5 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, PlantName) %>%
  dplyr::mutate(PlantName = strsplit(as.character(PlantName), ",")) %>%
  unnest(PlantName) %>%
  dplyr::mutate(Cover = "5_25") 
plant5_25$PlantName <- trimws(plant5_25$PlantName)

plant25_50 <- plantlist_25_50_4 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, PlantName) %>%
  dplyr::mutate(PlantName = strsplit(as.character(PlantName), ",")) %>%
  unnest(PlantName) %>%
  dplyr::mutate(Cover = "25_50") 
plant25_50$PlantName <- trimws(plant25_50$PlantName)

plant50_75 <- plantlist_50_75_3 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, PlantName) %>%
  dplyr::mutate(PlantName = strsplit(as.character(PlantName), ",")) %>%
  unnest(PlantName) %>%
  dplyr::mutate(Cover = "50_75") 
plant50_75$PlantName <- trimws(plant50_75$PlantName)


plant75_95 <- plantlist_75_95_2 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, PlantName) %>%
  dplyr::mutate(PlantName = strsplit(as.character(PlantName), ",")) %>%
  unnest(PlantName) %>%
  dplyr::mutate(Cover = "75_95") 
plant75_95$PlantName <- trimws(plant75_95$PlantName)

plant95_100 <- plantlist_95_100_1 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, PlantName) %>%
  dplyr::mutate(PlantName = strsplit(as.character(PlantName), ",")) %>%
  unnest(PlantName) %>%
  dplyr::mutate(Cover = "95_100") 
plant95_100$PlantName <- trimws(plant95_100$PlantName)


# rbind all plant data
plantcover <- rbind(plant0_5, plant5_25)
plantcover <- rbind(plantcover, plant25_50)
plantcover <- rbind(plantcover, plant50_75)
plantcover <- rbind(plantcover, plant75_95)
plantcover <- rbind(plantcover, plant95_100)


# Remove parentglobal ID and remove plot level duplicates
plantcover <- plantcover %>%
  dplyr::select(-ParentGlobalID, -ObjectID) %>%
  dplyr::distinct()
# Write list of plant species to csv to fix spelling mistakes
# plantnames <- plantcover %>%
 #  dplyr::select(-SoilPlot, -Cover) %>%
 #  dplyr::distinct()
# write.csv(plantnames, "L1/plantnames.csv", row.names = FALSE)
# Read in edited plant names and join to table
plantnames <- read.csv("L1/plantnames.csv")
plantcover <- plantcover %>%
  dplyr::left_join(plantnames) %>%
  dplyr::select(SoilPlot, PlantName = PlantNameEdited, FG)

# QC check - how many transects were recorded per plot?
veg_transect_qc <- MAFISA_2_2x2_Veg_Plot_0 %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(transect_count = n()) # Wide range of transects, 1-9
# Visualize transects per plot with histogram
ggplot(veg_transect_qc, aes(x = transect_count)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Number of transects per plot") +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(1, 10))


# Add cover class quantitative midpoint
unique(plantcover$Cover)
plantcover <- dplyr::mutate(plantcover, CoverEst = ifelse(Cover == "0_5", 2.5,
                                                    ifelse(Cover == "5_25", 15,
                                                    ifelse(Cover == "25_50", 37.5,
                                                    ifelse(Cover == "50_75", 62.5,
                                                    ifelse(Cover == "75-95", 85,
                                                    ifelse(Cover == "95_100", 97.5, NA)))))))
# Create a wide table with quantitative cover estimates averaged by species per plot
cover_wide <- plantcover %>%
  dplyr::group_by(SoilPlot, PlantName) %>%
  dplyr::summarise(CoverAverage = mean(CoverEst)) %>%
  dplyr::ungroup() %>%
  tidyr::spread(key = PlantName, value = CoverAverage, fill = 0) %>%
  dplyr::select(-V1)

# Calculate wide table by functional group, eliminating species records that couldn't be matched
fg_cover_wide <- plantcover %>%
  dplyr::left_join(plantnames)%>%
  dplyr::select(SoilPlot, FG, CoverEst) %>%
  dplyr::filter(!is.na(FG) & FG != "" & FG != " ") %>%
  dplyr::group_by(SoilPlot, FG) %>%
  dplyr::summarize(FG_avg = mean(CoverEst)) %>%
  dplyr::mutate_if(is.numeric, round, 1)%>%
  dplyr::ungroup() %>%
  tidyr::spread(key = FG, value = FG_avg, fill = 0)


# Take average total foliar cover by calculating mean of total cover record
totalfoliar <- MAFISA_2_2x2_Veg_Plot_0 %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(TotalFoliar = mean(TotalCover))

# Calculate species richness from plant cover
speciesrichness <- plantcover %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(SpeciesRichness = n())


# Join these derivatives into an analysis-ready table
names(MAFISA_2_2x2_Veg_Plot_0)
vegtable <- fg_cover_wide %>%
  dplyr::left_join(speciesrichness) %>%
  dplyr::left_join(totalfoliar) %>%
  dplyr::left_join(cover_wide) %>%
  dplyr::mutate_if(is.numeric, round, 1)

# Write to csv
write.csv(vegtable, "L1/Mafisa2_Veg2x2_BD_L1.1.csv", row.names = FALSE)



### Ground cover ingestion
names(MAFISA_2_Ground_Cover_0)
names(distance_1)

# Join plot names
plotnames <- dplyr::select(MAFISA_2_Ground_Cover_0, ParentGlobalID = GlobalID, SoilPlot)
names(distance_1)
distance_1 <- distance_1 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID:GlobalID, SoilPlot, Distance_from_center:Editor)

# QC check - how many observations per plot are there? They should be equal
# Check transects visited per plot
gc_transect_qc <- MAFISA_2_Ground_Cover_0 %>%
  dplyr::group_by(SoilPlot) %>%
  dplyr::summarise(transect_count = n())
# Visualize with histogram
groundcover_hist <- ggplot(gc_transect_qc, aes(x = transect_count)) + 
  geom_histogram(fill = "dodgerblue1") + xlab("Ground cover observations per plot") +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10))
groundcover_hist


# Join plot name to transect data and split hits across distances
cover_10cm <- distance_1 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, Distance_from_center, cover = cover_10cm) %>%
  dplyr::mutate(cover = strsplit(as.character(cover), ",")) %>%
  unnest(cover) %>%
  dplyr::mutate(Location = "10cm") 
cover_30cm <- distance_1 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, Distance_from_center, cover = cover_30cm) %>%
  dplyr::mutate(cover = strsplit(as.character(cover), ",")) %>%
  unnest(cover) %>%
  dplyr::mutate(Location = "30cm") 
cover_50cm <- distance_1 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, Distance_from_center, cover = cover_50cm) %>%
  dplyr::mutate(cover = strsplit(as.character(cover), ",")) %>%
  unnest(cover) %>%
  dplyr::mutate(Location = "50cm") 
cover_70cm <- distance_1 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, Distance_from_center, cover = cover_70cm) %>%
  dplyr::mutate(cover = strsplit(as.character(cover), ",")) %>%
  unnest(cover) %>%
  dplyr::mutate(Location = "70cm")
cover_90cm <- distance_1 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::select(ObjectID, ParentGlobalID, SoilPlot, Distance_from_center, cover = cover_90cm) %>%
  dplyr::mutate(cover = strsplit(as.character(cover), ",")) %>%
  unnest(cover) %>%
  dplyr::mutate(Location = "90cm") 

# Join tables
cover <- rbind(cover_10cm, cover_30cm)
cover <- rbind(cover, cover_50cm)
cover <- rbind(cover, cover_70cm)
cover <- rbind(cover, cover_90cm)
# Trim leading and trailing white spaces
cover$cover <- trimws(cover$cover)


# Calculate percent cover by plot, adjusted for number of transects observed
# by dividing 100 by number of points collected (25 per transect), and multiplying 
# cover values by this number 
cover_calc <- cover %>%
  dplyr::group_by(SoilPlot, cover) %>%
  dplyr::summarise(cover_count = n()) %>%
  dplyr::left_join(gc_transect_qc) %>%
  dplyr::mutate(cover_adj = ifelse(transect_count == 1, cover_count*4, 
                            ifelse(transect_count == 2, cover_count*2,
                            ifelse(transect_count == 3, cover_count*1.333333,
                            ifelse(transect_count == 4, cover_count*1,
                            ifelse(transect_count == 5, cover_count*0.8,
                            ifelse(transect_count == 6, cover_count*0.6666667, NA))))))) %>%
  dplyr::select(SoilPlot, cover, cover_adj) %>%
  dplyr::mutate_if(is.numeric, round, 1)
# Convert to wide table
cover_wide <- tidyr::spread(cover_calc, cover, cover_adj, fill = 0)



# Calculate height class percentages
height <- distance_1 %>%
  dplyr::left_join(plotnames) %>%
  dplyr::group_by(SoilPlot, PlantHeight) %>%
  dplyr::summarise(height_count = n()) %>%
  dplyr::left_join(gc_transect_qc) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(height_class_percent = ifelse(transect_count == 1, height_count*20,
                                      ifelse(transect_count == 2, height_count*10,
                                      ifelse(transect_count == 3, height_count*6.6666667,
                                      ifelse(transect_count == 4, height_count*5,
                                      ifelse(transect_count == 5, height_count*2,
                                      ifelse(transect_count == 6, height_count*3.333333, NA))))))) %>%
  dplyr::select(-height_count, -transect_count) %>%
  tidyr::spread(PlantHeight, height_class_percent, fill = 0) %>%
  dplyr::mutate_if(is.numeric, round, 0)


# Join height and gc 
gctable <- cover_wide %>%
  dplyr::left_join(height) %>%
  dplyr::select(-qc)


# Save to csv
write.csv(gctable, "L1/Mafisa2_GC_L1.1.csv", row.names = FALSE)




