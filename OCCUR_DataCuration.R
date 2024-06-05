###======================================================### 
###          Pre-Processing occurrences records          ###
###              By Cristina Ronquillo 2024              ###
###======================================================### 

#### The following script contains the steps for clean and filter
#### biodiversity records after the downloading process.

# Load packages ---------------------------------------------------------
library(data.table)  # big dataset manipulation
library(tidyverse)  # data manipulation
library(GADMTools)  # administrative units and GIS
library(CoordinateCleaner)  # data cleaning functions
library(lubridate) # dates
library(countrycode)  # country names standardization
library(bdc)
library(rWCVP)

# Environment and data ----------------------------------------------------
rm(list = ls(all.names = TRUE))
wd <- ''  # Write working directory path
setwd("")
group <- 'G2' # Choose a group
# Read occurrences csv from 'DownloadGBIF_data.R' and check dimensions
data <- data.table::fread("", encoding = 'UTF-8', sep = "\t") 
dim(data)
head(data)
# Reduced version of the gbif dataset -------------------------------------
# Filter fields 
colnames(data)
fields <- c("gbifID", "class", "order",
            "family", "genus", "species", "infraspecificEpithet", 
             "taxonRank", "scientificName","verbatimScientificNameAuthorship",
             "taxonKey","speciesKey",
             "countryCode", "locality", "stateProvince", 
             "decimalLatitude",	"decimalLongitude", "coordinatePrecision", 
             "day", "month", "year", 
             "basisOfRecord", "recordedBy", "issue")
data <- data[ , fields, with = FALSE]
setDT(data)
rm(fields)
# Previous filters --------------------------------------------------------
# 2. Check Basis of Record field
# 3. What type of occurrences do we have?
unique(data$basisOfRecord)
# And how many in each basisOfRecord class?
data[, .N, by = basisOfRecord]
# 4. Remove records without appropriate basis of record:
data <- data[basisOfRecord != 'MACHINE_OBSERVATION', ]  # ! means exclude
### TAXONOMY
unique(data$taxonRank)
data1 <- data[taxonRank == "SPECIES"|
             taxonRank == "VARIETY"|
             taxonRank == "SUBESPECIES", ]
checklist <- data1[, c("family", "genus", "species", "infraspecificEpithet", 
                       "verbatimScientificNameAuthorship","taxonRank", "scientificName")]
checklist <- unique(checklist)
fwrite(checklist, paste(group, '_checklist.csv'), sep = "\t")
# Geographical check #####
# 1. Check coordinates values: ----
# Discard records with latitude/longitude values equals to zero, exact same value or NULL
data1 <- data1[decimalLatitude != 0 & decimalLongitude != 0, ]
data1 <- data1[decimalLatitude != decimalLongitude, ]
min(data1$decimalLatitude)
max(data1$decimalLatitude)
min(data1$decimalLongitude)
max(data1$decimalLongitude)
# 2. Check coordinates precision: ----
# Number of decimal digits of coordinates as a measure of precision 
## Function to count number of decimals:
data1 <- bdc_coordinates_precision(data = data1, 
                                  lat = 'decimalLatitude', 
                                  lon = 'decimalLongitude', 
                                  ndec = 1) # Filter coordinates with at least 1 decimal place
data1 <- data1[.rou == 'TRUE', ]
data1$.rou <- NULL
# 3. Check records that don't meet location criteria ----
# Label coordinates placed in centroids of the country
cen <- cc_cap(data1,
               lon = "decimalLongitude",
               lat = "decimalLatitude",
               value = "clean")
# Label coordinates from museums, gardens, institutions, zoo's... 
cen <- cc_cen(cen,
                lon = "decimalLongitude",
                lat = "decimalLatitude",
                value = "clean")
# Label coordinates placed in gbif headquarters
cen <- cc_gbif(cen,
                lon = "decimalLongitude",
                lat = "decimalLatitude",
                value = "clean")
# Label coordinates from museums, gardens, institutions, zoo's... 
cen <- cc_inst(cen,
                lon = "decimalLongitude",
                lat = "decimalLatitude",
                value = "clean")
# 4. Check if coordinates are placed in the assigned country ---- 
# Point in polygon by country analysis
# Import a shapefile of the study area with Adm. Units borders at country level 
# Download world shapefile from https://gadm.org/download_world.html
countries <- read_sf('world_gadm0.shp', crs = 4326)
# Transform occurrences into spatial points and project:
data1 <- cen
data1$x <- data1$decimalLongitude
data1$y <- data1$decimalLatitude
datapoints  <- st_as_sf(x = data1,                         
                   coords = c("x", "y"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

# Point in polygon: join each point to a polygon based on position:
data1 <- st_join(datapoints, countries)
setnames(data1,"NAME_0", "GADM_Location")  # rename new field
rm(datapoints)
# Translate 'countryCode' information (ISO 3166-1-alpha-2 = "iso2c") 
# into country names of GADM 'countryCode'
data1$countryName <- countrycode(data1$countryCode, 
                                origin = "iso2c", 
                                destination = "country.name")
# Label those that didn't fall in any country polygon as 'SEA'
data1$GADM_Location[is.na(data1$GADM_Location)] <- "SEA"
setDT(data1)
data2 <- data1[GADM_Location != "SEA",]
# Check the names obtained
unique(data2$countryName)
unique(data2$GADM_Location)
# Make changes to match countryName and GADM-Location
data2$countryName[data2$countryName == "Czechia"] <- "Czech Republic" # Change 'Czechia'='Czech Republic'
data2$countryName[data2$countryName == "Svalbard & Jan Mayen"] <- 'Svalbard and Jan Mayen'
data2$countryName[data2$countryName == "Eswatini"] <- "Swaziland"
data2$countryName[data2$countryName == "St. Pierre & Miquelon"] <- "Saint Pierre and Miquelon"
data2$countryName[data2$countryName == "Turks & Caicos Islands"] <- "Turks and Caicos Islands"
data2$countryName[data2$countryName == "Åland Islands"] <- "Finland"
data2$countryName[data2$countryName == "Palestinian Territories"] <- "Israel"
data2$countryName[data2$countryName == "Myanmar (Burma)"] <- "Myanmar"
data2$countryName[data2$countryName == "North Macedonia"] <- "Macedonia"
data2$countryName[data2$countryName == "Hong Kong SAR China"] <- "Hong Kong"
data2$countryName[data2$countryName == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
data2$countryName[data2$countryName == "Côte d'Ivoire"] <- "Côte d'Ivoire"
data2$countryName[data2$countryName == "Congo - Kinshasa"] <- "Democratic Republic of the Congo"
data2$countryName[data2$countryName == "Réunion"] <- "Reunion"
data2$countryName[data2$countryName == "St. Barthélemy"] <- "Saint-Barth?lemy"
data2$countryName[data2$countryName == "Trinidad & Tobago"] <- "Trinidad and Tobago"
data2$countryName[data2$countryName == "U.S. Virgin Islands"] <- "Virgin Islands, U.S."
data2$countryName[data2$countryName == "Antigua & Barbuda"] <- "Antigua and Barbuda"
data2$countryName[data2$countryName == "Caribbean Netherlands"] <- "Bonaire, Sint Eustatius and Saba"
data2$countryName[data2$countryName == "Cocos (Keeling) Islands"] <- "Cocos Islands"
data2$countryName[data2$countryName == "Congo - Brazzaville"] <- "Republic of the Congo"
data2$countryName[data2$countryName == "Macao SAR China"] <- "Macao"
data2$countryName[data2$countryName == "Micronesia (Federated States of)"] <- "Micronesia"
data2$countryName[data2$countryName == "Saint Martin (French part)"] <- "Saint-Martin"
data2$countryName[data2$countryName == "St. Helena"] <- "Saint Helena"
data2$countryName[data2$countryName == "St. Kitts & Nevis"] <- "Saint Kitts and Nevis"
data2$countryName[data2$countryName == "St. Lucia"] <- "Saint Lucia"
data2$countryName[data2$countryName == "St. Vincent & Grenadines"] <- "Saint Vincent and the Grenadines"
data2$countryName[data2$countryName == "São Tomé & Príncipe"] <- "São Tomé and Príncipe"
data2$countryName[data2$countryName == "South Georgia & South Sandwich Islands"] <- "South Georgia and the South Sandwich Islands"
data2$countryName[data2$countryName == "Wallis & Futuna"] <- "Wallis and Futuna"
data2$countryName[data2$countryName == "United States Minor Outlying Islands (the)"] <- "United States"
data2$GADM_Location[data2$GADM_Location == "Republic of Congo"] <- "Republic of the Congo"
# data2$GADM_Location[data2$GADM_Location == "Mayotte"] <- "Comoros"
data2$GADM_Location[data2$GADM_Location == "Akrotiri and Dhekelia"] <- "Cyprus"
data2$GADM_Location[data2$GADM_Location == "Åland"] <- "Finland"
data2$GADM_Location[data2$GADM_Location == "Palestina"] <- "Israel"
data2$GADM_Location[data2$GADM_Location == "Northern Cyprus"] <- "Cyprus"
data2$GADM_Location[data2$GADM_Location == "South Sudan"] <- "Sudan"
data2$GADM_Location[data2$GADM_Location == "United States Minor Outlying Islands"] <- "United States"
# Match country names and label as 'FALSE' errors of location
data2 <- data2 %>% mutate(countryCheck = case_when(
                        GADM_Location != data2$countryName ~ FALSE,
                        TRUE ~ TRUE))
# Subset and extract records located in country assigned by collector ('correct')
setDT(data2)
data_incorrect <- data2[countryCheck != 'TRUE', ]

data_correct <- data2[countryCheck == 'TRUE', ]
# Taxonomic harmonisation ####
# devtools::install_github("matildabrown/rWCVPdata")
wcvp_names <- rWCVPdata::wcvp_names
wcvp_names <- wcvp_names %>% select('plant_name_id', 'taxon_status',
                                    'taxon_name','taxon_authors','accepted_plant_name_id')
spNames <- data_correct[,c('scientificName','taxonRank')]
spNames <- unique(spNames)
spNames$scientificName <- str_squish(spNames$scientificName)

spNames$genus <- word(spNames$scientificName, 1)
spNames$species <- word(spNames$scientificName, 2)
spNames$name <- word(spNames$scientificName, 1, 2)

spNames <- spNames %>% mutate(infra = case_when(
                       taxonRank == 'VARIETY' ~ word(spNames$scientificName, 4),
                                                TRUE ~ '')) %>% 
                        mutate(infra = case_when(infra != species ~ infra,
                                                  TRUE ~ ''))
spNames <- spNames %>% mutate(author = case_when(
                        taxonRank == 'VARIETY' ~ word(spNames$scientificName, 5,-1),
                        TRUE ~ word(spNames$scientificName, 3,-1)
                        ))
# SPECIES ONLY
spNames1 <- spNames[infra == '']
wcvpSp <- wcvp_match_names(spNames1,
                 name_col = 'name',
                 author_col = 'author')
# INFRASPECIFIC NAMES
spNames2 <- spNames[infra != '']
wcvpInfra <- wcvp_match_names(spNames2,
                         join_cols = c("genus","species", "infra"),
                         author_col = 'author')
wcvpInfra1 <- wcvpInfra %>% filter(match_type != '')
# Join tax. outputs
wcvpOUTPUT <- bind_rows(wcvpSp, wcvpInfra1, wcvpInfra2)
wcvpOUTPUT <- merge(wcvpOUTPUT2, wcvp_names, by.x = 'wcvp_accepted_id', 
                    by.y = 'plant_name_id', all.x = TRUE)
wcvpOUTPUT <- wcvpOUTPUT %>% filter(wcvp_accepted_id != '')                      
wcvpOUTPUT <- wcvpOUTPUT %>% filter(match_similarity >= 0.95)    
wcvpOUTPUT <- wcvpOUTPUT %>% filter(accepted_plant_name_id != '')   
unique(wcvpOUTPUT$match_type)
wcvpOUTPUT <- wcvpOUTPUT %>% filter(match_type == 'Exact (with author)'| match_type == 'Fuzzy (phonetic)')   
setDT(wcvpOUTPUT)
# Duplicates labeling ####
data_correct2 <- wcvpOUTPUT[data_correct, on = .(scientificName)]
data_correct2 <- data_correct2[wcvp_accepted_id != '',]
dataDup <- cc_dupl(data_correct2,
                   lon = "decimalLongitude",
                   lat = "decimalLatitude",
                   species = "wcvp_accepted_id")
# Create a short version for env. space analysis ####
dataDup <- dataDup[ ,c('wcvp_accepted_id','wcvp_accepted_id', 'taxon_name',
                      'decimalLongitude', 'decimalLatitude')]
saveRDS(dataDup, paste0(group, 'shortDT'))
