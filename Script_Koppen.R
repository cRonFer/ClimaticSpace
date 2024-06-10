###======================================================### 
###             Environmental space analyses             ###
###              By Cristina Ronquillo 2024              ###
### Loop to create ENV SPACE of each spp from list in    ### 
### native area or introduced and extraction of values   ### 
###======================================================### 
# Load packages  ------------------------------------------------
# data management
library(data.table)
library(dplyr)
library(tidyverse)
# gis
library(sf)
library(terra)
library(tidyterra)
library(raster)
library(maps)

rm(list = ls(all.names = TRUE))
mainDir <- ""
CRS <- "+proj=longlat +datum=WGS84 +no_defs"

df <- data.frame(sp = character(),
                 nOcc = as.integer(),
                 majClim = as.integer(),
                 order = character(),
                 stringsAsFactors = TRUE)

setwd(file.path(mainDir))
# Load Koppen raster #### 
kop_shp <- raster("kopen1kmMaskWorld.tif")
# plot(kop_shp)
# Reclassification as main classes 
Tropical <- c('X1','X2','X4')
Arid <- c('X5','X6','X7','X8')
Temperate <- c('X9','X10','X11', #dry summer
               'X12','X13','X14', #dry winter
               'X15','X16','X17') #no dry season
Cold <- c('X18','X19','X20','X21', #dry summer
          'X22','X23','X24','X25', #dry winter
          'X26','X27','X28','X29') #no dry season
Polar <- c('X30','X31')
# WCVP species and distribution information ####
wcvp_names <- rWCVPdata::wcvp_names
setDT(wcvp_names)
powo <- rWCVPdata::wcvp_distributions
setDT(powo)
# Powo botanical countries shapefile
powo_countries <- read_sf(paste0(mainDir,'level3_wcvp.shp'))
# Set the correct projection for shp
st_crs(powo_countries) <- CRS
# Extract only field of code for botanical countries
powo_countries <- powo_countries %>% dplyr::select(LEVEL3_COD)

# Load species records by order and extract Köppen-Geiger class from points ####
setwd(file.path(mainDir,'')) # Load dataTable obtained in OCCUR_DaraCuration.R from Figshare repo
orders <- list.files(pattern = "")
x =  2 # to iterate rows inside loop
for(name in orders){
  outp <- read.csv(paste0(mainDir,'Templates/OutputAnalysisKOP.csv'), header = TRUE, sep = ";")
  # Download Records
  outp[1,1] <- name
  data <- readRDS(name) # Reduced to min fields (species name, lat, lon)
  spList <- unique(data$wcvp_accepted_id)
  outp[1,2] <- length(spList)
  x =  2 # to iterate rows inside loop
  for (i in spList){ # Extract koppen main class for each species
    # spp points
    d <- data[wcvp_accepted_id == i, ]
    nameSP <- unique(d$taxon_name)
    nameSP <-nameSP[1]
    outp[x,1] <- nameSP
    outp[x,2] <- nrow(d)
    if(nrow(d)>0){
      d$x <- d$decimalLongitude
      d$y <- d$decimalLatitude
      datapoints  <- st_as_sf(x = d,                         
                              coords = c("x", "y"),
                              crs = CRS)
      values_kop <- raster::extract(kop_shp, 
                                    datapoints,
                                    cellnumbers = TRUE)[, 2]
      values_kop <- as.data.frame(sort(values_kop))
      colnames(values_kop)<-'values_kop_n'
      values_kop_n <- values_kop %>%
                        group_by(values_kop_n) %>%
                        summarise(n_val = n())
      for(k in 1:31){
      val <- as.integer(values_kop_n %>% 
                            filter(values_kop_n == k) %>% 
                            dplyr::select(n_val))
      outp[x, k+2]<- val
      }
      outp$Tropical <- rowSums(outp[, Tropical], na.rm = TRUE)
      outp$Tropical <- outp$Tropical/outp$nOcc
      outp$Arid <- rowSums(outp[, Arid], na.rm = TRUE)
      outp$Arid <- outp$Arid/outp$nOcc
      outp$Temperate <- rowSums(outp[, Temperate], na.rm = TRUE)
      outp$Temperate <- outp$Temperate/outp$nOcc
      outp$Cold <-  rowSums(outp[, Cold], na.rm = TRUE)
      outp$Cold <- outp$Cold/outp$nOcc
      outp$Polar <- rowSums(outp[, Polar], na.rm = TRUE)
      outp$Polar <- outp$Polar/outp$nOcc
      print(x)
      x <- x + 1
    }else{
   print(x)
   x <- x + 1
  }
  }
kopOutp <- outp[c(1, 34:38)]
kopOutp <- kopOutp %>%
            pivot_longer(-c(sp), names_to = "maj_clim") %>%
            group_by(sp) %>%
            slice_max(value) %>%
            ungroup()
kopOutp$value <- NULL
outp <- merge(outp, kopOutp, by = 'sp', all = TRUE)
outp[x,39] <- kopOutp$maj_clim
all <- outp
fwrite(all, paste0(name,'_kop.csv'), sep = ";")
}
# Now same process but filtering points by native region only  

x =  2 # to iterate rows inside loop
for(name in orders){
  outp <- read.csv(paste0(mainDir,'Templates/OutputAnalysisKOP.csv'), header = TRUE, sep = ";")
  spp1 <- native_a %>% filter(DT == name)
  data <- readRDS(name) 
  spList <- unique(spp1$sp)
  for (j in spList){ # Extract koppen main class for each species in native area
    # spp points
    ii <- unique(data[taxon_name == j, ][ ,wcvp_accepted_id])
    d <- data[wcvp_accepted_id == ii, ]
    i <- wcvp_names[taxon_name == j][ ,accepted_plant_name_id]
    outp[x,1] <- j
   # Filter native regions 
    powo_sp <- powo[plant_name_id == ii, ]
    list_nat <- powo_sp %>% filter(introduced == 0) %>%  
                            filter(extinct == 0) %>%  
                            filter(location_doubtful == 0) %>% 
                            pull(area_code_l3)
    sp_powo_countries <- powo_countries %>% filter(LEVEL3_COD %in% list_nat)
    st_crs(sp_powo_countries) <- CRS
    d$x <- d$decimalLongitude
    d$y <- d$decimalLatitude
    datapoints  <- st_as_sf(x = d,                         
                            coords = c("x", "y"),
                            crs = CRS)
    datapoints <- st_join(datapoints, powo_countries)
    datapoints <- datapoints %>% filter(LEVEL3_COD %in% list_nat)
    outp[x,2] <- nrow(datapoints)
      if(nrow(datapoints)!= 0){
            values_kop <- raster::extract(kop_shp, 
                                          datapoints,
                                          cellnumbers = TRUE)[, 2]
            values_kop <- as.data.frame(sort(values_kop))
            colnames(values_kop) <- 'values_kop_n'
            values_kop_n <- values_kop %>%
              group_by(values_kop_n) %>%
              summarise(n_val = n())
            for(k in 1:31){
              val <- as.integer(values_kop_n %>% 
                                  filter(values_kop_n == k) %>% 
                                  dplyr::select(n_val))
              outp[x, k+2]<-val
            }
            outp$Tropical <- rowSums(outp[, Tropical], na.rm = TRUE)
            outp$Tropical <- outp$Tropical/outp$nOcc
            outp$Arid <-  rowSums(outp[, Arid], na.rm = TRUE)
            outp$Arid <- outp$Arid/outp$nOcc
            outp$Temperate <-  rowSums(outp[, Temperate], na.rm = TRUE)
            outp$Temperate <-outp$Temperate/outp$nOcc
            outp$Cold <-  rowSums(outp[, Cold], na.rm = TRUE)
            outp$Cold <-outp$Cold/outp$nOcc
            outp$Polar <-  rowSums(outp[, Polar], na.rm = TRUE)
            outp$Polar <-outp$Polar/outp$nOcc
            print(x)
            x <- x + 1
          }else{
              print(x)
              x <- x + 1
            }
}
kopOutp <- nat[c(1, 34:38)]
kopOutp <- kopOutp %>%
              pivot_longer(-c(sp), names_to = "maj_clim") %>%
              group_by(sp) %>%
              slice_max(value) %>%
              ungroup()
kopOutp$value <- NULL
outp <- merge(outp, kopOutp, by = 'sp', all = TRUE)
outp[x,39] <- kopOutp$maj_clim
nat <- outp
fwrite(nat, paste0(name,'_kopNAT.csv'), sep = ";")
}
# Join results before and after filter by native information
sum <- merge(all, nat, by = 'sp', all = TRUE)
sum <- sum %>% 
  mutate(clim_change = case_when(maj_clim.x == maj_clim.y ~ 'EQ',
                                                    TRUE ~ 'NOEQ'))
fwrite(sum,'koppenClasses.csv',sep=";")


