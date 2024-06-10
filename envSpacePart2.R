###======================================================### 
###           Environmental space analyses 2             ###
###              By Cristina Ronquillo 2024              ###
### Loop to create ENV. SPACE of each species from list  ### 
###             in native or introduced area             ### 
###======================================================### 
# Visualisation
library(paletteer)
library(patchwork)
library(colorRamps)
library(ggplot2)
# stats
library(nFactors)
library(rSDM)
library(psych)
# data management
library(data.table)
library(dplyr)
# gis
library(sf)
library(terra)
library(tidyterra)
library(raster)
library(maps)

rm(list = ls(all.names = TRUE))
mainDir <- ""
CRS <- "+proj=longlat +datum=WGS84 +no_defs"
# Schoener's D function ####
# Quantifies the overlap between locations cells with most frequent conditions 
# The Schoener's D index varies from zero (total lack of congruence) to one (total congruence) 
SchoenersD <- function(x, y) {
  sub_values <- abs(x - y)
  D <- 1 - (sum(sub_values, na.rm = TRUE) / 2)
  return(D)
}
# LOAD files from envSpacePart1 Script #####
setwd(file.path(mainDir,'output_EnvPart1'))
climRaster_res <- readRDS('climRaster_res')
v4 <- readRDS('v4') 
env_space_area <- readRDS('env_space_area')
stack <- readRDS('env_space_area')
env_space <- readRDS('env_space')
area_values <- readRDS('area_values')

# Establish Limits of plots ####
xmin <- min(v4[, 1], na.rm = TRUE) 
xmax <- max(v4[, 1], na.rm = TRUE)
ymin <- min(v4[, 2], na.rm = TRUE)
ymax <- max(v4[, 2], na.rm = TRUE)
# Load POWO distributional information ####
powo <- rWCVPdata::wcvp_distributions
setDT(powo)
# Powo botanical countries shapefile
setwd(file.path(mainDir))
powo_countries <- read_sf('level3_wcvp.shp')
# Set the correct projection for shp
st_crs(powo_countries) <- CRS
# Extract only field of code for botanical countries
powo_countries <- powo_countries %>% dplyr::select(LEVEL3_COD)
# Species info ####
setwd(file.path(mainDir,'/data')) # data from 'OCCUR_DataCuration' script (Download from Figshare)
rlist <- list.files(pattern = "")
# Loop to calculate env. space by order and species ####
for(name in rlist){
    stack <- readRDS(paste0(mainDir,'/output_EnvPart1/env_space_area'))
    outp <- read.csv('Templates/OutputAnalysis.csv', header = TRUE, sep = ";")
    outp[1,1] <- name # Order
    data <- readRDS(name) # Reduced to 3 fields (species name code wcvp_accepted_id, latitude, longitude)
    spList <- unique(data$wcvp_accepted_id)
    outp[1,2] <- length(spList) # number of species in the order
    outp[1,3] <- nrow(data) # number of records in the order
    data$x <- data$decimalLongitude
    data$y <- data$decimalLatitude
    datapoints  <- st_as_sf(x = data,                         
                            coords = c("x", "y"),
                            crs = CRS)
    # Env. Space of all occurrences of order
    values_All <- raster::extract(climRaster_res, 
                                  datapoints,
                                  cellnumbers = TRUE)[, 1]
    coords_All <- v4[values_All, ]
    cell_All <- raster::extract(env_space, 
                                coords_All,
                                cellnumbers = TRUE)[, 1]
    n_All <- table(cell_All)
    env_space_All <- env_space
    values(env_space_All)[as.numeric(names(n_All))] <- n_All
    env_space_All[env_space_All == 0] <- NA
    outp[1,4] <- length(env_space_All[!is.na(env_space_All)])
    all_cell <- unique(cell_All) 
    stack <- stack(stack,env_space_All) # Join the new raster from order
    ii <- as.numeric(nlayers(stack))
    names(stack[[ii]]) <- name
    ### NOW we calculate the environmental space for each species 
    x = 2 # to iterate rows inside loop
    for (i in spList){
      if(i %in% powo$plant_name_id){
        powo_sp <- powo[plant_name_id == i, ]
        # Filter native regions 
        list_nat <- powo_sp %>% filter(introduced == 0) %>% 
          filter(extinct == 0) %>%  
          filter(location_doubtful == 0) %>% pull(area_code_l3)
        # Filter introduced regions 
        list_int <- powo_sp %>% filter(introduced == 1) %>% pull(area_code_l3)
    
        # Filter native regions polygons
        sp_powo_countries <- powo_countries %>% filter(LEVEL3_COD %in% list_nat)
        st_crs(sp_powo_countries) <- CRS
        # Filter introduced regions polygons
        introd_sp_powo_countries <- powo_countries %>% filter(LEVEL3_COD %in% list_int)
        st_crs(introd_sp_powo_countries) <- CRS
        # Filter Species points
        d <- data[wcvp_accepted_id == i, ]
        nameSP <- unique(d$taxon_name)
        nameSP <- nameSP[1]
        outp[x,1] <- nameSP
        outp[x,2] <- nrow(d)
        if(nrow(d) >= 10){
          d$x <- d$decimalLongitude
          d$y <- d$decimalLatitude
          datapoints  <- st_as_sf(x = d,                         
                                  coords = c("x", "y"),
                                  crs = CRS)
          # Env Space of all occurrences
          values_All <- raster::extract(climRaster_res, 
                                        datapoints,
                                        cellnumbers = TRUE)[, 1]
          coords_All <- v4[values_All, ]
          cell_All <- raster::extract(env_space, 
                                      coords_All,
                                      cellnumbers = TRUE)[, 1]
          n_All <- table(cell_All)
                env_space_All <- env_space
          values(env_space_All)[as.numeric(names(n_All))] <- n_All
          env_space_All[env_space_All == 0] <- NA
          outp[x,3] <- length(env_space_All[!is.na(env_space_All)])
          all_cell <- unique(cell_All) # for comparison in section bellow
          
          # Env space of occurrences within the distribution
          data_All <- st_join(datapoints, powo_countries)
          data_correct <- setDT(data_All)
          data_correct <- data_correct[LEVEL3_COD %in% list,]
          nCorrect <- nrow(data_correct)
          outp[x,4] <- nCorrect
          if(nCorrect >= 5){
            data_correct <- st_as_sf(x = data_correct)
            values_D <- raster::extract(climRaster_res, 
                                        data_correct,
                                        cellnumbers = TRUE)[, 1]
            coords_D <- v4[values_D, ]
            cell_D <- raster::extract(env_space, 
                                      coords_D,
                                      cellnumbers = TRUE)[, 1]
            n_D <- table(cell_D)
            env_space_D <- env_space
            values(env_space_D)[as.numeric(names(n_D))] <- n_D
            env_space_D[env_space_D == 0] <- NA
            stack <- stack(stack,env_space_D)
            ii <- as.numeric(nlayers(stack))
            names(stack[[ii]]) <- paste0(nameSP, "Dist")
            outp[x,5] <- length(env_space_D[!is.na(env_space_D)])
            dist_cell <- unique(cell_D)# for comparison in section bellow
            setDT(data_correct)
            
            # ONLY NATIVE INFO
            native <- data_correct %>% filter(LEVEL3_COD %in% list_nat)
            nCorrectNative <- nrow(native)
            outp[x,6] <- nCorrectNative
            if (nCorrectNative >= 5){
              native <- st_as_sf(x = native)
              values_n <- raster::extract(climRaster_res, 
                                          native,
                                          cellnumbers = TRUE)[, 1]
              coords_n <- v4[values_n, ]
              cell_n <- raster::extract(env_space, 
                                        coords_n,
                                        cellnumbers = TRUE)[, 1]
              n_n <- table(cell_n)
              env_space_n <- env_space
              values(env_space_n)[as.numeric(names(n_n))] <- n_n
              env_space_n[env_space_n == 0] <- NA
              
              stack <- stack(stack,env_space_n)
              ii <- as.numeric(nlayers(stack))
              names(stack[[ii]]) <- paste0(nameSP, "Nat")
             
              outp[x,7] <- length(env_space_n[!is.na(env_space_n)])
              nat_cell <- unique(cell_n) # for comparison in section bellow
              # INTRODUCED RECORDS
              introd <- data_correct %>% filter(LEVEL3_COD %in% list_int)
              outp[x,8] <- nrow(introd)
              if (nrow(introd) >= 5){
                introd <- st_as_sf(x = introd)
                values_i <- raster::extract(climRaster_res, 
                                            introd,
                                            cellnumbers = TRUE)[, 1]
                coords_i <- v4[values_i, ]
                cell_i <- raster::extract(env_space, 
                                          coords_i,
                                          cellnumbers = TRUE)[, 1]
                n_i <- table(cell_i)
                
                env_space_i <- env_space
                values(env_space_i)[as.numeric(names(n_i))] <- n_i
                env_space_i[env_space_i == 0] <- NA
                stack <- stack(stack,env_space_i)
                ii <- as.numeric(nlayers(stack))
                names(stack[[ii]]) <- paste0(nameSP, "Intr")
                outp[x,9] <- length(env_space_i[!is.na(env_space_i)])
                int_cell <- unique(cell_i) # for comparison in section bellow
              }else{
                env_space_i <- env_space
                values(env_space_i) <- NA
                outp[x,9] <- 0
                int_cell <- c(0) # for comparison in section bellow
              }
              ## Differences 
              outp[x, 10] <- case_when(identical(sort(dist_cell), sort(all_cell)) == TRUE ~ TRUE,
                                       TRUE ~ FALSE) # gbif info outside of countries indicated in POWO
              outp[x, 11] <- case_when(identical(sort(nat_cell), sort(dist_cell)) == TRUE ~ TRUE,
                                       TRUE ~ FALSE)  # native info not covering whole env. space
              inters <- intersect(int_cell, nat_cell)
              inters <- inters[!is.na(inters)]
              outp[x, 12] <- case_when(identical(sort(nat_cell),sort(int_cell)) == TRUE ~ 'IDENT', # introd info cover whole native dist
                                       TRUE ~ as.character(length(inters)))   
              # Introduce info differ from native:
              # if 0 -> native area does NOT SHARE env space with introduce area
              # if !0 -> native cells SHARED env space with introduce area
              
              # if shared number = env space of introd dist is IN native
              # if shared number = env space of native dist THEN:
              outp[x, 13] <- case_when(identical(sort(nat_cell),
                                                 sort(int_cell)) == FALSE ~ as.character(length(setdiff(int_cell,nat_cell))), 
                                       TRUE ~ '')  # this number indicates how many cells outside env. space of native info. 
              # Schoener's D 
              # Transform the abundance of each cell into probabilities.
              area_values_p <- area_values/sum(area_values, na.rm = TRUE) # Relative frequency of climate type for all the study area
              all_area_values <- values(env_space_All)
              all_area_values <- all_area_values/sum(all_area_values, na.rm = TRUE) # Relative frequency of climate type for distributional area
              D_values <- values(env_space_D)
              D_values <- D_values/sum(D_values, na.rm = TRUE) # Relative frequency of climate type for POWO distribution
              nat_values <- values(env_space_n)
              nat_values <- nat_values/sum(nat_values, na.rm = TRUE) # Relative frequency of climate type for POWO native distribution
              int_values <- values(env_space_i)
              int_values <- int_values/sum(int_values, na.rm = TRUE) # Relative frequency of climate type for POWO introduced distribution
              
              D <- SchoenersD(area_values_p, all_area_values)
              outp[x, 14] <- D
              outp[x, 15] <- SchoenersD(D_values, nat_values)
              outp[x, 16] <- SchoenersD(D_values, int_values)
              print(x)
              x <- x + 1
            }else{
              env_space_n <- env_space
              values(env_space_n) <- NA
              outp[x,7] <- 0
              nat_cell <- c(0) # for comparison in section bellow
              # INTRODUCED RECORDS
              introd <- data_correct %>% filter(LEVEL3_COD %in% list_int)
              outp[x,8] <- nrow(introd)
            if (nrow(introd) >= 5){
              introd <- st_as_sf(x = introd)
              values_i <- raster::extract(climRaster_res, 
                                          introd,
                                          cellnumbers = TRUE)[, 1]
              coords_i <- v4[values_i, ]
              cell_i <- raster::extract(env_space, 
                                        coords_i,
                                        cellnumbers = TRUE)[, 1]
              n_i <- table(cell_i)
              env_space_i <- env_space
              values(env_space_i)[as.numeric(names(n_i))] <- n_i
              env_space_i[env_space_i == 0] <- NA
              stack <- stack(stack,env_space_i)
              ii <- as.numeric(nlayers(stack))
              names(stack[[ii]]) <- paste0(nameSP, "Intr")
              outp[x,9] <- length(env_space_i[!is.na(env_space_i)])
              int_cell <- unique(cell_i) # for comparison in section bellow
            }else{
              env_space_i <- env_space
              values(env_space_i) <- NA
              outp[x,9] <- 0
              int_cell <- c(0) # for comparison in section bellow
            }
            # Set differences
            outp[x, 10] <- case_when(identical(sort(dist_cell), 
                                               sort(all_cell)) == TRUE ~ TRUE,
                                     TRUE ~ FALSE)  # gbif info outside of countries indicated in POWO
            outp[x, 11] <- case_when(identical(sort(nat_cell), 
                                               sort(dist_cell)) == TRUE ~ TRUE,
                                     TRUE ~ FALSE)  # native info not covering whole env. space
            inters <- intersect(int_cell, nat_cell)
            inters <- inters[!is.na(inters)]
            outp[x, 12] <- case_when(identical(sort(nat_cell),
                                               sort(int_cell)) == TRUE ~ 'IDENT', # introd info cover whole native dist
                                     TRUE ~ as.character(length(inters)))     
            outp[x, 13] <-  case_when(identical(sort(nat_cell),
                                                sort(int_cell)) == FALSE ~ as.character(length(setdiff(int_cell,nat_cell))), 
                                      TRUE ~ '')  # this number indicates how many cells outside env. space of native info. 
            # Schoener's D 
            area_values_p <- area_values/sum(area_values, na.rm = TRUE)# Relative frequency of climate type for all the study area
            all_area_values <- values(env_space_All)
            all_area_values <- all_area_values/sum(all_area_values, na.rm = TRUE) # Relative frequency of climate type for distributional area
            D_values <- values(env_space_D)
            D_values <- D_values/sum(D_values, na.rm = TRUE) # Relative frequency of climate type for POWO distribution
            nat_values <- values(env_space_n)
            nat_values <- nat_values/sum(nat_values, na.rm = TRUE) # Relative frequency of climate type for POWO native distribution
            int_values <- values(env_space_i)
            int_values <- int_values/sum(int_values, na.rm = TRUE) # Relative frequency of climate type for POWO introduced distribution
            D <- SchoenersD(area_values_p, all_area_values)
            outp[x, 14] <- D
            outp[x, 15] <- SchoenersD(D_values, nat_values)
            outp[x, 16] <- SchoenersD(D_values, int_values)
            print(x)
            x <- x + 1
            }
          }else{
              print(x)
              x <- x + 1
              }
        }else{
        print(x)
        x <- x + 1}
      }else{ 
      nameSP <- unique(d$taxon_name)  
      nameSP <- nameSP[1]
      outp[x, 1] <- nameSP
      print(x)
      x <- x + 1}
    }
    saveRDS(stack, paste0('stack_', name))
    outp <- tibble::rowid_to_column(outp, "ID")
    outp <- outp %>% mutate(propCellN_D = nCellN / nCellD) %>% 
                     mutate(propCellI_D = nCellI / nCellD) %>% 
                     mutate(propCellAll_D = nCellD / nCell) 
    outp[outp == 'Inf'] <- NA
    outp$class <- ''
    outp$NATeqINT2 <- as.integer(outp$NATeqINT)
    # Classification of configurations and saving outputs
    outp <- outp %>% 
              mutate(class = case_when(ID == 1 ~'',
                                      nOcc < 10 ~ 'notEvaluated',
                                      nOccD < 5 ~ 'notEvaluated',
                                      (nOccN >= 5 & nOccI < 5) ~ 'a',
                                      NATeqINT =='IDENT' ~'f',
                                      nCellN <= NATeqINT2 ~'e',
                                      NATeqINT2 == 0 ~ 'b',
                                      (INTextraCells == 0 & nCellI <= NATeqINT2) ~'c',
                                      TRUE ~ 'd'))
    fwrite(outp, paste0(name,'_output.csv'), sep=";")
    outp2 <- outp %>%
              group_by(class) %>%
              summarise(class_n = n())
    fwrite(outp2, paste0(name,'sumOUT.csv'), sep=";")
}
