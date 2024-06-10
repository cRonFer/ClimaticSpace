###========================================================### 
###         Environmental space analyses Part 1            ###
###              By Cristina Ronquillo 2024                ###
### Calculates PCA and create env. space of the study area ### 
###========================================================### 
# Load packages 
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
# Set projection for the entire project
CRS <- "+proj=longlat +datum=WGS84 +no_defs"
# Standardization of the variables (Normalization - Mean= 0 and Std= 1)
std <- function(x){(x - mean(x, na.rm = T)) / sd(x, na.rm = T)} 

# Load Climate data #####
# Read each raster of climate data and stack them 
rlist <- list.files(pattern = "*.tif$")
climRaster <- raster::stack(rlist)
# Now aggregate raster cells to your study resolution
r <- raster(climRaster, 1) # extract 1 raster to check resolution of cells
res(r) # cell res
climRaster_res <- raster::aggregate(climRaster, fact = 5, fun = mean) # aggregation = x12 
saveRDS(climRaster_res, 'climRaster_res')
r <- raster(climRaster_res, 1) # extract 1 raster to check NEW resolution of cells
res <- res(r)
rm(r)
# PCA  ####
# First we can reduce the variables to fewer variables using a PCA.
# Here we standardize and prepare the data.
v <- as.data.frame(values(climRaster_res)) # get env values from rasters
# str(v)
v2 <- apply(v, 2, std) # apply standardization function to dataframe of climate values
rem <- apply(is.na(v2), 1, any)
PCAdata <- as.data.frame(v2[!rem, ])
# Run the PCA
mat <- matrix(runif(nrow(PCAdata) * ncol(PCAdata), 0.00001, 0.00009), 
              ncol = ncol(PCAdata)) # add a very small randomness to avoid singularity
PCAdata2 <- PCAdata + mat
myPCA <- principal(PCAdata2,
            nfactors = 2,
            rotate = "varimax",
            scores = T)
prop.table(myPCA$values)
# Plot PCA
biplot.psych(myPCA, xlim.s = c(-3, 3), ylim.s = c(-3, 17)) # change limits to plot the whole pca

# Values of PCA of our study area ####
# Instead of assuming the absolute values of all Worldclim variables, 
# the next map assumes values of the linear combinations between these variables (PCA scores).
v3 <- v2[, 1:2] # create a vector with the same length as v2 but 2 columns
v3[!rem, ] <- myPCA$scores # insert PCA_scores (2axis-2columns) to the new vector
climate_PCA <- subset(climRaster_res, 1:2) # extract 2 raster layer as a layerbase to insert our pca values
values(climate_PCA) <- v3 # insert pca values of v3 into raster
names(climate_PCA) <- c("PC1", "PC2") # rename
# Environmental space ####
# The next step is to create the environmental space using the two PCA scores.
# Creates a Cartesian plan with PC1 and PC2 scores
v4 <- values(climate_PCA) # get env values from PCA
# Transform the two vars we want into the env space, 
# by taking the min and max scores (PCA score values) for each PCA axis
# and create a raster object. 
xmin <- min(v4[, 1], na.rm = TRUE) 
xmax <- max(v4[, 1], na.rm = TRUE)
ymin <- min(v4[, 2], na.rm = TRUE)
ymax <- max(v4[, 2], na.rm = TRUE)
# This function creates the cartesian plan comprising the min and max PCA scores
env_space <- raster(xmn = xmin, xmx = xmax, 
                    ymn = ymin, ymx = ymax,
                    res = 0.2) # SET HERE size of bins
values(env_space) <- 0
env_space_area <- env_space # duplicate this object for the next step
# Insert our PCA values into this env_space
# Extract PC1 and PC2, convert it in "classes of values" to plot in the map
env_space_v <- raster::extract(env_space, 
                       y = na.omit(v4), 
                       cellnumbers = TRUE)[, 1]
n_env_space_v <- table(env_space_v) # counts the frequency of "climates" (PC scores)

values(env_space_area)[as.numeric(names(n_env_space_v))] <- n_env_space_v
area_values <- values(env_space_area)
area_values[area_values == 0] <- NA
nCellTotal <- length(area_values[!is.na(area_values)])
values(env_space_area) <- area_values
stack <- env_space_area

# Save the following objects for 'EnvSpace2' script in 'output_EnvPart1' folder ####
saveRDS(v4,'v4')
saveRDS(env_space_area,'env_space_area')
saveRDS(env_space,'env_space')
saveRDS(area_values,'area_values')
saveRDS(climRaster_res,'climRaster_res')
