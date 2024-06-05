###======================================================### 
###   Code for plots - Figures in Ronquillo et al. 2024  ###
###              By Cristina Ronquillo 2024              ###
###======================================================### 
# Load packages  ------------------------------------------------
# visualisation
library(paletteer)
library(patchwork)
library(colorRamps)
library(ggplot2)
library(ggalluvial)
library(ggridges)
# data management
library(data.table)
library(stringr)
library(dplyr)
library(data.table)
library(readxl)
# GIS
library(raster)
library(sf)
# Data
library(rWCVP)

rm(list = ls(all.names = TRUE))
mainDir <- ""
setwd(file.path(mainDir))

CRS <- "+proj=longlat +datum=WGS84 +no_defs"
my_window <- extent(-2.2, 2, -2, 13) # Env. space limits
# Functions ####
# Read excels into dataframes
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
# Environmental space plots
clim_space_basic_plot <- function(){ # background and limits
  plot(my_window,
       xlab = "PC1", 
       ylab = "PC2",
       font = 2, font.lab = 2,
       main = spp_name)
}
env_space_basic_plot <- function(raster){
      plot(raster, col = 'lightgrey', border = 'transparent',
           legend = FALSE, add = TRUE)
}

# Read data from envSpacePart2 ####
mysheets <- read_excel_allsheets(".xlsx")
names(mysheets)
# Summarise species per class in each dataset (order) 
df <- data.frame(class = character(),
                 class_n = as.integer(), 
                 order = character(),
                 stringsAsFactors = TRUE)
for(i in 1:81){
  outp <- mysheets[[i]]
  outp <- outp[order(outp$ID, decreasing = FALSE), ] 
  name <- outp[1,2]
  outp <- outp[-1,]
  outp <- outp %>%
    group_by(class, order) %>%
    summarise(class_n = n())
  outp$order <- name
  df <- rbind(df, outp)
}
df1 <- df %>%
  group_by(class,order) %>%
  summarise(class_n = sum(class_n)) 
df1b <- df1 %>% 
  group_by(order) %>% 
  summarise(sumOrder = sum(class_n)) 
df1 <- merge(df1, df1b, by = 'order', all.x = TRUE)
df1 <- df1 %>% mutate(prop = class_n/sumOrder)
# Figure 2 - Barplot: number of species by order by class #####
factor_levels <- c("notEvaluated","g","f","e","d","c","b","a")
class_colors <- c(a = "#2166ac", b = "#91bfdb", c = "#e0f3f8",d = "#fee090", 
                e = "#fc8d59", f = "#d73027", g = "darkred", notEvaluated = "grey")

plot2 <- ggplot(df1) +
  aes(x = order, y = prop, fill = factor(class, levels = factor_levels)) +
  geom_col() +
  scale_fill_manual(values = class_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  ylab('Proportion of species') +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank()) + 
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))
ggsave('fig2.png', plot2, width = 50, height = 50, units = "cm", dpi = 600)

# Figure 2b - Plot climatic spaces by configuration ####
natcol <- adjustcolor('gold', alpha.f = 0.7)
intcol <- adjustcolor('dodgerblue', alpha.f = 0.3)

setwd(file.path(mainDir))
# Choose one:
spp_name <- 'Sorbaria.grandiflora'; order_stack <- 'Rosales_stack'; order_name <- 'Rosales_DT' # A
spp_name <- 'Stauntonia.hexaphylla'; order_stack <- 'G16_stack'; order_name <- 'G16_DT' # B
spp_name <- 'Cordia.dentata'; order_stack <- 'Boraginales_stack'; order_name <- 'Boraginales_DT' # C
spp_name <- 'Linaria.vulgaris'; order_stack <- 'G17_stack'; order_name <- 'G17_DT' # D
spp_name <- 'Hippobroma.longiflora'; order_stack <- 'G19_stack'; order_name <- 'G19_DT' # E 
spp_name <- 'Cotoneaster.rehderi'; order_stack <- 'Rosales_stack'; order_name <- 'Rosales_DT' # F
# Read stack
stk <- readRDS(order_stack)
stkDist <- stk[[paste0(spp_name,'Dist')]]
polDist <- rasterToPolygons(stkDist, dissolve = FALSE)
stkNat <- stk[[paste0(spp_name,'Nat')]]
poln <- rasterToPolygons(stkNat, dissolve = FALSE)
stkIntr <- stk[[paste0(spp_name,'Intr')]]
polInt <- rasterToPolygons(stkIntr, dissolve = FALSE)
env_space_All <- stk[[1]]
# Plot Climatic Space
tiff(file = paste(spp_name, "Fig2_A.tiff"), width = 5, height = 6, units = 'in', res = 300)
par(mar = c(2, 2, 2, 2))
clim_space_basic_plot()
env_space_basic_plot(env_space_All)
plot(polDist, add = TRUE)
plot(poln, col = natcol, add = TRUE)
plot(polInt, col = intcol, add = TRUE)
dev.off()
# Plot Map 
powo <- rWCVPdata::wcvp_distributions
setDT(powo)
# Powo botanical countries shapefile
powo_countries <- read_sf('level3_wcvp.shp')
# Set the correct projection for shp
st_crs(powo_countries) <- CRS
# Extract only field of code for botanical countries
powo_countries <- powo_countries %>% dplyr::select(LEVEL3_COD)
# Species info 
data <- readRDS(order_name) 
d <- data[taxon_name == spp_name, ]
idspp <- unique(d$wcvp_accepted_id)
d <- data[wcvp_accepted_id == idspp, ]
d$x <- d$decimalLongitude
d$y <- d$decimalLatitude
datapoints  <- st_as_sf(x = d, coords = c("x", "y"), crs = CRS)
powo_sp <- powo[plant_name_id == idspp, ]
# Filter native regions 
list_nat <- powo_sp %>% filter(introduced == 0) %>% dplyr::select(area_code_l3)
list_nat <- list_nat$area_code_l3
nat_powo_countries <- powo_countries %>% filter(LEVEL3_COD %in% list_nat)
st_crs(nat_powo_countries) <- CRS
# Filter introduced regions 
list_int <- powo_sp %>% filter(introduced == 1) %>% dplyr::select(area_code_l3)
list_int <- list_int$area_code_l3
introd_sp_powo_countries <- powo_countries %>% filter(LEVEL3_COD %in% list_int)
st_crs(introd_sp_powo_countries) <- CRS

# plot
tiff(file = paste(spp_name, "map.tiff"), width = 8, height = 5, units = 'in', res = 400)
ggplot() +
  geom_sf(data = powo_countries, fill = 'white', color = "grey") +
  geom_sf(data = introd_sp_powo_countries, fill = intcol, color = 'lightgrey') +
  geom_sf(data = nat_powo_countries, fill = natcol, color = 'lightgrey') +
  geom_sf(data = datapoints, color = 'black')+
  # xlim(min(d$x),max(d$x)) +
  # ylim(min(d$y),max(d$y)) +
  # xlim(-160,-60) +
  # ylim(-20, 35) +
  # ylim(-5010277.05, 9010277.05) +
  # coord_sf(crs='+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +R=6371228 +units=m +no_defs +type=crs') +
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 14, face = "bold"))
dev.off()

# Figure S1 - Climatic spaces by order ####
pal <- matlab.like(100)
env_space_area <- readRDS(paste0(mainDir,'env_space_area'))
pol <- rasterToPolygons(env_space_area, dissolve = TRUE)
setwd(file.path(mainDir,'stacks/order'))
stacks <- list.files(pattern = "")
stack <- env_space_area 
for(name in stacks){
    st1 <- readRDS(name)
    st1 <- st1[[2]]
    names(st1) <- paste0(name)
    stack <- stack(stack, st1)
}
# Plot individually each order changing x by the actual name
tiff(file = paste0(x,'order_stackCS.tiff'), width = 5, height = 5, units = 'cm', res = 400)
  plot(my_window, col = NA, xlab = '', ylab = '')
  plot(pol, col = 'lightgrey', border = 'transparent', add = TRUE)
  plot(stacks[1], col = pal, add = TRUE) + title(names(layer))
dev.off()

# Figure S2 - Comparison of resolutions for one species ####
spp_name <- 'Cycas.revoluta'
stkx05 <- readRDS('Cycadales_stack_x05')
stkDistx05 <- stkx05[[paste0(spp_name,'Dist')]]
polDistx05 <- rasterToPolygons(stkDistx05, dissolve = FALSE)
stkNatx05 <- stkx05[[paste0(spp_name,'Nat')]]
polnx05 <- rasterToPolygons(stkNatx05, dissolve = FALSE)
stkIntrx05 <- stkx05[[paste0(spp_name,'Intr')]]
polIntx05 <- rasterToPolygons(stkIntrx05, dissolve = FALSE)
env_space_Allx05 <- stkx05[[1]]

stkx02 <- readRDS('Cycadales_stack_x02')
stkDistx02 <- stkx02[[paste0(spp_name,'Dist')]]
polDistx02 <- rasterToPolygons(stkDistx02, dissolve = FALSE)
stkNatx02 <- stkx02[[paste0(spp_name,'Nat')]]
polnx02 <- rasterToPolygons(stkNatx02, dissolve = FALSE)
stkIntrx02 <- stkx02[[paste0(spp_name,'Intr')]]
polIntx02 <- rasterToPolygons(stkIntrx02, dissolve = FALSE)
env_space_Allx02 <- stkx02[[1]]

tiff("comparingScalesCS.tiff", width = 10, height = 8, units = 'in', res = 400)
par(mfrow = c(1,2))
clim_space_basic_plot()
env_space_basic_plot(env_space_Allx02)
plot(polDistx02, add = TRUE)
plot(polnx02, col = natcol, add = TRUE)
plot(polIntx02, col = intcol, add = TRUE)

clim_space_basic_plot()
env_space_basic_plot(env_space_Allx05)
plot(polDistx05, add = TRUE)
plot(polnx05, col = natcol, add = TRUE)
plot(polIntrx05, col = intcol, add = TRUE)
dev.off()
# Figure S3 - Environmental outliers #####
spp_name <- 'Cycas.revoluta'; order_name <- 'G1Cycadales' 
setwd(file.path(mainDir))
stack <- readRDS('G1Cycadales_shortDT_stackGBIFcycasrevoluta')
names(stack)

stkDist <- stack[[paste0(spp_name,'Dist')]]
polDist <- rasterToPolygons(stkDist, dissolve = FALSE)
plot(polDist)
stkGBIF <- stack[[paste0(spp_name,'GBIF')]]
polGBIF <- rasterToPolygons(stkGBIF, dissolve = FALSE)
plot(polGBIF)
stkNat <- stack[[paste0(spp_name,'Nat')]]
poln <- rasterToPolygons(stkNat, dissolve = FALSE)
plot(stkNat)
stkIntr <- stack[[paste0(spp_name,'Intr')]]
polInt <- rasterToPolygons(stkIntr, dissolve = FALSE)
plot(polInt)
env_space_All <- stk[[1]]

tiff(file = paste(spp_name, "FigS3.tiff"), 
     width = 5, height = 7, units = 'in', res = 300)
plot(my_window, col = NA, xlab = 'PCA1', ylab = 'PCA2')
plot(pol, col = 'lightgrey', border = 'transparent', add = TRUE)
plot(polGBIF,col = 'darkred', add = TRUE)
plot(polDist, col = 'darkgreen', add = TRUE)
dev.off()

##### diferent CS scale x2 vs x5

# Figure 3 and S4 ####
# Sum up spp per class in each dataset (order)
df <- data.frame(order = character(),
                 case = factor(),
                 prop = as.numeric())

for(i in 1:81){
  outp <- mysheets[[i]]
  outp <- outp[order(outp$ID,decreasing = FALSE), ] 
  name <- outp[1,2]
  outp <- outp[-1,]
  outp <- outp %>%
    filter(!is.na(nCell)) %>% 
    select(c(nCell,nCellD, nCellN)) %>% 
    mutate(NAT = nCellN/nCell)%>% 
    mutate(NAT_INT = nCellN/nCellD)
  outp <- outp[,c(4:5)]
  outp$order <- name
  outp <- outp %>% pivot_longer(!order, names_to = "case", values_to = "prop")
  df <- rbind(df,outp)
}
df$id <- rep(c(1:(nrow(df)/2)), each = 2)
# Plot ridges
names <- sort(unique(df$order))
df$case <- as.factor(df$case)
df <- df %>% filter(case %in% c('NAT','NAT_INT'))

ridges_plot <- ggplot(df) +
  geom_density_ridges(
    aes(x = prop, y = order, color = case, point_color = case, fill = case),
    jittered_points = TRUE, 
    quantile_lines = TRUE, vline_color = "darkred", point_size = 1,
    scale = 0.95, rel_min_height = .01,
    point_shape = "|", vline_size = 0.5, 
    size = 0.25, position = position_points_jitter(height = 0)) +
  scale_fill_manual(values = c("#00BD3EA6", "#FFBD33A6"), labels = c("NAT", "NAT_INT")) +
  scale_color_manual(values = c("#00BD3E", "#FFBD33"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#00BD3E", "#F5F158"), guide = "none") +
  coord_cartesian(clip = "off") +
  theme_ridges(center = TRUE) +
  theme(legend.position = "none", plot.background = element_rect('white'))+
  xlim(0,1) +
  labs(y = '', x = 'Proportion of selected cells')

ggsave('ridgesx02.png', ridges_plot, dpi = 500, width = 30, height = 30, units = 'cm')

# Wilcoxon statistical test
orders <- unique(df$order)
dfd <- data.frame(order = as.character(),
                  results = as.character(),
                  pvalue = as.numeric())
x = 1
for(i in orders){
  data <- df[order == i]
  data <- data %>%
    pivot_wider(names_from = case, values_from = prop) %>% 
    filter(!is.na(NAT))%>% 
    filter(!is.na(NAT_INT))
  dfd[x,1] <- i 
  dfd[x,2] <- wilcox.test(data$NAT_INT, data$NAT, 
                          paired = TRUE, 
                          alternative = "greater")$statistic
  dfd[x,3] <- wilcox.test(data$NAT_INT, data$NAT, 
                          paired = TRUE, 
                          alternative = "greater")$p.value
  x <- x+ 1
}
fwrite(dfd,'wilcoxon_results.csv', sep = ";")


# Figure 4 - Alluvial Koppen-Geiger #####
clim_levels <- c("Arid", "Tropical", "Temperate", "Cold","Polar","NC")
clim_colors <- c('Arid' = '#b35806', 'Tropical' = '#35978f',
                  'Temperate' = '#fee0b6','Cold' = '#4393c3',
                  'Polar' = '#542788', 'NC' = 'grey')

data <- read.csv('AfreqAlluvial.csv', sep = ";", header = TRUE)
data$maj_clim.x <- factor(data$maj_clim.x,levels = clim_levels)
data$maj_clim.y <- factor(data$maj_clim.y,levels = clim_levels)
data$clim_change <- as.factor(data$clim_change)

(plotAlluvial <- ggplot(data = data,
               aes(axis1 = maj_clim.x, axis2 = maj_clim.y, y = freq)) +
              geom_stratum(color = 'black') +
              geom_alluvium(aes(fill = maj_clim.x)) +
              geom_text(stat = "stratum",
                        aes(label = after_stat(stratum))) +
              geom_label(stat = "stratum", aes(label = after_stat(stratum)), 
               fontface = "bold", size = 4) +
              scale_x_discrete(limits = c("All data", "Native distribution"),
                               expand = c(0.10, 0.05)) +
              labs(y='') +
              scale_fill_manual(values = clim_colors) +
              theme(legend.position = "none",
                    axis.text.y = element_blank(),
                    axis.text.x = element_text(size = 11, face = "bold"),
                    axis.ticks = element_blank(),
                    panel.background = element_blank()))
letter <-'A'
ggsave(paste0(letter, 'alluvial.png'), plotAlluvial, width = 5, height = 5, units = 'in',dpi = 400)

# Figure S5 - Climatic spaces by K?ppen class ####
pal <- matlab.like(100)
env_space_area <- readRDS(paste0(mainDir,'env_space_area'))
pol <- rasterToPolygons(env_space_area, dissolve = TRUE)
stack <- readRDS(paste0(mainDir,'KOP_stackX2'))

tiff(file = ('KOP_stackCS.tiff'), width = 15, height = 10, 
     units = 'cm', res = 400)
plot(my_window, col = NA, xlab = '', ylab = '')
plot(pol, col = 'lightgrey', border = 'transparent', add = TRUE)
plot(stack[[2]], col = pal, add = TRUE) + title(names(stack[[2]])) # Change by climate class (2 to 6)
dev.off()

# Figure S6 - Frequency of K?ppen-Geiger climates by configuration ####
data <- read.csv('kopClim_TOTAL_classes.csv', sep=";", header = TRUE)
data <- data %>% filter(maj_clim != '')
data$maj_clim <- factor(data$maj_clim, levels = clim_levels)
(freqPlot <- ggplot(data) +
    aes(x = maj_clim, fill = maj_clim) +
    geom_bar() +
    scale_fill_manual(values = clim_colors) +
    theme_minimal() +
    labs(x = 'Climate types') +
    facet_wrap(~ class, scales = "free", nrow = 2, dir = 'h') +
    theme(legend.position = "none",
          strip.text.x = element_text(size = 12, color = "black", face = "bold"),
          plot.background = element_rect(fill = 'white',color = 'white'),
          axis.text.x = element_text(size = 10, face = "bold"))
  )

ggsave('barsKPOP.png', freqPlot, width = 15, height = 7, units = 'in', dpi = 400)


