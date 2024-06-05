###======================================================### 
###             Environmental space analyses             ###
###              By Cristina Ronquillo 2024              ###
###    This script performs the process of downloading   ### 
###  occurrences records from GBIF by groups of orders   ### 
###======================================================### 
# Load packages and functions ------------------------------------------------
library(rgbif)
library(tidyverse)
mainDir <- ""
setwd(mainDir)
# Download occurrences from GBIF - function
gbifDOWNL <- function(list){
            gbif_taxon_keys <- name_backbone_checklist(list) %>% # match to backbone
            filter(!matchType == "NONE") %>% # get matched names
            pull(usageKey)

occ_download(pred_in("taxonKey", gbif_taxon_keys),
             pred_in("occurrenceStatus","PRESENT"), 
             pred("hasCoordinate", TRUE), 
             pred("hasGeospatialIssue", FALSE),
             pred_not(pred_in("BASIS_OF_RECORD",
                              c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
             format = "SIMPLE_CSV",
             user = '',
             pwd = '',
             email = '')
}
# Create group folders
dir <- function(){
              if (file.exists(group)){
                setwd(file.path(mainDir, group))
              } else {
                dir.create(file.path(mainDir, group))
                setwd(file.path(mainDir, group))
              }
}
### G1 ####
list <- c('Cycadopsida', 'Ginkgoopsida', 'Gnetopsida', 'Lycopodiopsida')
gbifDOWNL(list)
group <- 'G1'
dir(group)
occ_download_wait('0018343-231120084113126')
data <- occ_download_get('0018343-231120084113126') %>% occ_download_import()
### G2 Pinales ####
list <- 'pinopsida'
gbifDOWNL(list)
group <- 'G2'
dir(group)
occ_download_wait('0018429-231120084113126')
data <- occ_download_get('0018429-231120084113126') %>%  occ_download_import()
### G3 ####
list <- 'Polypodiopsida'
gbifDOWNL(list)
group <- 'G3'
dir(group)
occ_download_wait('0031731-231120084113126')
data <- occ_download_get('0031731-231120084113126') %>% occ_download_import()
# Liliopsida ####
## G4 Asparagales
list <- 'Asparagales'
gbifDOWNL(list)
group <- 'G4'
dir(group)
occ_download_wait('0032709-231120084113126')
data <- occ_download_get('0032709-231120084113126') %>% occ_download_import()
## G5 !Asparagales
list <- c('Alismatales','Arecales', 'Commelinales','Pandanales','Zingiberales', 'Dioscoreales',
          'Liliales','Petrosaviales','Acorales')
gbifDOWNL(list)
group <- 'G5'
dir(group)
occ_download_wait('0032786-231120084113126')
data <- occ_download_get('0032786-231120084113126') %>% occ_download_import()
# Poales  ####
### G6 Cyperaceae
list <- c('Cyperaceae')
gbifDOWNL(list)
group <- 'G6'
dir(group)
occ_download_wait('0033214-231120084113126')
data <- occ_download_get('0033214-231120084113126') %>% occ_download_import()
setDT(data)
### G7 Poaceae
list <- c('Poaceae')
gbifDOWNL(list)
group <- 'G7'
dir(group)
occ_download_wait('0033333-231120084113126')
data <- occ_download_get('0033333-231120084113126') %>% occ_download_import()
### G8 (EXCEPT Cyperaceae & Poaceae)
list <- c('Bromeliaceae', 'Eriocaulaceae', 'Flagellariaceae', 'Joinvilleaceae',
          'Mayaceae', 'Restionaceae', 'Thurniaceae', 'Typhaceae', 'Juncaceae',
          'Centrolepidaceae', 'Ecdeiocoleaceae', 'Rapateaceae', 'Xyridaceae')
gbifDOWNL(list)
group <- 'G8'
dir(group)
occ_download_wait('0033398-231120084113126')
data <- occ_download_get('0033398-231120084113126') %>% occ_download_import()
### G9 Magnoliopsida - (orders with less than 5M occurrences) ####
list <- c('Cucurbitales','Oxidales','Huertales',
          'Boraginales','Chloranthales','Aquifoliales',
          'Trochodendrales',
          'Metteniusales','Amborellales','Paracryphiales',
          'Berberidopsidales',
          'Vahliales','Cardiopteridales',
          
          'Celastrales','Buxales',
          'Saxifragales','Zygophyllales','Gunnerales',
          'Austrobaileyales','Escalloniales','Canellales',
          'Crossosomatales','Bruniales','Vitales',
          'Icacinales','Picramniales',
          'Ceratophyllales',
          
          'Proteales',
          'Piperales','Laurales',
          'Garryales','Nymphaeales',
          'Magnoliales', 'Santalales',
          'Dilleniales','Cornales'
)
gbifDOWNL(list)
group <- 'G9'
dir(group)
occ_download_wait('0033402-231120084113126')
data <- occ_download_get('0033402-231120084113126') %>% occ_download_import()
### Magnoliopsida ####
### G10 (Brassicales, Solanales) ####
list <- c('Brassicales', 'Solanales')
gbifDOWNL(list)
group <- 'G10'
dir(group)
occ_download_wait('0033406-231120084113126')
data <- occ_download_get('0033406-231120084113126') %>% occ_download_import()
### G11 (Apiales) ####
list <- c('Apiales')
gbifDOWNL(list)
group <- 'G11'
dir(group)
occ_download_wait('0033408-231120084113126')
data <- occ_download_get('0033408-231120084113126') %>% occ_download_import()
### G12 (Ericales,Geraniales) ####
list <- c('Ericales','Geraniales')
gbifDOWNL(list)
group <- 'G12'
dir(group)
occ_download_wait('0033409-231120084113126')
data <- occ_download_get('0033409-231120084113126') %>% occ_download_import()
### G13 (Fagales) ####
list <- c('Fagales')
gbifDOWNL(list)
group <- 'G13'
dir(group)
occ_download_wait('0033414-231120084113126')
data <- occ_download_get('0033414-231120084113126') %>% occ_download_import()
### G14 (Fabales) ####
list <- c('Fabales')
gbifDOWNL(list)
group <- 'G14'
dir(group)
occ_download_wait('0033779-231120084113126')
data <- occ_download_get('0033779-231120084113126') %>% occ_download_import()
### G15 (Malpighiales) ####
list <- c('Malpighiales')
gbifDOWNL(list)
group <- 'G15'
dir(group)
occ_download_wait('0033788-231120084113126')
data <- occ_download_get('0033788-231120084113126') %>% occ_download_import()
### G16 (Ranunculales) ####
list <- c('Ranunculales')
gbifDOWNL(list)
group <- 'G16'
dir(group)
occ_download_wait('0033804-231120084113126')
data <- occ_download_get('0033804-231120084113126') %>% occ_download_import()
### G17 (Lamiales) ####
list <- c('Lamiales')
gbifDOWNL(list)
group <- 'G17'
dir(group)
occ_download_wait('0033805-231120084113126')
data <- occ_download_get('0033805-231120084113126') %>% occ_download_import()
### G18 (Gentianales) ####
list <- c('Gentianales')
gbifDOWNL(list)
group <- 'G18'
dir(group)
occ_download_wait('0033806-231120084113126')
data <- occ_download_get('0033806-231120084113126') %>% occ_download_import()
### G19 (Asterales) ####
list <- c('Asterales')
gbifDOWNL(list)
group <- 'G19'
dir(group)
occ_download_wait('0033834-231120084113126')
data <- occ_download_get('0033834-231120084113126') %>% occ_download_import()
### G20 (Caryophyllales) ####
list <- c('Caryophyllales')
gbifDOWNL(list)
group <- 'G20'
dir(group)
occ_download_wait('0033836-231120084113126')
data <- occ_download_get('0033836-231120084113126') %>% occ_download_import()
### G21 (Myrtales, Malvales) ####
list <- c('Myrtales','Malvales')
gbifDOWNL(list)
group <- 'G21'
dir(group)
occ_download_wait('0033848-231120084113126')
data <- occ_download_get('0033848-231120084113126') %>% occ_download_import()
### G22 (Rosales) ####
list <- c('Rosales')
gbifDOWNL(list)
group <- 'G22'
dir(group)
occ_download_wait('0033849-231120084113126')
data <- occ_download_get('0033849-231120084113126') %>% occ_download_import()
### G23 (Dipsacales) ####
list <- c('Dipsacales')
gbifDOWNL(list)
group <- 'G23'
dir(group)
occ_download_wait('0033850-231120084113126')
data <- occ_download_get('0033850-231120084113126') %>% occ_download_import()