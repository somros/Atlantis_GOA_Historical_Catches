#Halibut - IPHC data
# produce the HAL column for the ts forcing files, by box

library(tidyverse)
library(readxl)
library(rbgm)
library(sf)

catch_iphc <- read_xlsx('../data/Halibut/iphc-2021-tsd-026.xlsx', sheet = 1, range = 'A3:E635') #mt
incidental_iphc <- read.csv('../data/Halibut/incidental.csv', check.names = F) #mt
# areas
iphc_areas <- read_sf('../data/Halibut/iphc-geospatial-regulatoryareas/IPHC_RegulatoryAreas_PDC.shp')
iphc_areas %>% ggplot()+geom_sf(fill = NA)+geom_sf_label(aes(label = ET_ID))
