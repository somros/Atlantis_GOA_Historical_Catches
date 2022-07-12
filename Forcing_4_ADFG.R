# Salmon
# produce salmon columns for the ts forcing files, by box
# Harvest data from Curry Cunningham 

# read data
library(sf)
library(dplyr)
library(ggplot2)
library(rbgm)
library(readxl)

select <- dplyr::select

# read in Atlantis geometry
atlantis_bgm <- read_bgm('../data/GOA_WGS84_V4_final.bgm')
atlantis_box <- atlantis_bgm %>% box_sf()

# read in ADF&G Groundfish Regulatory Areas
adfg_areas <- st_read('../data/Salmon/reg_areas/reg_areas.shp')

adfg_areas <- adfg_areas %>% 
  select(REGISTRATI, REGISTRA_1) %>%
  rename(Area = REGISTRATI, Code = REGISTRA_1) %>%
  filter(Code != 'O' & Code != 'Y') %>%
  st_transform(crs = atlantis_bgm$extra$projection)

areas <- adfg_areas %>% pull(Code) %>% unique()

# read in catch data
catch <- read_xlsx('../data/Salmon/RB_GOA-salmon-catch-tonnes-by-area.xlsx', sheet = 1, range = 'A1:J3509')

# subset to years and areas of interest
catch <- catch %>%
  filter(Year > 1990, `Mgt Area Code` %in% areas) %>%
  select(Year, `Mgt Area Code`, MT, Species)

# Make a key between Atlantis boxes and ADFG areas. Base this on inside points
atlantis_pts <- atlantis_box %>%
  select(box_id, insideX, insideY) %>%
  st_set_geometry(NULL) %>%
  st_as_sf(coords = c('insideX','insideY'), crs = atlantis_bgm$extra$projection) %>%
  st_join(adfg_areas %>% select(Code))





