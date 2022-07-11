#Halibut - IPHC data
# produce the HAL column for the ts forcing files, by box

library(tidyverse)
library(readxl)
library(rbgm)
library(sf)

select <- dplyr::select

catch_iphc <- read_xlsx('../data/Halibut/iphc-2021-tsd-026.xlsx', sheet = 1, range = 'A3:E2635') #mt
incidental_iphc <- read.csv('../data/Halibut/incidental.csv', check.names = F) #mt
# areas
iphc_areas <- read_sf('../data/Halibut/iphc-geospatial-regulatoryareas/IPHC_RegulatoryAreas_PDC.shp')

# spatial distributions
dists <- read.csv('../data/seasonal_distributions/seasonal_distribution.csv')
dists <- dists %>% 
  select('Halibut_A_S3') %>%
  mutate(box_id = 0:(nrow(.)-1))

# atlantis geometry
atlantis_bgm <- read_bgm('../data/GOA_WGS84_V4_final.bgm')
atlantis_box <- atlantis_bgm %>% box_sf()

# areas 4A and 2B will be part out the model domain. How do we allocate? Just by proportion of the area is likely inaccurate
iphc_areas <- iphc_areas %>% st_transform(crs = atlantis_bgm$extra$projection)

atlantis_box %>% 
  ggplot()+
  geom_sf(fill = NA)+
  geom_sf(data = (iphc_areas %>% filter(ET_ID %in% c('2B','2C','3A','3B','4A'))), fill = NA)+
  geom_sf_label(data = (iphc_areas %>% filter(ET_ID %in% c('2B','2C','3A','3B','4A'))), aes(label = ET_ID))

# look at the areas alone
iphc_areas %>%
  filter(ET_ID %in% c('2B','2C')) %>%
  ggplot()+
  geom_sf(fill = NA)

# Assigning Atlantis boxes to IPHC areas ----------------------------------

# Doing this based on the inside points, because the overlap between IPHC areas and Atlantis geometry is imperfect
atlantis_iphc_key <- atlantis_box %>%
  select(box_id, insideX, insideY) %>%
  st_set_geometry(NULL) %>%
  st_as_sf(coords = c('insideX','insideY'), crs = atlantis_bgm$extra$projection) %>%
  st_join(iphc_areas %>% select(ET_ID)) %>%
  filter(!(box_id %in% c(94, 97) & ET_ID == '2C')) # boxes 94 and 97 (small, one boundary) fall right in the overlap between 2B and 2C - assigning them to the larger 2B (will hardly matter)

# Proportions of areas 2B and 4A overlapping with Atlantis GOA ------------
# For areas 2B and 4A, we assign the catch to the Atlantis domain based on the proportion of total biomass caught in FISS surveys

# Area 4A

# read in FISS data
fiss_data <- read.csv('../data/Halibut/FISS/fiss_cleaned_09222021.csv')

fiss_data <- fiss_data %>%
  select(SURVEY_YEAR, START_LAT, START_LON, LGL_HAL_WT, SUBLGL_HAL_WT, IPHC_REG) %>%
  distinct() %>%
  st_as_sf(coords = c('START_LON', 'START_LAT'), crs = 4326) %>%
  st_transform(crs = atlantis_bgm$extra$projection)

# proportion of Atlantis geometry within IPHC area 4A
atlantis_4a <- atlantis_box %>% 
  st_union() %>%
  st_intersection((iphc_areas %>% filter(ET_ID == '4A'))) %>%
  st_as_sf()

# fiss points falling into the Atlantis geometry
fiss_atlantis <- fiss_data %>%
  filter(IPHC_REG == '4A') %>%
  st_intersection(atlantis_4a)

biom_4a <- fiss_data %>% 
  filter(IPHC_REG == '4A') %>%
  mutate(TOT = LGL_HAL_WT + SUBLGL_HAL_WT) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(TOT = sum(TOT)) %>%
  ungroup() %>%
  st_set_geometry(NULL)
  
biom_4a_atlantis <- fiss_atlantis %>% 
  mutate(TOT = LGL_HAL_WT + SUBLGL_HAL_WT) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(TOT = sum(TOT)) %>%
  ungroup() %>%
  st_set_geometry(NULL)

prop_4a <- biom_4a %>%
  left_join(biom_4a_atlantis, by = 'SURVEY_YEAR') %>%
  mutate(Prop = TOT.y / TOT.x) %>%
  select(SURVEY_YEAR, Prop)

ggplot()+geom_sf(data = fiss_atlantis)+geom_sf(data = atlantis_4a, fill = NA)

# Area 2B

atlantis_2b <- atlantis_box %>% 
  st_union() %>%
  st_intersection((iphc_areas %>% filter(ET_ID == '2B'))) %>%
  st_as_sf()

# fiss points falling into the Atlantis geometry
fiss_atlantis <- fiss_data %>%
  filter(IPHC_REG == '2B') %>%
  st_intersection(atlantis_2b)

biom_2b <- fiss_data %>% 
  filter(IPHC_REG == '2B') %>%
  mutate(TOT = LGL_HAL_WT + SUBLGL_HAL_WT) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(TOT = sum(TOT)) %>%
  ungroup() %>%
  st_set_geometry(NULL)

biom_2b_atlantis <- fiss_atlantis %>% 
  mutate(TOT = LGL_HAL_WT + SUBLGL_HAL_WT) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(TOT = sum(TOT)) %>%
  ungroup() %>%
  st_set_geometry(NULL)

prop_2b <- biom_2b %>%
  left_join(biom_2b_atlantis, by = 'SURVEY_YEAR') %>%
  mutate(Prop = TOT.y / TOT.x) %>%
  select(SURVEY_YEAR, Prop)

ggplot()+geom_sf(data = fiss_atlantis)+geom_sf(data = atlantis_2b, fill = NA)



