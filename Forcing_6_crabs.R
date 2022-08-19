# Crab

# King Crab: 
# ADF&G closed the Kodiak and Alaska Peninsula commercial red king crab fisheries prior to the start of the 1983/84 season 
# in response to declining fishery CPUE, harvest, and abundance estimates from annual assessment surveys

# Tanner crab:
# Southeast GOA: Annual Management Report for the 2016/2017 Southeast Alaska and Yakutat Tanner Crab Fisheries
# Crabs in Statistical Area A mostly come from inside waters, so ignore those
# Statistical Area D is in the Yakutat, use that. Only tanner catches are reported
# Westward GOA: Annual Management Report for Shellfish Fisheries in the Kodiak, Chignik, and South Peninsula Districts, 2020

# BC
# Tanner: DFO: "There currently is no commercial fishery and recreational harvesters rarely encounter them because of their depth."
# King

library(tidyverse)
library(sf)
library(rbgm)
library(lubridate)

select <- dplyr::select

# make a match between crab areas and Atlantis boxes, by inside point

# A key from box to stat area
# let's build this based on the location of the boxes
atlantis_bgm <- read_bgm('../data/GOA_WGS84_V4_final.bgm') 
atlantis_box <- atlantis_bgm %>% 
  box_sf()
atlantis_pts <-  atlantis_box %>%
  st_set_geometry(NULL) %>%
  select(box_id, insideX, insideY, boundary, botz) %>%
  st_as_sf(coords = c('insideX', 'insideY'), crs = atlantis_bgm$extra$projection)

crab_areas <- read_sf('../data/Crab/Kodiak_Chignik_AlaskaPeninsula_Tanner_Crab_Districts/PVTC_Kodiak_Chignik_AKPen_Districts_2014_Present_GCS_WGS1984.shp')

crab_areas <- crab_areas %>% 
  select(DISTRICT_N) %>%
  rename(Area = DISTRICT_N) %>%
  st_transform(crs = atlantis_bgm$extra$projection)

#################################################################################################
# Make some figures for the methods
# add some coastline
atlantis_box <- atlantis_bgm %>% box_sf()
atlantis_crs <- atlantis_bgm$extra$projection
coast <- maps::map(database = "worldHires", regions = c("Canada","US"), plot=FALSE, fill=TRUE)
coast_sf <- coast %>% st_as_sf(crs = 4326) %>% st_transform(crs=atlantis_crs)
this_bbox <- crab_areas %>% st_bbox()

p <- ggplot()+
  geom_sf(data = atlantis_box, aes(fill = botz, alpha = .5), color = 'navy')+
  geom_sf(data = crab_areas, fill = NA, color = 'red', size = 1)+
  geom_sf(data = coast_sf, fill = 'grey')+
  coord_sf(xlim = c(this_bbox$xmin, this_bbox$xmax), ylim = c(this_bbox$ymin, this_bbox$ymax))+
  geom_sf_label(data = crab_areas, aes(label = Area), size = 5)+
  theme_bw()+
  theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12))+
  labs(title = 'ADF&G crab statistical Areas and Atlantis geometry', fill = 'Box depth', x = '', y = '')
p

ggsave('../methods/images/adfg_crab.pdf', p, width = 7, height = 4)

# when bck on land we need to fix the colors
#TODO: when back find a shapefile for YaKUTAT
#################################################################################################

# change names
crab_areas$Area <- gsub('Kodiak District', 'Kodiak', crab_areas$Area)
crab_areas$Area <- gsub('Chignik District', 'Chignik', crab_areas$Area)
crab_areas$Area <- gsub('South Peninsula District', 'Peninsula', crab_areas$Area)

area_key <- atlantis_pts %>%
  filter(boundary == 'FALSE') %>%
  st_join(crab_areas) %>%
  filter(!is.na(Area)) %>%
  st_set_geometry(NULL) %>%
  select(box_id, Area)

# these_boxes <- area_key %>% pull(box_id)
# 
# crab_areas %>%
#   ggplot()+
#   geom_sf(fill = NA)+
#   geom_sf(data = area_key)+
#   geom_sf(data=((atlantis_box %>% filter(box_id %in% these_boxes))), fill = NA, color = 'red')

# For Yakutat, we have to assign boxes manually
boxes_yakutat <- data.frame(box_id = c(70,71,72,73,74,75,76,77,78,79,80), Area = 'Yakutat')

area_key <- rbind(area_key, boxes_yakutat)

# Read catch data for Tanner crab
tanner_yakutat <- read.csv('../data/Crab/Tanner_Yakutat.csv') %>% mutate(Area = 'Yakutat')
tanner_kodiak <- read.csv('../data/Crab/Tanner_Kodiak.csv') %>% mutate(Area = 'Kodiak')
tanner_chignik <- read.csv('../data/Crab/Tanner_Chignik.csv') %>% mutate(Area = 'Chignik')
tanner_peninsula <- read.csv('../data/Crab/Tanner_Peninsula.csv') %>% mutate(Area = 'Peninsula')

tanner <- rbind(tanner_yakutat, tanner_kodiak, tanner_chignik, tanner_peninsula) %>%
  mutate(Catch_mt = Catch_lbs * 0.4536 / 1000)

# read in spatial distributions for Tanner crab
s <- read.csv('../data/seasonal_distributions/seasonal_distribution_inverts.csv')
s_tanner <- s %>% 
  select('Crab_tanner_A_S3') %>% 
  mutate(box_id = 0:(nrow(.)-1)) %>%
  rename(s = Crab_tanner_A_S3)

# attribute catch to boxes
t <- tanner %>%
  select(-Catch_lbs) %>%
  left_join(area_key, by = 'Area') %>%
  left_join(s_tanner, by = 'box_id') %>%
  group_by(Year, Area) %>%
  mutate(s_by_area = sum(s)) %>%
  ungroup() %>%
  mutate(Catch_box_mt_yr = Catch_mt * s / s_by_area) %>%
  select(Year, box_id, Catch_box_mt_yr)
  
# turn to catch in mgs
all_dates <- data.frame('Date' = seq(as.Date('1991-01-01'), as.Date('2020-12-31'), by = 'days')) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))

t1 <- t %>%
  full_join(all_dates, by = 'Year') %>%
  group_by(Year, box_id) %>%
  mutate(Month_day = paste(Month, Day, sep = '_'),
         Catch_box_day_mt = Catch_box_mt_yr / length(Month_day),
         Catch_box_day_mgs = Catch_box_day_mt * 1e9 / (60*60*24) / (20 * 5.7)) %>%
  ungroup() %>%
  filter(Day == 1) %>%
  select(Date, box_id, Catch_box_day_mgs) 

# expand to boxes with no catch
t2 <- expand.grid(Date = t1$Date, box_id = 0:91) %>%
  full_join(t1, by = c('Date','box_id')) %>%
  mutate(Catch_box_day_mgs = replace_na(Catch_box_day_mgs, 0)) %>%
  distinct()

# Write this out ----------------------------------------------------------

# boxes
all_boxes <- 0:91 # only AK boxes

# list files for each area - need to do it outside the loop because we use timestamps to order them and each iteration would cock up the timestamp
details_ak <- file.info(list.files('../output/AKFIN/', full.names = T))
details_ak <- details_ak[with(details_ak, order(as.POSIXct(mtime))), ]
files_ak <- rownames(details_ak)

# Only doing AK here
for(b in 1:length(all_boxes)){
  
  this_box <- all_boxes[b]
  
  this_file <- read.table(files_ak[b], skip = 309)
  
  this_tanner <- t2 %>% filter(box_id == this_box)
  
  if(nrow(this_tanner)>0) {
    # replace forage in the original 
    this_file$V51 <- this_tanner$Catch_box_day_mgs
  }
  
  # make header
  header_file <- paste0('../output/AKFIN/catch', this_box, '.ts')
  # write header lines
  writeLines(readLines(files_ak[b], n = 309), con = header_file)
  # write table body
  write.table(this_file, file = header_file, append = T, sep = " ", row.names = FALSE, col.names = FALSE)
  
} 
