library(tidyverse)
library(lubridate)
library(data.table)
library(rbgm)
library(sf)

select <- dplyr::select

# This is for the US (Alaska) part of the model.
# See data from Sean Anderson for the BC part of the model

# Here we will need:
# Catch by species data from AKFIN
all_files <- list.files('../data/AKFIN/Catch/by_fisheries/',full.names = TRUE)
file_list <- lapply(all_files, read.csv,fileEncoding='UTF-8-BOM')
catch_byf <- rbindlist(file_list)

# what are areas 680 and 621 in this data?
catch_621 <- catch_byf %>% filter(NMFS.Area == 621)
catch_680 <- catch_byf %>% filter(NMFS.Area == 680)

# after a look online, the (few) records from areas 621 and 680 should get aggregated, respectively, with 620 and 650
catch_byf <- catch_byf %>% 
  mutate(NMFS.Area = replace(NMFS.Area, NMFS.Area == 621, 620),
         NMFS.Area = replace(NMFS.Area, NMFS.Area == 680, 650))

# Seasonal distributions S1-S4
s <- read.csv('../data/seasonal_distributions/seasonal_distribution.csv')
s_inv <- read.csv('../data/seasonal_distributions/seasonal_distribution_inverts.csv')

# group file
atlantis_fg <- read.csv('../data/GOA_Groups.csv', header = T)

# A key from species to Atlantis group
catch_byf %>% select(Species.Group, Species) %>% distinct() %>% write.csv('../data/species_key.csv', row.names = F)
species_key <- read.csv('../data/species_key_FG.csv') %>% filter(!is.na(Atlantis_fg))

# A key from box to stat area
# let's build this based on the location of the boxes
atlantis_bgm <- read_bgm('../data/GOA_WGS84_V4_final.bgm') 
atlantis_pts <- atlantis_bgm %>% 
  box_sf() %>%
  st_set_geometry(NULL) %>%
  select(box_id, insideX, insideY, boundary) %>%
  st_as_sf(coords = c('insideX', 'insideY'), crs = atlantis_bgm$extra$projection)

nmfs_sf <- st_read("../data/gf95_nmfs/gf95_nmfs.shp")
nmfs_sf <- nmfs_sf %>% 
  filter(NMFS_AREA %in% (catch_byf %>% select(NMFS.Area) %>% distinct() %>% pull())) %>%
  st_transform(crs = atlantis_bgm$extra$projection)

# have a look
nmfs_sf %>% ggplot()+geom_sf()+geom_sf_label(aes(label = NMFS_AREA))

area_key <- st_join(atlantis_pts, (nmfs_sf %>% select(NMFS_AREA))) %>%
  mutate(NMFS_AREA = ifelse(boundary == TRUE, NA, NMFS_AREA))
  
# 1. aggregate catch to Atlantis groups based on key
catch_fg <- catch_byf %>% 
  mutate(Species_long = paste(Species.Group, Species)) %>%
  left_join((species_key %>% mutate(Species_long = paste(Species.Group, Species)) %>% select(Species_long, Atlantis_fg)), by = 'Species_long') %>%
  filter(!is.na(Atlantis_fg))

# Prepare the data: get month and year as variable, group by time step and NMFS area and Atlantis group, sum catch
catch_fg <- catch_fg %>% mutate(Month = month(Week.Ending.Date),
                   Month.Year = make_date(year = Year, month = Month)) %>%
  group_by(Month.Year, NMFS.Area, Atlantis_fg) %>%
  summarize(Catch_mt = sum(Catch..mt., na.rm = T))

# what fg's do we have catch data for?
all_fg <- catch_fg %>% pull(Atlantis_fg) %>% unique()

# build a complete time series with all months and NMFS areas

all_years <- unique(year(catch_fg$Month.Year))
all_months <- 1:12
all_dates <- list()

all_dates <- data.frame(expand.grid(all_years, all_months)) %>%
  arrange(Var1, Var2) %>%
  mutate(Month.Year = make_date(Var1, Var2)) %>%
  pull(Month.Year)

all_areas <- catch_fg %>% pull(NMFS.Area) %>% unique()

template <- data.frame(expand.grid('Month.Year' = all_dates, 'NMFS.Area' = all_areas))
# do for one group

this_fg <- 'POL'
this_name <- atlantis_fg %>% filter(Code == this_fg) %>% pull(Name)
this_stage <- 'A'

this_catch <- catch_fg %>% filter(Atlantis_fg == this_fg) %>% select(-Atlantis_fg)

if((atlantis_fg %>% filter(Code == this_fg) %>% pull(NumStages)) > 1){
  distribution <- s
} else {
  distribution <- s1
}

this_dist <- distribution %>% select(contains(paste(this_name, this_stage, sep = '_'))) %>%
  mutate(box_id = 0:(nrow(.)-1)) %>%
  select(box_id, contains('S1')) %>%
  set_names(c('box_id','s'))

# join this_catch with the template, and fill the missing values with 0
this_catch_complete <- template %>% full_join(this_catch, by = c('Month.Year', 'NMFS.Area')) %>%
  mutate(Catch_mt = coalesce(Catch_mt, 0))


tt <- this_catch_complete %>% full_join((area_key %>% st_set_geometry(NULL)), by = c('NMFS.Area' = 'NMFS_AREA'))

# now join proportions from s by box_id
ttt <- tt %>% full_join(this_dist, by = 'box_id') %>%
  group_by(Month.Year, NMFS.Area) %>%
  mutate(Prop_by_NMFS_area = sum(s))



s1 <- s %>% select(contains(this_fg))


s1 <- t(s) %>%
  data.frame() %>%
  set_names(0:(ncol(.)-1))
  
# 4. allocate catch to boxes based on S
# 5. interpolate from weeks to days
# 6. convert to appropriate units
# 7. save

# we will write it out in a separate script - will need a header, organize by box, etc.

