# Forage fish from nontarget species data
# Adding columns for the nonforage species - data from AKFIN

# 2. Ecosystem Species Bycatch. Details with Processor/Vessel Counts
# - Year: 2003 - 2022 (2003 is the earliest)
# - FMP Area: GOA
# - FMP Subarea: --
#   - NMFS Area: --
#   - Gear: --
#   - Target Fishery: --
#   - Species Group Name: --

library(tidyverse)
library(sf)
library(rbgm)
library(lubridate)

select <- dplyr::select

nontarget <- read.csv('../data/AKFIN/Nontarget_Ecosystem_Species_Bycatch/Nontarget Species Bycatch.csv', fileEncoding = 'UTF-8-BOM')

s <- read.csv('../data/seasonal_distributions/seasonal_distribution.csv')

nontarget_species <- nontarget %>%
  pull(Species.Group.Name) %>%
  unique() %>%
  sort()

catch_species <- read.csv('../data/species_key_FG.csv') %>%
  pull(Species) %>%
  unique()

# There is overlap between species here and species reported in the By Fishery data from AKFIN, and it is difficult to know how they overlap exactly
# forage fish: in catch by fishery data, the only (few) records are from 2000-2003, for some reason

# eul <- catch_species %>% filter(Atlantis_fg == 'EUL')
# cap <- catch_species %>% filter(Atlantis_fg == 'CAP')
# fos <- catch_species %>% filter(Atlantis_fg == 'FOS')
# san <- catch_species %>% filter(Atlantis_fg == 'SAN') # no sandlance at all
# sqd <- catch_species %>% filter(Atlantis_fg == 'SQD') # sqd 

# pull forage fish from the nontarget species

forage_key <- data.frame(Species_data =  c("Capelin", 
            "Pacific Sand lance", 
            #"Surf smelt", 
            #"Smelt (Family Osmeridae)", 
            "Eulachon", 
            "Deep sea smelts (bathylagidae)", 
            "Lanternfishes (myctophidae)" 
            #, "Other osmerids"
            ),
            Species = c('Capelin','Sandlance','Eulachon','Forage_slope','Forage_slope'))

forage_catch <- nontarget %>% 
  filter(Species.Group.Name %in% (unique(forage_key$Species_data))) %>%
  select(Week.Ending.Date, NMFS.Area, Species.Group.Name, Nontarget.Estimate..mt.) %>%
  mutate(Date = as.Date(Week.Ending.Date),
         Year = year(Date)) %>%
  select(-Week.Ending.Date) %>%
  filter(Year < 2021) # we only want years up to 2020

# Note: no records from area 650. It may be gear related (no bottom trawling in area 650?)

# Temporal scale. Data comes with ending day of the week of the fishing event. However, aggregating by month is going to leave a lot of gaps
# for some species in some statistical areas, that we'll have to impute or pad with 0's
# Just calc total catch by year and then assume constant removals for each day of that year

forage_catch_1 <- forage_catch %>%
  group_by(Species.Group.Name, NMFS.Area, Year) %>%
  summarise(Catch_mt_yr = sum(Nontarget.Estimate..mt.)) %>%
  ungroup()

# aggregate species into Atlantis groups
forage_catch_2 <- forage_catch_1 %>%
  left_join(forage_key, by = c('Species.Group.Name'='Species_data')) %>%
  group_by(Species, NMFS.Area, Year) %>%
  summarise(Catch_mt_yr = sum(Catch_mt_yr)) %>%
  ungroup()
  
# now enter spatial information
# mind that some species (like Capelin) have non-zero s values in island boxes
# TODO: amend this, but for now fudge it here

s_forage <- s %>%
  select(grep(paste(unique(forage_key$Species), collapse = "|"), names(s), ignore.case = T)) 

s_forage <- s_forage %>%
  select(grep('A_S3', names(s_forage), ignore.case = T))

# reallocate CAP from island boxes to nearby boxes (same as salmon)

s_21_CAP <- s_forage[22,]$'Capelin_A_S3'
s_40_CAP <- s_forage[41,]$'Capelin_A_S3'

# do all the replacements from here on out
# box 21 
# Capelin
s_forage[17,]$'Capelin_A_S3' <- s_forage[17,]$'Capelin_A_S3' + s_21_CAP / 2
s_forage[23,]$'Capelin_A_S3' <- s_forage[23,]$'Capelin_A_S3' + s_21_CAP / 2
s_forage[22,]$'Capelin_A_S3' <- 0

# box 40
# Capelin
s_forage[36,]$'Capelin_A_S3' <- s_forage[36,]$'Capelin_A_S3' + s_40_CAP / 4
s_forage[37,]$'Capelin_A_S3' <- s_forage[37,]$'Capelin_A_S3' + s_40_CAP / 4
s_forage[42,]$'Capelin_A_S3' <- s_forage[42,]$'Capelin_A_S3' + s_40_CAP / 4
s_forage[44,]$'Capelin_A_S3' <- s_forage[44,]$'Capelin_A_S3' + s_40_CAP / 4
s_forage[41,]$'Capelin_A_S3' <- 0

s_forage_long <- s_forage %>%
  set_names(c('Capelin','Sandlance','Forage_slope','Eulachon')) %>%
  mutate(box_id = 0:(nrow(.)-1)) %>%
  pivot_longer(-box_id, names_to = 'Species', values_to = 's')

# match NMFS areas to Atlantis boxes

# A key from box to stat area
# let's build this based on the location of the boxes
atlantis_bgm <- read_bgm('../data/GOA_WGS84_V4_final.bgm') 
atlantis_pts <- atlantis_bgm %>% 
  box_sf() %>%
  st_set_geometry(NULL) %>%
  select(box_id, insideX, insideY, boundary, botz) %>%
  st_as_sf(coords = c('insideX', 'insideY'), crs = atlantis_bgm$extra$projection)

nmfs_sf <- st_read("../data/gf95_nmfs/gf95_nmfs.shp")
nmfs_sf <- nmfs_sf %>% 
  filter(NMFS_AREA %in% (forage_catch %>% select(NMFS.Area) %>% distinct() %>% pull())) %>%
  st_transform(crs = atlantis_bgm$extra$projection)

# have a look
nmfs_sf %>% ggplot()+geom_sf()+geom_sf_label(aes(label = NMFS_AREA))

area_key <- st_join(atlantis_pts, (nmfs_sf %>% select(NMFS_AREA))) %>%
  mutate(NMFS_AREA = ifelse(boundary == TRUE | botz == 0, NA, NMFS_AREA)) %>%
  st_set_geometry(NULL) %>%
  select(box_id, NMFS_AREA) %>%
  filter(box_id < 92) # keep only boxes within the US

# join with the catch data
forage_catch_3 <- forage_catch_2 %>%
  full_join(area_key, by = c('NMFS.Area'='NMFS_AREA'))

# join with s
forage_catch_4 <- forage_catch_3 %>%
  full_join(s_forage_long, by = c('Species','box_id')) %>%
  drop_na() %>%
  group_by(Species, Year, NMFS.Area) %>%
  mutate(s_by_area = sum(s)) %>%
  ungroup() %>%
  mutate(Catch_mt_box_yr = Catch_mt_yr * s / s_by_area) %>%
  select(Year, Species, box_id, Catch_mt_box_yr)

all_dates <- data.frame('Date' = seq(as.Date('2003-01-01'), as.Date('2020-12-31'), by = 'days')) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))

forage_catch_5 <- forage_catch_4 %>%
  full_join(all_dates, by = 'Year') %>%
  group_by(Species, Year, box_id) %>%
  mutate(Month_day = paste(Month, Day, sep = '_'),
         Catch_mt_box_day = Catch_mt_box_yr / length(Month_day),
         Catch_box_day_mgs = Catch_mt_box_day * 1e9 / (60*60*24) / (20 * 5.7)) %>%
  ungroup() %>%
  filter(Day == 1) %>%
  select(Species, Date, box_id, Catch_box_day_mgs) 

forage_catch_6 <- forage_catch_5 %>%
  pivot_wider(names_from = Species, values_from = Catch_box_day_mgs)

# now complete missing boxes with 0 catch
all_dates_1 <- data.frame('Date' = seq(as.Date('1991-01-01'), as.Date('2020-12-31'), by = 'days')) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
  filter(Day == 1) %>%
  pull(Date)

template <- expand.grid(Date = all_dates_1, box_id = 0:max(forage_catch_6$box_id))

forage_catch_7 <- template %>%
  full_join(forage_catch_6, by = c('Date','box_id')) %>%
  replace(is.na(.),0)

# Write this out ----------------------------------------------------------

# boxes
all_boxes <- unique(forage_catch_7$box_id)

# list files for each area - need to do it outside the loop because we use timestamps to order them and each iteration would cock up the timestamp
details_ak <- file.info(list.files('../output/AKFIN/', full.names = T))
details_ak <- details_ak[with(details_ak, order(as.POSIXct(mtime))), ]
files_ak <- rownames(details_ak)

# Only doing AK here
for(b in 1:length(all_boxes)){
  
  this_box <- all_boxes[b]
  
  this_file <- read.table(files_ak[b], skip = 309)
  
  this_forage <- forage_catch_7 %>% filter(box_id == this_box)
  
  # replace salmon in the original 
  this_file$V47 <- this_forage$Capelin
  this_file$V48 <- this_forage$Sandlance
  this_file$V49 <- this_forage$Forage_slope
  this_file$V50 <- this_forage$Eulachon

  # make header
  header_file <- paste0('../output/AKFIN/catch', this_box, '.ts')
  # write header lines
  writeLines(readLines(files_ak[b], n = 309), con = header_file)
  # write table body
  write.table(this_file, file = header_file, append = T, sep = " ", row.names = FALSE, col.names = FALSE)
  
} 
