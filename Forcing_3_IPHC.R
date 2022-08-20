#Halibut - IPHC data
# produce the HAL column for the ts forcing files, by box

library(tidyverse)
library(readxl)
library(rbgm)
library(sf)
library(lubridate)
library(maps)
library(mapdata)
library(data.table)

select <- dplyr::select

catch_iphc <- read_xlsx('../data/Halibut/iphc-2021-tsd-026.xlsx', sheet = 1, range = 'A3:E2635') #mt https://www.iphc.int/datatest/commercial-fisheries
incidental_iphc <- read.csv('../data/Halibut/incidental.csv', check.names = F) #mt https://iphc.int/data/datatest/non-directed-commercial-discard-mortality-fisheries
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

# areas 4A and 2B will be part out the model domain
iphc_areas <- iphc_areas %>% st_transform(crs = atlantis_bgm$extra$projection)

#################################################################################################
# Make some figures for the methods
# add some coastline
iphc_areas1 <- iphc_areas %>% filter(ET_ID %in% c('2B', '2C', '3A', '3B', '4A'))
atlantis_box <- atlantis_bgm %>% box_sf()
atlantis_crs <- atlantis_bgm$extra$projection
coast <- maps::map(database = "worldHires", regions = c("Canada","US"), plot=FALSE, fill=TRUE)
coast_sf <- coast %>% st_as_sf(crs = 4326) %>% st_transform(crs=atlantis_crs)
this_bbox <- iphc_areas1 %>% st_bbox()

p <- ggplot()+
  geom_sf(data = atlantis_box %>% filter(boundary == FALSE), aes(fill = botz), color = 'navy')+
  scale_fill_gradient(low="blue", high="white")+
  geom_sf(data = atlantis_box %>% filter(boundary == TRUE), fill = 'grey', color = 'navy')+
  geom_sf(data = iphc_areas1, fill = NA, color = 'red', size = 1)+
  geom_sf(data = coast_sf, fill = 'grey')+
  coord_sf(xlim = c(this_bbox$xmin, this_bbox$xmax), ylim = c(this_bbox$ymin, this_bbox$ymax))+
  geom_sf_label(data = iphc_areas1, aes(label = ET_ID), nudge_y = -100000, size = 5)+
  theme_bw()+
  theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12))+
  labs(fill = 'Box depth', x = '', y = '')
p

ggsave('../methods/images/iphc.png', p, width = 9, height = 4)

# NOTE: in 5A we have only boundary boxes and a sliver of 2 dynamic boxes, we can probably approximate those as if they belong to 5B
# when bck on land we need to fix the colors
#################################################################################################

# atlantis_box %>% 
#   ggplot()+
#   geom_sf(fill = NA)+
#   geom_sf(data = (iphc_areas %>% filter(ET_ID %in% c('2B','2C','3A','3B','4A'))), fill = NA)+
#   geom_sf_label(data = (iphc_areas %>% filter(ET_ID %in% c('2B','2C','3A','3B','4A'))), aes(label = ET_ID))

# # look at the areas alone
# iphc_areas %>%
#   filter(ET_ID %in% c('2B','2C')) %>%
#   ggplot()+
#   geom_sf(fill = NA)

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
  select(SURVEY_YEAR, START_LAT, START_LON, LGL_HAL_WT, SUBLGL_HAL_WT, IPHC_REG, HOOKS_RETRIEVED) %>%
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
  mutate(TOT = (LGL_HAL_WT + SUBLGL_HAL_WT)/HOOKS_RETRIEVED) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(TOT = sum(TOT)) %>%
  ungroup() %>%
  st_set_geometry(NULL)
  
biom_4a_atlantis <- fiss_atlantis %>% 
  mutate(TOT = (LGL_HAL_WT + SUBLGL_HAL_WT)/HOOKS_RETRIEVED) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(TOT = sum(TOT)) %>%
  ungroup() %>%
  st_set_geometry(NULL)

prop_4a <- biom_4a %>%
  left_join(biom_4a_atlantis, by = 'SURVEY_YEAR') %>%
  mutate(Prop = TOT.y / TOT.x) %>%
  select(SURVEY_YEAR, Prop)

# ggplot()+geom_sf(data = fiss_atlantis)+geom_sf(data = atlantis_4a, fill = NA)

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
  mutate(TOT = (LGL_HAL_WT + SUBLGL_HAL_WT)/HOOKS_RETRIEVED) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(TOT = sum(TOT)) %>%
  ungroup() %>%
  st_set_geometry(NULL)

biom_2b_atlantis <- fiss_atlantis %>% 
  mutate(TOT = (LGL_HAL_WT + SUBLGL_HAL_WT)/HOOKS_RETRIEVED) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(TOT = sum(TOT)) %>%
  ungroup() %>%
  st_set_geometry(NULL)

prop_2b <- biom_2b %>%
  left_join(biom_2b_atlantis, by = 'SURVEY_YEAR') %>%
  mutate(Prop = TOT.y / TOT.x) %>%
  select(SURVEY_YEAR, Prop)

# ggplot()+geom_sf(data = fiss_atlantis)+geom_sf(data = atlantis_2b, fill = NA)

# expand to missing years
years_fiss <- prop_4a %>% pull(SURVEY_YEAR)
years_catch <- catch_iphc %>% pull(Year1) %>% unique()

setdiff(years_catch, years_fiss) # need to add years before 1998 and 2020
# assume the proportion of biomass inside the Atlantis area was constant 1991-1998 and 2019-2020
prop_4a_1 <- rbind(data.frame(SURVEY_YEAR = 1991:1997, Prop = rep(prop_4a[prop_4a$SURVEY_YEAR == 1998,]$Prop,7)),
                 prop_4a,
                 data.frame(SURVEY_YEAR = 2020, Prop = prop_4a[prop_4a$SURVEY_YEAR == 2019,]$Prop))

prop_2b_1 <- rbind(data.frame(SURVEY_YEAR = 1991:1997, Prop = rep(prop_2b[prop_2b$SURVEY_YEAR == 1998,]$Prop,7)),
                 prop_2b,
                 data.frame(SURVEY_YEAR = 2020, Prop = prop_2b[prop_2b$SURVEY_YEAR == 2019,]$Prop))

prop_long <- prop_4a_1 %>%
  left_join(prop_2b_1, by = 'SURVEY_YEAR') %>%
  set_names(c('year','2B','4A')) %>%
  pivot_longer(-year, names_to = 'area', values_to = 'prop')


# Bring in incidental catch -----------------------------------------------
# area 4 gets broken into subareas in 2006 in this dataset... so approximate the proportion in 4A to area 4 on average for the years we have data for and break down the catch from 4 for years prior to 2006

incidental_iphc_1 <- incidental_iphc %>%
  select(Year, `2B`, `2C`, `3A`, `3B`, `4A`, `4B`, `4CDE+CA`, `4`) %>%
  rowwise() %>%
  mutate(`4` = ifelse(is.na(`4`), `4A`+`4B`+`4CDE+CA`, `4`)) %>%
  ungroup()

A_to_4 <- incidental_iphc_1 %>%
  mutate(prop_a = `4A` / `4`) %>%
  pull(prop_a) %>%
  mean(na.rm = T)

incidental_iphc_1 <- incidental_iphc_1 %>%
  rowwise() %>%
  mutate(`4A` = ifelse(is.na(`4A`), `4` * A_to_4, `4A`)) %>%
  ungroup() %>%
  select(Year:`4A`) %>%
  pivot_longer(-Year, names_to = 'area', values_to = 'catch_mt') %>%
  rename(year = Year)

# add incidental catch to total catch
catch_iphc_tot <- catch_iphc %>%
  set_names(c('year','area','stat_area','catch_mt','vessels')) %>%
  filter(area %in% c('2B','2C','3A','3B','4A')) %>%
  select(year,area,catch_mt) %>%
  group_by(year, area) %>% # this step becomes necessary because we are dropping the statistical areas
  summarise(catch_mt = sum(catch_mt)) %>%
  ungroup() %>%
  full_join(incidental_iphc_1, by = c('year','area')) %>%
  mutate(catch_mt.y = replace_na(catch_mt.y, 0),
         catch_mt = catch_mt.x + catch_mt.y) %>%
  select(-catch_mt.x, -catch_mt.y)
  
# Allocate catch to Atlantis box --------------------------------------------
# Doing this based on summer biomass distribution S3

# Correct catch of areas 2B and 4A based on proportions
t <- catch_iphc_tot %>%
  left_join(prop_long, by = c('year','area')) %>%
  rowwise() %>%
  mutate(catch_mt_1 = ifelse(is.na(prop), catch_mt, catch_mt * prop)) %>%
  ungroup() %>%
  select(-catch_mt, -prop)

# Allocate catch per area per year to each box
# What do we do with catch that is taken from boundary boxes? Default behavior to allocate that to other boxes in the IPHC area
t1 <- atlantis_iphc_key %>%
  st_set_geometry(NULL) %>%
  rename(area = ET_ID) %>%
  full_join(t, by = 'area') %>%
  left_join(dists, by = 'box_id') %>%
  rename(s = Halibut_A_S3) %>%
  group_by(year, area) %>%
  mutate(sum_s_by_area = sum(s), # Allocate proportion of biomass S3
         catch_mt_box = catch_mt_1 * s / sum_s_by_area) %>% # Grouping by IPHC area, break down catch based on S3
  ungroup() %>%
  select(year, box_id, catch_mt_box)

# Convert catch in mt/y into catch per day in mg N s-1
all_dates <- data.frame('Date' = seq(as.Date('1991-01-01'), as.Date('2020-12-31'), by = 'days')) %>%
  mutate(year = year(Date), month = month(Date), day = day(Date))

t2 <- t1 %>%
  full_join(all_dates, by = 'year') %>%
  group_by(year, box_id) %>%
  mutate(month_day = paste(month, day, sep = '_'),
         catch_box_day_mt = catch_mt_box / length(month_day),
         catch_box_day_mgs = catch_box_day_mt * 1e9 / (60*60*24) / (20 * 5.7)) %>%
  ungroup() %>%
  filter(day == 1) %>%
  select(Date, box_id, catch_box_day_mgs)

# Write Halibut catch to existing catch.ts files --------------------------

all_boxes <- t2 %>% pull(box_id) %>% unique()

# separate AK boxes from BC boxes
ak_boxes <- all_boxes[all_boxes < 92]
bc_boxes <- all_boxes[all_boxes > 91]

# list files for each area - need to do it outside the loop because we use timestamps to order them and each iteration would cock up the timestamp
details_ak <- file.info(list.files('../output/AKFIN/', full.names = T))
details_ak <- details_ak[with(details_ak, order(as.POSIXct(mtime))), ]
files_ak <- rownames(details_ak)

details_bc <- file.info(list.files('../output/DFO/', full.names = T))
details_bc <- details_bc[with(details_bc, order(as.POSIXct(mtime))), ]
files_bc <- rownames(details_bc)

# Do AK boxes first
for(b in 1:length(ak_boxes)){
  
  this_box <- ak_boxes[b]
  
  this_file <- read.table(files_ak[b], skip = 309)
  
  this_hal <- t2 %>% filter(box_id == this_box)
  
  # replace HAL in the original 
  this_file$V22 <- this_hal$catch_box_day_mgs
  
  # make header
  header_file <- paste0('../output/AKFIN/catch', this_box, '.ts')
  # write header lines
  writeLines(readLines(files_ak[b], n = 309), con = header_file)
  # write table body
  write.table(this_file, file = header_file, append = T, sep = " ", row.names = FALSE, col.names = FALSE)
  
} 

# Now do BC boxes
for(b in 1:length(bc_boxes)) {
  
  this_box <- bc_boxes[b]

  this_file <- read.table(files_bc[b], skip = 309)
  
  this_hal <- t2 %>% filter(box_id == this_box)
  
  # replace HAL in the original
  this_file$V22 <- this_hal$catch_box_day_mgs
  
  # make header
  header_file <- paste0('../output/DFO/catch', this_box, '.ts')
  # write header lines
  writeLines(readLines(files_bc[b], n = 309), con = header_file)
  # write table body
  write.table(this_file, file = header_file, append = T, sep = " ", row.names = FALSE, col.names = FALSE)
  
}


# Compare catch from IPHC and AKRO-BLEND ----------------------------------

# Halibut catch is reported in the AKRO-BLEND set
# How does it compare to catch from IPHC? 
# Closest comparison will be areas 2c, 3a, 3b, 4a with NMFS 610-650
# iphc <- catch_iphc %>%
#   set_names(c('year','area','stat_area','catch_mt','vessels')) %>%
#   filter(area %in% c('2B','3A','3B','4A'), year > 1990) %>%
#   group_by(year, area) %>%
#   summarise(catch = sum(catch_mt)) %>%
#   ungroup() %>%
#   mutate(Set = 'IPHC')
# 
# # Here we will need:
# # Catch by species data from AKFIN
# all_files <- list.files('../data/AKFIN/Catch/by_fisheries/',full.names = TRUE)
# file_list <- lapply(all_files, read.csv,fileEncoding='UTF-8-BOM')
# catch_byf <- rbindlist(file_list)
# 
# # what are areas 680 and 621 in this data?
# catch_621 <- catch_byf %>% filter(NMFS.Area == 621)
# catch_680 <- catch_byf %>% filter(NMFS.Area == 680)
# 
# # after a look online, the (few) records from areas 621 and 680 should get aggregated, respectively, with 620 and 650
# catch_byf <- catch_byf %>% 
#   mutate(NMFS.Area = replace(NMFS.Area, NMFS.Area == 621, 620),
#          NMFS.Area = replace(NMFS.Area, NMFS.Area == 680, 650))
# 
# catch_byf %>%
#   filter(Species.Group == 'Halibut') %>%
#   group_by(Trip.Target.Name) %>%
#   tally()
# 
# # in the AKRO data, 72% of the halibut catch comes from directed catches, 26% from sablefish trips, and the rest is negligible
# 
# akro <- catch_byf %>%
#   filter(Species.Group == 'Halibut') %>%
#   select(Week.Ending.Date, NMFS.Area, Catch..mt.) %>%
#   mutate(Year = year(Week.Ending.Date)) %>%
#   group_by(Year, NMFS.Area) %>%
#   summarise(catch = sum(Catch..mt.)) %>%
#   ungroup() %>%
#   set_names(c('year','area','catch')) %>%
#   mutate(Set = 'AKRO')
# 
# all_hal <- rbind(iphc, akro)
# 
# all_hal %>%
#   ggplot(aes(x = year, y = catch, color = Set))+
#   geom_line()+
#   theme_bw()+
#   facet_wrap(~area)

# it seems as though the IPHC data should include all halibut catches from directed commercial activities and from incidental by-catch
# filter out halibut from AKRO and DFO data
