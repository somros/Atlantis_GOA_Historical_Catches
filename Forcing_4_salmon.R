# Salmon
# produce salmon columns for the ts forcing files, by box
# Harvest data from Curry Cunningham for Alaska (ADF&G)
# Harvest data for BC from Jason Parsley

# read data
library(sf)
library(tidyverse)
library(rbgm)
library(readxl)
library(lubridate)
library(maps)
library(mapdata)

select <- dplyr::select

# read in Atlantis geometry
atlantis_bgm <- read_bgm('../data/GOA_WGS84_V4_final.bgm')
atlantis_box <- atlantis_bgm %>% box_sf()

# read in spatial distributions
s <- read.csv('../data/seasonal_distributions/seasonal_distribution.csv')

#TODO: there is a problem with chinook and coho in the island boxes in the S1-S4 - it should not be there. Amend in s file and produce new prm files
# Alaska ------------------------------------------------------------------

# read in ADF&G Groundfish Regulatory Areas
adfg_areas <- st_read('../data/Salmon/reg_areas/reg_areas.shp')

adfg_areas <- adfg_areas %>% 
  select(REGISTRATI, REGISTRA_1) %>%
  rename(Area = REGISTRATI, Code = REGISTRA_1) %>%
  filter(Code != 'O' & Code != 'Y') %>%
  st_transform(crs = atlantis_bgm$extra$projection)

areas <- adfg_areas %>% pull(Code) %>% unique()

#################################################################################################
# Make some figures for the methods
# add some coastline
atlantis_box <- atlantis_bgm %>% box_sf()
atlantis_crs <- atlantis_bgm$extra$projection
coast <- maps::map(database = "worldHires", regions = c("Canada","US"), plot=FALSE, fill=TRUE)
coast_sf <- coast %>% st_as_sf(crs = 4326) %>% st_transform(crs=atlantis_crs)
this_bbox <- adfg_areas %>% st_bbox()

p <- ggplot()+
  geom_sf(data = atlantis_box, aes(fill = botz, alpha = .5), color = 'navy')+
  geom_sf(data = adfg_areas, fill = NA, color = 'red', size = 1)+
  geom_sf(data = coast_sf, fill = 'grey')+
  coord_sf(xlim = c(this_bbox$xmin, this_bbox$xmax), ylim = c(this_bbox$ymin, this_bbox$ymax))+
  geom_sf_label(data = adfg_areas, aes(label = Code), size = 5)+
  theme_bw()+
  theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12))+
  labs(title = 'ADF&G Areas and Atlantis geometry', fill = 'Box depth', x = '', y = '')
p

ggsave('../methods/images/adfg.png', p, width = 9, height = 4)

# when bck on land we need to fix the colors
#################################################################################################

# read in catch data
catch <- read_xlsx('../data/Salmon/RB_GOA-salmon-catch-tonnes-by-area.xlsx', sheet = 1, range = 'A1:J3509')

# subset to years and areas of interest
catch <- catch %>%
  filter(Year > 1990, `Mgt Area Code` %in% areas) %>%
  select(Year, `Mgt Area Code`, MT, Species)

# Make a key between Atlantis boxes and ADFG areas. Base this on inside points

# Island and boundary boxes:
# No catch will be taken in Atlantis from Island and boundary boxes. If the spatial distributions S1-S4 were correct, 
# this would not be an issue: no proportion of biomass would be allocated to either. This does not hold true for several species
atlantis_pts <- atlantis_box %>%
  filter(box_id < 92) %>%
  select(box_id, insideX, insideY) %>%
  st_set_geometry(NULL) %>%
  st_as_sf(coords = c('insideX','insideY'), crs = atlantis_bgm$extra$projection) %>%
  st_join(adfg_areas %>% select(Code))

# manually add missing values
atlantis_pts[atlantis_pts$box_id %in% c(0,2,11),]$Code <- 'M'
atlantis_pts[atlantis_pts$box_id %in% c(35,40),]$Code <- 'K'

# Join box-point key with catch data
t <- atlantis_pts %>%
  st_set_geometry(NULL) %>%
  full_join(catch, by = c('Code' = 'Mgt Area Code'))

# s long
all_species <- catch %>% pull(Species) %>% unique() %>% sort()

s_salmon <- s %>%
  select(grep(paste(all_species, collapse = "|"), names(s), ignore.case = T)) 

# Fix non-zero adult chinook and coho in island boxes...
# 21: nearest boxes are 16 and 22
# 40: nearest boxes are 35, 36, 41, 43
# 99: nearest boxes are 96, 100, 101

s_21_SCH <- s_salmon[22,]$'Salmon_chinook_A_S3'
s_21_SCO <- s_salmon[22,]$'Salmon_coho_A_S3'
s_40_SCH <- s_salmon[41,]$'Salmon_chinook_A_S3'
s_40_SCO <- s_salmon[41,]$'Salmon_coho_A_S3'
s_99_SCH <- s_salmon[100,]$'Salmon_chinook_A_S3'
s_99_SCO <- s_salmon[100,]$'Salmon_coho_A_S3'

# do all the replacements from here on out
# box 21 
# Chinook
s_salmon[17,]$'Salmon_chinook_A_S3' <- s_salmon[17,]$'Salmon_chinook_A_S3' + s_21_SCH / 2
s_salmon[23,]$'Salmon_chinook_A_S3' <- s_salmon[23,]$'Salmon_chinook_A_S3' + s_21_SCH / 2
s_salmon[22,]$'Salmon_chinook_A_S3' <- 0
# Coho
s_salmon[17,]$'Salmon_coho_A_S3' <- s_salmon[17,]$'Salmon_coho_A_S3' + s_21_SCO / 2
s_salmon[23,]$'Salmon_coho_A_S3' <- s_salmon[23,]$'Salmon_coho_A_S3' + s_21_SCO / 2
s_salmon[22,]$'Salmon_coho_A_S3' <- 0
# box 40
# Chinook
s_salmon[36,]$'Salmon_chinook_A_S3' <- s_salmon[36,]$'Salmon_chinook_A_S3' + s_40_SCH / 4
s_salmon[37,]$'Salmon_chinook_A_S3' <- s_salmon[37,]$'Salmon_chinook_A_S3' + s_40_SCH / 4
s_salmon[42,]$'Salmon_chinook_A_S3' <- s_salmon[42,]$'Salmon_chinook_A_S3' + s_40_SCH / 4
s_salmon[44,]$'Salmon_chinook_A_S3' <- s_salmon[44,]$'Salmon_chinook_A_S3' + s_40_SCH / 4
s_salmon[41,]$'Salmon_chinook_A_S3' <- 0
# coho
s_salmon[36,]$'Salmon_coho_A_S3' <- s_salmon[36,]$'Salmon_coho_A_S3' + s_40_SCO / 4
s_salmon[37,]$'Salmon_coho_A_S3' <- s_salmon[37,]$'Salmon_coho_A_S3' + s_40_SCO / 4
s_salmon[42,]$'Salmon_coho_A_S3' <- s_salmon[42,]$'Salmon_coho_A_S3' + s_40_SCO / 4
s_salmon[44,]$'Salmon_coho_A_S3' <- s_salmon[44,]$'Salmon_coho_A_S3' + s_40_SCO / 4
s_salmon[41,]$'Salmon_coho_A_S3' <- 0
# box 99\
# Chinook
s_salmon[97,]$'Salmon_chinook_A_S3' <- s_salmon[97,]$'Salmon_chinook_A_S3' + s_99_SCH / 3
s_salmon[101,]$'Salmon_chinook_A_S3' <- s_salmon[101,]$'Salmon_chinook_A_S3' + s_99_SCH / 3
s_salmon[102,]$'Salmon_chinook_A_S3' <- s_salmon[102,]$'Salmon_chinook_A_S3' + s_99_SCH / 3
s_salmon[100,]$'Salmon_chinook_A_S3' <- 0
# Coho
s_salmon[97,]$'Salmon_coho_A_S3' <- s_salmon[97,]$'Salmon_coho_A_S3' + s_99_SCO / 3
s_salmon[101,]$'Salmon_coho_A_S3' <- s_salmon[101,]$'Salmon_coho_A_S3' + s_99_SCO / 3
s_salmon[102,]$'Salmon_coho_A_S3' <- s_salmon[102,]$'Salmon_coho_A_S3' + s_99_SCO / 3
s_salmon[100,]$'Salmon_coho_A_S3' <- 0

s_long <- s_salmon %>%
  select(grep('A_S3', names(s_salmon))) %>%
  set_names(all_species) %>%
  mutate(box_id = 0:(nrow(.)-1)) %>%
  pivot_longer(-box_id, names_to = 'Species', values_to = 's')

t1 <- t %>%
  left_join(s_long, by = c('Species', 'box_id')) %>%
  group_by(Species, Year, Code) %>%
  mutate(s_by_area = sum(s)) %>%
  ungroup() %>%
  mutate(Catch_box_mt = MT * s / s_by_area) %>%
  select(Species, Year, box_id, Catch_box_mt)

# Convert catch in mt/y into catch per day in mg N s-1
all_dates <- data.frame('Date' = seq(as.Date('1991-01-01'), as.Date('2020-12-31'), by = 'days')) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))

t2 <- t1 %>%
  full_join(all_dates, by = 'Year') %>%
  group_by(Species, Year, box_id) %>%
  mutate(Month_day = paste(Month, Day, sep = '_'),
         Catch_box_day_mt = Catch_box_mt / length(Month_day),
         Catch_box_day_mgs = Catch_box_day_mt * 1e9 / (60*60*24) / (20 * 5.7)) %>%
  ungroup() %>%
  filter(Day == 1) %>%
  select(Species, Date, box_id, Catch_box_day_mgs) 

t3 <- t2 %>%
  pivot_wider(names_from = Species, values_from = Catch_box_day_mgs) %>%
  select(Date, box_id, Chinook, Chum, Coho, Pink, Sockeye)


# Canada ------------------------------------------------------------------

# The data Isaac received from Jason Parsley seems very similar to what is publicly available here: https://open.canada.ca/data/en/dataset/7ac5fe02-308d-4fff-b805-80194f8ddeb4
# Data is organized by gear (gill net, seine, troll)
# Issues: only commercial catch; in pieces (no weight); only back to 1996; 1996-2004 is post-season, 2005-2020 is in-season
# I also have a pretty hard time getting a shapefile of the Management Areas (is available as online map on Canada Open Data but they don't let you export it / download it)
# Aggregate catch from all areas and apportion based on values of s (i.e. ditch the DFO areas)
# Aggregate all gears
# Get an "average size" for each species and multiply by the pieces to have an estimate of the weight

# read data
post_gill <- read_xlsx('../data/Salmon/FISHDATA-3814-IKaplan-Commercial_Salmon_Catch_1996-2020.xlsx', sheet = 2, range = 'A3:K152')
in_gill <- read_xlsx('../data/Salmon/FISHDATA-3814-IKaplan-Commercial_Salmon_Catch_1996-2020.xlsx', sheet = 3, range = 'A3:Q229')
post_seine <- read_xlsx('../data/Salmon/FISHDATA-3814-IKaplan-Commercial_Salmon_Catch_1996-2020.xlsx', sheet = 4, range = 'A3:K117')
in_seine <- read_xlsx('../data/Salmon/FISHDATA-3814-IKaplan-Commercial_Salmon_Catch_1996-2020.xlsx', sheet = 5, range = 'A3:Q180')
post_troll <- read_xlsx('../data/Salmon/FISHDATA-3814-IKaplan-Commercial_Salmon_Catch_1996-2020.xlsx', sheet = 6, range = 'A3:K191')
in_troll <- read_xlsx('../data/Salmon/FISHDATA-3814-IKaplan-Commercial_Salmon_Catch_1996-2020.xlsx', sheet = 7, range = 'A3:Q440')

# Based on the map at https://www.pac.dfo-mpo.gc.ca/fm-gp/maps-cartes/areas-secteurs/index-eng.html, the areas that overlap with Atlantis are:
# 101,142,130,103,3,4,104,105,5,1,2,102,6,106,7,107,8,108,9,109,10,110,11,111

dfo_areas <- c(101,142,130,103,3,4,104,105,5,1,2,102,6,106,7,107,8,108,9,109,10,110,11,111)

# in-season data contains retained and discarded for each species
# ASSUMPTION: only use kept fish
in_gill <- in_gill %>% filter(MGMT_AREA %in% dfo_areas) %>% select(CALENDAR_YEAR, SOCKEYE_KEPT, COHO_KEPT, PINK_KEPT, CHUM_KEPT, CHINOOK_KEPT)
in_seine <- in_seine %>% filter(MGMT_AREA %in% dfo_areas) %>% select(CALENDAR_YEAR, SOCKEYE_KEPT, COHO_KEPT, PINK_KEPT, CHUM_KEPT, CHINOOK_KEPT)
in_troll <- in_troll %>% filter(MGMT_AREA %in% dfo_areas) %>% select(CALENDAR_YEAR, SOCKEYE_KEPT, COHO_KEPT, PINK_KEPT, CHUM_KEPT, CHINOOK_KEPT)

in_all <- rbind(in_gill, in_seine, in_troll) %>%
  set_names(c('Year','Sockeye','Coho','Pink','Chum','Chinook')) %>%
  pivot_longer(-Year, names_to = 'Species', values_to = 'Catch_pcs') %>%
  mutate(Catch_pcs = replace_na(Catch_pcs, 0)) %>%
  group_by(Year, Species) %>%
  summarise(Catch_pcs = sum(Catch_pcs)) %>%
  ungroup()

# post season data
post_gill <- post_gill %>% filter(MGMT_AREA %in% dfo_areas) %>% select(CALENDAR_YEAR, SOCKEYE_KEPT, COHO_KEPT, PINK_KEPT, CHUM_KEPT, CHINOOK_KEPT)
post_seine <- post_seine %>% filter(MGMT_AREA %in% dfo_areas) %>% select(CALENDAR_YEAR, SOCKEYE_KEPT, COHO_KEPT, PINK_KEPT, CHUM_KEPT, CHINOOK_KEPT)
post_troll <- post_troll %>% filter(MGMT_AREA %in% dfo_areas) %>% select(CALENDAR_YEAR, SOCKEYE_KEPT, COHO_KEPT, PINK_KEPT, CHUM_KEPT, CHINOOK_KEPT)

post_all <- rbind(post_gill, post_seine, post_troll) %>%
  set_names(c('Year','Sockeye','Coho','Pink','Chum','Chinook')) %>%
  pivot_longer(-Year, names_to = 'Species', values_to = 'Catch_pcs') %>%
  mutate(Catch_pcs = replace_na(Catch_pcs, 0)) %>%
  group_by(Year, Species) %>%
  summarise(Catch_pcs = sum(Catch_pcs)) %>%
  ungroup()

# now paste all together
dfo <- rbind(post_all, in_all)

# enter average body size. These were collated by Isaac, TODO: check where these come from and amend
# ASSUMPTION: not all caught fish will be this size, jacks are also counted towards the catch

weight_key <- data.frame('Species' = c('Chinook','Chum','Coho','Pink','Sockeye'), 'Weight_g' = c(6809,5000,3600,1814,2700))

dfo <- dfo %>%
  left_join(weight_key, by = 'Species') %>%
  mutate(Catch_mt = Catch_pcs * Weight_g / 1000000) %>%
  select(Year, Species, Catch_mt)

# apportion to boxes in BC
s_dfo <- s_long %>% filter(box_id > 91)

t4 <- dfo %>% 
  full_join(s_dfo, by = 'Species') %>%
  group_by(Species, Year) %>%
  mutate(s_by_area = sum(s)) %>%
  ungroup() %>%
  mutate(Catch_box_mt = Catch_mt * s / s_by_area) %>%
  select(Year, Species, box_id, Catch_box_mt)

# Convert catch in mt/y into catch per day in mg N s-1
all_dates <- data.frame('Date' = seq(as.Date('1996-01-01'), as.Date('2020-12-31'), by = 'days')) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))

t5 <- t4 %>%
  full_join(all_dates, by = 'Year') %>%
  group_by(Species, Year, box_id) %>%
  mutate(Month_day = paste(Month, Day, sep = '_'),
         Catch_box_day_mt = Catch_box_mt / length(Month_day),
         Catch_box_day_mgs = Catch_box_day_mt * 1e9 / (60*60*24) / (20 * 5.7)) %>%
  ungroup() %>%
  filter(Day == 1) %>%
  select(Species, Date, box_id, Catch_box_day_mgs) 

t6 <- t5 %>%
  pivot_wider(names_from = Species, values_from = Catch_box_day_mgs)

# assume constant catch 1991-1996
catch_1996 <- t6 %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
  filter(Year == 1996)

yr <- rep(1991:1995, each = length(catch_1996$Year))
filler <- data.frame(Year = yr, catch_1996[rep(seq_len(nrow(catch_1996)), 5),]) %>%
  select(-Date, -Year.1) %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep = '-'))) %>%
  select(Date, box_id, Chinook:Sockeye)
  
t7 <- rbind(filler, t6)


# View --------------------------------------------------------------------

# Let's have a look at this

# # Alaska
# t3 %>%
#   mutate(Year = year(Date), Month = month(Date)) %>%
#   select(-Date) %>%
#   pivot_longer(-c(Year,Month,box_id), names_to = 'Species', values_to = 'Catch_mgs') %>%
#   group_by(Species, Year, Month) %>%
#   summarise(Catch_mgs = sum(Catch_mgs)) %>%
#   ungroup() %>%
#   select(-Month) %>%
#   distinct() %>%
#   mutate(Catch_mt_yr = Catch_mgs * (60*60*24) * 365 * 20 * 5.7 / 1e9) %>%
#   ggplot()+
#   geom_area(aes(x = Year, y = Catch_mt_yr, fill = Species))
# 
# catch %>%
#   group_by(Species, Year) %>%
#   summarise(Catch_mt = sum(MT)) %>%
#   ggplot()+geom_area(aes(x = Year, y = Catch_mt, fill = Species))
# 
# 
# # Canada
# t7 %>%
#   mutate(Year = year(Date), Month = month(Date)) %>%
#   select(-Date) %>%
#   pivot_longer(-c(Year,Month,box_id), names_to = 'Species', values_to = 'Catch_mgs') %>%
#   group_by(Species, Year, Month) %>%
#   summarise(Catch_mgs = sum(Catch_mgs)) %>%
#   ungroup() %>%
#   select(-Month) %>%
#   distinct() %>%
#   mutate(Catch_mt_yr = Catch_mgs * (60*60*24) * 365 * 20 * 5.7 / 1e9) %>%
#   filter(Year > 1995) %>%
#   ggplot()+
#   geom_area(aes(x = Year, y = Catch_mt_yr, fill = Species))
# 
# dfo %>%
#   ggplot()+geom_area(aes(x = Year, y = Catch_mt, fill = Species))
  
# Write to files ----------------------------------------------------------

t8 <- rbind(t3, t7)

# missing catch from a few boxes in the last years
t9 <- t8 %>% 
  complete(Date, box_id) %>%
  replace(is.na(.),0)

all_boxes <- 0:108

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

########################################################################
# # test #
# # salmon gets amplified somewhere. Where?
# test <- t9 %>%
#   pivot_longer(-c(Date, box_id), names_to = 'Species', values_to = 'mgN') %>%
#   filter(Species == 'Pink') %>%
#   mutate(Year = year(Date)) %>%
#   select(-Date) %>%
#   distinct() %>%
#   group_by(Year) %>%
#   summarise(mgN_all = sum(mgN)) %>%
#   ungroup() %>%
#   mutate(tons = mgN_all * 5.7 * 20 * (60*60*24) * 365 / 1e9)

# up to here everything seems like in the data
########################################################################

# Do AK boxes first
for(b in 1:length(ak_boxes)){
  
  this_box <- ak_boxes[b]
  
  this_file <- read.table(files_ak[b], skip = 309)
  
  this_salmon <- t9 %>% filter(box_id == this_box)
  
  # replace salmon in the original 
  this_file$V41 <- this_salmon$Chinook
  this_file$V42 <- this_salmon$Chum
  this_file$V43 <- this_salmon$Coho
  this_file$V44 <- this_salmon$Pink
  this_file$V45 <- this_salmon$Sockeye
  
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
  
  this_salmon <- t9 %>% filter(box_id == this_box)
  
  # replace salmon in the original 
  this_file$V41 <- this_salmon$Chinook
  this_file$V42 <- this_salmon$Chum
  this_file$V43 <- this_salmon$Coho
  this_file$V44 <- this_salmon$Pink
  this_file$V45 <- this_salmon$Sockeye
  
  # make header
  header_file <- paste0('../output/DFO/catch', this_box, '.ts')
  # write header lines
  writeLines(readLines(files_bc[b], n = 309), con = header_file)
  # write table body
  write.table(this_file, file = header_file, append = T, sep = " ", row.names = FALSE, col.names = FALSE)
  
}


# CHecking pink salmon catches --------------------------------------------

# The end result seems to have pretty large catches of pink salmon, sometimes bigger than pollock
# is this in the data?

# how much pink do we have?
pink <- catch %>% filter(Species == 'Pink') %>%
  group_by(Year) %>%
  summarise(Catch = sum(MT)) %>%
  ungroup() %>%
  set_names(c('year','catch_adfg'))

# check pink here, we have a big catch
pink_dfo <- dfo %>%
  filter(Species == 'Pink') %>%
  select(-Species) %>%
  set_names(c('year','catch_dfo'))

all_pink <- pink %>%
  full_join(pink_dfo, by = 'year') %>%
  mutate(catch = catch_adfg + catch_dfo)

write.csv(all_pink, 'pink.csv', row.names = F)
