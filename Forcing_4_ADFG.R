# Salmon
# produce salmon columns for the ts forcing files, by box
# Harvest data from Curry Cunningham 

# read data
library(sf)
library(dplyr)
library(ggplot2)
library(rbgm)
library(readxl)
library(lubridate)

select <- dplyr::select

# read in Atlantis geometry
atlantis_bgm <- read_bgm('../data/GOA_WGS84_V4_final.bgm')
atlantis_box <- atlantis_bgm %>% box_sf()

# read in spatial distributions
s <- read.csv('../data/seasonal_distributions/seasonal_distribution.csv')

#TODO: there is a problem with chinook and coho in the island boxes in the S1-S4 - it should not be there. Amend in s file and produce new prm files

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

# one species at a time
all_species <- catch %>% pull(Species) %>% unique() %>% sort()

this_species <- all_species[1]

# join with spatial distributions
this_t <- t %>% 
  filter(Species == this_species)

this_s <- s %>% 
  select(grep(paste(this_species, 'A', 'S3', sep = '_'), names(s), ignore.case = T)) %>%
  mutate(box_id = 0:(nrow(.)-1)) %>%
  set_names(c('s','box_id'))

this_t1 <- this_t %>%
  left_join(this_s, by = 'box_id') %>%
  group_by(Year, Code) %>%
  mutate(s_by_area = sum(s)) %>%
  ungroup() %>%
  mutate(Catch_box_mt = MT * s / s_by_area) %>%
  select(Year, box_id, Catch_box_mt)

# Convert catch in mt/y into catch per day in mg N s-1
all_dates <- data.frame('Date' = seq(as.Date('1991-01-01'), as.Date('2020-12-31'), by = 'days')) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))

this_t2 <- this_t1 %>%
  full_join(all_dates, by = 'Year') %>%
  group_by(Year, box_id) %>%
  mutate(Month_day = paste(Month, Day, sep = '_'),
         Catch_box_day_mt = Catch_box_mt / length(Month_day),
         Catch_box_day_mgs = Catch_box_day_mt * 1e9 / (60*60*24) / (20 * 5.7)) %>%
  ungroup() %>%
  filter(Day == 1) %>%
  select(Date, box_id, Catch_box_day_mgs)
