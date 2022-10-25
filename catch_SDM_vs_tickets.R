# Alberto Rovellini
# 10/24/2022
# Code to compare spatial distributions from Catch + SDM versus fish ticket data

library(tidyverse)
library(rbgm)
library(sf)
library(data.table)
library(lubridate)
library(gridExtra)
library(viridis)

select <- dplyr::select

# Read data ---------------------------------------------------------------
# Atlantis BGM
atlantis_bgm <- read_bgm('../data/GOA_WGS84_V4_final.bgm')
atlantis_box <- atlantis_bgm %>% box_sf()
atlantis_crs <- atlantis_bgm$extra$projection

# Atlantis groups for column names
atlantis_fg <- read.csv('../data/GOA_Groups.csv', header = T) %>%
  filter(IsImpacted == 1) # only keep groups that are marked as IsImpacted by fisheries in the group.csv file
all_fg <- atlantis_fg$Code

# Read and manipulate catch.ts forcing files ------------------------------
# list forcing files
details_ak <- file.info(list.files('../output/AKFIN/', full.names = T))
details_ak <- details_ak[with(details_ak, order(as.POSIXct(mtime))), ]
files_ak <- rownames(details_ak)

details_bc <- file.info(list.files('../output/DFO/', full.names = T))
details_bc <- details_bc[with(details_bc, order(as.POSIXct(mtime))), ]
files_bc <- rownames(details_bc)

# read all files
all_boxes <- 0:108

# separate AK boxes from BC boxes
ak_boxes <- all_boxes[all_boxes < 92]
bc_boxes <- all_boxes[all_boxes > 91]

# Alaska
ak_data <- list()

for(b in 1:length(ak_boxes)){
  this_box <- ak_boxes[b]
  this_tab <- read.table(files_ak[b], skip = 309)
  colnames(this_tab) <- c('Time', all_fg) # add column names
  this_tab <- this_tab %>% mutate(box_id = this_box) # add a column for the box number
  ak_data[[b]] <- this_tab
} 

all_ak <- rbindlist(ak_data)

# We do not consider BC here because the fish ticket data is for AK
all_catch <- all_ak

# transform time steps into dates
this_origin <- as.Date('1991-01-01', tz = 'UTC')

# get values in mt per month
all_catch_long_box <- all_catch %>%
  mutate(Date = as.Date(Time, "%Y-%m-%d", origin = this_origin, tz = 'UTC')) %>%
  select(Date, box_id, KWT:BIV) %>%
  pivot_longer(-c(Date, box_id), names_to = 'Species', values_to = 'Catch_mgs') %>%
  mutate(Catch_mt_day = Catch_mgs * 60 * 60 * 24 * 20 * 5.7 / 1e9,
         Catch_mt_month = Catch_mt_day * 30, # THIS IS AN APPROXIMATION FOR VISUALISATION PURPOSES
         Year = year(Date)) %>% 
  select(Year, Date, box_id, Species, Catch_mt_month) %>%
  distinct() %>%
  # group_by(Year, box_id, Species) %>%
  # mutate(Catch_mt_year = sum(Catch_mt_month)) %>% # sum over months for annual catch
  ungroup() 

# Read and manipulate fish ticket data ------------------------------------

# read in raw fish ticket data
dat1 <- read.csv('../data/AKFIN/Fish_Tickets/Fish_Tickets_v1_1991-1998.csv', fileEncoding = 'UTF-8-BOM')
dat2 <- read.csv('../data/AKFIN/Fish_Tickets/Fish_Tickets_v1_1999-2006.csv', fileEncoding = 'UTF-8-BOM')
dat3 <- read.csv('../data/AKFIN/Fish_Tickets/Fish_Tickets_v1_2007-2014.csv', fileEncoding = 'UTF-8-BOM')
dat4 <- read.csv('../data/AKFIN/Fish_Tickets/Fish_Tickets_v1_2015-2020.csv', fileEncoding = 'UTF-8-BOM')

dat <- rbind(dat1, dat2, dat3, dat4)

# aggregate landings by month
dat1 <- dat %>%
  select(Week.Ending.Date, FMP.Area, Statistical.Area, Species.Common.Name, Whole.Pounds..Detail.) %>%
  mutate(Week.Ending.Date = as.Date(Week.Ending.Date), 
         Year = year(Week.Ending.Date), 
         Month = month(Week.Ending.Date)) %>%
  group_by(Year, Month, FMP.Area, Statistical.Area, Species.Common.Name) %>%
  summarise(Tot.pounds = sum(Whole.Pounds..Detail.)) %>%
  mutate(Catch.mt = Tot.pounds * 0.454 * 1e-9)
  

# map fish ticket species to Atlantis groups
ticket_species <- dat %>% select(Species.Common.Name) %>% distinct() %>% arrange()
#write.csv(ticket_species, 'atlantis_ticket_key.csv', row.names = F)
key <- read.csv('atlantis_ticket_key.csv')
key <- key %>% drop_na()

dat2 <- dat1 %>%
  left_join(key, by = 'Species.Common.Name') %>%
  filter(!is.na(Atlantis_fg))

# map stat areas in fish tickets to Atlantis boxes based on centroid if each cell
gf_areas <- st_read('../data/AKFIN/Fish_Tickets/Groundfish_Statistical_Areas_2001/PVG_Statewide_2001_Present_GCS_WGS1984.shp')

gf_areas <- gf_areas %>%
  filter(FMP_AREA_C == 'GOA') %>%
  mutate(DISTRICT_N = replace_na(DISTRICT_N, 'Other'), # need to do this because filter will drop NAs
         SUBDISTRIC = replace_na(SUBDISTRIC, 'Other')) %>%
  filter(DISTRICT_N != 'Inside District') %>% # ditch PWS and SE Inside
  filter(SUBDISTRIC != 'Northern Southeast Inside Subdistrict') %>%
  filter(SUBDISTRIC != 'Southern Southeast Inside Subdistrict') %>%
  select(STAT_AREA) %>%
  st_transform(crs = atlantis_crs)

# make a key to match stat areas to Atlantis boxes
areas_centroids <- st_centroid(gf_areas)

# for each centroid, do a spatial join with the Atlantis geometry to assign it to a box
match_area_box <- areas_centroids %>%
  st_join(atlantis_box %>% select(box_id))

# for those centroids that have not been assigned, assign them to the closest object
# we can do this because we threw away the PWS and SE Inside cells
# will then need to zero-out boundary boxes, thus losing catch

match_area_box <- match_area_box %>%
  rowwise() %>%
  mutate(box_id = ifelse(is.na(box_id), st_nearest_feature(geometry, atlantis_box), box_id))

# view
ggplot()+geom_sf(data = match_area_box, aes(color = box_id))+geom_point()+theme_bw()

# make a key
area_key <- match_area_box %>%
  filter(!is.na(box_id)) %>%
  st_set_geometry(NULL)

dat3 <- dat2 %>%
  left_join(match_area_box, by = c('Statistical.Area'='STAT_AREA')) %>%
  filter(!is.na(box_id)) %>%
  group_by(box_id, Year, Month, Atlantis_fg) %>%
  summarise(Tot.Catch = sum(Catch.mt))

# let's ditch boundary boxes and island boxes - make those zeroes 
dat4 <- dat3 %>%
  left_join(atlantis_box %>% st_set_geometry(NULL) %>% select(box_id, botz, boundary), by = 'box_id') %>%
  rowwise() %>%
  mutate(Tot.Catch = ifelse(botz >= 0 | isTRUE(boundary), 0, Tot.Catch)) 

# Compare -----------------------------------------------------------------

# View: ratio over time per box; animation in space

# The landings from the fish tickets are part of what contributes to catch data, so direct comparison is not viable
# here we are interested instead in the relative spatial patterns
# Put the data frames in the same format
catch <- all_catch_long_box %>%
  mutate(Month = month(Date),
         Step = paste(Year, Month, sep = '-')) %>%
  select(Year, Month, Step, box_id, Species, Catch_mt_month) %>%
  set_names(c('Year', 'Month', 'Step', 'box_id', 'Species', 'Catch_mt')) %>%
  mutate(Set = 'Catch and SDM')

ticket <- dat4 %>%
  mutate(Step = paste(Year, Month, sep = '-')) %>%
  select(Year, Month, Step, box_id, Atlantis_fg, Tot.Catch) %>%
  set_names(c('Year', 'Month', 'Step', 'box_id', 'Species', 'Catch_mt')) %>%
  mutate(Set = 'Fish Tickets')

# for each species and time step, get the proportion in a given box over the total
catch <- catch %>%
  group_by(Step, Species) %>%
  mutate(Tot = sum(Catch_mt),
         Prop = Catch_mt/Tot) %>%
  select(Step, box_id, Species, Prop, Catch_mt, Set) %>%
  ungroup()

ticket <- ticket %>%
  group_by(Step, Species) %>%
  mutate(Tot = sum(Catch_mt),
         Prop = Catch_mt/Tot) %>%
  select(Step, box_id, Species, Prop, Catch_mt, Set) %>%
  ungroup()

# calculate ratio. If >1 catch+SDM overestimates the catch, if <1 it underestimates it
horizontal_join <- catch %>%
  full_join(ticket, by = c('Step','box_id','Species')) %>%
  mutate(Ratio = Prop.x / Prop.y,
         logRatio = log(Ratio)) 

# some cleaning
horizontal_join <- horizontal_join %>%
  select(Step, box_id, Species, logRatio, Catch_mt.x, Catch_mt.y) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  drop_na()

# view ratio over time
horizontal_join %>%
  ggplot()+
  geom_line(aes(x = as.Date(paste(Step, '01', sep = '-')), y = logRatio, color = box_id))+
  theme_bw()+
  facet_wrap(~Species, scales = 'free')
# not really informative, but it shows huge variations

# view catch vs catch
horizontal_join %>%
  ggplot()+
  geom_point(aes(x = Catch_mt.x, y = Catch_mt.y, color = box_id))+
  theme_bw()+
  facet_wrap(~Species, scales = 'free')
# again showing that the two approaches return very different results

# mean and CV of the logratio in space
# first need to get rid of NA and Inf
mean_cv <- horizontal_join %>%
  filter(is.finite(logRatio)) %>%
  group_by(box_id, Species) %>%
  summarise(Mean_logRatio = mean(logRatio),
            CV_logRatio = cv(logRatio)) %>%
  pivot_longer(-c(box_id, Species), names_to = 'Quantity', values_to = 'Value')

formap <- atlantis_box %>%
  full_join(mean_cv, by = 'box_id') %>%
  select(box_id, Species, Quantity, Value) %>%
  filter(!is.na(Quantity))

# we need to do this one species at a time
# do the major species only:

these_species <- c('POL', 'COD', 'ATF', 'POP', 'SBF', 'HAL', 'RFS', 'REX', 'FFS')

for(i in 1:length(these_species)){
  
  this_formap <- formap %>% filter(Species == these_species[i])
  
  xs <- split(this_formap,f = this_formap$Quantity)
  
  p1 <- ggplot(xs$Mean_logRatio) + 
    geom_sf(aes(fill = Value))+
    scale_fill_gradient2()+
    theme_bw()+
    facet_wrap(~Quantity, nrow=2)+
    labs(title = these_species[i])
  
  p2 <- ggplot(xs$CV_logRatio) + 
    geom_sf(aes(fill = Value))+
    scale_fill_gradient2()+
    theme_bw()+
    facet_wrap(~Quantity, nrow=2)
  
  p <- arrangeGrob(p1,p2)
  ggsave(paste0('../output/graphics/SDM_vs_tickets/', these_species[i], '.png'), p, width = 10, height = 7)
}

# varies between species, but all in all it shows that the spatial patterns from fish ticket landing data are
# different from the allocation of catch based on biomass distributions. So be warned
