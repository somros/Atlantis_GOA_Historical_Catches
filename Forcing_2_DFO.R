# DFO catch data from Sean Anderson
# this will be for boxes in BC
# temporal resolution is lower than in AKFIN data

library(tidyverse)
library(rbgm)
library(sf)
library(maps)
library(mapdata)
library(lubridate)
library(data.table)
library(maps)
library(mapdata)

select <- dplyr::select

#read data
catch_dfo <- readRDS('../data/DFO/goa-clim-bc-groundfish-2020.rds') # dfo catch data
# drop halibut, we will use catch estimates from IPHC
catch_dfo <- catch_dfo %>% filter(species_common_name != 'pacific halibut')

# group file
atlantis_fg <- read.csv('../data/GOA_Groups.csv', header = T) %>%
  filter(IsImpacted == 1) # only keep groups that are marked as IsImpacted by fisheries in the group.csv file

# BGM and spatial
atlantis_bgm <- read_bgm('../data/GOA_WGS84_V4_final.bgm') # bgm
atlantis_box <- atlantis_bgm %>% box_sf()
dfo_sf <- read_sf('../data/DFO_management_areas/BCMajorAreas/Major.shp') # DFO areas
s <- read.csv('../data/seasonal_distributions/seasonal_distribution.csv') # Seasonal distributions S1-S4
s_inv <- read.csv('../data/seasonal_distributions/seasonal_distribution_inverts.csv') # Seasonal distributions S1-S4

# make a species to FG key, based on previous work
write.csv(data.frame('cn' = unique(catch_dfo$species_common_name), 
                     'sn' = unique(catch_dfo$species_science_name)),
          '../data/key_dfo.csv', row.names = FALSE)
key_dfo <- read.csv('../data/key_dfo_fg.csv')

#tag the functional group codes and names to the catch dfo data
catch_dfo <- catch_dfo %>%
  left_join(key_dfo %>% select(-cn), by = c('species_science_name'='sn')) %>%
  left_join(atlantis_fg %>% select(Code, Name), by = c('fg'='Code'))

# areas:
dfo_areas <- unique(catch_dfo$major_stat_area_description)
# attribute Atlantis boxes to the BC areas based on inside point. We should only have 5ABCDE in the GOA model domain
# add an area column to the catch data
catch_dfo <- catch_dfo %>%
  mutate(AREA = substr(major_stat_area_description, 1, 2))

# make a dataframe with the inside points for each Atlantis box
atlantis_pts <- atlantis_box %>% 
  st_set_geometry(NULL) %>%
  select(box_id, insideX, insideY, boundary, botz) %>%
  st_as_sf(coords = c('insideX', 'insideY'), crs = atlantis_bgm$extra$projection)

# turn the DFO spatial frame to the right CRS and drop areas that are not in the catch
dfo_sf <- dfo_sf %>% 
  filter(MAJOR %in% (catch_dfo %>% select(AREA) %>% distinct() %>% pull())) %>%
  st_transform(crs = atlantis_bgm$extra$projection)

# make a key box to area
area_key <- st_join(atlantis_pts, (dfo_sf %>% select(MAJOR))) %>%
  mutate(MAJOR = ifelse(boundary == TRUE | botz == 0, NA, MAJOR)) %>%
  filter(box_id > 91) # keep only boxes within BC

#################################################################################################
# Make some figures for the methods
# add some coastline
dfo_sf1 <- dfo_sf %>% filter(MAJOR %in% c('5A', '5B', '5C', '5D', '5E'))
atlantis_box <- atlantis_bgm %>% box_sf()
atlantis_crs <- atlantis_bgm$extra$projection
coast <- maps::map(database = "worldHires", regions = c("Canada","US"), plot=FALSE, fill=TRUE)
coast_sf <- coast %>% st_as_sf(crs = 4326) %>% st_transform(crs=atlantis_crs)
this_bbox <- dfo_sf1 %>% st_bbox()

p <- ggplot()+
  geom_sf(data = atlantis_box %>% filter(boundary == FALSE), aes(fill = botz), color = 'navy')+
  scale_fill_gradient(low="blue", high="white")+
  geom_sf(data = atlantis_box %>% filter(boundary == TRUE), fill = 'grey', color = 'navy')+
  geom_sf(data = dfo_sf1, fill = NA, color = 'red', size = 1)+
  geom_sf(data = coast_sf, fill = 'grey')+
  coord_sf(xlim = c(this_bbox$xmin, this_bbox$xmax), ylim = c(this_bbox$ymin, this_bbox$ymax))+
  geom_sf_label(data = dfo_sf1, aes(label = MAJOR), nudge_y = -10000, size = 5)+
  theme_bw()+
  theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12))+
  labs(fill = 'Box depth (m)', x = '', y = '')
p

ggsave('../methods/images/dfo.png', p, width = 6, height = 4)

# NOTE: in 5A we have only boundary boxes and a sliver of 2 dynamic boxes, we can probably approximate those as if they belong to 5B
# when bck on land we need to fix the colors
#################################################################################################

# prepare the data: group by year, area, fg, and sum the catch
catch_dfo_fg <- catch_dfo %>%
  group_by(year, AREA, fg, Name) %>%
  summarize(Catch_kg = sum(catch_kg, na.rm = T)) %>%
  ungroup() %>%
  set_names(c('Year', 'Area', 'Code', 'Name', 'Catch_kg'))

# 1991-1995
# DFO catch data only starts from 1996, for now let's assume catch in 1991-1995 is identical to 1996, and constant
dfo_1996 <- catch_dfo_fg %>% filter(Year == 1996)
yr <- rep(1991:1995, each = nrow(dfo_1996))
filler <- data.frame(Year = yr, dfo_1996[rep(seq_len(nrow(dfo_1996)), 5), -1])

#TODO: review this assumption - it may not hold for previous years
catch_dfo_fg <- rbind(filler, catch_dfo_fg) # paste to real data  

# these are the groups in the DFO catch data
all_fg <- catch_dfo_fg %>% pull(Code) %>% unique() 

# build a complete time series with all months and DFO areas
all_years <- unique(catch_dfo$year)
all_areas <- catch_dfo_fg %>% pull(Area) %>% unique()
template <- data.frame(expand.grid('Year' = all_years, 'Area' = all_areas)) # the DFO version does not have months

# Prepare a sequence of all days in the data
# we will use this to divide monthly values from the data to daily values based on the number of  days
# we will then only keep the first of those values and make sure we use typeCatchts 1 in force.prm, to limit file size  
# we do this for DFO annual data too because we get to divide by the correect number of days in a year, accounting for leap years etc.
# this should minimize issues with the time step variable in the catch time series files
all_dates <- data.frame('Date' = seq(as.Date('1991-01-01'), as.Date('2020-12-31'), by = 'days')) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))

decompose_catch <- function(this_fg, catch_frame, group_frame, s_frame, si_frame, area_frame, days_frame, template_frame){
  
  # this_fg <- 'ATF'
  # catch_frame = catch_dfo_fg
  # group_frame = atlantis_fg
  # s_frame = s
  # si_frame = s_inv
  # area_frame = area_key
  # days_frame = all_dates
  # template_frame = template
  
  print(paste('Doing', this_fg))
  
  this_name <- group_frame %>% filter(Code == this_fg) %>% pull(Name)
  this_stage <- 'A'
  
  this_catch <- catch_frame %>% filter(Code == this_fg) %>% select(-Code)
  
  if((group_frame %>% filter(Code == this_fg) %>% pull(NumStages)) > 1){
    distribution <- s_frame
  } else {
    distribution <- si_frame
  }
  
  this_dist <- distribution %>% select(contains(paste(this_name, this_stage, sep = '_'))) %>%
    mutate(box_id = 0:(nrow(.)-1)) %>%
    select(box_id, contains('S1')) %>%
    set_names(c('box_id','s'))
  
  # join this_catch with the template, and fill the missing values with 0, then join with Atlantis boxes for each NMFS area
  this_catch_complete <- template_frame %>% 
    full_join(this_catch, by = c('Year', 'Area')) %>%
    mutate(Catch_kg = coalesce(Catch_kg, 0)) %>% 
    full_join((area_frame %>% st_set_geometry(NULL)), by = c('Area' = 'MAJOR')) %>%
    filter(!is.na(box_id))
  
  # now join proportions from s by box_id
  this_catch_box <- this_catch_complete %>% 
    left_join(this_dist, by = 'box_id') %>%
    group_by(Year, Area) %>%
    mutate(s_by_area = sum(s),
           corr = s/s_by_area) %>% # s/sum of s within a NMFS area
    ungroup() %>%
    mutate(Catch_box_kg = Catch_kg * corr) %>% # multiply catch in the DFO area by the proportion that goes to each box in the DFO area
    select(Year, box_id, Catch_box_kg) %>% 
    complete(Year, box_id) %>% # add boundary boxes
    mutate(Catch_box_mt = coalesce(Catch_box_kg, 0)/1000) %>% # add 0 catch in boundary boxes
    filter(!is.na(Year)) %>%
    select(-Catch_box_kg)
  
  # now break down the catch in time, aiming for daily values
  daily_catch <- this_catch_box %>% 
    full_join(days_frame, by = c('Year')) %>%
    group_by(Year, box_id) %>%
    mutate(Catch_box_day_mt = Catch_box_mt / length(Day), # get catch in mt by day by box
           Catch_box_day_mgs = Catch_box_day_mt * 1e9 / (60*60*24) / (20 * 5.7)) %>% # convert from mt to mg s-1
    ungroup() %>%
    filter(Day == 1) %>% # only keep the first day of each month, to make the files smaller
    select(box_id, Date, Catch_box_day_mgs) %>%
    arrange(box_id, Date) %>%
    mutate(Code = this_fg)
  
  return(daily_catch)
  
}

all_catch <- rbindlist(lapply(all_fg, decompose_catch, catch_frame = catch_dfo_fg, 
                              group_frame = atlantis_fg, 
                              s_frame = s, 
                              si_frame = s_inv, 
                              area_frame = area_key,
                              days_frame = all_dates, 
                              template_frame = template))

# View --------------------------------------------------------------------
# make time series plots to visualize this and to compare with original data

all_catch %>%
  filter(Code == 'POL') %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Code) %>%
  summarise(Catch_mt = sum(Catch_box_day_mgs * 5.7 * 20 * 60 * 60 * 24 / 1e9)) %>%
  ungroup() %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  ggplot(aes(x = Year, y = Catch_mt))+
  geom_line()+
  theme_bw()+
  labs(title = 'From catch.ts')

catch_dfo %>%
  filter(species_common_name == 'walleye pollock') %>%
  filter(AREA %in% c('5B','5C','5D','5E')) %>%
  group_by(year) %>%
  summarize(catch_kg = sum(catch_kg, na.rm = T)) %>%
  ungroup() %>%
  select(year, catch_kg) %>%
  ggplot(aes(x = year, y = catch_kg/1000))+
  geom_line()+
  theme_bw()+
  labs(title = 'From DFO')

# make one for plots to show, by year 
all_catch %>%
  filter(Code %in% c('COD', 'ATF', 'POL' , 'POP', 'SBF', 'FHS', 'REX', 'FFS', 'FFD', 'RFS', 'RFP', 'RFD', 'THO')) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Code) %>%
  summarise(Catch_mt = sum(Catch_box_day_mgs * 5.7 * 20 * 60 * 60 * 24 / 1e9)) %>%
  ungroup() %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  ggplot(aes(x = Year, y = Catch_mt, fill = Name))+
  geom_area()+
  theme_bw()+
  labs(x = 'Year', y = 'Catch (mt)')

# Write catch.ts file -----------------------------------------------------

# now write out a ts file for one box
# however, we will have to add a lot of species here

#remove all existing files first
dir.create('../output/DFO')
all_files <- list.files('../output/DFO', full.names = T)
lapply(all_files, file.remove)

for(b in 1:length(unique(all_catch$box_id))){
  
  this_box <- unique(all_catch$box_id)[b]
  
  t <- all_catch %>% filter(box_id == this_box)
  # turn date into days from origin
  origin <- t$Date[1]
  t <- t %>% 
    mutate(Time = as.numeric(difftime(Date, origin, units = 'days'))) %>%
    select(Time, Code, Catch_box_day_mgs)
  
  # expand to all functional groups
  # make a template
  tmp <- expand.grid('Time' = unique(t$Time), 'Code' = unique(atlantis_fg$Code))
  # join it to the data
  tt <- tmp %>% 
    left_join(t, by = c('Time', 'Code')) %>%
    mutate(Catch_box_day_mgs = coalesce(Catch_box_day_mgs, 0))
  
  # now have it as horizontal table
  ttt <- tt %>% pivot_wider(names_from = Code, values_from = Catch_box_day_mgs)
  
  # make header
  header_file <- paste0('../output/DFO/catch', this_box, '.ts')
  
  cat(paste("# Historical catch time series file for Atlantis GOA box", this_box, ", years 1991-2020\n", sep = " "), file = header_file, append = T)
  cat("#\n", file = header_file, append = T)
  cat(paste0("## COLUMNS ", ncol(ttt), "\n"), file = header_file, append = T)
  cat("##\n", file = header_file, append = T)
  
  # write time column
  cat("## COLUMN1.name Time\n", file = header_file, append = T)
  cat("## COLUMN1.long_name Time\n", file = header_file, append = T)
  cat("## COLUMN1.units days since 1991-01-01 12:00:00\n", file = header_file, append = T)
  cat("## COLUMN1.missing_value 0\n", file = header_file, append = T)
  cat("##\n", file = header_file, append = T)
  
  all_fg <- atlantis_fg$Code
  
  # write column for each group
  for(i in 1:length(all_fg)){
    cat(paste0("## COLUMN", i+1, ".name ", all_fg[i], "\n"), file = header_file, append = T)
    cat(paste0("## COLUMN", i+1, ".long_name ", all_fg[i], "\n"), file = header_file, append = T)
    cat(paste0("## COLUMN", i+1, ".units mg s-1\n"), file = header_file, append = T)
    cat(paste0("## COLUMN", i+1, ".missing_value 0\n"), file = header_file, append = T)
    cat("##\n", file = header_file, append = T)
  }
  
  write.table(ttt, file = header_file, append = T, sep = " ", row.names = FALSE, col.names = FALSE)
}
