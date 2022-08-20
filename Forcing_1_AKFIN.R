library(tidyverse)
library(lubridate)
library(data.table)
library(rbgm)
library(sf)
library(RColorBrewer)
library(viridis)
library(maps)
library(mapdata)

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

# remove halibut catch, because we use IPHC data #
catch_byf <- catch_byf %>% 
  filter(Species.Group != 'Halibut')

# Seasonal distributions S1-S4
s <- read.csv('../data/seasonal_distributions/seasonal_distribution.csv')
s_inv <- read.csv('../data/seasonal_distributions/seasonal_distribution_inverts.csv')

# group file
atlantis_fg <- read.csv('../data/GOA_Groups.csv', header = T) %>%
  filter(IsImpacted == 1) # only keep groups that are marked as IsImpacted by fisheries in the group.csv file

# A key from species to Atlantis group
catch_byf %>% select(Species.Group, Species) %>% distinct() %>% write.csv('../data/species_key.csv', row.names = F)
species_key <- read.csv('../data/species_key_FG.csv') %>% filter(!is.na(Atlantis_fg))

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
  filter(NMFS_AREA %in% (catch_byf %>% select(NMFS.Area) %>% distinct() %>% pull())) %>%
  st_transform(crs = atlantis_bgm$extra$projection)

# have a look
nmfs_sf %>% ggplot()+geom_sf()+geom_sf_label(aes(label = NMFS_AREA))

#################################################################################################
# Make some figures for the methods
# add some coastline
atlantis_box <- atlantis_bgm %>% box_sf()
atlantis_crs <- atlantis_bgm$extra$projection
coast <- maps::map(database = "worldHires", regions = c("Canada","US"), plot=FALSE, fill=TRUE)
coast_sf <- coast %>% st_as_sf(crs = 4326) %>% st_transform(crs=atlantis_crs)
nmfs_sf1 <- nmfs_sf %>% st_transform(crs=atlantis_crs)
this_bbox <- nmfs_sf1 %>% st_bbox()

p <- ggplot()+
  geom_sf(data = atlantis_box %>% filter(boundary == FALSE), aes(fill = botz), color = 'navy')+
  scale_fill_gradient(low="blue", high="white")+
  geom_sf(data = atlantis_box %>% filter(boundary == TRUE), fill = 'grey', color = 'navy')+
  geom_sf(data = (nmfs_sf1 %>% filter(AREA > 1e+10)), fill = NA, color = 'red', size = 1)+
  geom_sf(data = coast_sf, fill = 'lightgrey')+
  coord_sf(xlim = c(this_bbox$xmin, this_bbox$xmax), ylim = c(this_bbox$ymin, this_bbox$ymax))+
  geom_sf_label(data = (nmfs_sf1 %>% filter(AREA > 1e+10)), aes(label = NMFS_AREA), nudge_y = -100000, size = 5)+
  theme_bw()+
  theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12))+
  labs(fill = 'Box depth', x = '', y = '')
p

ggsave('../methods/images/nmfs.png', p, width = 9, height = 4)
# when bck on land we need to fix the colors
# make boundaries gray and use a better scale of blues for depth
#################################################################################################

area_key <- st_join(atlantis_pts, (nmfs_sf %>% select(NMFS_AREA))) %>%
  mutate(NMFS_AREA = ifelse(boundary == TRUE | botz == 0, NA, NMFS_AREA)) %>%
  filter(box_id < 92) # keep only boxes within the US
  
# Aggregate species catch to Atlantis groups based on key
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
# 
all_dates <- data.frame(expand.grid(all_years, all_months)) %>%
  arrange(Var1, Var2) %>%
  mutate(Month.Year = make_date(Var1, Var2)) %>%
  pull(Month.Year)

all_areas <- catch_fg %>% pull(NMFS.Area) %>% unique()

template <- data.frame(expand.grid('Month.Year' = all_dates, 'NMFS.Area' = all_areas))

# Prepare a sequence of all days in the data
# we will use this to divide monthly values from the data to daily values based on the number of  days
# we will then only keep the first of those values and make sure we use typeCatchts 1 in force.prm, to limit file size  
all_dates <- data.frame('Date' = seq(as.Date('1991-01-01'), as.Date('2020-12-31'), by = 'days')) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))

# write a function that:
# 1. Distributes the catch in each NMFS area among Atlantis boxes
# 2. Converts monthly catch in mt to catch in mg N s-1
# TODO: it is actually unclear in the manual whether this is mg or mg N. From Isaac's Matlab code and the Get_Imposed_Catch() routine, there are hints that this should be mg N

decompose_catch <- function(this_fg, catch_frame, group_frame, s_frame, si_frame, area_frame, days_frame, template_frame){
  
  # this_fg <- 'POL'
  # catch_frame = catch_fg
  # group_frame = atlantis_fg
  # s_frame = s
  # si_frame = s_inv
  # area_frame = area_key
  # days_frame = all_dates
  # template_frame = template
  
  print(paste('Doing', this_fg))
  
  this_name <- group_frame %>% filter(Code == this_fg) %>% pull(Name)
  this_stage <- 'A'
  
  this_catch <- catch_frame %>% filter(Atlantis_fg == this_fg) %>% select(-Atlantis_fg)
  
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
    full_join(this_catch, by = c('Month.Year', 'NMFS.Area')) %>%
    mutate(Catch_mt = coalesce(Catch_mt, 0)) %>% 
    full_join((area_frame %>% st_set_geometry(NULL)), by = c('NMFS.Area' = 'NMFS_AREA')) 
  
  # now join proportions from s by box_id
  this_catch_box <- this_catch_complete %>% 
    left_join(this_dist, by = 'box_id') %>%
    group_by(Month.Year, NMFS.Area) %>%
    mutate(s_by_NMFS_area = sum(s),
           corr = s/s_by_NMFS_area) %>% # s/sum of s within a NMFS area
    ungroup() %>%
    mutate(Catch_box_mt = Catch_mt * corr) %>% # multiply catch in the NMFS area by the proportion that goes to each box in the NMFS area
    select(Month.Year, box_id, Catch_box_mt) %>% 
    complete(Month.Year, box_id) %>% # add boundary boxes
    mutate(Catch_box_mt = coalesce(Catch_box_mt, 0)) %>% # add 0 catch in boundary boxes
    filter(!is.na(Month.Year)) %>%
    mutate(Year = year(Month.Year), Month = month(Month.Year)) # add time columns for later
  
  # now break down the catch in time, aiming for daily values
  daily_catch <- this_catch_box %>% 
    full_join(days_frame, by = c('Year', 'Month')) %>%
    group_by(Year, Month, box_id) %>%
    mutate(Catch_box_day_mt = Catch_box_mt / length(Day), # get catch in mt by day by box
           Catch_box_day_mgs = Catch_box_day_mt * 1e9 / (60*60*24) / (20 * 5.7)) %>% # convert from mt to mg s-1
    ungroup() %>%
    filter(Day == 1) %>% # only keep the first day of each month
    select(box_id, Date, Catch_box_day_mgs) %>%
    arrange(box_id, Date) %>%
    mutate(Code = this_fg)
  
  return(daily_catch)
}

all_catch <- rbindlist(lapply(all_fg, decompose_catch, catch_frame = catch_fg, 
                     group_frame = atlantis_fg, 
                     s_frame = s, 
                     si_frame = s_inv, 
                     area_frame = area_key,
                     days_frame = all_dates, 
                     template_frame = template))

# # test
# all_catch %>% filter(box_id == 99) %>% pull(Catch_box_day_mgs) %>% sum()

# View --------------------------------------------------------------------
# make time series plots to visualize this and to compare with original data

all_catch %>%
  filter(Code == 'COD') %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  full_join(all_dates, by = c('Year', 'Month')) %>%
  group_by(Year, Month, Code) %>%
  summarise(Catch_mt = sum(Catch_box_day_mgs * 5.7 * 20 * 60 * 60 * 24 / 1e9)) %>%
  ungroup() %>%
  mutate(Date = make_date(year = Year, month = Month)) %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  ggplot(aes(x = Date, y = Catch_mt))+
  geom_line()+
  theme_bw()+
  labs(title = 'From catch.ts')

catch_byf %>%
  filter(Species.Group == 'Pacific Cod') %>%
  select(Week.Ending.Date, Catch..mt.) %>%
  mutate(Year = year(Week.Ending.Date),
         Month = month(Week.Ending.Date)) %>%
  group_by(Year, Month) %>%
  summarise(Catch_mt = sum(Catch..mt.)) %>%
  ungroup() %>%
  mutate(Date = make_date(year = Year, month = Month)) %>%
  ggplot(aes(x = Date, y = Catch_mt))+
  geom_line()+
  theme_bw()+
  labs(title = 'From AKFIN')

# make one for plots to show, by year 
# this is for ESSAS
tier3_key <- data.frame(species = 
                          c("Pollock",
                            "Pacific cod",
                            "Sablefish",
                            "Northern and Southern rock sole",
                            "Flathead sole",
                            "Dover",
                            "Northern Rockfish",
                            "POP",
                            "Dusky rockfish",
                            "Rougheye Blackspotted rockfish",
                            "Rex Sole",
                            "Arrowtooth flounder"),
                        Code = 
                          c('POL','COD','SBF','FFS','FHS','FFD','RFS','POP','RFP','RFD','REX','ATF'))

these_species <- tier3_key$Code
colourCount = length(these_species)
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

all_catch %>%
  filter(Code %in% these_species) %>%
  # filter(Code == 'POL') %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  full_join(all_dates, by = c('Year', 'Month')) %>%
  group_by(Year, Code) %>%
  summarise(Catch_mt = sum(Catch_box_day_mgs * 5.7 * 20 * 60 * 60 * 24 / 1e9)) %>%
  ungroup() %>%
  #mutate(Date = make_date(year = Year, month = Month)) %>%
  left_join(tier3_key, by = 'Code') %>%
  ggplot(aes(x = Year, y = Catch_mt, fill = species))+
  geom_area()+
  scale_fill_manual(values=getPalette(colourCount))+
  scale_colour_manual(values=getPalette(colourCount))+
  scale_x_continuous(breaks = seq(1990,2020,5))+
  theme_bw()+
  labs(x = 'Year', y = 'Catch (mt)', fill = '', title = 'Catch in the Gulf of Alaska')

# Write catch.ts file -----------------------------------------------------

# now write out a ts file for one box
# however, we will have to add a lot of species here

#remove all existing files first
dir.create('../output/AKFIN')
all_files <- list.files('../output/AKFIN', full.names = T)
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
  header_file <- paste0('../output/AKFIN/catch', this_box, '.ts')
  
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
    cat(paste0("## COLUMN", i+1, ".units mg N s-1\n"), file = header_file, append = T)
    cat(paste0("## COLUMN", i+1, ".missing_value 0\n"), file = header_file, append = T)
    cat("##\n", file = header_file, append = T)
  }
  
  write.table(ttt, file = header_file, append = T, sep = " ", row.names = FALSE, col.names = FALSE)
}



# Plot for ESSAS ----------------------------------------------------------

# library(gifski)
# 
# atlantis_box <- atlantis_bgm %>% box_sf() %>% select(box_id)
# 
# catch_pol <- all_catch %>% 
#   filter(Code == 'POL') %>%
#   mutate(Catch_mt = Catch_box_day_mgs * 5.7 * 20 * 60 * 60 * 24 / 1e9,
#          Year = year(Date),
#          Month = month(Date)) %>%
#   group_by(box_id, Year, Month) %>%
#   summarise(Catch = sum(Catch_mt)) %>%
#   ungroup() 
# 
# catch_pol <- atlantis_box %>% left_join(catch_pol, by = 'box_id') %>% filter(!is.na(Year))
# 
# all_years <- unique(catch_pol$Year)
# all_months <- unique(catch_pol$Month)
# 
# for(y in 1:length(all_years)){
#   for(m in 1:length(all_months)){
#     
#     p <- catch_pol %>%
#       filter(Year == all_years[y], Month == all_months[m]) %>%
#       ggplot()+
#       geom_sf(aes(fill = log1p(Catch), color = NULL))+
#       scale_fill_viridis(limits = c(0, 6))+
#       theme_bw()+
#       labs(title = paste('Walleye pollock GOA catch in', all_years[y], '-',  all_months[m]), fill = 'Log catch (mt)')
#     
#     ggsave(paste0('../output/GIF/',all_years[y],'-',all_months[m],'.png'), p, width = 9, height = 4, dpi = 300)
#     
#   }
# }
# 
# details = file.info(list.files(path='../output/GIF/', pattern = '*.png', full.names = TRUE))
# details = details[with(details, order(as.POSIXct(mtime))), ]
# files = rownames(details)
# 
# gifski(files, gif_file = "animation.gif", width = 900, height = 400, delay = 0.2)
