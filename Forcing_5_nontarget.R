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

eul <- catch_fg %>% filter(Atlantis_fg == 'EUL')
cap <- catch_fg %>% filter(Atlantis_fg == 'CAP')
fos <- catch_fg %>% filter(Atlantis_fg == 'FOS')
san <- catch_fg %>% filter(Atlantis_fg == 'SAN') # no sandlance at all
sqd <- catch_fg %>% filter(Atlantis_fg == 'SQD') # sqd 

# pull forage fish from the nontarget species

forage_key <- data.frame(Species =  c("Capelin", 
            "Pacific Sand lance", 
            #"Surf smelt", 
            #"Smelt (Family Osmeridae)", 
            "Eulachon", 
            "Deep sea smelts (bathylagidae)", 
            "Lanternfishes (myctophidae)" 
            #, "Other osmerids"
            ),
            Name = c('Capelin','Sandlance','Eulachon','Forage_slope','Forage_slope'))

forage_catch <- nontarget %>% 
  filter(Species.Group.Name %in% (unique(forage_key$Species))) %>%
  select(Week.Ending.Date, NMFS.Area, Species.Group.Name, Nontarget.Estimate..mt.) %>%
  mutate(Date = as.Date(Week.Ending.Date),
         Year = year(Date)) %>%
  select(-Week.Ending.Date)

# Temporal scale. Data comes with ending day of the week of the fishing event. However, aggregating by month is going to leave a lot of gaps
# for some species in some statistical areas, that we'll have to impute or pad with 0's
# Just calc total catch by year and then assume constant removals for each day of that year

forage_catch_1 <- forage_catch %>%
  group_by(Species.Group.Name, NMFS.Area, Year) %>%
  summarise(Catch_mt = sum(Nontarget.Estimate..mt.))

# aggregate species into Atlantis groups


# now enter spatial information
# mind that some species (like Capelin) have non-zero s values in island boxes
# TODO: amend this, but for now fudge it here

s_forage <- s %>%
  select(grep(paste(unique(forage_key$Name), collapse = "|"), names(s), ignore.case = T)) 

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

