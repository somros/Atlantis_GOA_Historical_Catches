# Alberto Rovellini
# August 26 2022
# Code to explore the catch length composition from observer data, convert it to age composition, and work out age composition of the catch
# The goal is to build the CatchTS_agedistribXXX parameters for groups without selectivity patterns from stock assessments (e.g. Tier >3)
# We only need this for vertebrates

# NORPAC Lenght Report data retrieved from AKFIN Answers on August 26 2022
# https://akfinbi.psmfc.org/analytics/saw.dll?Dashboard&PortalPath=%2fshared%2fStock%20Assessment%2f_portal%2fStock%20Assessment&Page=NORPAC%20Length%20Report&Done=Dashboard%26PortalPath%3d%252fshared%252fStock%2520Assessment%252f_portal%252fStock%2520Assessment%26Page%3dObserver%2520and%2520EM%2520Data%26ViewState%3duggn42gl9p6okl96jimfkrtb0a

# Fields:
# Year: 1990:2020
# FMP Area: GOA
# FMP Subarea: CG, SE, WG, WOC, WY
# NMFS Area: --Select Value--
# Gear Code: --Select Value--
# Gear Description: --Select Value--
# Performance: --Select Value--
# Performance Description: --Select Value--
# Species Code: --Select Value--
# Species Name:ALASKA PLAICE,ALASKA SKATE,ALEUTIAN SKATE,ARROWTOOTH FLOUNDER,ATKA MACKEREL,BERING SKATE,BIG SKATE,BIGMOUTH SCULPIN,BLACK ROCKFISH,BLACKTAIL SNAILFISH GROUP,BLUE SHARK,BOCACCIO ROCKFISH,BUTTER SOLE,BUTTERFLY SKATE,CALIFORNIA TONGUEFISH,CANARY ROCKFISH,CHINOOK SALMON,CHUM SALMON,COHO SALMON,COMMANDER SKATE,DARK BLOTCHED ROCKFISH,DARK ROCKFISH,DEEPSEA SKATE,DEEPSEA SOLE,DOVER SOLE,DUSKY ROCKFISH,DUSKY ROCKFISH UNIDENTIFIED,ENGLISH SOLE,FLATHEAD SOLE,GIANT GRENADIER,GREAT SCULPIN,GRENADIER UNIDENTIFIED,HARLEQUIN ROCKFISH,JACK MACKEREL,KAMCHATKA FLOUNDER,KELP GREENLING,LINGCOD,LONGHEAD DAB,LONGNOSE LANCETFISH,LONGNOSE SKATE,LONGSPINE THORNYHEAD ROCKFISH,MUD SKATE,NORTHERN ROCK SOLE,NORTHERN ROCKFISH,OLIVE ROCKFISH,PACIFIC COD,PACIFIC FLATNOSE,PACIFIC GRENADIER,PACIFIC HAKE,PACIFIC HALIBUT,PACIFIC HERRING,PACIFIC MACKEREL,PACIFIC OCEAN PERCH,PACIFIC SLEEPER SHARK,PACIFIC TOMCOD,PARALOMIS MULTISPINA,PARALOMIS VERILLI,PETRALE SOLE,PINK SALMON,PLAIN SCULPIN,POLLOCK,QUILLBACK ROCKFISH,RED BANDED ROCKFISH,REDSTRIPE ROCKFISH,REX SOLE,ROCK SOLE UNIDENTIFIED,ROUGHEYE ROCKFISH,ROUGHTAIL SKATE,SABLEFISH (BLACKCOD),SAFFRON COD,SALMON SHARK,SAND SOLE,SHARPCHIN ROCKFISH,SHORTBELLY ROCKFISH,SHORTRAKER ROCKFISH,SHORTRAKER/ROUGHEYE ROCKFISH,SHORTSPINE THORNYHEAD,SILVERGRAY ROCKFISH,SKATE UNIDENTIFIED,SNAILFISH UNIDENTIFIED,SOCKEYE SALMON,SOUPFIN SHARK,SOUTHERN ROCK SOLE,SPINY DOGFISH SHARK,SPLITNOSE ROCKFISH,STARRY FLOUNDER,THORNYHEAD ROCKFISH UNIDENT,THRESHER SHARK,TIGER ROCKFISH,WARTY SCULPIN,WHITEBLOTCHED SKATE,WHITEBROW SKATE,WIDOW ROCKFISH,YELLOW IRISH LORD,YELLOWEYE ROCKFISH,YELLOWFIN SOLE,YELLOWMOUTH ROCKFISH,YELLOWTAIL ROCKFISH
# Species Sex: --Select Value--
# Length Greater Than: --

library(tidyverse)

# Read data ---------------------------------------------------------------
dat <- read.csv('../data/AKFIN/Observer/norpac_length_report.csv', skip = 8) # NORPAC Lenght Report
atlantis_fg <- read.csv('../data/GOA_Groups.csv')
lh_pars <- read.csv('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/build_init_prm_10COHORTS/data/life_history_parameters.csv')
tier3_selex <- read.csv('../data/CatchTS_agedistribXXX.csv')

# Data exploration --------------------------------------------------------
# What NMFS areas do we have?
unique(dat$NMFS.Area) # 630 610 620 640 621 631 650 # 621 used to indicate Shelikof St. Not sure about 631

# How many data points do we have per species?
dat %>% group_by(Species.Name) %>% tally() %>% arrange(desc(n))

# Map NORPAC species to Atlantis groups -----------------------------------
# For all the Tier 3 species, we already have selectivity patterns (by age)

# Make a key from these to Atlantis groups
species <- dat %>% select(Species.Name) %>% distinct() %>% arrange(Species.Name)
write.csv(species, '../data/NORPAC_Atlantis_key_TMP.csv', row.names = F)

# read in the version with the Atlantis groups
key <- read.csv('../data/NORPAC_Atlantis_key.csv')

# Calculate catch at age and proportions ----------------------------------
# Aggregate catches by Atlantis group
dat1 <- dat %>%
  left_join(key, by = 'Species.Name') %>%
  select(Year, Code, Length..cm., Frequency) %>%
  left_join(lh_pars %>% select(Code, Linf_FUNC, k_FUNC, a_FUNC, b_FUNC, tmax_Max_age), by = 'Code') %>%
  mutate(Age = round(-log((Linf_FUNC - Length..cm.) / Linf_FUNC) / k_FUNC), # might want something different than round
         Weight_g = a_FUNC * Length..cm. ^ b_FUNC,
         Weight_age_g = Weight_g * Frequency) %>%
  rowwise() %>%
  mutate(Age = ifelse(is.finite(Age) & Age < tmax_Max_age, Age, tmax_Max_age)) %>% # fish length >= Linf results in Inf or NaN ages. For those, set them to max age
  ungroup()

# assign ages to age classes
dat2 <- dat1 %>%
  left_join(lh_pars %>% select(Code, num_age_class), by = 'Code') %>%
  mutate(Age_class_size = tmax_Max_age / num_age_class) %>%
  rowwise() %>%
  mutate(Age_class = findInterval(x = Age, vec = Age_class_size * (1:num_age_class), left.open = T)+1) %>%
  group_by(Code, Year, Age_class) %>%
  summarise(Weight_age_class_g = sum(Weight_age_g)) # adding up the weight of all fish of a size class

dat3 <- dat2 %>%
  group_by(Code, Year) %>%
  mutate(Species_annual_catch_g = sum(Weight_age_class_g)) %>%
  group_by(Code, Year, Age_class) %>%
  mutate(Age_class_annual_catch_g = sum(Weight_age_class_g)) %>% # sum over a year
  ungroup() %>%
  mutate(Catch_prop_by_year = Age_class_annual_catch_g / Species_annual_catch_g) %>%
  distinct()

# fill missing age classes
max_age_class <- max(dat3$Age_class)

dat4 <- dat3 %>%
  complete(Code, Year, Age_class) %>%
  select(Code, Year, Age_class, Catch_prop_by_year) %>%
  left_join(lh_pars %>% select(Code, num_age_class), by = 'Code') %>%
  rowwise() %>%
  mutate(Catch_prop_by_year = ifelse(Age_class <= num_age_class & is.na(Catch_prop_by_year), 0, Catch_prop_by_year)) %>%
  ungroup() %>%
  drop_na()

# sanity check
dat4 %>% group_by(Year, Code) %>% summarise(check = sum(Catch_prop_by_year)) %>% pull(check) 

# get long-term averages, but we need to weigh this by number of caught inds per year
# e.g., say one year they caught a lot of age 4 pollock because it was a particularly strong age class. This should be accounted for as it may skew the average

# first, we need to exclude years where a group was not caught at all (i.e. does not appear in the data)
# this is mostly a concern for species that do not show up often
dat5 <- dat4 %>%
  group_by(Code, Year) %>%
  mutate(Check = sum(Catch_prop_by_year)) %>%
  ungroup() %>%
  filter(Check > 0) %>%
  select(-Check) %>%
  group_by(Code, Age_class) %>%
  summarise(Prop = mean(Catch_prop_by_year))

dat5 %>% group_by(Code) %>% summarise(check = sum(Prop))

# view
dat5 %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  ggplot()+
  geom_bar(aes(x = Age_class, y = Prop), stat = 'identity')+
  theme_bw()+
  facet_wrap(~Name)

# many species have a peak in the last age class. This can mean two things:
# 1. The fishery selects for the largest and oldest individuals and these make up for the bulk of the catch
# 2. The calculations are incorrect - for example we overestimate the weight of the large catch, or we overestimate the age of long fish
# Flatfish offers some egregious examples, both in complexes (FFS and FFD) and single species (REX and FHS)
# The good news is that for some of these we have selex curves from stock assessments

dat6 <- dat5 %>%
  filter(Code %in% setdiff(unique(dat5$Code), unique(tier3_selex$Code)))

dat6 %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  ggplot()+
  geom_bar(aes(x = Age_class, y = Prop), stat = 'identity')+
  theme_bw()+
  facet_wrap(~Name)

# there are also cases where this approach is misleading. For example, for sharks, the fact that bigger and older sharks
# have not been caught / recorded does not mean that the fishery does not select for them
# need to find a way to filter the approach: require a certain number of data points, for example?

write.csv(dat6 %>% select(-Age_class), '../data/tier4plus_selex.csv', row.names = F)
