# Alberto Rovellini
# August 25 2022
# For Tier 3 GOA stocks, map selectivity patterns from stock assessments to the 10 age classes of Atlantis groups
# These will be used to produce the parameter vectors CatchTS_agedistribXXX for the harvest.prm files

library(readxl)
library(tidyverse)
library(data.table)
library(RColorBrewer)

# read in life history parameters
lh_pars <- read.csv('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/build_init_prm_10COHORTS//data/life_history_parameters.csv')

# read in selectivity patterns from Tier 3 stock assessments (GOA)
dat <- read_excel('../data/Selectivity patterns tier 3 GOA stocks.xlsx', sheet = 2, skip = 1)

which_data <- grep('\\...',colnames(dat))
these_species <- colnames(dat)[!grepl('\\...',colnames(dat))]

dat <- dat[,which_data] %>%
  set_names(these_species) %>%
  mutate(age = 1:nrow(.))

# pivot
dat_long <- dat %>% 
  pivot_longer(-age, names_to = 'species', values_to = 'selex') %>%
  arrange(species, age) %>%
  drop_na()

# fudge age classes so that they are multiples of 10

new_dat <- list()
for(i in 1:length(these_species)){
  
  this_dat <- dat_long %>% filter(species == these_species[i])
  this_maxage <- max(this_dat$age)
  this_selex <- this_dat$selex
  
  closest_10 <- round(this_maxage, -1) # round to closest 10
  
  if(closest_10 < this_maxage) {
    new_selex <- this_selex[1:closest_10]
  } else {
    new_selex <- c(this_selex, rep(this_selex[length(this_selex)], abs(this_maxage-closest_10)))
  }
  
  new_dat[[i]] <- data.frame(age = 1:closest_10, species = these_species[i], selex = new_selex)
  
}

dat_long_10 <- rbindlist(new_dat) # replace the dat_long above

# Map species to Atlantis funtional groups
# Some are 1:1, some species are part of a functional group (FFS, RFS, RFP, RFD)
# Because the species here are the most representative (abundant) in their respective groups, map them to the functional group, but remember that this should be revisited to reflect selex patterns and life histories of the other species

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

dat_long_10 <- dat_long_10 %>%
  left_join(tier3_key, by = 'species')

# add proportional age composition
dat_long_10 <- dat_long_10 %>%
  left_join(lh_pars %>% select(Code, M_FUNC), by = 'Code') %>%
  mutate(exponential_decay = exp(-M_FUNC*(age-1))) %>%
  group_by(Code) %>%
  mutate(proportional_age_distribution = exponential_decay/ sum(exponential_decay)) %>%
  ungroup()

# add age class column
# when averaging selex, weigh it by the proportional number of individuals in each annual cohorts
# That was we give more weight to more abundant annual cohorts within the age class
dat_long_10 <- dat_long_10 %>%
  group_by(species) %>%
  mutate(age_class = rep(1:10, each = max(age)/10)) %>%
  ungroup() %>%
  group_by(Code, species, age_class) %>%
  summarise(selex_age_class = weighted.mean(selex, proportional_age_distribution)) %>%
  ungroup()

# write out for calculations
write.csv(dat_long_10,'../data/selex_10_age_classes.csv', row.names = F)

# make plots
these_species <- tier3_key$species
colourCount = length(these_species)
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

p <- dat_long %>%
  filter(species %in% these_species) %>%
  ggplot(aes(x = age, y = selex))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=getPalette(colourCount))+
  #scale_x_continuous(limits = c(0,30), breaks = c(0,30,5))+
  theme_bw()+
  labs(x = 'Age', y = 'Selectivity', color = '')+
  facet_wrap(~species, scales = 'free_x')#+
  #theme(strip.background = element_rect(fill = NA))
p
ggsave('../methods/images/Selectivity_patterns_tier3.pdf',p,width = 10, height = 4)

dat_long_10 %>%
  filter(species %in% these_species) %>%
  ggplot(aes(x = age_class, y = selex_age_class, color = species))+
  geom_point(size = 2)+
  geom_line(size  = 1.2)+
  scale_colour_manual(values=getPalette(colourCount))+
  scale_x_continuous(limits = c(0,10), breaks = c(0,10,1))+
  theme_bw()+
  labs(x = 'Age', y = 'Selectivity', color = '')
