# Alberto Rovellini
# August 25 2022
# Convert selectivity patterns to Atlantis parameter CatchTS_agedistribXXX vectors 
# This is for Tier 3 groups and it is based on selectivity from stock assessment models

# We need:
# 1. Numbers at age. For now use the values from the initial conditions (1990), but do we need annual values?
# 2. Selectivity patterns, by age class
# 3. Weight at age from initial conditions

# For the calculations, assume Q=1

# This is going to result in something static, but probably an OK approximation
# Also bear in mind that selectivity patterns vary in space
# We will need to get halibut and salmon from somewhere else

library(tidyverse)
library(data.table)

# Read in selectivity
selex <- read.csv('../data/selex_10_age_classes.csv')
selex <- selex %>% select(Code, age_class, selex_age_class)
# read in Atlantis fg
atlantis_fg <- read.csv('../data/GOA_Groups.csv')
# read in population data
pop_data <- read.csv('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/build_init_prm_10COHORTS/data/nums_age_functional_groups.csv')
# life history parameters
age_mat <- read.table('../data/age_mat.txt', sep = '')

# keep numbers at age and weight at age
pop_data <- pop_data %>%
  select(Code, age_class, wet_weight_g, numbers_at_age) %>%
  filter(age_class!=0) # drop age 0 animals - these are the recruits

# get caught individuals assuming Q=1
pop_data_and_catch <- pop_data %>%
  left_join(selex, by = c('Code', 'age_class')) %>%
  drop_na() %>%
  mutate(caught_inds = numbers_at_age * selex_age_class,
         caught_biom_g = caught_inds * wet_weight_g) %>%
  group_by(Code) %>%
  mutate(catch_tot_g = sum(caught_biom_g)) %>%
  ungroup() %>%
  mutate(catch_prop = caught_biom_g / catch_tot_g)

# view
pop_data_and_catch %>%
  ggplot()+
  geom_bar(aes(x = age_class, y = catch_prop), stat = 'identity')+
  scale_x_continuous(breaks = seq(1,10,1))+
  theme_bw()+
  facet_wrap(~Code)

# make figure for methods
p <- pop_data_and_catch %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  ggplot()+
  geom_bar(aes(x = age_class, y = catch_prop), stat = 'identity')+
  scale_x_continuous(breaks = seq(1,10,1))+
  theme_bw()+
  labs(x = 'Age class', y = 'Proportion of biomass caught')+
  facet_wrap(~Name)
p
# ggsave('../methods/images/Selectivity_patterns_tier3_fg.png',p,width = 10, height = 4)

# sanity check
# pop_data_and_catch %>%
#   select(Code, catch_prop) %>%
#   group_by(Code) %>%
#   summarise(check = sum(catch_prop))

# write.csv(pop_data_and_catch %>% select(Code, catch_prop), '../data/CatchTS_agedistribXXX.csv', row.names = F)


# Other species -----------------------------------------------------------

# 1/31/2023
# Here we calculate CatchTS_agedistribXXX for species that are not Tier 3.
# We attempted to estimate these from NORPAC catch length comps, but that approach has a couple of issues
# mainly, that we go length -> weight -> age -> age class, and this results in many animals in the older age class
# so catch age comps seem pretty skewed toward oldest age bin
# As a first pass, assume that age at selex = age at maturity for species that we do not have assessment selex curves for

# We need:
# 1. Numbers at age. For now use the values from the initial conditions (1990), but do we need annual values?
# 2. Age (class) at 50% maturity from prm, and constructed maturity ogive from that
# 3. Weight at age from initial conditions

# For the calculations, assume Q=1

# first, identify the species that we are missing
impacted <- atlantis_fg %>% filter(IsImpacted == 1) %>% pull(Code)
impacted_todo <- setdiff(impacted, pop_data_and_catch %>% pull(Code) %>% unique())

# clean age mat
age_mat <- age_mat %>%
  set_names('Code','age') %>%
  mutate(Code = gsub('_age_mat', '', Code)) %>%
  left_join(atlantis_fg %>% select(Code, NumCohorts), by = 'Code') %>%
  filter(Code %in% impacted_todo)

# make a table like selex but based on maturity, so that then you can just reapply the code above
selex_todo_list <- list()

for(i in 1:length(age_mat$Code)){
  
  this_code <- age_mat$Code[i] 
  this_agemat <- age_mat %>% filter(Code == this_code) %>% pull(age)
  this_numcohorts <- age_mat %>% filter(Code == this_code) %>% pull(NumCohorts)
  
  this_df <- data.frame('Code' = this_code, 'age_class' = 1:this_numcohorts)
  this_df <- this_df %>%
    rowwise() %>%
    mutate(selex_age_class = case_when(
      age_class < this_agemat-1 ~ 0,
      age_class == this_agemat-1 ~ 0.1,
      age_class == this_agemat ~ 0.5,
      age_class == this_agemat+1 ~ 0.9,
      age_class > this_agemat+1 ~ 1
    ))
  
  selex_todo_list[[i]] <- this_df
}

selex_todo <- rbindlist(selex_todo_list)

# now rerun code above on new groups
pop_data_and_catch_todo <- pop_data %>%
  left_join(selex_todo, by = c('Code', 'age_class')) %>%
  drop_na() %>%
  mutate(caught_inds = numbers_at_age * selex_age_class,
         caught_biom_g = caught_inds * wet_weight_g) %>%
  group_by(Code) %>%
  mutate(catch_tot_g = sum(caught_biom_g)) %>%
  ungroup() %>%
  mutate(catch_prop = caught_biom_g / catch_tot_g)

# view
pop_data_and_catch_todo %>%
  ggplot()+
  geom_bar(aes(x = age_class, y = catch_prop), stat = 'identity')+
  scale_x_continuous(breaks = seq(1,10,1))+
  theme_bw()+
  facet_wrap(~Code)

# make figure for methods
p <- pop_data_and_catch_todo %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  ggplot()+
  geom_bar(aes(x = age_class, y = catch_prop), stat = 'identity')+
  scale_x_continuous(breaks = seq(1,10,1))+
  theme_bw()+
  labs(x = 'Age class', y = 'Proportion of biomass caught')+
  facet_wrap(~Name)
p
# ggsave('../methods/images/Selectivity_patterns_tier3_fg.png',p,width = 10, height = 4)

# sanity check
# pop_data_and_catch_todo %>%
#   select(Code, catch_prop) %>%
#   group_by(Code) %>%
#   summarise(check = sum(catch_prop))

# write.csv(pop_data_and_catch %>% select(Code, catch_prop), '../data/CatchTS_agedistribXXX.csv', row.names = F)

# merge
catch_age <- rbind(pop_data_and_catch %>% select(Code, age_class, catch_prop),
                   pop_data_and_catch_todo %>% select(Code, age_class, catch_prop))

# reorder
idx <- intersect(atlantis_fg$Code, catch_age$Code)
catch_age <- catch_age %>% arrange(factor(Code, levels = idx))

# Write vectors of CatchTS_agedistribXXX ----------------------------------

# write one per impacted group of vertebrates
newfile <- paste0('../output/CatchTS_agedistribXXX.prm')
file.create(newfile)

for(i in 1:length(idx)){
  
  this_code <- idx[i]
  this_num <- catch_age %>% filter(Code == this_code) %>% pull(age_class) %>% max()
  this_vec <- catch_age %>% filter(Code == this_code) %>% pull(catch_prop)
  
  
  cat(paste0('CatchTS_agedistrib', this_code, ' ', this_num), file=newfile, append=TRUE,'\n')
  cat(this_vec, file=newfile, append=TRUE,'\n')

}
