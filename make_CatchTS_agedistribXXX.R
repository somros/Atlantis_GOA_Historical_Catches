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

# Read in selectivity
selex <- read.csv('../data/selex_10_age_classes.csv')
selex <- selex %>% select(Code, age_class, selex)
# read in population data
pop_data <- read.csv('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/build_init_prm_10COHORTS/data/nums_age_functional_groups.csv')

# keep numbers at age and weight at age
pop_data <- pop_data %>%
  select(Code, age_class, wet_weight_g, numbers_at_age) %>%
  filter(age_class!=0) # drop age 0 animals - these are the recruits

# get caught individuals assuming Q=1
pop_data_and_catch <- pop_data %>%
  left_join(selex, by = c('Code', 'age_class')) %>%
  drop_na() %>%
  mutate(caught_inds = numbers_at_age * selex,
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
