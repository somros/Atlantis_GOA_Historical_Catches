# View forcings in time and space for feedback from the team and for the manual

library(tidyverse)
library(sf)
library(lubridate)
library(rbgm)
library(data.table)
library(plotly)
library(htmlwidgets)
library(viridis)

# Atlantis BGM
atlantis_bgm <- read_bgm('../data/GOA_WGS84_V4_final.bgm')
atlantis_box <- atlantis_bgm %>% box_sf()

# Atlantis groups for column names
atlantis_fg <- read.csv('../data/GOA_Groups.csv', header = T) %>%
  filter(IsImpacted == 1) # only keep groups that are marked as IsImpacted by fisheries in the group.csv file
all_fg <- atlantis_fg$Code

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

# Alaska first

ak_data <- list()

for(b in 1:length(ak_boxes)){
  this_box <- ak_boxes[b]
  this_tab <- read.table(files_ak[b], skip = 309)
  colnames(this_tab) <- c('Time', all_fg) # add column names
  this_tab <- this_tab %>% mutate(box_id = this_box) # add a column for the box number
  ak_data[[b]] <- this_tab
} 

all_ak <- rbindlist(ak_data)

# Canada

bc_data <- list()

for(b in 1:length(bc_boxes)){
  this_box <- bc_boxes[b]
  this_tab <- read.table(files_bc[b], skip = 309)
  colnames(this_tab) <- c('Time', all_fg) # add column names
  this_tab <- this_tab %>% mutate(box_id = this_box) # add a column for the box number
  bc_data[[b]] <- this_tab
} 

all_bc <- rbindlist(bc_data)

# together
all_catch <- rbind(all_ak, all_bc)

# transform time steps into dates
this_origin <- as.Date('1991-01-01', tz = 'UTC')

# something wrong here
test <- all_catch %>%
  select(Time, box_id, SPI) %>%
  mutate(Date = as.Date(Time, "%Y-%m-%d", origin = this_origin, tz = 'UTC')) %>%
  mutate(Catch_mt_day = SPI * 60 * 60 * 24 * 20 * 5.7 / 1e9,
         Catch_mt_month = Catch_mt_day * 30, # THIS IS AN APPROXIMATION FOR VISUALISATION PURPOSES
         Year = year(Date)) %>%
  select(Year, Date, box_id, Catch_mt_month) %>%
  distinct() %>%
  group_by(Year, box_id) %>%
  mutate(Catch_mt_year = sum(Catch_mt_month)) %>% # sum over months for annual catch
  ungroup() %>%
  select(Year, box_id, Catch_mt_year) %>%
  distinct() %>%
  group_by(Year) %>%
  summarise(Catch_mt_year = sum(Catch_mt_year)) %>% # sum across boxes
  ungroup() 

all_catch_long_box <- all_catch %>%
  mutate(Date = as.Date(Time, "%Y-%m-%d", origin = this_origin, tz = 'UTC')) %>%
  select(Date, box_id, KWT:BIV) %>%
  pivot_longer(-c(Date, box_id), names_to = 'Species', values_to = 'Catch_mgs') %>%
  mutate(Catch_mt_day = Catch_mgs * 60 * 60 * 24 * 20 * 5.7 / 1e9,
         Catch_mt_month = Catch_mt_day * 30, # THIS IS AN APPROXIMATION FOR VISUALISATION PURPOSES
         Year = year(Date)) %>% 
  select(Year, Date, box_id, Species, Catch_mt_month) %>%
  distinct() %>%
  group_by(Year, box_id, Species) %>%
  mutate(Catch_mt_year = sum(Catch_mt_month)) %>% # sum over months for annual catch
  ungroup() 

all_catch_long <- all_catch_long_box %>%
  select(Year, box_id, Species, Catch_mt_year) %>%
  distinct() %>%
  group_by(Year, Species) %>%
  summarise(Catch_mt_year = sum(Catch_mt_year)) %>% # sum across boxes
  ungroup() 


# View --------------------------------------------------------------------

# what groups can we ditch for visualization purposes?
these_species <- all_catch_long %>% filter(Catch_mt_year > 0) %>% pull(Species) %>% unique() 

# organize in a few groups for the purposes of visualization in panels

key <- atlantis_fg %>% select(Code, Name) %>%
  mutate(Group = c(rep('Mammals', 9), rep('Seabirds', 4), rep('Sharks and rays', 3), rep('Groundfish', 2), 'Forage',
                   rep('Groundfish', 6), rep('Sharks and rays', 3), rep('Groundfish', 9), rep('Invertebrates', 2),
                   rep('Salmon', 5), 'Groundfish', rep('Forage', 4), rep('Invertebrates', 11)))

t <- all_catch_long %>%
  left_join(key, by = c('Species'='Code')) %>%
  filter(Species %in% these_species) 

t %>%
  ggplot()+
  geom_bar(aes(x = Year, y = Catch_mt_year, fill = Name), stat = 'identity')+
  theme_bw()

#################################################################################
# make plots for methods
colourCount <- length(unique(t %>% filter(Group == 'Groundfish') %>% pull(Name)))
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))

groups <- unique(t$Group)

p1 <- t %>%
  filter(Group == 'Groundfish') %>%
  ggplot()+
  geom_bar(aes(x = Year, y = Catch_mt_year, fill = Name), stat = 'identity')+
  scale_fill_manual(values = getPalette(colourCount))+
  theme_bw()+
  labs(x = '', y = 'Catch (mt)', title = 'Groundfish')
p1

p2 <- t %>%
  filter(Group == 'Invertebrates') %>%
  ggplot()+
  geom_bar(aes(x = Year, y = Catch_mt_year, fill = Name), stat = 'identity')+
  scale_fill_manual(values = getPalette(colourCount))+
  theme_bw()+
  labs(x = '', y = 'Catch (mt)', title = 'Invertebrates')
p2

p3 <- t %>%
  filter(Group == 'Forage') %>%
  ggplot()+
  geom_bar(aes(x = Year, y = Catch_mt_year, fill = Name), stat = 'identity')+
  scale_fill_manual(values = getPalette(colourCount))+
  theme_bw()+
  labs(x = '', y = 'Catch (mt)', title = 'Forage fish')
p3

p4 <- t %>%
  filter(Group == 'Sharks and rays') %>%
  ggplot()+
  geom_bar(aes(x = Year, y = Catch_mt_year, fill = Name), stat = 'identity')+
  scale_fill_manual(values = getPalette(colourCount))+
  theme_bw()+
  labs(x = '', y = 'Catch (mt)', title = 'Sharks and rays')
p4

p5 <- t %>%
  filter(Group == 'Salmon') %>%
  ggplot()+
  geom_bar(aes(x = Year, y = Catch_mt_year, fill = Name), stat = 'identity')+
  scale_fill_manual(values = getPalette(colourCount))+
  theme_bw()+
  labs(x = '', y = 'Catch (mt)', title = 'Pacific salmon')
p5


ggsave('../methods/images/time_series1.pdf', p1, width = 9, height = 4.8)
ggsave('../methods/images/time_series2.pdf', p2, width = 9, height = 3)
ggsave('../methods/images/time_series3.pdf', p3, width = 9, height = 2)
ggsave('../methods/images/time_series4.pdf', p4, width = 9, height = 2.5)
ggsave('../methods/images/time_series5.pdf', p5, width = 9, height = 2.5)

#################################################################################

fig <- plot_ly(t, x = ~Year, y = ~Catch_mt_year, color = ~Name, type = 'bar')
fig <- fig %>% layout(barmode = 'stack', legend = list(traceorder = 'normal'))

#Export
saveWidget(ggplotly(fig), file = "../output/graphics/myplot.html")

# View in space -----------------------------------------------------------

all_catch_long_box_year <- all_catch_long_box %>%
  left_join(key, by = c('Species'='Code')) %>%
  filter(Species %in% these_species) %>%
  group_by(Name, Year, box_id) %>%
  summarise(Catch_mt_year = sum(Catch_mt_year)) %>%
  ungroup() 

spatial <- atlantis_box %>%
  select(box_id) %>%
  full_join(all_catch_long_box_year, by = 'box_id')
  
all_species <- spatial %>% pull(Name) %>% unique()

for(i in 1:length(all_species)){
  
  this_species <- all_species[i]
  this_spatial <- spatial %>% filter(Name == this_species)
  
  p <- this_spatial %>%
    ggplot()+
    geom_sf(aes(fill = Catch_mt_year), color = NA)+
    scale_fill_viridis()+
    theme_bw()+
    facet_wrap(~Year, ncol = 2)+
    labs(title = this_species, fill = 'Catch (mt)')
  
  ggsave(paste('../output/graphics/spatial/', this_species, '.png', sep = ''), p, height = 20, width = 8)
  
}
