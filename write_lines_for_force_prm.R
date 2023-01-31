# Alberto Rovellini
# 1/31/2022
# write lines of force.prm file to read the catch.ts files into Atlantis

# here's what it needs to look like, for each file (from SETas):
# Catchts0.name box0catch
# Catchts0.location 4043667.571 1150676.493  0
# Catchts0.data inputs/catchts/boundary.ts
# Catchts0.rewind 0

library(dplyr)
library(ggplot2)
library(tidyr)
library(rbgm)
library(sf)

goa_boxes <- 0:108

# catch files
goa_catch <- list.files('../output/goa_catch_hist/')

bgm_file <- '../data/GOA_WGS84_V4_final.bgm'

atlantis_bgm <- read_bgm(bgm_file)
bgm_sf <- atlantis_bgm %>% box_sf()
bgm_proj <- atlantis_bgm$extra$projection

# # list inside points and plot 
bgmLines <- readLines(bgm_file)
pts <- data.frame('from_bgm' = bgmLines[grepl('inside', bgmLines)])
pts <- pts %>%
  separate(from_bgm, c('box', 'x', 'y'), sep = ' ') %>%
  mutate(box = gsub('.inside', '', gsub('box', '', box))) %>%
  mutate_if(is.character, as.numeric)

# # some validation
# 
# pts_sf <- pts %>%
#   st_as_sf(coords = c('x','y'), crs = bgm_proj)
# 
# ggplot()+
#   geom_sf(data = bgm_sf, aes(color = box_id), fill = NA)+
#   geom_sf(data = pts_sf, aes(color = box))
# # looks good

# write lines

newfile <- paste0('../output/catch_for_forceprm.prm')
file.create(newfile)

for(i in 1:length(goa_boxes)){
  
  this_box <- i-1
  this_x <- pts %>% filter(box == this_box) %>% pull(x)
  this_y <- pts %>% filter(box == this_box) %>% pull(y)
  
  cat(paste0('Catchts', this_box, '.name', ' ', 'box', this_box, 'catch'), file=newfile, append=TRUE,'\n')
  cat(paste0('Catchts', this_box, '.location', ' ', this_x, ' ', this_y, ' ', this_box), file=newfile, append=TRUE,'\n')
  cat(paste0('Catchts', this_box, '.data', ' ', '../goa_catch_hist/catch', this_box, '.ts'), file=newfile, append=TRUE,'\n')
  cat(paste0('Catchts', this_box, '.rewind', ' ', 0), file=newfile, append=TRUE,'\n\n')
  
}
