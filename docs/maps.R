


library(tidyverse)
library(sf)
library(USAboundaries)
library(RColorBrewer)
library(ggspatial)
library(fiftystater)

conus = us_states() %>% filter(!name %in% c('Alaska', 'Hawaii', 'Puerto Rico')) %>% 
  st_transform(5070)

plot(conus$geometry)
  
### AQUIFERS
aquifer = read_sf('data/shps/us_aquifers.shp') %>% 
  st_transform(5070) %>% 
  st_cast("MULTIPOLYGON")

# aquifer2 = aquifer %>% st_transform(4326)
# 
box = conus %>% 
   st_bbox() %>% 
   st_as_sfc()
temp = st_intersects(aquifer, box)
# 
aquifer = aquifer[which(lengths(temp) != 0), ]

aquifer = aquifer %>% filter(AQ_NAME != 'Other rocks')
num = length(unique(aquifer$AQ_NAME))
RColorBrewer::display.brewer.all(n=10, exact.n=FALSE)
col1 = RColorBrewer::brewer.pal(9,"Blues")
col2 = RColorBrewer::brewer.pal(9,"YlOrRd")
col3 = RColorBrewer::brewer.pal(9,"YlGnBu")
col4 = colorRampPalette(brewer.pal(9,"Spectral"))(num)

pals4 = leaflet::colorFactor(col4, domain = aquifer$AQ_NAME)

ggplot() +
  geom_sf(data = conus, fill = 'azure', size = 1) +
  geom_sf(data = aquifer, aes(fill = AQ_NAME)) +
  scale_fill_manual(values = col4) +
  theme(legend.position = "bottom") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) 
  #guides(fill = guide_legend(override.aes = list(fill = pals4)))









