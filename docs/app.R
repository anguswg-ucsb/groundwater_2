library(tidyverse)
library(sf)
library(leaflet)
tmp = plot_well(join_time, 1)
ggsave(tmp, file = 'docs/img/well-1.png', width = 1, height = 1)



popup <- paste("<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:300px'><b>",
               '<h2 align="center"><strong>', join_time$wellid[1], "</strong></h2><br>", '<center><img src="',
               './img/well-1.png', '" height="150px" width="200px" border="8"></center><br> </div>')

tmp2 = join_spatial %>% filter(wellid == 'well 1') %>% st_transform(4326)
map = leaflet() %>%
  addTiles( ) %>%
  addMarkers(data = tmp2, popup = popup )

map

htmlwidgets::saveWidget(map, file = 'docs/map.html')
install.packages('htmlwidgets')

library(mapview)
normalizePath(path.expand('./docs/map.html'))
mapview::mapshot(map, file = 'docs/map.html')

cross talk###


webshot::install_phantomjs()


saveRDS(join_time, file = "data/join_time.rds")

tt = readRDS("data/join_time.rds")

https://stackoverflow.com/questions/41399795/savewidget-from-htmlwidget-in-r-cannot-save-html-file-in-another-folder
