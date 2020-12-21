

library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(USAboundaries)
library(sp)
library(leaflet)
library(RColorBrewer)

source("docs/utils.R")





#################################################################
######################### THRESHOLD #############################
#################################################################

# READ IN DATA ---------------- (USGS + STATE DATA JOINED)
join_time = readRDS('data/ca/ca-join-all.rds')
join_spatial = readRDS('data/ca/ca-join-spatial.rds')

ca = us_states() %>% 
  filter(name == 'California') %>% 
  st_transform(5070)

# **** RUN AFTER READING IN DATA ****

# THRESHOLD ------ (10 - 20 DISTINCT MEASUREMENTS & 5 DIST YEARS)
join_time = join_time %>%  filter(measurement_dist >= 20, year_dist >= 5)
join_spatial = join_spatial %>% filter(measurement_dist >= 20, year_dist >= 5)


#################################################################
########################## TESTS ################################
#################################################################
counties = us_counties() %>% 
  filter(state_name == 'California') %>% 
  st_transform(5070)

tmp1 = usgs_spatial %>% 
  st_transform(5070)


gg_tmp = ggplot() +
  geom_sf(data = ca) +
  geom_sf(data = counties, size = .5) +
  geom_sf(data = tmp3, aes(col = dtw))

ggplotly(gg_tmp)


county_tmp = counties %>% select(name)

county_tmp2 = st_intersection(county_tmp, join_spatial)
county_tmp2 
tmp_join = left_join(join_time, select(county_tmp2, wellid, name), by = "wellid") 
joseph_outline = ama2 %>% 
  filter(OBJECTID == 1) 

joseph_shp = ama2 %>% 
  filter(OBJECTID == 1) %>% 
  st_intersection(x)
joseph_wells = join_time %>% filter(wellid %in% joseph_shp$wellid)
joseph_pts = join_spatial %>% filter(wellid %in% joseph_shp$wellid)












# 58730 ---- Corocoran
plotBuffer(join_spatial, 58730, 20000)

# 42086 ---- Palmsprings
plotBuffer(join_spatial, 42086, 20000)

# 28739 ---- McFarland
plotBuffer(join_spatial, 28739, 15000)

# 24614 ---- Coalina
plotBuffer(join_spatial, 24614, 20000)

# 50563 ---- Santa Barbara & SLO
plotBuffer(join_spatial, 50563, 20000)


# 80 years of records
tmp2 = join_time %>% filter(time_span >= 80, measurement_dist >= 250, measurement_dist <= 1000)
tmp3 = join_spatial %>% 
  filter(wellid %in% tmp2$wellid)

plotMultipleWells(tmp2)

#################################################################
######################## NEGATIVE DTW ###########################
#################################################################
negatives = join_time %>% filter(dtw < 0)

neg_time = join_time %>% filter(wellid %in% negatives$wellid)


tmp1 = neg_time %>%
  arrange(measurement_dist)
  split(neg_time$wellid) %>% 
  head(3) %>% 
  bind_rows()
  
tmp2 = neg_time %>%
  filter(wellid %in% c(87209, 70968, 87298))

tmp2$level = cut(tmp2$dtw,c(-40,-30,-20,-10,-5, -1, 0, 2))


tmp2 = tmp2 %>% 
  group_by(wellid, level) %>% 
  mutate(freq = n())
plotNegativeWells(tmp2)

data = sample(0:40, 200, replace=T)
a = c(-20,-10);b = c(-10,-5);c = c(-5, 5)
my_bins = matrix(rbind(a, b, c), ncol=2)
shx = lattice::shingle(tmp2$dtw, intervals=my_bins)
#################################################################
########################## LEAFLET ##############################
#################################################################

# LEAFLET CRS = 4326
x = join_spatial %>%
  st_transform(4326) %>% 
  group_by(wellid)

counties = us_counties() %>% 
  filter(state_name == 'California') %>% 
  st_cast('MULTIPOLYGON') %>% 
  st_transform(4326)



# COLOR PALLETES 
#RColorBrewer::display.brewer.all(n=4, exact.n=FALSE)

nb.cols = 10
col1 = RColorBrewer::brewer.pal(9,"Blues")
# col2 = brewer.pal(9,"YlOrRd")
col2 = colorRampPalette(brewer.pal(9,"YlOrRd"))(nb.cols)
col3 = RColorBrewer::brewer.pal(9,"YlGnBu")
col4 = RColorBrewer::brewer.pal(9,"Spectral")
col5 = RColorBrewer::brewer.pal(9,"Greys")
col6 = palette(c('black', 'white'))

pals1 = colorFactor('navy', domain = x$dtw)
pals2 = colorNumeric(col2, domain = c(0, 1000))
pals3 = colorBin(col3, domain = 1:8)
# pals4 = colorFactor(col4, domain = aquifer2$AQ_NAME)
pals5 = colorFactor(col6, domain = x$source)

# LEGEND LABELS
labels = c('0', '100', '200',
           '300', '400',
           '500', '600',
           '700', '800', '900', '1000')
agency_labs = c('DWR Well', 'USGS')

 
# records 4 times a year
emw_spatial = x %>% 
  filter(source == 'Environmental Monitoring Well') 
emw_spatial = emw_spatial %>%
  filter(dtw < 1000, dtw >= 0) %>% 
  select(dtw, gw_elev, date, date_min, date_max, measurement_dist, source, wellid)

# records every ~6 months
dwr_spatial = x %>% 
  filter(source == 'DWR Well')
dwr_spatial = dwr_spatial %>%
  filter(dtw < 1000, dtw >= 0) %>% 
  select(dtw, gw_elev, date, date_min, date_max, measurement_dist, source, wellid)

# records every ~ 1 month
usgs_spatial = x %>% 
  filter(source == 'USGS')
usgs_spatial = usgs_spatial %>%
  filter(dtw < 1000, dtw >= 0) %>% 
  select(dtw, gw_elev, date, date_min, date_max, measurement_dist, source, wellid)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = 'Base') %>% 
  addPolygons(data = counties,
              fillColor  = 'darkcyan',
              fillOpacity = 0.2,
              color = 'black',
              weight = 1,
              label = ~name,
              group = 'Counties') %>% 
  addCircleMarkers(data = usgs_spatial, #clusterOptions = markerClusterOptions(interactive()),
                   radius = 4,
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = 'black',
                   # color = ifelse(r1$source == 'USGS', 'black', NA),
                   opacity = .8, 
                   weight = .5,
                   stroke = T,
                   popup = leafpop::popupTable(st_drop_geometry(usgs_spatial), feature.id = FALSE,
                                               row.numbers = FALSE), group = 'USGS') %>% 
  addCircleMarkers(data = dwr_spatial,
                   radius = 4, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = 'black',
                   # color = ifelse(r2$source == 'USGS', 'black', NA),
                   opacity = .8, 
                   weight = .5,
                   stroke = T,
                   popup = leafpop::popupTable(st_drop_geometry(dwr_spatial), feature.id = FALSE,
                                               row.numbers = FALSE), group = 'DWR') %>% 
  addCircleMarkers(data = emw_spatial,
                   radius = 4, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8,
                   color = 'black',
                   # color = ifelse(r2$source == 'USGS', 'black', NA),
                   opacity = .8, 
                   weight = .5,
                   stroke = T,
                   popup = leafpop::popupTable(st_drop_geometry(emw_spatial), feature.id = FALSE,
                                               row.numbers = FALSE), group = 'EMW') %>% 
  addLayersControl(overlayGroups = c('USGS', 'DWR', 'EMW'),
                   options = layersControlOptions(collapsed = FALSE),
                   baseGroups = c("Base")) %>% 
  addLegend(pal = pals2,
            values = c(0, 1000),
            opacity = .9,
            title = 'Depth to water (ft)', # Title
            position = "bottomleft",
            labFormat = function(type, cuts, p) {
              paste0(labels)}) 
  # addLegend(pal = colorFactor(c("white", "black"), domain = NULL),
  #           values = c('AZ', 'USGS'),
  #           opacity = 0.8,
  #           title = '',
  #           position = "bottomleft")


#################################################################
################ LEAFLET --- GROUP BY RANGE #####################
#################################################################

# RANGES (0 - 1000 +)
r0 =  dtw_range(x, -200, 0) %>% select(wellid, dtw, date,source, measurement_dist)
r1 = dtw_range(x, 0, 100) %>% select(wellid, dtw, date, source, measurement_dist)
r2 = dtw_range(x, 100, 200) %>% select(wellid, dtw, date, source, measurement_dist)
r3 = dtw_range(x, 200, 300) %>% select(wellid, dtw, date, source, measurement_dist)
r4 = dtw_range(x, 300, 400) %>% select(wellid, dtw, date, source, measurement_dist)
r5 = dtw_range(x, 400, 500) %>% select(wellid, dtw, date, source, measurement_dist)
r6= dtw_range(x, 500, 600) %>% select(wellid, dtw, date, source, measurement_dist)
r7 = dtw_range(x, 600, 700) %>% select(wellid, dtw, date, source, measurement_dist)
r8 = dtw_range(x, 700, 800) %>% select(wellid, dtw, date, source, measurement_dist)
r9 = dtw_range(x, 800, 900) %>% select(wellid, dtw, date, source, measurement_dist)
r10 = dtw_range(x, 900, 1000) %>% select(wellid, dtw, date, source, measurement_dist)
r11 = dtw_range(x, 1000, 1400) %>% select(wellid, dtw, date, source, measurement_dist)


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = 'Base') %>% 
  addCircleMarkers(data = r1, #clusterOptions = markerClusterOptions(interactive()),
                   radius = 4,
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r1$source == 'USGS', 'black', NA),
                   opacity = .8, 
                   weight = 1,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r1, feature.id = FALSE,
                                               row.numbers = FALSE), group = '0 - 100 ft') %>% 
  addCircleMarkers(data = r2,
                   radius = 4, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r2$source == 'USGS', 'black', NA),
                   opacity = .8, 
                   weight = 1,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r2, feature.id = FALSE,
                                               row.numbers = FALSE), group = '100 - 200 ft') %>% 
  addCircleMarkers(data = r3,
                   radius = 4,#clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r3$source == 'USGS', 'black', NA),
                   opacity = .8, 
                   weight = 1,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r3, feature.id = FALSE,
                                               row.numbers = FALSE), group = '200 - 300 ft') %>%
  addCircleMarkers(data = r4,
                   radius = 4,#clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r4$source == 'USGS', 'black', NA),
                   opacity = .8,
                   weight = 1,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r4, feature.id = FALSE,
                                               row.numbers = FALSE), group = '300 - 400 ft') %>% 
  addCircleMarkers(data = r5,
                   radius = 4,#clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r5$source == 'USGS', 'black', NA),
                   opacity = .8,
                   weight = 1,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r5, feature.id = FALSE,
                                               row.numbers = FALSE), group = '400 - 500 ft') %>%
  addCircleMarkers(data = r6,
                   radius = 4,#clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r6$source == 'USGS', 'black', NA),
                   opacity = .8,
                   weight = 1,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r6, feature.id = FALSE,
                                               row.numbers = FALSE), group = '500 - 600 ft') %>%
  addCircleMarkers(data = r7,
                   radius = 4,#clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r7$source == 'USGS', 'black', NA),
                   opacity = .8,
                   weight = 1,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r7, feature.id = FALSE,
                                               row.numbers = FALSE), group = '600 - 700 ft') %>% 
  addCircleMarkers(data = r8,
                   radius = 4,#clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r8$source == 'USGS', 'black', NA),
                   opacity = .8,
                   weight = 1,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r8, feature.id = FALSE,
                                               row.numbers = FALSE), group = '700 - 800 ft') %>%
  # addCircleMarkers(data = r9, 
  #                   radius = 4, #clusterOptions = markerClusterOptions(interactive()),
  #                  fillColor = ~pals2(dtw), 
  #                  fillOpacity = .8, 
  #                  color = ifelse(r9$source == 'USGS', 'black', NA),
  #                  opacity = .8,
  #                  weight = 1,
  #                  stroke = TRUE,
  #                  popup = leafpop::popupTable(r9, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '800 - 900 ft') %>% 
  # addCircleMarkers(data = r10,
#                     radius = 4, #clusterOptions = markerClusterOptions(interactive()),
#                  fillColor = ~pals2(dtw), 
#                  fillOpacity = .8, 
#                  color = ifelse(r10$source == 'USGS', 'black', NA),
#                  opacity = .8,
#                  weight = 1,
#                  stroke = TRUE,
#                  popup = leafpop::popupTable(r10, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '900 - 1000 ft') %>% 
# addCircleMarkers(data = r11,
#                  radius = 4,#clusterOptions = markerClusterOptions(interactive()),
#                  fillColor = ~pals2(dtw), 
#                  fillOpacity = .8, 
#                  color = ifelse(r11$source == 'USGS', 'black', NA),
#                  opacity = .8,
#                  weight = 1,
#                  stroke = TRUE,
#                  popup = leafpop::popupTable(r11, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '> 1000 ft') %>%
addCircleMarkers(data = r0,
                 radius = 4, #clusterOptions = markerClusterOptions(interactive()),
                 fillColor = ~pals1(dtw), 
                 fillOpacity = .8, 
                 color = ifelse(r0$source == 'USGS', 'black', NA),
                 opacity = .8,
                 weight = 1,
                 stroke = TRUE,
                 popup = leafpop::popupTable(r0, feature.id = FALSE,
                                             row.numbers = FALSE), group = '< 0 ft') %>%
  addLayersControl(overlayGroups = c('< 0 ft', '0 - 100 ft', '100 - 200 ft', '200 - 300 ft',
                                     '300 - 400 ft', '400 - 500 ft',
                                     '500 - 600 ft', '600 - 700 ft',
                                     '700 - 800 ft', '> 1000 ft'),
                   options = layersControlOptions(collapsed = FALSE),
                   baseGroups = c("Base")) %>% 
  addLegend(pal = pals2,
            values = c(0, 1000),
            opacity = .9,
            title = 'Depth to water (ft)', # Title
            position = "bottomleft",
            labFormat = function(type, cuts, p) {
              paste0(labels)}) %>% 
  addLegend(pal = colorFactor(c("white", "black"), domain = NULL),
            values = c('AZ', 'USGS'),
            opacity = 0.8,
            title = '',
            position = "bottomleft")


#################################################################
#################################################################

