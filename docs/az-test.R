

library(tidyverse)
library(sf)
library(sp)
library(lubridate)
library(RColorBrewer)
library(USAboundaries)
library(leaflet)
library(plotly)

source("docs/utils.R")


join_time = readRDS("data/join_time.rds")
join_spatial = readRDS('data/join_spatial.rds')


#######################################################
######## USGS SPATIAL + TIMESERIES DATAFRAMES #########
#######################################################

usgs = readRDS('data/usgs-unique-sites.rds')
usgs_time = readRDS("data/usgs-time.rds")
state = readRDS('data/state-unique-site.rds') 
state_time = readRDS('data/state-time.rds')


usgs = usgs %>% 
  select(!source)
usgs_time = usgs_time %>% 
  mutate(id = 'USGS') %>%
  group_by(site_id) %>% 
  select(!c(agency_cd, source))
usgs_time = usgs_time %>% 
  group_by(site_id) %>% 
  filter(measurement_dist >= 10) %>% 
  arrange(site_id, year) %>% 
  mutate(measure_period = year - lag(year))
### STATE UNIQUE AND WELL TIME SERIES DATAFRAMES ---------- FILTER FOR THE SITE_ID WHICH DO NOT APPEAR IN USGS TIME SERIES DATAFRAME ###
state = state %>% 
  select(!c(source))
state_time = state_time %>% 
  mutate(id = 'AZ') %>%
  group_by(site_id) %>% 
  select(!c(agency_cd, source))
state_time = state_time %>% 
  group_by(site_id) %>% 
  filter(measurement_dist >= 10) %>% 
  arrange(site_id, year) %>% 
  mutate(measure_period = year - lag(year))
join_time = rbind(usgs_time, state_time) %>% 
  group_by(site_id) %>%
  arrange(desc(site_id))
join_time$wellid = join_time %>% group_indices(site_id) 
join_spatial = join_time %>% 
  st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
  st_transform(5070) %>%
  as_Spatial() %>% 
  remove.duplicates() %>% 
  st_as_sf() %>% 
  group_by(wellid)

#######################################################
#################### THRESHOLDING #####################
#######################################################
join_time = join_time %>%
  group_by(wellid) %>% 
  mutate(dec_date = decimal_date(date))

join_spatial = join_spatial %>%
  arrange(id) %>% 
  group_by(wellid) %>% 
  mutate(dec_date = decimal_date(date))
join_spatial = join_spatial[!duplicated(join_spatial$wellid),]

join_time = join_time %>% filter(year_dist >= 5)
join_spatial = join_spatial %>% filter(wellid %in% join_time$wellid)

join_time = join_time %>% 
  group_by(wellid) %>% 
  rename('source' = id)
join_spatial = join_spatial %>% 
  group_by(wellid) %>% 
  rename('source' = id)

# join_time = join_time %>% transform(wellid = as.character(wellid))
# join_spatial= join_spatial %>% transform(wellid = as.character(wellid))
# 
# join_time = join_time %>% filter(wellid != 6008)
# join_spatial = join_spatial %>% filter(wellid != 6008)
# join_time = join_time %>% select(wellid, date, dtw,
#                                  year, date_min, date_max, id,
#                                  measurement_dist, year_dist, measure_period,
#                                  lat, lng, dec_date, site_id)
# 
# join_spatial = join_spatial %>% select(wellid, date, dtw,
#                                  year, date_min, date_max, id,
#                                  measurement_dist, year_dist, dec_date)\
# join_time = join_time %>%
#   mutate(time_span = max(year) - min(year))
# join_spatial = join_spatial %>%
#   mutate(time_span = max(year) - min(year))
# saveRDS(join_time, file = 'data/join_time.rds')
# saveRDS(join_spatial, file = 'data/join_spatial.rds')

# join_time$range = cut(join_time$dtw, breaks = seq(0, 2000, by = 100), labels = paste('R', 1:20, sep = '' ))
# join_spatial$range = cut(join_spatial$dtw, breaks = seq(0, 2000, by = 100), labels = paste('R', 1:20, sep = '' ))
                      
################################################################################
##################### LEAFLET + ACTIVE MANAGEMENT AREAS #########################
################################################################################

# Arizona state shape (CRS = 5070)
az = us_states() %>%
  filter(state_name == 'Arizona') %>% 
  st_transform(5070)

# read in AMA shapefiles
ama = read_sf('data/Act_Man_Areas.shp') %>% 
  st_transform(5070) %>% 
  st_cast("MULTIPOLYGON")
ama2 = ama %>% st_transform(4326)

### AQUIFERS
aquifer = read_sf('data/shps/us_aquifers.shp') %>% 
  st_transform(5070) %>% 
  st_cast("MULTIPOLYGON")

aquifer2 = aquifer %>% st_transform(4326)

box = az %>% st_transform(4326) %>% 
  st_bbox() %>% 
  st_as_sfc()
temp = st_intersects(aquifer2, box)

aquifer2 = aquifer2[which(lengths(temp) != 0), ]

temp2 = join_time %>% filter(dtw < 0)

### LEAFLET MAPS

x = join_spatial %>% st_transform(4326) 

#RColorBrewer::display.brewer.all(n=4, exact.n=FALSE)

nb.cols = 12
nb.cols2 = 2
col1 = RColorBrewer::brewer.pal(9,"Blues")
col2 = colorRampPalette(brewer.pal(9,"YlOrRd"))(nb.cols)
col3 = RColorBrewer::brewer.pal(9,"YlGnBu")
col4 = RColorBrewer::brewer.pal(9,"Spectral")
col5 = RColorBrewer::brewer.pal(9,"Greys")
col6 = palette(c('black', 'white'))

pals1 = colorFactor('navy', domain = x$dtw)
pals2 = colorNumeric(col2, domain = x$dtw)
pals3 = colorBin(col3, domain = 1:8)
pals4 = colorFactor(col4, domain = aquifer2$AQ_NAME)
pals5 = colorFactor(col6, domain = x$source)

labels = c('0 - 100', '100 - 200', '200 - 300',
           '300 - 400', '400 - 500',
           '500 - 600', '600 - 700',
           '700 - 800', '800 - 900', '> 900')
agency_labs = c('State', 'USGS')

# a = x %>% filter(range == 'R1') %>% select(wellid, date, dtw, measurement_dist, id)
# b = x %>% filter(range == 'R2') %>% select(wellid, date, dtw, measurement_dist, id)
# c = x %>% filter(range == 'R3') %>% select(wellid, date, dtw, measurement_dist, id)
# d = x %>% filter(range == 'R4') %>% select(wellid, date, dtw, measurement_dist, id)
# e = x %>% filter(range == 'R5') %>% select(wellid, date, dtw, measurement_dist, id)
# f = x %>% filter(range == 'R6') %>% select(wellid, date, dtw, measurement_dist, id)
# g = x %>% filter(range == 'R7') %>% select(wellid, date, dtw, measurement_dist, id)
# h = x %>% filter(range == 'R8') %>% select(wellid, date, dtw, measurement_dist, id)
# i = x %>% filter(range == 'R9') %>% select(wellid, date, dtw, measurement_dist, id)
# j = x %>% filter(range == 'R10') %>% select(wellid, date, dtw, measurement_dist, id)

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
# Add blue (negatives) to Legend: midway colorFactor, set midpoint to 0

# https://rstudio.github.io/dygraphs/
# https://mikejohnson51.github.io/COVID-19-dashboard/09_autocomplete_window.html

library(inlmisc)

#inlmisc::AddSearchButton()
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = 'Tiles') %>% 
  addPolygons(data = ama2,
              fillColor  = ~pals3(OBJECTID), fillOpacity = 0.3,
              color = 'black',
              label = ~MAP_LABEL, group = 'AMA') %>% 
  addPolygons(data = aquifer2, fillColor = ~pals4(AQ_NAME), fillOpacity = 0.3,
              color = 'black',
              label = ~AQ_NAME, group = 'Aquifer') %>% 
  addCircleMarkers(data = r1, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r1$source == 'USGS', 'black', NA),
                   opacity = .7, 
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r1, feature.id = FALSE,
                                               row.numbers = FALSE), group = '0 - 100 ft') %>% 
  addCircleMarkers(data = r2, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r2$source == 'USGS', 'black', NA),
                   opacity = .7, 
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r2, feature.id = FALSE,
                                               row.numbers = FALSE), group = '100 - 200 ft') %>% 
  # addLayersControl(overlayGroups = c('< 0 ft', '0 - 100 ft', '100 - 200 ft', '200 - 300 ft',
  #                                    '300 - 400 ft', '400 - 500 ft',
  #                                    '500 - 600 ft', '600 - 700 ft',
  #                                    '700 - 800 ft', '800 - 900 ft', '900 - 1000 ft', '> 1000 ft', 'AMA', 'Aquifer'),
  #                  baseGroups = c("Tiles")) %>%
  # addLegend(pal = pals2,
  #           values = x$dtw,
  #           opacity = 0.5,
  #           title = 'Depth to water (ft)', # Title
  #           position = "bottomleft",
  #           labFormat = function(type, cuts, p) {
  #             paste0(labels)}) %>%
  # addLegend(pal = colorFactor(c("white", "black"), domain = NULL),
  #           values = c('USGS', 'USGS'),
  #           opacity = 0.7,
  #           title = '',
  #           position = "bottomleft")
  addCircleMarkers(data = r3, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r3$source == 'USGS', 'black', NA),
                   opacity = .7, 
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r3, feature.id = FALSE,
                                               row.numbers = FALSE), group = '200 - 300 ft') %>%
  addCircleMarkers(data = r4, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r4$source == 'USGS', 'black', NA),
                   opacity = .7,
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r4, feature.id = FALSE,
                                               row.numbers = FALSE), group = '300 - 400 ft') %>% 
  addCircleMarkers(data = r5, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r5$source == 'USGS', 'black', NA),
                   opacity = .7,
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r5, feature.id = FALSE,
                                               row.numbers = FALSE), group = '400 - 500 ft') %>%
  addCircleMarkers(data = r6, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r6$source == 'USGS', 'black', NA),
                   opacity = .7,
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r6, feature.id = FALSE,
                                               row.numbers = FALSE), group = '500 - 600 ft') %>%
  addCircleMarkers(data = r7, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r7$source == 'USGS', 'black', NA),
                   opacity = .7,
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r7, feature.id = FALSE,
                                               row.numbers = FALSE), group = '600 - 700 ft') %>% 
  addCircleMarkers(data = r8, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r8$source == 'USGS', 'black', NA),
                   opacity = .7,
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r8, feature.id = FALSE,
                                               row.numbers = FALSE), group = '700 - 800 ft') %>%
  addCircleMarkers(data = r9, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r9$source == 'USGS', 'black', NA),
                   opacity = .7,
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r9, feature.id = FALSE,
                                               row.numbers = FALSE), group = '800 - 900 ft') %>% 
  addCircleMarkers(data = r10, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r10$source == 'USGS', 'black', NA),
                   opacity = .7,
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r10, feature.id = FALSE,
                                               row.numbers = FALSE), group = '900 - 1000 ft') %>% 
  addCircleMarkers(data = r11, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r11$source == 'USGS', 'black', NA),
                   opacity = .7,
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r11, feature.id = FALSE,
                                               row.numbers = FALSE), group = '> 1000 ft') %>%
  addCircleMarkers(data = r0, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals1(dtw), 
                   fillOpacity = .8, 
                   color = ifelse(r0$source == 'USGS', 'black', NA),
                   opacity = .7,
                   weight = 2.5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r0, feature.id = FALSE,
                                               row.numbers = FALSE), group = '< 0 ft') %>%
  addLayersControl(overlayGroups = c('< 0 ft', '0 - 100 ft', '100 - 200 ft', '200 - 300 ft',
                                     '300 - 400 ft', '400 - 500 ft',
                                     '500 - 600 ft', '600 - 700 ft',
                                     '700 - 800 ft', '800 - 900 ft', '900 - 1000 ft', '> 1000 ft', 'AMA', 'Aquifer'),
                   baseGroups = c("Tiles")) %>% 
  addLegend(pal = pals2,
            values = x$dtw,
            opacity = .9,
            title = 'Depth to water (ft)', # Title
            position = "bottomleft",
            labFormat = function(type, cuts, p) {
              paste0(labels)}) %>% 
  addLegend(pal = colorFactor(c("white", "black"), domain = NULL),
            values = c('AZ', 'USGS'),
            opacity = 0.7,
            title = '',
            position = "bottomleft")

wells = join_time %>% filter(wellid == 5775)
gg = ggplot(data = wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 2) +
  ylim(max(wells$dtw) + 100, 0) +
  labs(# caption = paste('Min depth:', min(wells$dtw), '\nMax depth:',
       #                 max(wells$dtw), '\nNumber of measurements:',
       #                 wells$measurement_dist),
       x = 'Year',
       y = 'DTW (ft)') + 
  theme_bw() +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 20, hjust = 0.5),
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face = 'bold', color="black", size=16), 
        axis.title.y = element_text(face = 'bold', color="black", size=16),
        plot.caption = element_text(face = 'bold', color = 'black', size = 14),
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        legend.position = 'none', 
        strip.text.x = element_text(face = 'bold', size = 10)) 
plot = ggplotly(gg, tooltip = c('x', 'y')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
print(plot)


# 
# j$test <- sub("^", "well", j$test )



tmp = dtw_range(x, 500, 600)
range1 = join_time %>% filter(wellid %in% tmp$wellid)
plotMultipleWells(range1)
plot_well(join_time, '5775')
plotRange(join_spatial, join_time, 400, 500)
plot_well(join_time, 8820)
plot_well(join_time, 18463)
multi_well_plot(join_spatial, join_time, 500, 600)
# depth decreases in 1980
plot_well(join_time, 6682)
plot_well(join_time, 17216)
plot_well(join_time, 351)

plot_well(join_time, 7797)


#######################################################
#######################################################
#######################################################






well = join_spatial %>% filter(wellid == '5775')
buffer = st_buffer(join_spatial[well, ], 10000)
near2 = st_intersection(join_spatial, buffer)

tmp = join_time %>% filter(wellid %in% near2$wellid) %>% 
  group_by(wellid)
plotMultipleWells(tmp)
tmp2 = join_time %>% filter(wellid %in% c('well 2', 'well 3', 'well 256', 'well 719'))
multi_well_plot(join_spatial, join_time, 600, 700)
ggplot(data = tmp, aes(x = date, y = dtw)) +
  geom_line(aes(y = dtw, col = wellid)) +
  facet_wrap(~wellid)

font = list(
  family = 'Courier',
  size = 15,
  color = 'white'
)
plot_well(join_time, 5324)
plot_well = function(df_time, id) {
  label = list(
    bgcolor = '#232F34',
    bordercolor = 'transparent',
    font = font
  )
  font = list(
    family = 'Courier',
    size = 15,
    color = 'white'
  )
  wells = df_time %>% filter(wellid == paste('well', id))
  gg = ggplot(data = wells, aes(x = date, y = dtw)) +
    geom_line(aes(col = wellid), size = 2) +
    scale_y_reverse() +
    # ylim(max(wells$dtw) + 100, 0) +
    labs(title = paste('Well', id),
         caption = paste('Min depth:', min(wells$dtw), '\nMax depth:',
                          max(wells$dtw), '\nNumber of measurements:',
                          wells$measurement_dist),
         x = 'Year',
         y = 'DTW (ft)') + 
    theme_bw() +
    theme(plot.title = element_text(face = 'bold',color = 'black', size = 20, hjust = 0.5),
          axis.text.x = element_text(color="black", size=14), 
          axis.text.y = element_text(color="black", size=14), 
          axis.title.x = element_text(face = 'bold', color="black", size=16), 
          axis.title.y = element_text(face = 'bold', color="black", size=16),
          plot.caption = element_text(face = 'bold', color = 'black', size = 14),
          panel.grid.major = element_line(colour = "#808080"),
          panel.grid.minor = element_line(colour = "#808080", size = 1),
          legend.position = 'none', 
          strip.text.x = element_text(face = 'bold', size = 10)) 
  plot = ggplotly(gg, tooltip = c('x', 'y')) %>%
    style(hoverlabel = label) %>% 
           layout(font = font, 
           yaxis = list(fixedrange = TRUE)) %>% 
    config(displayModeBar = FALSE)
  print(plot)
}
plot_well(join_time, 2624)
multi_well_plot(join_spatial, join_time, 500, 600)
plot1 = ggplot(data = df_time, aes(x = date, y = dtw)) +
  geom_line(aes(y = dtw, col = wellid), size = 1) +
  ylim(max(df_time$dtw) + 100, 0) +
  labs(x = 'Year',
       y = 'DTW (ft)') + 
  theme_bw() +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1))
print(plot1)
label = list(
  bgcolor = '#232F34',
  bordercolor = 'transparent',
  font = font
)

tmp3 = join_time %>%  filter(wellid == 'well 3')
tmp2 = join_time %>% filter(measurement_dist > 30)
  # filter(wellid %in% c('well 36117', 'well 36070', 'well 36129', 'well 36118'))
plot_test = ggplot(data = tmp2, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10)) +
  facet_wrap(~wellid)

ggplotly(plot_test, tooltip = c('x', 'y')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)

##################################################################
##################################################################
##################################################################

# # USGS UNIQUE AND WELL TIME SERIES DATAFRAMES
# usgs = usgs %>% 
#   select(!c(year_dist, source))
# 
# usgs_time = usgs_time %>% 
#   group_by(site_id) %>% 
#   select(!c(year_dist, source))
# 
# ### STATE UNIQUE AND WELL TIME SERIES DATAFRAMES ---------- FILTER FOR THE SITE_ID WHICH DO NOT APPEAR IN USGS TIME SERIES DATAFRAME ###
# state = state %>% 
#   select(!c(year_dist, source)) %>% 
#   filter(!site_id %in% usgs$site_id)
# 
# state_time = state_time %>% 
#   group_by(site_id) %>% 
#   select(!c(year_dist, source)) %>% 
#   filter(!site_id %in% usgs_time$site_id)
# 
# # BIND ROWS FROM USGS WELLS WITH STATE ONLY WELLS + ADD UNIQUE 'WELLID #' BY ROW NUMBER
# join = rbind(usgs, state)
#   # select(!c(agency_cd, year_dist, source))
# 
# # CREATE SF OBJECT FROM LAT + LONG COORDS, EXTRA STEP TAKEN TO REMOVE WELLS WHICH ARE AT THE EXACT SAME LAT + LONG (DUPLICATES)
# join_spatial = join %>% 
#   st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
#   st_transform(5070) %>%
#   as_Spatial() %>% 
#   remove.duplicates() %>% 
#   st_as_sf() 
# 
# 
# join_spatial$wellid = paste("well", 1:nrow(join_spatial)) 
# 
# # BIND ROWS FROM USGS + STATE TIME SERIES DATA, THEN FILTER OUT THE SPATIAL DUPLICATES 
# join_time = rbind(usgs_time, state_time)
#   # select(!c(agency_cd, year_dist, source))
# join_time = left_join(join_time, select(join_spatial, site_id, wellid), by = "site_id") 
# join_time = join_time %>% filter(wellid %in% join_spatial$wellid)
# 
# # FILTER BY THRESHOLD - atleast 10 measurements and atleast 1 measurement every 5 years
# thresh = join_time %>% 
#   group_by(wellid) %>% 
#   filter(measurement_dist >= 10) %>% 
#   mutate(measure_period = year - lag(year)) %>%
#   filter(measure_period > 5)
# 
# join_time = join_time %>% 
#   filter(!wellid %in% thresh$wellid, measurement_dist >= 10) %>%
#   mutate(measure_period = year - lag(year))
# join_time$range = cut(join_time$dtw, breaks = seq(0, 2000, by = 100),
#             labels = paste('R', 1:20, sep = '' )) 
# join_spatial = join_spatial %>%
#   filter(!wellid %in% thresh$wellid) %>%
#   filter(measurement_dist >= 10)
# 
# join_spatial$range = cut(join_spatial$dtw, breaks = seq(0, 2000, by = 100),
#                          labels = paste('R', 1:20, sep = '' )) 



#######################################################
#######################################################


#Criteria: > =10 measurements, >= 5 distinct years, 1 pt after 2010, 1 pt before 1980 

# az_spear <- az_nwis_all %>% #creating new df to calculate Spearman's Rank 
#   group_by(site_id) %>% #by well 
#   dplyr::summarise(spear_rank = cor(dec_date, dtw, 
#                    method='spearman'))
# 
# az_nwis_spear <- inner_join(az_spear, az_nwis_all, by = "site_id") %>% #adding in df with spear ranks and simplifying w/ criteria
#     filter(measurement_dist >= 10) %>% #only kept wells with >= 10 measurments
#   filter(year_dist >=5) %>% #only kept wells with >=5 distinct years
#   filter(date_max >= 2010) %>% #identifying wells that have measurements at or beyond 2010
#   filter(date_min <= 1980) %>% #identifying wells that have measurements at or before 1980
#   distinct(site_id, .keep_all = TRUE) %>%  #only keeping unique rows (don't need all dtw measurements for the purpose of this dataset)
#   select(agency_cd, site_id, lat, long, date_min, date_max, measurement_dist, year_dist, spear_rank) #%>% 
#   #drop_na(lat) #no missing lat, long
# 
# length(unique(az_nwis_spear$site_id))












#######################################################




# my.path = 'assignment.csv'
# 
# df = read.csv(my.path)
# 
# popup <- paste("<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:300px'><b>",
#                '<a href=', df$InfoURL, ' target="_blank"> <h2 align="center"><strong>', df$Name, "</strong></h2></a><br>",
#                "</b>", df$Description, '<br><br><center><img src="', df$ImageURL, '" height="150px" width="200px" border="8"></center><br> </div>')
# 
# 
# map = leaflet() %>%
#   addTiles( ) %>%
#   addMarkers(data = df, lat = df$Latitude, lng = df$Longitude, popup = popup )
# 
# map
# 
# htmlwidgets::saveWidget(map, file = paste0(dirname(my.path), "/map.html"))

# 
# 
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron, group = 'Tiles') %>%
#   addPolygons(data = ama2,
#               fillColor  = ~pals3(OBJECTID), fillOpacity = 0.3,
#               color = 'black',
#               label = ~MAP_LABEL, group = 'AMA') %>%
#   addPolygons(data = aquifer2, fillColor = ~pals4(AQ_NAME), fillOpacity = 0.3,
#               color = 'black',
#               label = ~AQ_NAME, group = 'Aquifer') %>%
#   addCircleMarkers(data = a, #clusterOptions = markerClusterOptions(interactive()),
#                    fillColor = ~pals2(dtw),
#                    fillOpacity = .8,
#                    stroke = T,
#                    popup = leafpop::popupTable(a, feature.id = FALSE,
#                                                row.numbers = FALSE), group = '0 - 100 ft')
# addCircleMarkers(data = b, #clusterOptions = markerClusterOptions(interactive()),
#                  fillColor = ~pals2(dtw),
#                  fillOpacity = .8,
#                  stroke = T,
#                  popup = leafpop::popupTable(b, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '100 - 200 ft') %>%
# addCircleMarkers(data = c, #clusterOptions = markerClusterOptions(interactive()),
#                  fillColor = ~pals2(dtw),
#                  fillOpacity = .8,
#                  stroke = T,
#                  popup = leafpop::popupTable(c, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '200 - 300 ft') %>%
# addCircleMarkers(data = d, #clusterOptions = markerClusterOptions(interactive()),
#                  fillColor = ~pals2(dtw),
#                  fillOpacity = .8,
#                  color = ~pals5(id),
#                  opacity = .8,
#                  stroke = T,
#                  popup = leafpop::popupTable(d, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '300 - 400 ft') %>%
# addCircleMarkers(data = e, #clusterOptions = markerClusterOptions(interactive()),
#                  fillColor = ~pals2(dtw),
#                  fillOpacity = .8,
#                  color = ~pals5(id),
#                  opacity = .8,
#                  stroke = T,
#                  popup = leafpop::popupTable(e, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '400 - 500 ft') %>%
# addCircleMarkers(data = f, #clusterOptions = markerClusterOptions(interactive()),
#                  fillColor = ~pals2(dtw),
#                  fillOpacity = .8,
#                  color = ~pals5(id),
#                  opacity = .8,
#                  stroke = T,
#                  popup = leafpop::popupTable(f, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '500 - 600 ft') %>%
# addCircleMarkers(data = g, #clusterOptions = markerClusterOptions(interactive()),
#                  fillColor = ~pals2(dtw),
#                  fillOpacity = .8,
#                  color = ~pals5(id),
#                  opacity = .8,
#                  stroke = T,
#                  popup = leafpop::popupTable(g, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '600 - 700 ft') %>%
# addCircleMarkers(data = h, #clusterOptions = markerClusterOptions(interactive()),
#                  fillColor = ~pals2(dtw),
#                  fillOpacity = .8,
#                  color = ~pals5(id),
#                  opacity = 1,
#                  stroke = T,
#                  popup = leafpop::popupTable(h, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '700 - 800 ft') %>%
# addCircleMarkers(data = i, #clusterOptions = markerClusterOptions(interactive()),
#                  color = ~pals2(dtw), fillOpacity = .8,
#                  stroke = FALSE,
#                  popup = leafpop::popupTable(i, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '800 - 900 ft') %>%
# addCircleMarkers(data = j, #clusterOptions = markerClusterOptions(interactive()),
#                  color = ~pals2(dtw), fillOpacity = .8,
#                  stroke = FALSE,
#                  popup = leafpop::popupTable(i, feature.id = FALSE,
#                                              row.numbers = FALSE), group = '900 > ft') %>%
# addLegend(pal = pals2, # Previously defined palette
#   values = x$dtw, # Values from data frame
#   opacity = 0.7, # Opacity of legend
#   title = 'Depth to water (ft)', # Title
#   position = "bottomright",
#   labFormat = function(type, cuts, p) {  # Here's the trick
#     paste0(labels)}) %>%
# addLegend( # Legend options
#   pal = pals5, # Previously defined palette
#   values = x$id, # Values from data frame
#   opacity = 0.7, # Opacity of legend
#   title = '', # Title
#   position = "bottomright",
#   labFormat = function(type, cuts, p) {  # Here's the trick
#     paste0(agency_labs)}) %>%
# # addLayersControl(overlayGroups = c("circles"))
# addLayersControl(overlayGroups = c('0 - 100 ft', '100 - 200 ft', '200 - 300 ft',
#                                    '300 - 400 ft', '400 - 500 ft',
#                                    '500 - 600 ft', '600 - 700 ft',
#                                    '700 - 800 ft', '800 - 900 ft', '900 > ft', 'AMA', 'Aquifer'),
#                  baseGroups = c("Tiles"))
#   # 
#   # 


#######################################################
#######################################################
#######################################################

# # match 'well #' from spatial dataframe to corresponding well in timeseries
# usgs_time = az_nwis_all %>% 
#   group_by(site_id) 
# usgs_time = left_join(usgs_time, select(usgs, site_id, wellid), by = "site_id") 
# usgs_time = usgs_time %>% filter(wellid %in% usgs_spatial$wellid)
# 
# # FILTER BY THRESHOLD - atleast 10 measurements and atleast 1 measurement every 5 years
# thresh = usgs_time %>% 
#   group_by(wellid) %>% 
#   filter(measurement_dist >= 10) %>% 
#   mutate(measure_period = year - lag(year)) %>% 
#   filter(any(measure_period > 5))
# usgs_time = usgs_time %>% filter(!wellid %in% thresh$wellid, measurement_dist >= 10) %>% 
#   mutate(measure_period = year - lag(year))
# usgs_spatial = usgs_spatial %>% 
#   filter(!wellid %in% thresh$wellid) %>% 
#   filter(measurement_dist >= 10) 
# # 
# 
# ### STATE SPATIAL + TIMESERIES DATAFRAMES
# 
# # add 'well #' for each unique well
# state = adwr_unique_sites %>% 
#   filter(year >= 1960, year <= 2000)
# state$wellid = paste("well", 1:nrow(state))
#
# # remove wells located at same lat/long but have different well ID
# state_spatial = state %>% 
#   st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
#   st_transform(5070) %>% 
#   as_Spatial() %>% 
#   remove.duplicates() %>% 
#   st_as_sf() %>% 
#   group_by(site_id)
# 
# # match 'well #' from spatial dataframe to corresponding well in timeseries
# state_time = adwr_all %>% 
#   group_by(site_id) 
# state_time = left_join(state_time, select(state, site_id, wellid), by = "site_id") 
# state_time = state_time %>% filter(wellid %in% state_spatial$wellid)
# 
# thresh_state = state_time %>% 
#   group_by(wellid) %>% 
#   filter(measurement_dist >= 10) %>% 
#   mutate(measure_period = year - lag(year)) %>% 
#   filter(any(measure_period > 5))
# state_time = state_time %>% filter(!wellid %in% thresh_state$wellid, measurement_dist >= 10) %>% 
#   mutate(measure_period = year - lag(year))
# state_spatial = state_spatial %>% 
#   filter(!wellid %in% thresh_state$wellid) %>% 
#   filter(measurement_dist >= 10)
# 
# state_time = state_time %>% filter(year >= 1960, year <= 2000)

# # add 'well #' for each unique well
# usgs = az_nwis_unique_sites
# usgs$wellid = paste("well", 1:nrow(usgs))
# 
# # remove wells located at same lat/long but have different well ID
# usgs_spatial = usgs %>% 
#   st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
#   st_transform(5070) %>%
#   as_Spatial() %>% 
#   remove.duplicates() %>% 
#   st_as_sf() %>% 
#   group_by(wellid)
# 
# # match 'well #' from spatial dataframe to corresponding well in timeseries
# usgs_time = az_nwis_all %>% 
#   group_by(site_id) 
# usgs_time = left_join(usgs_time, select(usgs, site_id, wellid), by = "site_id") 
# usgs_time = usgs_time %>% filter(wellid %in% usgs_spatial$wellid)
# 
# # FILTER BY THRESHOLD - atleast 10 measurements and atleast 1 measurement every 5 years
# thresh = usgs_time %>% 
#   group_by(wellid) %>% 
#   filter(measurement_dist >= 10) %>% 
#   mutate(measure_period = year - lag(year)) %>% 
#   filter(any(measure_period > 5))
# usgs_time = usgs_time %>% filter(!wellid %in% thresh$wellid, measurement_dist >= 10) %>% 
#   mutate(measure_period = year - lag(year))
# usgs_spatial = usgs_spatial %>% 
#   filter(!wellid %in% thresh$wellid) %>% 
#   filter(measurement_dist >= 10) 
# 
# ### STATE SPATIAL + TIMESERIES DATAFRAMES
# 
# # add 'well #' for each unique well
# state = adwr_unique_sites %>% 
#   filter(year >= 1960, year <= 2000)
# state$wellid = paste("well", 1:nrow(state))
# # remove wells located at same lat/long but have different well ID
# state_spatial = state %>% 
#   st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
#   st_transform(5070) %>% 
#   as_Spatial() %>% 
#   remove.duplicates() %>% 
#   st_as_sf() %>% 
#   group_by(site_id)
# 
# # match 'well #' from spatial dataframe to corresponding well in timeseries
# state_time = adwr_all %>% 
#   group_by(site_id) 
# state_time = left_join(state_time, select(state, site_id, wellid), by = "site_id") 
# state_time = state_time %>% filter(wellid %in% state_spatial$wellid)
# 
# thresh_state = state_time %>% 
#   group_by(wellid) %>% 
#   filter(measurement_dist >= 10) %>% 
#   mutate(measure_period = year - lag(year)) %>% 
#   filter(any(measure_period > 5))
# state_time = state_time %>% filter(!wellid %in% thresh_state$wellid, measurement_dist >= 10) %>% 
#   mutate(measure_period = year - lag(year))
# state_spatial = state_spatial %>% 
#   filter(!wellid %in% thresh_state$wellid) %>% 
#   filter(measurement_dist >= 10)
# 
# state_time = state_time %>% filter(year >= 1960, year <= 2000)






















     
     
     
     
     
