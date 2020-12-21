
# Angus Watters
# Arizona Groundwater 
# 11-26-2020

library(tidyverse)
library(sf)
library(USAboundaries)
library(sp)
library(leaflet)
library(plotly)
library(RColorBrewer)

source("docs/utils.R")

# usgs = readRDS('data/usgs-unique-sites.rds')
# usgs_time = readRDS("data/usgs-time.rds")
# state = readRDS('data/state-unique-site.rds')
# state_time = readRDS('data/state-time.rds')
# saveRDS(join_time, file = 'data/az/az-join-time.rds')
# saveRDS(join_spatial, file = 'data/az/az-join-spatial.rds')

join_time = readRDS("data/az/az-join-time.rds")
join_spatial = readRDS('data/az/az-join-spatial.rds')

# FILTER BY THRESHOLD - atleast 10 measurements and atleast 1 measurement every 5 years
# thresh = join_time %>% 
#   group_by(wellid) %>% 
#   mutate(measure_period = year - lag(year)) %>%
#   filter(measure_period > 5)
# 
# join_time = join_time %>% 
#   filter(!wellid %in% thresh$wellid, measurement_dist >= 10) %>%
#   mutate(measure_period = year - lag(year))
# 
# join_spatial = join_spatial %>%
#   filter(!wellid %in% thresh$wellid) %>%
#   filter(measurement_dist >= 10)


################################################
#################### TEMP ######################
################################################


hist(join_time$avg_dtw, bteaks = 20)

temp_az = temp2 %>% filter(wellid == c('6001', '5998', '814', '4261')) 

temp_space = join_spatial %>% filter(wellid %in% temp_az$wellid) %>% 
  st_transform(4326)

join_time = join_time %>% group_by(wellid) %>% 
  mutate(range = (max_dtw - min_dtw))

join_time = join_time %>% select(wellid, source, date, dtw,
                                       avg_dtw, min_dtw, max_dtw, range,
                                      date_min, date_max, dec_date, year,
                                      measurement_dist, year_dist, measure_period, time_span,
                                              lat, lng)

temp2 = join_time %>% 
  filter(source == 'AZ', range >= 150, measurement_dist >= 20, time_span >= 40)

temp3 = temp %>% 
  filter(source == 'USGS', range >= 150, measurement_dist >= 20, time_span >= 40)
plotMultipleWells(temp2)

hist(temp$measurement_dist, breaks = 10) 
hist(temp$time_span, breaks = 10) 
hist(temp$measurement_dist, breaks = 10) 
# join_time = join_time %>% select(wellid, source, date, dtw,
#                                  avg_dtw, min_dtw, max_dtw, measurement_dist,
#                                  year_dist, measure_period, time_span, year, date_min, date_max, dec_date, lat, lng)

# ******* 5998, 6001, 6002 near Lake Powell ----> DTW from 500ft rises to ~ 150ft *******
tmp = join_time %>% filter(wellid == c('6001', '5998', '814', '4261')) 
ggplot(data = temp2) +
  geom_pointrange(
    mapping = aes(x = wellid, y = dtw),
    stat = "summary",
    fun.min = min,
    fun.max = max,
    fun = median
  )



plotWell(join_time, 6001)
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = 'Base') %>% 
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>% 
  addPolygons(data = ama2,
              fillColor  = ~pals3(OBJECTID),
              fillOpacity = 0.5,
              color = 'black',
              weight = 1,
              label = ~MAP_LABEL, group = 'AMA') %>%
  addCircleMarkers(data = temp_space, #clusterOptions = markerClusterOptions(interactive()),
                   radius = 4,
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = 'black',
                   # color = ifelse(r1$source == 'USGS', 'black', NA),
                   opacity = .8, 
                   weight = .5,
                   stroke = T,
                   popup = leafpop::popupTable(temp_space, feature.id = FALSE,
                                               row.numbers = FALSE), group = 'ADWR') %>% 
  addLayersControl(overlayGroups = c('USGS', 'ADWR', 'Negative depths', 'AMA', 'Aquifer'),
                   options = layersControlOptions(collapsed = FALSE),
                   baseGroups = c("Base", 'Terrain')) %>% 
  addLegend(pal = pals2,
            values = c(0, 1500),
            opacity = .9,
            title = 'Depth to water (ft)', # Title
            position = "bottomleft",
            labFormat = function(type, cuts, p) {
              paste0(labels)}) 

########################### ADD RANGE COLUMN TO JOIN SPATIAL ##############################
tmp2 = join_spatial %>%
  arrange(date)
tmp1 = join_time %>% group_by(wellid) %>%
  arrange(date) %>%
  slice(n =1)

join_spatial = left_join(join_spatial, select(tmp1, wellid, range), by = 'wellid')

join_spatial = join_spatial %>% select(wellid, source, date, dtw,
                                 avg_dtw, min_dtw, max_dtw, range,
                                 date_min, date_max, dec_date, year,
                                 measurement_dist, year_dist, measure_period, time_span)


################################################
################ NEGATIVE CASES ################
################################################

#Criteria: > =10 measurements, >= 5 distinct years, 1 pt after 2010, 1 pt before 1980 
negatives = join_time %>% filter(year_dist >= 5, dtw <= 0)

neg_time = join_time %>% filter(wellid %in% negatives$wellid)
neg_time = neg_time %>% 
  mutate(case = case_when(wellid == 5490 ~ "1A",
                          wellid == 5534 ~ "1A",
                          wellid == 5538 ~ "1A",
                          wellid == 222 ~ "1C",
                          wellid == 226 ~ "1C",
                          wellid == 259 ~ "1C",
                          wellid == 2633 ~ "1C",
                          wellid == 5240 ~ "1C",
                          wellid == 5985 ~ "1C",
                          wellid == 1 ~ "2C",
                          wellid == 524 ~ "2C",
                          wellid == 5503 ~ "2C",
                          wellid == 5506 ~ "3B"))
    df_list[[length(df_list) + 1]] = df1
  }
}
save_df = bind_rows(df_list)
neg_list = negatives %>%
  group_by(wellid) %>% 
  slice(n = 1) %>% 
  select(wellid, measurement_dist)
plotNegativeWells(neg_time)


plotWell(neg_time, 1)
# CASES:


# 1A ARTESIAN to NON-ARTESIAN (3)
# (negative to positive sequentially) - KEEP

# Well 5490 (38 measurements)
plotWell(neg_time, 5490)

# well 5534
plotWell(neg_time, 5534)

# well 5538
plotWell(neg_time, 5538)

# 1B NON-ARTESIAN to ARTESIAN 
# (positive to negative sequentially) - KEEP unless otherwise noted


# 1C VERY SHALLOW WELL, FLUCUATIONS BETWEEN GROUND SURFACE AND ARTESIAN - KEEP ()

# Well 222 (27 measurements)
plotWell(neg_time, 222)

# well 226 (60 measurements)
plotWell(neg_time, 226)

# well 259 (62 measurements)
plotWell(neg_time, 259)

# well 2633 (29 measurements)
plotWell(neg_time, 2633)

# well 5240 (36 measurements)
plotWell(neg_time, 5240)

#well 5985 (48 measurements)
plotWell(neg_time, 5985)


# Case 1d: Well appears to have a pause in pumping activity (multiple years), returns to artesian, and then is pumped again 

# Case 2a: Measurements are sequential except for a random negative, therefore was likely a recording error

# Case 2b: Measurements are sequential except for periods of negative values that appear to be recorded incorrectly 

# Case 2c: Measurements are sequential and within reason for well depth however all measurements for that well are negative and are likely recorded incorrectly

# well 1 (10 measurements)
plotWell(neg_time, 1)

# well 524 (13 measurements)
plotWell(neg_time, 524)

# well 5503 (16 measurements)
plotWell(neg_time, 5503)


# Case 3a: Measurements are sequential except for truly wonky number which doesn’t make sense

# Case 3b: Measurements are sequential and then dataset changes to record 0 values only, or some other illogical number continuously, which does not make sense

# well 5506 (52 measurements)
plotWell(neg_time, 5506)






spear_rank = join_time %>% 
  group_by(wellid) %>% 
  summarise(spear_rank = cor(dec_date, dtw, method = 'spearman'))
join_spear = inner_join(spear_rank, join_time, by = 'wellid') %>% 
  filter(year_dist >= 5, date_max >= 2010, date_min <= 1980) 

split_df = split(neg_wells, neg_wells$wellid)

################################################
################### LANDSAT ####################
################################################
library(getlandsat)
library(raster)

# Question 1:
bb = read_csv('data/uscities.csv') %>%
  filter(city == 'Chino Valley') %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(7000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

# Question 2:

# Step 1
scenes = lsat_scenes()

bb_wgs = st_transform(bb, 4326) %>% st_bbox()

down = scenes %>%
  filter(min_lat <= bb_wgs$ymin, max_lat >= bb_wgs$ymax, min_lon <= bb_wgs$xmin,
         max_lon >= bb_wgs$xmax,
         as.Date(acquisitionDate) == as.Date('2014-08-08'))

write.csv(down, file = 'data/az/chino-valley-groundwater.csv', row.names = FALSE)

meta = read_csv('data/az/chino-valley-groundwater.csv')

files = lsat_scene_files(meta$download_url) %>%
  filter(grepl(paste0('B', 1:6, '.TIF$', collapse = '|'), file)) %>%
  arrange(file) %>%
  pull(file)

# Step 3
st = sapply(files, lsat_image)
s = raster::stack(st) %>%
  setNames(c('Coastal', 'Blue', 'Green', 'Red', 'NIR', 'SWIR1'))


cropper = bb %>% st_as_sf() %>%
  st_transform(crs(s))
r = crop(s, cropper)

nat_col = plotRGB(r, r = 4, g = 3, b = 2)
inf_NIR = plotRGB(r, r = 5, g = 4, b = 3, stretch = 'lin')
false_SWIR = plotRGB(r, r = 5, g = 6, b = 4, stretch = 'lin')
false_SWIR = plotRGB(r, r = 5, g = 7, b = 1, stretch = 'hist')
false_agr = plotRGB(r, r = 6, g = 5, b = 2, stretch = 'hist')

################################################
############## LEAFLET DATA  ###################
################################################

x = join_spatial %>% st_transform(4326)

# Arizona state shape (CRS = 5070)
az = us_states() %>%
  filter(state_name == 'Arizona') %>% 
  st_transform(5070)

az_outline = az %>% 
  st_cast('MULTILINESTRING') %>% 
  st_transform(4326)

# read in AMA shapefiles
ama = read_sf('data/az-ama-shps/Act_Man_Areas.shp') %>% 
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
library(RColorBrewer)

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

pals1 = colorFactor('cyan', domain = x$dtw)
pals2 = colorNumeric(col2, domain = c(0, 1500))
pals3 = colorBin(col3, domain = 1:8)
pals4 = colorFactor(col4, domain = aquifer2$AQ_NAME)
pals5 = colorFactor(col6, domain = x$source)

# LEGEND LABELS
labels = c('0', '100', '200',
           '300', '400',
           '500', '600',
           '700', '800', '900', '1000')
agency_labs = c('ADWR', 'USGS')
plotWell(join_time, 5998)

adwr_spatial = x %>% 
  filter(source == 'AZ')
adwr_spatial = adwr_spatial %>%
  # filter(dtw < 1000, dtw >= 0) %>% 
  select(wellid, dtw, date, source, measurement_dist)

# records every ~ 1 month
usgs_spatial = x %>% 
  filter(source == 'USGS')
usgs_spatial = usgs_spatial %>%
  # filter(dtw < 1000, dtw >= 0) %>% 
  select(wellid, dtw, date, source, measurement_dist)

r0 =  dtw_range(x, -200, 0) %>% select(wellid, dtw, date,source, measurement_dist)


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = 'Base') %>% 
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>% 
  addPolygons(data = ama2,
              fillColor  = ~pals3(OBJECTID),
              fillOpacity = 0.5,
              color = 'black',
              weight = 1,
              label = ~MAP_LABEL, group = 'AMA') %>%
  # addPolygons(data = aquifer2,
  #             fillColor = ~pals4(AQ_NAME),
  #             fillOpacity = 0.3,
  #             color = 'black',
  #             weight = 1.5,
  #             label = ~AQ_NAME, group = 'Aquifer') %>%
  addPolygons(data = az_outline,
              fillOpacity = 0.1,
              color = 'black',
              weight = 2) %>% 
  addCircleMarkers(data = usgs_spatial, #clusterOptions = markerClusterOptions(interactive()),
                   radius = 4,
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = 'black',
                   # color = ifelse(r1$source == 'USGS', 'black', NA),
                   opacity = .8, 
                   weight = .5,
                   stroke = T,
                   popup = leafpop::popupTable(usgs_spatial, feature.id = FALSE,
                                               row.numbers = FALSE), group = 'USGS') %>% 
  addCircleMarkers(data = adwr_spatial,
                   radius = 4, #clusterOptions = markerClusterOptions(interactive()),
                   fillColor = ~pals2(dtw), 
                   fillOpacity = .8, 
                   color = 'black',
                   # color = ifelse(r2$source == 'USGS', 'black', NA),
                   opacity = .8, 
                   weight = .5,
                   stroke = T,
                   popup = leafpop::popupTable(adwr_spatial, feature.id = FALSE,
                                               row.numbers = FALSE), group = 'ADWR') %>% 
  addCircleMarkers(data = r0, #clusterOptions = markerClusterOptions(interactive()),
                   radius = 4,
                   fillColor = ~pals1(dtw), 
                   fillOpacity = .8, 
                   color = 'black',
                   #color = ifelse(r0$source == 'USGS', 'black', NA),
                   opacity = .8,
                   weight = .5,
                   stroke = TRUE,
                   popup = leafpop::popupTable(r0, feature.id = FALSE,
                                               row.numbers = FALSE), group = 'Negative depths') %>% 
  addLayersControl(overlayGroups = c('USGS', 'ADWR', 'Negative depths', 'AMA', 'Aquifer'),
                   options = layersControlOptions(collapsed = FALSE),
                   baseGroups = c("Base", 'Terrain')) %>% 
  addLegend(pal = pals2,
            values = c(0, 1500),
            opacity = .9,
            title = 'Depth to water (ft)', # Title
            position = "bottomleft",
            labFormat = function(type, cuts, p) {
              paste0(labels)}) 





################################################
################ BUFFER ANALYSIS ###############
################################################
font = list(
  family = 'Courier',
  size = 15,
  color = 'white')
label = list(
  bgcolor = '#232F34',
  bordercolor = 'transparent',
  font = font)

cont_records = join_time %>%
  filter(wellid != 6008) %>% 
  mutate(time_span = max(year) - min(year)) %>% 
  filter(time_span > 50)



# Range: 300 - 500 ft ------------ 65 years
plotRange(cont_records, 300, 500) 
plotBuffer(join_spatial, 4573, 12500)
plotBuffer(join_spatial, 4740, 12500)
plotBuffer(join_spatial, 3472, 12500)

# Range: 400 - 500 ft ------------ 65 years
plotRange(cont_records, 400, 500)
plotBuffer(join_spatial, 4401, 12500)
plotBuffer(join_spatial, 4182, 12500)
plotBuffer(join_spatial, 4993, 20000)

# Range: 500 - 600 ft ------------ 50 years
plotRange(cont_records, 500, 600)
plotBuffer(join_spatial, 5730, 30000)
plotBuffer(join_spatial, 4953, 12500)
plotBuffer(join_spatial, 1853, 20000)
plotBuffer(join_spatial, 5743, 20000)


# # 4573
# w1 = join_spatial %>% filter(wellid == 4573)
# b1 = st_buffer(join_spatial[w1, ], 12500)
# n1 =st_intersection(join_spatial, b1)
# time1 = cont_records %>% filter(wellid %in% n1$wellid)
# plotMultipleWells(time1)


################################################
################################################
################################################



################################################
############## AMA WELL ANALYSIS ###############
################################################

# Joseph City AMA

joseph_outline = ama2 %>% 
  filter(OBJECTID == 1) 

joseph_shp = ama2 %>% 
  filter(OBJECTID == 1) %>% 
  st_intersection(x)
joseph_wells = join_time %>% filter(wellid %in% joseph_shp$wellid)
joseph_pts = join_spatial %>% filter(wellid %in% joseph_shp$wellid)

joseph_gg = ggplot() +
  geom_sf(data = joseph_outline) + 
  geom_sf(data = joseph_pts, aes(fill = dtw, text = 'wellid', size = 2))

ggplotly(joseph_gg, tooltip = c('dtw', 'text')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font) 

plot_test1 = ggplot(data = joseph_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

joseph_ama_wells = ggplotly(plot_test1, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
# Prescott AMA
presc_shp = ama2 %>% 
  filter(OBJECTID == 2) %>% 
  st_intersection(x)
presc_wells = join_time %>% filter(wellid %in% presc_shp$wellid)
plotMultipleWells(presc_wells)

plot_test2 = ggplot(data = presc_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

prescott_ama_wells = ggplotly(plot_test2, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
# Pheonix AMA
pheonix_shp = ama2 %>% 
  filter(OBJECTID == 3) %>% 
  st_intersection(x)
pheonix_wells = join_time %>% filter(wellid %in% pheonix_shp$wellid)
plotMultipleWells(pheonix_wells)

pheonix_wells = pheonix_wells %>% 
  filter(!range %in% c('R7', 'R8', 'R9', 'R10')) %>% 
  mutate(date_length = date_max - date_min) %>% 
  filter(date_length > 40)


plot_test3 = ggplot(data = pheonix_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

pheonix_ama_wells = ggplotly(plot_test3, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)

# Harquahala AMA
harq_shp = ama2 %>% 
  filter(OBJECTID == 4) %>% 
  st_intersection(x)
harq_wells = join_time %>% filter(wellid %in% harq_shp$wellid)
plotMultipleWells(harq_wells)

plot_test4 = ggplot(data = harq_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

ggplotly(plot_test4, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)

# Pinal AMA
pinal_shp = ama2 %>% 
  filter(OBJECTID == 5) %>% 
  st_intersection(x)
pinal_wells = join_time %>% filter(wellid %in% pinal_shp$wellid)
plotMultipleWells(pinal_wells)

plot_test5 = ggplot(data = pinal_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

ggplotly(plot_test5, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
# Tuscon AMA
tucson_shp = ama2 %>% 
  filter(OBJECTID == 6) %>% 
  st_intersection(x)
tucson_wells = join_time %>% filter(wellid %in% tucson_shp$wellid)
plotMultipleWells(tucson_wells)

plot_test6 = ggplot(data = tucson_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')
# Joseph City AMA
joseph_shp = ama2 %>% 
  filter(OBJECTID == 1) %>% 
  st_intersection(x) %>% 
  mutate(name = 'JOSEPH CITY')
joseph_wells = join_time %>% filter(wellid %in% joseph_shp$wellid)


plot_test1 = ggplot(data = joseph_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  ylim(1000, 0) +
  geom_smooth(col = 'black') +
  labs(title = paste(joseph_shp$name),
       x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10))

joseph_ggplotly = ggplotly(plot_test1, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
joseph_ggplotly
ggplotly(plot_test6, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
# Douglas AMA
douglas_shp = ama2 %>% 
  filter(OBJECTID == 7) %>% 
  st_intersection(x)
douglas_wells = join_time %>% filter(wellid %in% douglas_shp$wellid)

plot_test7 = ggplot(data = douglas_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

ggplotly(plot_test7, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)


# Santa Cruz AMA
santacruz_shp = ama2 %>% 
  filter(OBJECTID == 8) %>% 
  st_intersection(x)
santacruz_wells = join_time %>% filter(wellid %in% santacruz_shp$wellid)

plot_test8 = ggplot(data = santacruz_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

ggplotly(plot_test8, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)


######################################################
################### NEGATIVE DTW #####################
######################################################



#Criteria: > =10 measurements, >= 5 distinct years, 1 pt after 2010, 1 pt before 1980 

spear_rank = join_time %>% 
  group_by(wellid) %>% 
  summarise(spear_rank = cor(dec_date, dtw, method = 'spearman'))
join_spear = inner_join(spear_rank, join_time, by = 'wellid') %>% 
  filter(year_dist >= 5, date_max >= 2010, date_min <= 1980) 

neg_wells = join_spear %>% filter(dtw < 0)
split_df = split(neg_wells, neg_wells$wellid)

neg1 = join_time %>% filter(wellid %in% r0$wellid)


######### Negative well time series ###############

# well 1
neg2 = join_spatial %>% filter(wellid == 1)
buff2 = st_buffer(join_spatial[neg2, ], 15000)
near_neg2 = st_intersection(join_spatial, buff2)
near_time2 = join_time %>% filter(wellid %in% near_neg2$wellid)
plotNegativeWells(near_time2)
# well 222
neg3 = join_spatial %>% filter(wellid == 222)
buff3 = st_buffer(join_spatial[neg3, ], 12500)
near_neg3 = st_intersection(join_spatial, buff3)
near_time3 = join_time %>% filter(wellid %in% near_neg3$wellid)
plotNegativeWells(near_time3)
# well 524
neg1 = join_spatial %>% filter(wellid == 524)
buff1 = st_buffer(join_spatial[neg1, ], 15000)
near_neg1 = st_intersection(join_spatial, buff1)
near_time1 = join_time %>% filter(wellid %in% near_neg1$wellid)
plotNegativeWells(near_time1)
# Chino Valley
neg4 = join_spatial %>% filter(wellid == 5534)
buff4 = st_buffer(join_spatial[neg4, ], 12500)
near_neg4 = st_intersection(join_spatial, buff4)
near_time4 = join_time %>% filter(wellid %in% near_neg4$wellid)
plotNegativeWells(near_time4)



######################################################
################### RANGE ANALYSIS ###################
######################################################

# SLOPE
slope = join_time %>% 
  group_by(wellid) %>% 
  mutate(rownum = row_number()) %>% 
  summarise(slope = lm(dtw ~ rownum)$coefficients['rownum'])


# 250 - 400 ft
range300 = join_time %>% 
  filter(dtw > 250, dtw < 400) %>% 
  add_count(wellid) %>% 
  mutate(sd = sd(dtw)) %>% 
  filter(n >= 100, sd < 36)
plotMultipleWells(range300)
library(htmlwidgets)

htmlwidgets::saveWidget(prescott_ggplotly, file = 'img/prescott-ama-plot.')
prescott_ggplotly
# 400 - 600 ft
range400 = join_time %>% 
  filter(dtw > 400, dtw < 600) %>% 
  add_count(wellid) %>% 
  mutate(sd = sd(dtw)) %>% 
  filter(n >= 60, sd < 55)

plotMultipleWells(range400)

BAMMtools::getJenksBreaks(range400$sd, 4)

# 600 - 1100 ft
range600 = join_time %>% 
  filter(dtw > 600, dtw < 1100) %>% 
  add_count(wellid) %>% 
  mutate(sd = sd(dtw)) %>% 
  filter(n >= 50)

plotMultipleWells(range0)


hist(range600$n)

hist(range0$n, breaks = 100)

# Wells with dramatic decrease in DTW
# 5934
# 5936

# 3211
# 4993

# 5934
decr1 = join_spatial %>% filter(wellid == 5934)
rad1 = st_buffer(join_spatial[decr1, ], 15000)
near_decr1 = st_intersection(join_spatial, rad1)
decr1_time = join_time %>% filter(wellid %in% near_decr1$wellid)
plotMultipleWells(decr1_time)


decr2 = join_spatial %>% filter(wellid == 4993)
rad2 = st_buffer(join_spatial[decr2, ], 15000)
near_decr2 = st_intersection(join_spatial, rad2)
decr2_time = join_time %>% filter(wellid %in% near_decr2$wellid)
plotMultipleWells(decr2_time)

decr3 = join_spatial %>% filter(wellid == 3211)
rad3 = st_buffer(join_spatial[decr3, ], 15000)
near_decr3 = st_intersection(join_spatial, rad3)
decr3_time = join_time %>% filter(wellid %in% near_decr3$wellid)
plotMultipleWells(decr3_time)

# post 1980 well replenishment
# 4401
# 4493
# 4070








