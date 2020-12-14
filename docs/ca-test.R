

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

# READ IN DATA ---------------- (USGS + STATE DATA JOINED)
join_time = readRDS('data/ca/ca-join-all.rds')
join_spatial = readRDS('data/ca/ca-join-spatial.rds')

# USGS & STATE DATA
# usgs_time = readRDS('data/ca/ca-usgs-all.rds')
# state_time = readRDS('data/ca/ca-state-all.rds')

# saveRDS(state, file = 'data/ca/ca-state-all.rds')
# saveRDS(usgs_time, file = 'data/ca/ca-usgs-all.rds')
# saveRDS(join_time, file = 'data/ca/ca-join-all.rds')
# saveRDS(join_spatial, file = 'data/ca/ca-join-spatial.rds')

###############################################
######### CLEANING STATE & USGS DATA ##########
###############################################

# state = readr::read_delim('data/ca/gama_all_dtw_elev_clean_LB.txt', delim ="\t")
state = read.delim('data/ca/gama_all_dtw_elev_clean_LB.txt')

state = state %>% 
  rename(date = MEASUREMENT.DATE)
  rename(site_id = WELL.NUMBER, dtw = DEPTH.TO.WATER, source = SOURCE,
         gw_elev = GW.ELEVATION, lat = LATITUDE, lng = LONGITUDE)

state = state %>%
  # mutate(date = as.factor(date)) %>%#converting from character to factor to change to date
  # mutate(date = as.Date(parse_date_time(date, c('mdy', 'ymd_hms')))) %>%
  mutate(year = year(date)) %>% #extracting year to determine unique years later
  mutate(dec_date = decimal_date(date)) %>%
  group_by(site_id) %>% #by well...calculate:
  mutate(date_min = min(dec_date),
         date_max = max(dec_date),
         measurement_dist = n_distinct(dtw), #distinct well measurements by well
         year_dist = n_distinct(year))
state = state %>% 
  mutate(time_span = max(year) - min(year)) %>%
  mutate(measure_period = year - lag(year)) %>% 
  select(site_id, source, year, date, dec_date, date_min, date_max,
       dtw, gw_elev, measurement_dist,
       year_dist, time_span, measure_period, lat, lng)

usgs_time = ca_nwis_all %>%
  drop_na(dtw) %>%
  mutate(time_span = max(year) - min(year)) %>%
  arrange(site_id, year) %>%
  mutate(measure_period = year - lag(year)) %>% 
  select(!agency_cd)
# usgs_time = usgs_time %>% rename(lat = lat_nad83, lng = long_nad83)
usgs_time = usgs_time %>% 
  mutate(dec_date = decimal_date(date), source = 'USGS') %>% 
  select(site_id, source, year, date, dec_date, date_min, date_max,
                                   dtw, measurement_dist,
                                   year_dist, time_span, measure_period, lat, lng)

###############################################
###############################################





##############################################
########## JOIN USGS + STATE DATA  ###########
##############################################
  
# *** NOTE *** --- go back and figure out wells that pick up where other left off, ex. (2002-11, 2011-19) -> T10000002709 - MW-7 

# BIND ROWS OF USGS & STATE WELLS
join_time = rbind(usgs_time, state)

# CREATE COMMON WELL ID 
join_time$wellid = join_time %>% group_indices(site_id) 

# DATAFRAME WITH EACH UNIQUE WELL ---- (MEASUREMENTS ARE FROM MOST RECENT RECORDING)
join_spatial = join_time %>% 
  group_by(site_id) %>% 
  arrange(desc(date)) %>% 
  slice(n = 1) 

# REMOVE DUPLICATES BY IDENTICAL LAT + LONG
join_spatial = join_spatial %>% 
  arrange(desc(measurement_dist)) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4269) %>% 
  st_transform(5070) %>%
  as_Spatial() %>% 
  remove.duplicates() %>% 
  st_as_sf() %>% 
  group_by(site_id)  

# FILTER OUT DUPLICATES FROM TIME SERIES 
join_time = join_time %>%
  filter(wellid %in% join_spatial$wellid)

# CHANGE WELLID TYPE FROM INT TO CHR
join_time = join_time %>% mutate(wellid = as.character(wellid))
join_spatial = join_spatial %>% mutate(wellid = as.character(wellid))

##############################################
############## THRESHOLDING ##################
##############################################

# **** RUN AFTER READING IN DATA ****
# THRESHOLD ------ (10 - 20 DISTINCT MEASUREMENTS & 5 DIST YEARS)

join_time = join_time %>%  filter(measurement_dist >= 20, year_dist >= 5)

join_spatial = join_spatial %>% filter(measurement_dist >= 20, year_dist >= 5)

# records every ~6 months
dwr_time = join_time %>% 
  filter(source == 'DWR Well')

# records 4 times a year
emw_time = join_time %>% 
  filter(source == 'Environmental Monitoring Well')

# records every ~ 1 month
usgs_time = join_time %>% 
  filter(source == 'USGS')

hist(usgs_time$dtw, breaks = 100)


################################################################################
##################### LEAFLET + ACTIVE MANAGEMENT AREAS #########################
################################################################################

# Arizona state shape (CRS = 5070)
ca = us_states() %>%
  filter(state_name == 'California') %>% 
  st_transform(5070)

# # read in AMA shapefiles
# ama = read_sf('data/Act_Man_Areas.shp') %>% 
#   st_transform(5070) %>% 
#   st_cast("MULTIPOLYGON")
# ama2 = ama %>% st_transform(4326)
# 
# # US Aquifers
# aquifer = read_sf('data/shps/us_aquifers.shp') %>% 
#   st_transform(5070) %>% 
#   st_cast("MULTIPOLYGON")
# 
# aquifer2 = aquifer %>% st_transform(4326)
# 
# box = az %>% st_transform(4326) %>% 
#   st_bbox() %>% 
#   st_as_sfc()
# temp = st_intersects(aquifer2, box)
# 
# aquifer2 = aquifer2[which(lengths(temp) != 0), ]
# 
# temp2 = join_time %>% filter(dtw < 0)

### LEAFLET MAPS

x = join_spatial %>% st_transform(4326) 

#RColorBrewer::display.brewer.all(n=4, exact.n=FALSE)

nb.cols = 10
nb.cols2 = 2
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

labels = c('0', '100', '200',
           '300', '400',
           '500', '600',
           '700', '800', '900')
agency_labs = c('DWR Well', 'USGS')

#################################################################
################ LEAFLET --- GROUP BY SOURCE ####################
#################################################################


#################################################################
#################################################################



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










#################################################################
#################################################################

# usgs_time = usgs_time %>% 
#   filter(measurement_dist >= 10) %>% 
#   arrange(site_id, year) %>% 
#   mutate(measure_period = year - lag(year)) 
#   
# ### STATE UNIQUE AND WELL TIME SERIES DATAFRAMES ---------- FILTER FOR THE SITE_ID WHICH DO NOT APPEAR IN USGS TIME SERIES DATAFRAME ###
# state = state %>% 
#   select(!c(source))
# state_time = state_time %>% 
#   mutate(id = 'AZ') %>%
#   group_by(site_id) %>% 
#   select(!c(agency_cd, source))
# state_time = state_time %>% 
#   group_by(site_id) %>% 
#   filter(measurement_dist >= 10) %>% 
#   arrange(site_id, year) %>% 
#   mutate(measure_period = year - lag(year))
# join_time = rbind(usgs_time, state_time) %>% 
#   group_by(site_id) %>%
#   arrange(desc(site_id))
# join_time$wellid = join_time %>% group_indices(site_id) 
# join_spatial = join_time %>% 
#   st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
#   st_transform(5070) %>%
#   as_Spatial() %>% 
#   remove.duplicates() %>% 
#   st_as_sf() %>% 
#   group_by(wellid)
# 
# 
# # add well # for each unique well
# usgs = ca_nwis_unique_sites
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
# # match well # from spatial dataframe to corresponding well in timeseries
# usgs_time = ca_nwis_all %>% 
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
# # add  for each unique well
# state = adwr_unique_sites %>% 
#   filter(date > 1960-01-01, date < as.Date.character('2000-01-01'))
# state$wellid = paste("well", 1:nrow(state))
# 
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
# # match  from spatial dataframe to corresponding well in timeseries
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
# 
# 
# 








