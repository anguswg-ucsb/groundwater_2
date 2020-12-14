


library(tidyverse)
library(readxl)
library(sf)
library(USAboundaries)
library(USAboundariesData)
library(dataRetrieval)
library(janitor)
library(lubridate)

# Arizona Dept Water Resource dtw data
adwr_dtw= read_xlsx('data/gwis_dtw.xlsx') %>% # read in AZ dtw data
  clean_names()  %>% 
  rename(site_well_site_id = wlwa_site_well_site_id) # rename columns for compatibility

# read in AZ dtw data
  #rename(site_id = WLWA_SITE_WELL_SITE_ID, date = WLWA_MEASUREMENT_DATE, 
         #dtw_ft= WLWA_DEPTH_TO_WATER) # rename columns for ease of use

# Arizona Dept Water Resource well sites
adwr_site = read_xlsx('data/gwis_sites.xlsx') %>%  # read in AZ site location data
  clean_names()
  #rename(site_id = SITE_WELL_SITE_ID, lat = SITE_LATITUDE_DECIMAL, lon = SITE_LONGIT_DECIMAL, sisrc_code = SITE_SISRC_CODE)
  #st_as_sf(coords=c('lon', 'lat'), crs = 4269) %>% # adds geometries to joined data and CRS of 4269 # selects needed columns
  #st_transform(5070) # rename columns for ease of use



# join well site data with site sf object
adwr_join = inner_join(adwr_dtw, adwr_site, by = "site_well_site_id") %>%  #joining by site ID
  select(site_sisrc_code, site_well_site_id, wlwa_measurement_date, wlwa_depth_to_water)

# Arizona shape in 5070
az_shp = us_states() %>% # AZ state shape
  filter(name == 'Arizona') %>%  # filters only state of AZ
  st_transform(4269) # reiterates CRS = 4269

# Makes AZ dataframe an Sf object, assigning the site long/lats to CRS = 4269
az_state_spatial = adwr_site %>% 
  st_as_sf(coords=c("site_longit_decimal", "site_latitude_decimal"), crs = 4269) %>% 
  mutate(lat_nad83 = unlist(map(geometry,2)), 
         long_nad83 = unlist(map(geometry,1))) # brings back lat/long columns that were used to make geometry features

# Clip well site data to Arizona boundary and filter to date after 2010
adwr_az_clip = st_intersection(az_shp, az_state_spatial) %>% 
  select(site_well_site_id, lat_nad83, long_nad83)

  
  # selects needed columns
  #group_by(site_id) %>% 
  #arrange(desc(date)) %>% 
  #slice(n = 1)


length(unique(adwr_az_clip$site_well_site_id)) #checking in on how many sites there are left 

#az_state_map = ggplot() +
 # geom_sf(data = adwr_az_clip,
  #        colour = "forestgreen", 
   #       size = 0.1,
    #      alpha = 0.5) +
  #geom_sf(data = az_shp,
    #      fill = NA,
   #       colour = "black") +
  #theme_classic()


#Create df with only wells inside the state boundary 
az_state_clean_clip = inner_join(adwr_join, adwr_az_clip, by = "site_well_site_id") %>% 
  select(!geometry)

length(unique(az_state_clean_clip$site_well_site_id)) #checking in on how many sites there are left 


az_join_deb_all = az_state_clean_clip %>% 
  select(agency_cd = site_sisrc_code, site_id = site_well_site_id, 
         dtw_ft = wlwa_depth_to_water, lat_nad83, long_nad83, measure_date = wlwa_measurement_date) %>% #renaming to have reproducible code below
  drop_na(dtw_ft) %>% #dropping rows that do not have a well measurement
  drop_na(lat_nad83) %>% #dropping rows that do not have spatial info 
  mutate(date = as.Date(measure_date, "%m/%d/%Y")) %>% #putting date into useable format
  mutate(year = lubridate::year(date)) %>% #extracting year to determine unique years later
  mutate(dec_date = decimal_date(date)) %>% #converting to decimal date
  #mutate(check_date = date_decimal(dec_date, tz = "UTC")) #decimal dates look correct
  group_by(site_id) %>% #by well...calculate
  mutate(date_min = min(dec_date), 
         date_max = max(dec_date),
         measurement_dist = n_distinct(dtw_ft), #distinct well measurements by well
         year_dist = n_distinct(year))

adwr_all = az_join_deb_all %>% 
  select(agency_cd, site_id, date, dtw = dtw_ft, date_min, date_max, year, measurement_dist, year_dist, lat_nad83, long_nad83) %>% 
  mutate(source = "lb_state")

adwr_unique_sites = az_join_deb_all %>% 
  select(agency_cd, site_id, date, dtw = dtw_ft, date_min, date_max, year, measurement_dist, year_dist, lat_nad83, long_nad83) %>% 
  mutate(source = "lb_state") %>% 
  arrange(desc(date)) %>% 
  distinct(site_id, .keep_all = TRUE) %>% 
  mutate(id = 'AZ')










### ANGUS CODE:
# 
# tmp = adwr_unique_sites %>% 
#   mutate(x = 1:n())
# tmp2 = nwis_dtw_time %>% 
#   filter(site_id == 332703111530001)
# # TIME SERIES GGPLOT - DTW vs. TIME
# # expand usgs data to 1970 - 1990, highlighting 1980
# 
# # arizona state dtw vs time
# adwr_dtw_time = adwr_all %>% 
#   filter(date > 1975-01-01, date < as.Date('1985-01-01'), measurement_dist >= 300) %>% 
#   group_by(site_id) %>% 
#   mutate(delta = dtw_ft - lag(dtw_ft), site = 1:n()) %>% 
#   filter(delta < 100, delta > -100) %>% 
#   arrange(site_id)
# 
# 
#  # USGS dtw vs time
# nwis_dtw_time = az_nwis_all %>% 
#   filter(date > 1975-01-01, date < as.Date.character('1985-01-01'), measurement_dist >= 75) %>% 
#   group_by(site_id) %>% 
#   mutate(delta = dtw_ft - lag(dtw_ft), site = 1:n()) %>% 
#   filter(delta < 100, delta > -100) %>% 
#   arrange(site_id)
# 
# #tmp3 = adwr_nwis_join_all %>% 
#  # filter(date > as.Date('1975-01-01'), date < as.Date('1985-01-01'), measurement_dist >= 50) %>% 
#   #group_by(site_id) %>% 
#   #arrange(site_id)
# 
# 
# # ADWR dtw vs. time (1975-85) displaying sites with over 300 measurements
# #adwr_dtw_time_plot = 
# adwr_dtw_time %>%  
#   ggplot(aes(x = date, y = dtw_ft)) +
#   geom_line(aes(y = dtw_ft, col = site_id), size = 1) +
#   labs(title = 'DTW vs.time (1975-1985)',
#        x = 'Date',
#        y = 'DTW (ft)',
#        subtitle = 'Arizona Dept. Water Resources data') +
#   scale_y_reverse() +
#   theme_grey() 
# 
#    # ADWR change in dtw (delta)
# #adwr_dtw_delta_plot = 
# adwr_dtw_time %>%  ggplot(aes(x = date, y = dtw_ft)) +
#   geom_line(aes(y = delta, col = site_id), size = 1) +
#   labs(title = 'DTW delta period between 1975-1985',
#        x = 'Date',
#        y = 'Change in measurement (ft)',
#        subtitle = 'Arizona Dept. Water Resources data') +
#   theme_grey() 
# 
# # NWIS dtw vs. time (1975-85) displaying sites with over 100 measurements
# #nwis_dtw_plot = 
# nwis_dtw_time %>% ggplot(aes(x = date, y = dtw_ft)) +
#   geom_line(aes(y = dtw_ft, col = site_id), size = 1) +
#   labs(title = 'DTW vs.time (1975-1985)', 
#        x = 'Date', 
#        y = 'DTW (ft)', 
#        subtitle = 'USGS data') +
#   scale_y_reverse() +
#   theme(axis.text.x = element_text(size = 25)) +
#   theme_grey() 
# 
# # NWIS change in dtw (1975-85) (delta)
# nwis_dtw_delta_plot = nwis_dtw_time %>%  ggplot(aes(x = date, y = dtw_ft)) +
#   geom_line(aes(y = delta, col = site_id), size = 1) +
#   labs(title = 'DTW delta period between 1975-1985',
#        x = 'Date',
#        y = 'Change in measurement (ft)',
#        subtitle = 'USGS data') +
#   theme_grey()
# 
# 
# ### SPACIAL CODE - IDENTIFYING DISTANCE BETWEEN MATCHING SITE IDs IN ADWR & USGS DATA ### 
# adwr_nwis_join = inner_join(adwr_unique_sites, az_nwis_unique_sites, by ='site_id') %>% 
#   arrange(site_id) %>% 
#   unique()
# #select(site_id, id.x, date.x, dtw_ft, lat_nad83.x, long_nad83.x, id.y, date.y, dtw, lat_nad83.y, long_nad83.y)
# 
# 
# adwr_nwis_join_all = inner_join(adwr_all, az_nwis_all, by ='site_id')
# 
# range = adwr_unique_sites %>% filter(date > as.Date('1975-01-01'), date < as.Date('1990-01-01'))
# # ADWR and NWIS join 
# 
# adwr_nwis_join = inner_join(adwr_unique_sites, az_nwis_unique_sites, by ='site_id') %>% 
#   arrange(site_id)
# #select(site_id, id.x, date.x, dtw_ft, lat_nad83.x, long_nad83.x, id.y, date.y, dtw, lat_nad83.y, long_nad83.y)
# 
# 
# adwr_nwis_join_all = inner_join(adwr_all, az_nwis_all, by ='site_id')
# 
# 
# dist_join = adwr_nwis_join %>% 
#   select(site_id, id.x, date.x, dtw_ft, lat_nad83.x, long_nad83.x, id.y, date.y, dtw, lat_nad83.y, long_nad83.y) %>% 
#   mutate(dist_long = ((long_nad83.x - long_nad83.y) * cos(lat_nad83.y)), 
#          dist_lat = (lat_nad83.x - lat_nad83.y), dist = sqrt((dist_long * dist_long) + (dist_lat * dist_lat)), 
#          dist_meter = dist * 111139)
# 
# 
# 
# dist_join = adwr_nwis_join %>% 
#   select(site_id, id.x, date.x, dtw_ft, lat_nad83.x, long_nad83.x, id.y, date.y, dtw, lat_nad83.y, long_nad83.y) %>% 
#   mutate(dist_long = ((long_nad83.x - long_nad83.y) * cos(lat_nad83.y)), 
#          dist_lat = (lat_nad83.x - lat_nad83.y), dist = sqrt((dist_long * dist_long) + (dist_lat * dist_lat)), 
#          dist_meter = dist * 111139)
# 
# 
# 
# max(dist$dist_id)
# max(dist_join$dist_meter)
# mean(dist$dist_id)
# mean(dist_join$dist_meter)
# median(dist$dist_id)
# median(dist_join$dist_meter)
# 
# # ADWR/NWIS Join with ADWR geometries
# adwr_geom = adwr_nwis_join %>% 
#   mutate(row_number= 1:n()) %>% 
#   st_as_sf(coords = c('long_nad83.x', 'lat_nad83.x'), crs = 4269) %>% 
#   mutate(lat_nad83.x = unlist(map(geometry,2)), 
#          long_nad83.x = unlist(map(geometry,1))) %>% 
#   st_transform(5070)
# 
# # ADWR/NWIS Join with NWIS geometries
# nwis_geom = adwr_nwis_join %>% 
#   mutate(id = row_number()) %>% 
#   st_as_sf(coords = c('long_nad83.y', 'lat_nad83.y'), crs = 4269) %>% 
#   st_transform(5070)
# 
# # Find the nearest geometry feature in ADWR to NWIS geometries  
# 
# #near = adwr_geom %>% group_by(site_id) %>% 
#  # mutate(np = st_nearest_feature(geometry, nwis_geom), 
#   #       id = which(nwis_geom$site_id == site_id),
#    #      dist_np = as.numeric(st_distance(geometry, nwis_geom[np,])), dist_id = as.numeric(st_distance(geometry, nwis_geom[id,]))) %>% # dist_np column calcs the distance from each ADWR site to its nearest feature
#   #select(site_id, np , dist_np, id, dist_id, lat_nad83.y, long_nad83.y, lat_nad83.x, long_nad83.x)
# 
# #near = adwr_geom %>% group_by(site_id) %>% 
#   #mutate(np = st_nearest_feature(geometry, nwis_geom), 
#    #      dist_np = as.numeric(st_distance(geometry, nwis_geom[np,]))) %>% # dist_np column calcs the distance from each ADWR site to its nearest feature
#   #select(site_id, np, dist_np, lat_nad83.y, long_nad83.y, lat_nad83.x, long_nad83.x)
# 
# dist = adwr_geom %>% group_by(site_id) %>% 
#   mutate(id = which(nwis_geom$site_id == site_id), dist_id = as.numeric(st_distance(geometry, nwis_geom[id,]))) %>% # dist_np column calcs the distance from each ADWR site to its nearest feature
#   select(site_id, id, dist_id, lat_nad83.y, long_nad83.y, lat_nad83.x, long_nad83.x)
# 
# 
# # what does it mean when a site from ADWR is nearest to several rows in NWIS ??
# adwr_filt = adwr_unique_sites %>% filter(site_id %in% c('313312110082301', '313312110082302', '313312110082303')) %>% 
#   st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
#   st_transform(5070)
# nwis_filt = az_nwis_unique_sites %>% filter(site_id %in% c('313312110082301', '313312110082302', '313312110082303')) %>% 
#   st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
#   st_transform(5070)
# 
# tmp1 = az_state_clean_clip %>% filter(site_well_site_id == ('313312110082301'))
#                                                                         
#                                       
# tmp2 = az_state_clean_clip %>% filter(site_well_site_id == ('313312110082302'))
# tmp3 = az_state_clean_clip %>% filter(site_well_site_id == ('313312110082303'))
# 
# # ggplot time series dtw vs date of state vs usgs data
# # expand usgs data to 1970 - 1990, highlighting 1980
# 
#   
#   
#   
# 
# #ggplot() +
#  # geom_line(data = tmp1, aes(col = 'blue'))
# 
# # drop ADWR geometries and keeps respective Lat/longs by unlisting
# adwr_nogeo = adwr_geom %>%  
#   mutate(lat_nad83.x = unlist(map(geometry,2)), 
#          long_nad83.x = unlist(map(geometry,1))) %>% 
#   st_drop_geometry() %>% 
#   select(site_id, lat_nad83.x, long_nad83.x)
# 
# # drop NWIS geometries and keeps respective Lat/longs by unlisting
# nwis_nogeo = nwis_geom %>% 
#   mutate(lat_nad83.y = unlist(map(geometry,2)), 
#          long_nad83.y = unlist(map(geometry,1))) %>% 
#   st_drop_geometry() %>% 
#   select(site_id, lat_nad83.y, long_nad83.y)
#   
# # Join ADWR/NWIS data frames to get np column with NWIS lat/long
# tmp3 = inner_join(adwr_nogeo, nwis_nogeo, by = 'site_id') %>% 
#   select(site_id, np, dist_np, lat_nad83.y, long_nad83.y) %>% 
#   st_as_sf(coords = c('long_nad83.y', 'lat_nad83.y'), crs = 5070) # adds NWIS geometries back to dataframe that now contains np and dist_np columns
# 
# # add back ADWR geometries  
# tmp4 = adwr_nogeo %>% 
#   st_as_sf(coords = c('long_nad83.x', 'lat_nad83.x'), crs = 5070) %>% 
# 
# x = st_distance(tmp4[200,], tmp3[200,])
# 
# # Summary
# max_dist = max(near$dist_id) # maximum distance = 11,2021 meters (11 km)
# min_dist = min(near$dist_id) # min distance =  0.03 meters
# mean_dist =mean(near$dist_id) # mean distance = 417 meter
# quantile(near$dist_id, probs = c(0.25, 0.50, 0.75))
# median(near$dist_id)
# hist(near$dist_id, breaks = 20)
# 
# 
# outlier = near %>% 
#   arrange(-dist_id) %>% 
#   head(10)



# 
# which(outlier$site_id == 340923113413401)
# nwis_geom %>% filter(site_id == 340923113413401)
# adwr_geom %>% filter(site_id == 340923113413401)
# adwr_nwis_join[230,]
# # ADWR site ID, NWIS index (340923113413401,299), (313610110163201,22)
# 
# 
# ggplot() +
#   geom_sf(data = az_shp,
#           fill = NA,
#           colour = "black") +
#   geom_sf(data = nwis_filt, aes(col = 'red')) +
#   geom_sf(data = outlier[2,], aes(col = 'green')) +
#   geom_sf(data = nwis_geom[299,], aes(col = 'red')) +
#   geom_sf(data = outlier[1,], aes(col = 'green'))
# 
# ggplot() +
#   geom_sf(data = az_shp,
#           fill = NA,
#           colour = "black") +
#   geom_sf(data = nwis_filt, aes(col = 'red'), size = 5) +
#   geom_sf(data = adwr_filt, aes(col = 'green'), size = 4)


# ADWR site ID, NWIS index (340923113413401,299), (313610110163201,22)
# NWIS
#

# max_dist = max(near$dist_np) # maximum distance = 7439 meters
# min_dist = min(near$dist_np) # min distance =  0.03 meters
# mean_dist =mean(near$dist_np) # mean distance = 112 meter
# hist(near$dist_np, breaks = 50)
# 
# 
# 
# ggplot() +
#   geom_sf(data = nwis_geom,
#           colour = "forestgreen", 
#           size = 4,
#           alpha = .5)+
#   geom_sf(data = az_shp,
#           fill = NA,
#           colour = "black") +
#   geom_sf(data = adwr_geom, colour = "blue", 
#           size = 2,
#           alpha= 0.5)
# theme_classic()




















# Full join of ADWR well data with USGS data
#gwis_nwis_join = full_join(gwis_az_clip, az_all, by ='site_id') %>% 
 # select(sisrc_code, site_id, date.x, date.y, dtw_ft.x, dtw_ft.y, lat_nad83, long_nad83) %>% 
  #arrange(site_id) 
# Extract unique rows from join and then removed duplicate site_id rows 
#unique_wells = gwis_nwis_join %>% 
#  distinct(site_id, .keep_all = TRUE)

#unique_wells_2 = unique(gwis_nwis_join)
#mutate lat long columns with x, y 
#drop geoms



# filters only wells which have data from ADWR and USGS 
#tmp = unique_wells %>% na.omit() %>%st_as_sf(coords = c('x', 'y'), crs = 4269) %>% 
 # st_transform(5070) %>% 
#  na.omit()

#removes the ADWR geometry and creates a new geometry from the lat/long coordinates given by USGS
#tmp2 = tmp %>% st_drop_geometry() %>% 
 # st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
  #st_transform(5070)

# st_distance checks the distance between points, the reason for doing this is to see if the location data is the same for the wells with identical site ID's in the ADWR and USGS data sets
# checking the distance between these points finds that the locations differ between the data sets, from 60 meters - 30 kms
#x = st_distance(tmp[200,], tmp2[200,])
#hist(as.numeric(x), breaks = 100
#)


##tmp3 = unique_wells %>% 
  #select(date.x, site_id, dtw_ft.x) %>% na.omit

#sisrc_adwr = unique_wells %>% 
#  filter(sisrc_code == 'ADWR')

#sisrc_usgs = unique_wells %>% 
 # filter(sisrc_code == 'USGS')

#sisrc_drilr = unique_wells %>% 
 # filter(sisrc_code == is.na())

#az_state_map = ggplot() +
#  geom_sf(data = tmp2,
 #         colour = "blue", 
  #        size = 2) +
#  geom_sf(data = tmp3,
 #         colour = "red", 
  #        size = 0.1,
   #       alpha = 0.5) +
  #geom_sf(data = az_shp_5070,
   #       fill = NA,
    #      colour = "black") +
  #theme_classic()


#x = st_distance(tmp, tmp2)
#hist(as.numeric(x), breaks = 100
 #    )







  

