

# USGS_NWIS_dtw_17
# author: Angus Watters
# date: 9/15/2020


library(tidyverse)
library(readxl)
library(sf)
library(USAboundaries)
library(USAboundariesData)
library(dataRetrieval)
library(lubridate)

# Sites Lat, long
ca_site <- readNWISdata(stateCd="California", #for Arizona
                     service="site", #pull all sites held w/in USGS/NWIS
                     seriesCatalogOutput=TRUE) %>% #include this to include site_tp_cd
  renameNWISColumns() %>% #rename columns so they are universal across all pulls
  filter(site_tp_cd == "GW") %>% #only keep sites associated with groundwater
  dplyr::select(agency_cd, site_no, dec_lat_va, dec_long_va, site_tp_cd) %>% #only keep relevent columns 
  distinct(site_no, .keep_all = TRUE) #only keeping distinct sites to ensure no repeats

#Dtw


# Dtw
ca_nwis_dtw = readNWISdata(stateCd="California", #for Arizona
                           service="gwlevels", #pull all groundwater levels
                           startDT="1900-01-01", #start as early as possible
                           endDT="2020-01-01") %>% #start as current as possible
  renameNWISColumns() %>%  #rename columns so they are universal across all pulls
  select(agency_cd, site_no, lev_dt, lev_va) #only keep relevent columns %>% 


#oin
ca_nwis_join <- left_join(ca_nwis_dtw, ca_site, by = "site_no") %>% #join site information to each dtw site row 
  dplyr::select(agency_cd.x, site_no, dec_lat_va, dec_long_va, lev_dt, lev_va) %>% 
  dplyr::rename(agency_cd = agency_cd.x, 
                site_id = site_no, 
                dtw = lev_va, 
                lat = dec_lat_va, 
                long = dec_long_va, 
                measure_date = lev_dt) %>% #renaming to have reproducible code below
  drop_na(dtw) %>% #drop any rows with depth to water measurments
  drop_na(lat) #drop any rows that do not have lat/long measurements

ca_shp <- us_states(resolution = "high", states = "CA") %>%  #read in AZ shp file
  st_transform(crs = 4269) #set CRS for NAD83

 #was 4326, after st_transform is 4269 #NEEDED TO REPROJECT ALL .SHP (do not have to manually assign crs because crs is defined)


ca_nwis_spatial <- ca_nwis_join %>% 
  distinct(site_id, .keep_all = TRUE) %>% #only looking for unique sites so less to process spatially 
  st_as_sf(coords=c("long",
                    "lat"),
           crs = 4269)

#st_crs(az_nwis_spatial) #4269, this works to set the crs as NAD83 because USGS NWIS is in NAD83


ca_clip <- st_intersection(ca_shp, ca_nwis_spatial) %>% # This filters for points inside of the az shape file
  select(site_id)


length(unique(ca_clip$site_id)) #checking in on how many sites there are left 
#35780 (so 14 wells were taken out of the df that were outside AZ bounds)


#CHECK in map
#az_map <- ggplot() +
# geom_sf(data = az_clip,
#        colour = "forestgreen", 
#       size = 0.1,
#      alpha = 0.5)+
#geom_sf(data = az_shp,
#       fill = NA,
#      colour = "black")

#Create df with only wells inside the state boundary 
ca_nwis_clip <- inner_join(ca_nwis_join, ca_clip, by = "site_id") 

#az_nwis_geom = az_nwis_clip %>% select(site_id, geometry)

#az_nwis_clip = az_nwis_clip %>% select(!geometry)
#Clean

ca_nwis_clean <- ca_nwis_clip %>% 
  mutate(date = as.factor(measure_date)) %>% #converting from character to factor to change to date
  mutate(date = as.Date(date, format ="%Y-%m-%d")) %>% #putting date into useable format
  drop_na(date) %>% #some measurement only have year and month but not day
  mutate(year = lubridate::year(date)) %>% #extracting year to determine unique years later
  mutate(dec_date = decimal_date(date)) %>%
  group_by(site_id) %>% #by well...calculate:
  mutate(date_min = min(dec_date), 
         date_max = max(dec_date),
         measurement_dist = n_distinct(dtw), #distinct well measurements by well
         year_dist = n_distinct(year)) 

ca_nwis_all <- ca_nwis_clean %>% 
  select(agency_cd, site_id, date, dtw, date_min, date_max, year, measurement_dist, year_dist, lat_nad83 = lat, long_nad83 = long, measure_date) %>%
  mutate(source = "lb_national") 
#slice(n=1)


ca_nwis_unique_sites <- ca_nwis_clean %>% 
  select(agency_cd, site_id, date, dtw, date_min, date_max, year, measurement_dist, year_dist, lat_nad83 = lat, long_nad83 = long, measure_date) %>% 
  mutate(source = "lb_national") %>% 
  distinct(site_id, .keep_all = TRUE) %>% 
  mutate(id = 'US')
saveRDS(ca_nwis_unique_sites, file = 'data/ca_usgs_unique.rds')
saveRDS(ca_nwis_all, file = 'data/ca_usgs_time.rds')
