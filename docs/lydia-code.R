
library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(kableExtra)
library(skimr)
library(naniar)
library(VIM)
library(ggfortify)
library(lubridate)
library(tsibble)
library(dplyr)


#Spatial Clipping 
library(USAboundaries) # Download shape files
library(USAboundariesData)
library(sf) 

# 1. Read both files in (csvs) 

az_dtw <- read_xlsx('data/gwis_dtw.xlsx')%>% 
  clean_names()  %>% 
  rename(site_well_site_id = wlwa_site_well_site_id) #renaming to match the same column in az_site 

az_site = read_xlsx('data/gwis_sites.xlsx') %>% 
  clean_names() 
# 2. Join dtw and site filies

az_join <- inner_join(az_dtw, az_site, by = "site_well_site_id") %>%  #joining by site ID
  select(site_sisrc_code, site_well_site_id, wlwa_measurement_date, wlwa_depth_to_water) #selecting useful columns


#3 Clip to state boundaries

#Below process based on Robin Lovelace guide, 6.1, https://geocompr.robinlovelace.net/reproj-geo-data.html


#Read in .shp file for AZ
az_shp <- us_states(resolution = "high", states = "AZ") %>% 
  st_transform(crs = 4269) #set CRS for NAD83

#Make AZ dataframe a .shp
az_state_spatial <- az_site %>% 
  st_as_sf(coords=c("site_longit_decimal",
                    "site_latitude_decimal")) #creating geometry column with lat and long to clip with az_shp (AZ has multiple GCS assigned to the data, so we want to reprojet all in NAD83)

?st_is_longlat(az_state_spatial) #Returns NA indicating that there is no geographic coordinate system assigned (GCS) 

az_state_spatial_nad83 = st_set_crs(az_state_spatial, 4269) %>%  #Here I am assigned a 4269 GCS. Since there is no defined coordinate system, we can't reproject. We could assign this when we make the AZ dataframe a .shp, but I am walking thorugh the process here for clarity. 
  mutate(lat_nad83 = unlist(map(geometry,2)),
         long_nad83 = unlist(map(geometry,1))) 

#st_is_longlat(az_state_spatial_nad83) #Returns TRUE, indicating that there is a GCS assigned


#Clip the AZ dataframe/.shp to the AZ state boundary .shp  
az_state_clip <- st_intersection(az_shp, az_state_spatial_nad83) %>% # This filters for points inside of the az shape file
  select(site_well_site_id, lat_nad83, long_nad83)


length(unique(az_state_clip$site_well_site_id)) #checking in on how many sites there are left 
#34236 (so ~200 wells were taken out of the DF that were outside AZ bounds)


#CHECK in map
az_state_map <- ggplot() +
  geom_sf(data = az_state_clip,
          colour = "forestgreen", 
          size = 0.1,
          alpha = 0.5)+
  geom_sf(data = az_shp,
          fill = NA,
          colour = "black") +
  theme_classic()
az_state_map

#Create df with only wells inside the state boundary 
az_state_clean_clip <- inner_join(az_join, az_state_clip, by = "site_well_site_id") %>% 
  select(!geometry) 


length(unique(az_state_clean_clip$site_well_site_id)) #checking in on how many sites there are left 


# kable(head(az_state_clean_clip, 5)) %>%  #Just printing top of the table as a sample
#   kable_styling()


# 4. Clean
az_join_deb_all <- az_state_clean_clip %>% 
  select(agency_cd = site_sisrc_code, site_id = site_well_site_id, dtw_ft = wlwa_depth_to_water, lat_nad83, long_nad83, measure_date = wlwa_measurement_date) %>% #renaming to have reproducible code below
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

az_adwr_all <- az_join_deb_all %>% 
  select(agency_cd, site_id, date, dtw_ft, date_min, date_max, measurement_dist, year_dist, lat_nad83, long_nad83) %>% 
  mutate(source = "lb_state")

az_adwr_unique_sites <- az_join_deb_all %>% 
  select(agency_cd, site_id, date_min, date_max, measurement_dist, year_dist, lat_nad83, long_nad83) %>% 
  mutate(source = "lb_state") %>% 
  distinct(site_id, .keep_all = TRUE)


# ADWR and NWIS join 

adwar_nwis_join = inner_join(az_adwr_unique_sites, az_nwis_unique_sites, by ='site_id') %>% 
  arrange(site_id) 

adwr_geom = adwar_nwis_join %>% 
  mutate(row_number= 1:n()) %>% 
  st_as_sf(coords = c('long_nad83.x', 'lat_nad83.x'), crs = 4269) %>% 
  st_transform(5070)

nwis_geom = adwar_nwis_join %>% 
  mutate(id = row_number()) %>% 
  st_as_sf(coords = c('long_nad83.y', 'lat_nad83.y'), crs = 4269) %>% 
  st_transform(5070)


near = adwr_geom %>% group_by(site_id) %>% 
  mutate(np = st_nearest_feature(geometry, nwis_geom), 
         dist_np = as.numeric(st_distance(geometry, nwis_geom[np,]))) %>% 
  select(site_id, np , dist_np)
  
ggplot() +
  geom_sf(data = nwis_geom,
          colour = "forestgreen", 
          size = 4,
          alpha = .5)+
  geom_sf(data = az_shp,
          fill = NA,
          colour = "black") +
  geom_sf(data = adwr_geom, colour = "blue", 
          size = 2,
          alpha= 0.5)
  theme_classic()
  
geom_join = st_is_within_distance(nwis_geom, adwr_geom, dist = 200)

x = st_distance(adwr_geom[6,], nwis_geom[6,])

near_index = apply(x, 1, order)[1, ]



hist(as.numeric(x), breaks = 100
)

select(state_name) %>%  
  st_cast("MULTILINESTRING") %>%  
  mutate(dist = st_distance(., denver_sf)) %>%  
  slice_min(dist, n = 3)
















