source("../docs/ca-state-dtw.R") 
source("../docs/ca-usgs-dtw.R") 
source("docs/utils.R")




# User interface ----
ui <- fluidPage( 
  titlePanel('Mike Johnson: COVID-19 Dashboard')
)



# Server logic ----
server <- function(input, output) { }

shinyApp(ui, server)




#######################################################
######## USGS SPATIAL + TIMESERIES DATAFRAMES #########
#######################################################

# Arizona state shape (CRS = 5070)
az = az_shp_4269 %>%  
  st_transform(5070)

# USGS UNIQUE AND WELL TIME SERIES DATAFRAMES
usgs = az_nwis_unique_sites 
usgs_time = az_nwis_all %>% 
  group_by(site_id) 


# STATE UNIQUE AND WELL TIME SERIES DATAFRAMES
state = adwr_unique_sites %>% 
  filter(!site_id %in% usgs$site_id)
state_time = adwr_all %>% 
  group_by(site_id) %>% 
  filter(!site_id %in% usgs_time$site_id) # FILTER FOR THE SITE_ID WHICH DO NOT APPEAR IN USGS TIME SERIES DATAFRAME

# BIND ROWS FROM USGS WELLS WITH STATE ONLY WELLS + ADD UNIQUE 'WELLID #' BY ROW NUMBER
join = rbind(usgs, state) %>% 
  select(!c(agency_cd, year_dist, source))
join$wellid = paste("well", 1:nrow(join)) 

# CREATE SF OBJECT FROM LAT + LONG COORDS, EXTRA STEP TAKEN TO REMOVE WELLS WHICH ARE AT THE EXACT SAME LAT + LONG (DUPLICATES)
join_spatial = join %>% 
  st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
  st_transform(5070) %>%
  as_Spatial() %>% 
  remove.duplicates() %>% 
  st_as_sf() %>% 
  group_by(wellid)

# BIND ROWS FROM USGS + STATE TIME SERIES DATA, THEN FILTER OUT THE SPATIAL DUPLICATES 
join_time = rbind(usgs_time, state_time) %>% 
  select(!c(agency_cd, year_dist, source))
join_time = left_join(join_time, select(join, site_id, wellid), by = "site_id") 
join_time = join_time %>% filter(wellid %in% join_spatial$wellid)


# FILTER BY THRESHOLD - atleast 10 measurements and atleast 1 measurement every 5 years
thresh = join_time %>% 
  group_by(wellid) %>% 
  filter(measurement_dist >= 10) %>% 
  mutate(measure_period = year - lag(year)) %>%
  filter(measure_period > 5)

join_time = join_time %>% 
  filter(!wellid %in% thresh$wellid, measurement_dist >= 10) %>%
  mutate(measure_period = year - lag(year))

join_spatial = join_spatial %>%
  filter(!wellid %in% thresh$wellid) %>%
  filter(measurement_dist >= 10)








