library(dplyr)
library(tidyr)
library(sf)
library(SQI)

prj <- 4326 # geographic wgs84

data(sqidat)

# spatial data ------------------------------------------------------------

# county and smc shed data data, intersected with sample data above

# county
cntys <- st_read('S:/Spatial_Data/CA_Counties/cnty24k97.shp') %>% 
  st_transform(crs = prj) %>% 
  .[sqidat, ] %>% 
  select(NAME) %>% 
  rename(cnty = NAME) %>% 
  mutate_if(is.factor, as.character)

# sheds, as sf
sheds <- st_read('S:/Spatial_Data/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009/SMCSheds2009.shp') %>%
  st_as_sf %>% 
  st_transform(crs = prj) %>% 
  select(SMC_Name) %>% 
  mutate_if(is.factor, as.character)

# regional water quality board boundaries
rwqbs <- st_read('S:/Spatial_Data/RWQCBdistricts/rwqcbnda.shp') %>%
  st_as_sf %>% 
  st_transform(crs = prj) %>% 
  select(RBNAME) %>% 
  filter(RBNAME %in% c('Los Angeles', 'San Diego', 'Santa Ana')) %>% 
  group_by(RBNAME) %>% 
  summarize() %>% 
  mutate_if(is.factor, as.character)

# get intersection with sample data
sqidat <- sqidat %>% 
  st_intersection(cntys) %>% 
  st_intersection(sheds) %>% 
  st_intersection(rwqbs)

# constraint classes
load(file = '../../Channels in developed landscapes_RM/Marcus/landscape_mod/data/calicls.RData')

cnstr <- calicls %>% 
  dplyr::select(COMID, strcls, core0.10, core0.50, core0.90) %>% 
  rename(
    lower = core0.10, 
    meds = core0.50,
    upper = core0.90
  ) %>% 
  .[sheds,]

cnstrnogeo <- cnstr %>% st_set_geometry(NULL)
sqidat <- sqidat %>% 
  left_join(cnstrnogeo, by = 'COMID')

save(cntys, file = 'data/cntys.RData', compress = 'xz')
save(sheds, file = 'data/sheds.RData', compress = 'xz')
save(rwqbs, file = 'data/rwqbs.RData', compress = 'xz')
save(cnstr, file = 'data/cnstr.RData', compress = 'xz')
save(sqidat, file = 'data/sqidat.RData', compress = 'xz')

# stream polylines --------------------------------------------------------

data(sheds)

strms <- st_read('../../Channels in developed landscapes_RM/Data/strm_constraints/strm_constraints.shp') %>% 
  .[sheds, ] %>% 
  dplyr::select(COMID, Ref10)

save(strms, file = 'data/strms.RData', compress = 'xz')
