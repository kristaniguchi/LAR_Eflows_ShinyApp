library(rgdal)
library(tibble)
river2 <- readOGR("data/LAR_reporting_reaches.shp")
river2 <- spTransform(river2, CRS("+proj=longlat +datum=WGS84 +no_defs")) # used for blue lines


river_obj <- readOGR("data/reporting_nodes/LAR_reporting_nodes_201002_reportingreaches.shp")
river <- spTransform(river_obj, CRS("+proj=longlat +datum=WGS84 +no_defs")) # only obtain (lat,lng) for reporting nodes


river_mainstem <- readOGR("data/mainstem/LAR_mainstem.shp")
river_mainstem <- spTransform(river_mainstem, CRS("+proj=longlat +datum=WGS84 +no_defs"))


reporting_reaches <- readOGR("data/reporting_reach/LAR_reporting_reaches.shp")
reporting_reaches <- spTransform(reporting_reaches, CRS("+proj=longlat +datum=WGS84 +no_defs"))


WRP_shp <- tribble(~wrp,~Lat, ~Long,
                   'Tillman WRP', 34.18213, -118.4794,
                   'Burbank WRP', 34.18237, -118.3189,
                   'Glendale WRP', 34.13943, -118.2749)

ws_map <- readOGR("data/ws/LAR_ws.shp")
ws_map <- spTransform(ws_map, CRS("+proj=longlat +datum=WGS84 +no_defs"))
