library(terra)
library(sf)
 # load raster ####
elev <-  rast("LF2020_Elev_220_CONUS/LC20_Elev_220.tif")
class(elev)
elev
plot(elev)
 

# Load UT state boundary ####

ut <-  read_sf("UtahStateBoundary/Utah.shp")
plot(ut)
ut
ut <-  ut[2,]
plot(ut)
crs(elev)

ut_5070 <-  st_transform(ut, crs = crs(elev))
plot(ut_5070)
st_crs(ut_5070)

# crop elevation to UT state boundary
?crop
crop(elev, ut)
ut_crop <- crop(elev, ut_5070)
plot(ut_crop)


# Mask cropped elevation to the shape of Utah ####
elev_ut <- mask(ut_crop, ut_5070)
plot(elev_ut)


# Load capture site locations from dragons

library(tidyverse)
library(DBI)


db_conn <-  dbConnect(RSQLite::SQLite(),
                      "databases_WLF/dragons.db")


captures_sites <-  dbGetQuery(db_conn, "SELECT * FROM capture_sites;")
class(captures_sites)


captures_sites <-  st_as_sf(captures_sites,
                            coords = c("utm_x", "utm_y"),
                            crs = 32612)


class(captures_sites)
captures_sites


# convert points to Albers CRS ####
capture_sites_5070 <-  st_transform(captures_sites, crs = 5070)
plot(capture_sites_5070)


# Make a map

?geom_raster
?geom_sf


elev_df <-  as.data.frame(elev, xy = TRUE)


ggplot()+
  geom_raster(data =  elev_df, mapping = aes(x=x, y=y, fill = LC20_Elev_220))+
  geom_sf(data = ut_5070)+
  geom_sf(data = capture_sites_5070)+
  theme_void()
