library(stars)
library(gstat)

####################################
# load data                        #                            
####################################


# station data ------------------------------------------------------------
read_stations <- function(airport, iata){
  # load data 
  df2018 <- haven::read_dta(file.path(dataFlug, paste0("Hauptflughaefen_Laerm/", airport, "/", iata, "_complete_merged_2018.dta")))
  df2019 <- haven::read_dta(file.path(dataFlug, paste0("Hauptflughaefen_Laerm/", airport, "/", iata, "_complete_merged_2019.dta")))
  df2020 <- haven::read_dta(file.path(dataFlug, paste0("Hauptflughaefen_Laerm/", airport, "/", iata, "_complete_merged.dta")))
  df2021 <- haven::read_dta(file.path(dataFlug, paste0("Hauptflughaefen_Laerm/", airport, "/", iata, "_complete_merged_2021.dta")))
  
  # row bind
  df <- rbind(df2018, df2019, df2020, df2021)
  
  # return 
  return(df)
}

fra_stations <- read_stations(airport = "Frankfurt", iata = "fra")


# contour data ------------------------------------------------------------

contour_haupt <- st_read(file.path(dataFlug, "Contour_Maps/Hauptflughaefen/Mair_Lden_17.shp"))

####################################
# preparation                      #                            
####################################

# clean measuring data 
fra_stations <- fra_stations %>% select(date, l_den_flug, station, lat_station, lon_station)
fra_stations <- fra_stations %>% filter(!is.na(l_den_flug))

# # month variable
# fra_stations$year_mon <- format(as.Date(fra_stations$date), "%Y-%m")
# 
# # average by month
# fra_stations_month <- fra_stations %>% group_by(year_mon, station) %>% summarise(avg_lden = mean(l_den_flug, na.rm = TRUE),
#                                                                                  lat_station = first(lat_station),
#                                                                                  lon_station = first(lon_station))
# 
# make sf
fra_stations.sf <- st_as_sf(fra_stations, coords = c("lon_station", "lat_station"), crs = 4326)

# change projection
contour_haupt <- st_transform(contour_haupt, crs = 32632)
fra_stations.sf <- st_transform(fra_stations.sf, crs = 32632)

# union airports
contour_union <- contour_haupt %>% group_by(ICAO) %>% summarise(geometry = st_union(geometry))

####################################
# calculation                      #                            
####################################

# get airport
fra_contour <- contour_union %>% filter(ICAO == "EDDF")

# grid
grd <- st_as_sfc(st_bbox(fra_contour))
grd <- st_as_stars(grd, dx = 100, dy = 100)

grd <- grd[fra_contour]
grd <- st_transform(grd, crs = 32632)

tst <- st_join(fra_stations.sf, st_as_sf(grd))
tst <- tst[!is.na(tst$values), ]

# interpolate
form <- gstat::idw(formula = l_den_flug ~ 1, locations = fra_stations.sf, newdata = grd, idp = 2)

z <- predict(form, grd)



tst <- data.frame(matrix(unlist(int_fra), nrow = length(int_fra), byrow = TRUE))

ggplot()+
  geom_stars(data = int_fra, aes(fill = var1.pred, x = x, y = y))+
  geom_sf(fra_contour)





bbox <- st_bbox(fra_stations.sf)

fra_stations.sf <- fra_stations.sf[!is.na(fra_stations.sf$l_den_flug), ]

grd <- fra_contour %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_make_grid(cellsize = c(100, 100), 
               what = "centers") %>% 
  st_as_sf(crs = st_crs(fra_contour))

grd <- grd[fra_contour]

fra_stations.sp <- as(fra_stations.sf, "Spatial")

grd_sp <- as(as(grd, "Spatial"), "SpatialPixels")

pred_idw <- gstat::idw(l_den_flug ~ 1, locations = fra_stations.sp, newdata = grd_sp)

pred_idw.df <- as.data.frame(pred_idw)
pred_idw.sf <- st_as_sf(pred_idw.df, coords = c("coords.x1", "coords.x2"), crs = 32632)

tm_shape(pred_idw.sf)+
  tm_dots(col = "var1.pred")+
  tm_shape(fra_stations.sf)+
  tm_dots()

