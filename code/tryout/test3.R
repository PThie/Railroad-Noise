# buffering ---------------------------------------------------------------


# buffer around line 
bf <- st_buffer(main_tracks, dist = 50)
bf5000 <- st_buffer(main_tracks, dist = 5000)

# form union
uni <- st_union(bf)
uni5000 <- st_union(bf5000)

# check whether it worked
tm_shape(uni)+
  tm_polygons()

# load housing data
hk <- haven::read_dta("M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/On-site/v4/HK_allVersions_ohneText.dta")

# use only year 2017
hk.df <- hk[hk$ajahr == 2017,]

# drop empty coordinates
hk.df <- hk.df[!is.na(hk.df$lat),]
hk.df <- hk.df[!is.na(hk.df$lon),]
hk.df <- hk.df[hk.df$lat_utm > 0, ]
hk.df <- hk.df[hk.df$lon_utm > 0, ]

hk.df$n <- ave(1:length(hk.df$obid), hk.df$obid, FUN = length)

hk.df <- hk.df[hk.df$n == hk.df$spell, ] 

sum(duplicated(hk.df$obid))

hk.df$n <- NULL

# make sf object
hk.df <- st_as_sf(hk.df, coords = c("lat", "lon"), crs = 4326)
hk.df <- st_transform(hk.df, crs = 32632)
hk.df <- hk.df[, c("obid", "geometry")]

bula <- st_read("M:/_FDZ/interne Daten/Gebietseinheit/Bundesland/2019/VG250_LAN.shp")
bula <- st_transform(bula, crs = 32632)
bula <- bula[, c("GEN", "geometry")]

hk.by <- st_join(hk.sf, bula, all.x = TRUE)
hk.by <- hk.by[hk.by$GEN == "Bayern",]

# intersect with union 
log <- st_intersects(hk.sf, uni)
hk.sf$log <- lengths(log)
hk.jt <- hk.sf[!(hk.sf$log == 0),]

log <- st_intersects(hk.df, uni5000)
hk.df$log <- lengths(log)




# see if it worked
tm_shape(hk.by)+
  tm_dots(size = 0.2)+
  tm_shape(uni5000)+
  tm_polygons(col = "red", alpha = 0.3)

