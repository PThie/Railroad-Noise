############################################################
# Load Data                                                #
############################################################

# alternative railroads
alt_rail <- st_read(file.path(dataPath, "umgebungslaerm/schiene/Basisdaten/Mrail_Source_17.shp"))

# housing data
hk <- readRDS(file.path(dataPath, "housing/Temp/hk_complete_prep.rds"))

# originally buffered data (from previous step)
hk_buffer_org <- readRDS(file.path(dataPath, "housing/Temp/hk_buffered.rds"))

# municipality information
gem <- st_read(file.path(dataGebiete, "Gemeinde/2019/VG250_GEM.shp"))

# zuordnung
zuord <- fread(file.path(dataGebiete, "Zuordnung/zuordnung_r1_plz_gem_krs_2019.csv"))

# load function
source(file.path(codePath, "functions/intersection_buffer.Rd"))

############################################################
# preparation                                              #
############################################################

# make same projection
alt_rail <- st_transform(alt_rail, crs = 32632)

gem <- st_transform(gem, crs = 32632)

# prepare zuordnung -------------------------------------------------------
# assign grid to those municipality with the highest share
zuord_prep <- zuord
zuord_prep <- zuord_prep[order(zuord_prep$idm, -zuord_prep$anteil), c(1, 4, 5, 9)]
zuord_prep$anteil <- NULL

zuord_prep <- zuord_prep %>% group_by(idm) %>% summarise(idm = first(idm),
                                                         AGS_gem = first(AGS_gem),
                                                         GEN_gem = first(GEN_gem))

# add zeros to AGS
zuord_prep$AGS_gem <- as.character(zuord_prep$AGS_gem)
zuord_prep$AGS_gem <- ifelse(test = nchar(zuord_prep$AGS_gem) == 7,
                             yes = paste0("0", zuord_prep$AGS_gem),
                             no = paste0(zuord_prep$AGS_gem))

############################################################
# Buffering                                                #
############################################################

# buffer 
bf25 <- st_buffer(alt_rail, dist = 25)
bf50 <- st_buffer(alt_rail, dist = 50)
bf100 <- st_buffer(alt_rail, dist = 100)
bf250 <- st_buffer(alt_rail, dist = 250)
bf500 <- st_buffer(alt_rail, dist = 500)
bf750 <- st_buffer(alt_rail, dist = 750)
bf1000 <- st_buffer(alt_rail, dist = 1000)
bf2000 <- st_buffer(alt_rail, dist = 2000)
bf3000 <- st_buffer(alt_rail, dist = 3000)
bf5000 <- st_buffer(alt_rail, dist = 5000)

############################################################
# make sf                                                  #
############################################################
# probably not needed for the final data set

# drop objects which do not have a geographic information

# HK ----------------------------------------------------------------------
hk <- hk[!is.na(hk$lat_gps), ]
hk <- hk[!is.na(hk$lon_gps), ]

# define as sf
hk.sf <- st_as_sf(hk, coords = c("lon_gps", "lat_gps"), crs = 4326, remove = FALSE)
hk.sf <- st_transform(hk.sf, crs = 32632)

############################################################
# Intersection                                             #
############################################################

hk_alt_rail.sf <- bf.intersection.fun(hk.sf)

############################################################
# Treated Municipalities                                   #
############################################################

# treated municipalities --------------------------------------------------
# define municipalities which are affected by tracks

# determine municipalities which are crossed by tracks
int <- st_intersects(gem, alt_rail)
gem$logical <- lengths(int)
gem$logical[gem$logical != 0] <- 1

# prepare municipality data for merge
gem <- st_drop_geometry(gem)

# drop everything except AGS and indicator
gem <- gem[, c("AGS", "logical")]

# rename
colnames(gem) <- c("AGS", "affected_munic")

# drop duplicates
gem <- gem[!duplicated(gem$AGS), ]

# merge zuordnung and municipality data
zuord_prep <- merge(zuord_prep, gem, by.x = "AGS_gem", by.y = "AGS", all.x = TRUE)


# merge affected municipalities to housing --------------------------------

hk_alt_rail.sf <- merge(hk_alt_rail.sf, zuord_prep, by.x = "r1_id", by.y = "idm", all.x = TRUE) 

############################################################
# merge distances                                          #
############################################################
# merge the distances calculated for the original data
# (to save calculation time)

# drop geometries
hk_buffer_org <- st_drop_geometry(hk_buffer_org)
hk_alt_rail.sf <- st_drop_geometry(hk_alt_rail.sf)

# keep only variables needed from original data
hk_buffer_org <- hk_buffer_org %>% select(obid, regional_type, settlement_density, closest_largcenter, distance_largcenter,
                                          distance_medcenter, closest_medcenter, closest_smalgcenter, distance_smalcenter,
                                          closest_airport, distance_airport, closest_industry, distance_industry, distance_streets)

# merge by object ID
hk_alt_rail.sf <- merge(hk_alt_rail.sf, hk_buffer_org, by = "obid", all.x = TRUE)

############################################################
# export                                                   #
############################################################

saveRDS(hk_alt_rail.sf, file.path(dataPath, "housing/Temp/hk_buffered_alt_rail.rds"))
