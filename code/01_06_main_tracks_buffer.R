############################################################
# Load Data                                                #
############################################################

# load main tracks
main_tracks <- st_read(file.path(dataPath, "main_tracks/tracks5805.shp"))

# housing data
hk <- readRDS(file.path(dataPath, "housing/Temp/hk_complete_prep.rds"))

# municipality information
gem <- st_read(file.path(dataGebiete, "Gemeinde/2019/VG250_GEM.shp"))

# zuordnung
zuord <- fread(file.path(dataGebiete, "Zuordnung/zuordnung_r1_plz_gem_krs_2019.csv"))

# regional info (regional centers settlement density, and  regional types)
regional_types <- read.fst(file.path(dataPath, "raumtyp/raumtyp_siedlungsdicht_nach_gemeinde_prep.fst"))
regional_center <- readRDS(file.path(dataPath, "raumzentren/raumzentren_nach_gemeinde_prep.rds"))

# noise sources
air_noise <- readRDS(file.path(dataPath, "umgebungslaerm/air_noise_combinded.rds"))
industry_noise <- readRDS(file.path(dataPath, "umgebungslaerm/ind_noise_combinded.rds"))
street_noise <- readRDS(file.path(dataPath, "umgebungslaerm/str_noise_combinded.rds"))
streets <- st_read(file.path(dataPath, "umgebungslaerm/strasse/Hauptverkehrsstrassen/Basisdaten/Mroad_Source_17.shp"))

# noise barriers
noise_bar <- readRDS(file.path(dataPath, "Schallschutzwand/ssw_prep.rds"))

# load function
source(file.path(codePath, "functions/intersection_buffer.Rd"))

############################################################
# preparation                                              #
############################################################

# same projections 
main_tracks <- st_transform(main_tracks, crs = 32632)

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
# buffer around the main tracks

# buffer 
bf25 <- st_buffer(main_tracks, dist = 25)
bf50 <- st_buffer(main_tracks, dist = 50)
bf100 <- st_buffer(main_tracks, dist = 100)
bf250 <- st_buffer(main_tracks, dist = 250)
bf500 <- st_buffer(main_tracks, dist = 500)
bf750 <- st_buffer(main_tracks, dist = 750)
bf1000 <- st_buffer(main_tracks, dist = 1000)
bf2000 <- st_buffer(main_tracks, dist = 2000)
bf3000 <- st_buffer(main_tracks, dist = 3000)
bf5000 <- st_buffer(main_tracks, dist = 5000)

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

# function intersects housing data with buffers and determines (via dummies)
# in which buffer the object lies
# this is exclusively defined, i.e. an object can only lie within one ring

hk.sf <- bf.intersection.fun(hk.sf)


# -------------------------------------------------------------------------
# construct 50 meter buffer (only, i.e. includes the range of 25m and 50m)
hk.sf <- hk.sf %>% mutate(bf50_only = case_when(bf25 == 1 | bf50 == 1 ~ 1))
hk.sf$bf50_only[is.na(hk.sf$bf50_only)] <- 0

############################################################
# Treated Municipalities                                   #
############################################################

# treated municipalities --------------------------------------------------
# define municipalities which are affected by tracks

# determine municipalities which are crossed by tracks
int <- st_intersects(gem, main_tracks)
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

hk.sf <- merge(hk.sf, zuord_prep, by.x = "r1_id", by.y = "idm", all.x = TRUE) 

############################################################
# additional controls                                      #
############################################################


# -------------------------------------------------------------------------
# regional types
regional_types$municipality <- NULL
hk.sf <- merge(hk.sf, regional_types, by.x = "AGS_gem", by.y = "ags", all.x = TRUE)

# as numeric
hk.sf$settlement_density <- as.numeric(hk.sf$settlement_density)

# -------------------------------------------------------------------------
# regional center
regional_center <- st_set_geometry(regional_center, regional_center$geometry)
regional_center <- st_transform(regional_center, crs = 32632)
regional_center <- regional_center[!st_is_empty(regional_center$geometry), ]

# subset for large center (Oberzentrum == 1)
largcenter <- regional_center[regional_center$center_identifier == 1, ]

# subset for medium center (Mittelzentrum == 2)
medcenter <- regional_center[regional_center$center_identifier == 2, ]

# subset for small center (Grundzentrum == 3)
smalcenter <- regional_center[regional_center$center_identifier == 3, ]


# -------------------------------------------------------------------------
# distances regional centers

# gives the closest center 
ordering_largcenter <- apply(st_distance(hk.sf, largcenter), 1, which.min)
ordering_medcenter <- apply(st_distance(hk.sf, medcenter), 1, which.min)
ordering_smalcenter <- apply(st_distance(hk.sf, smalcenter), 1, which.min)

# add column with closest center to housing
hk.sf$closest_largcenter <- largcenter$municipality[ordering_largcenter]
hk.sf$closest_medcenter <- medcenter$municipality[ordering_medcenter]
hk.sf$closest_smalcenter <- smalcenter$municipality[ordering_smalcenter]

# add distance to closest center to housing
hk.sf$distance_largcenter <- as.numeric(apply(st_distance(hk.sf, largcenter), 1, min)) / 1000 # to get kilometers
hk.sf$distance_medcenter <- as.numeric(apply(st_distance(hk.sf, medcenter), 1, min)) / 1000 # to get kilometers
hk.sf$distance_smalcenter <- as.numeric(apply(st_distance(hk.sf, smalcenter), 1, min)) / 1000 # to get kilometers


# -------------------------------------------------------------------------
# distances noise sources

##### Airport

# closest airport
ordering_airport <- apply(st_distance(hk.sf, air_noise), 1, which.min)
hk.sf$closest_airport <- air_noise$ICAO[ordering_airport]

# distance to closest airport
hk.sf$distance_airport <- as.numeric(apply(st_distance(hk.sf, air_noise), 1, min)) / 1000


##### Industry

# closest industry site
ordering_industry <- apply(st_distance(hk.sf, industry_noise), 1, which.min)
hk.sf$closest_industry <- industry_noise$Agglomerat[ordering_industry]

# distance to closest industry site
hk.sf$distance_industry <- as.numeric(apply(st_distance(hk.sf, industry_noise), 1, min)) / 1000

##### Street

# hk.sf <- readRDS(file.path(dataPath, "housing/Temp/hk_buffered.rds"))

streets <- st_transform(streets, crs = 32632)
nearest <- st_nearest_feature(hk.sf, streets)
saveRDS(nearest, file.path(dataPath, "housing/Temp/nearest_streets.rds"))
hk.sf$distance_streets <- as.numeric(st_distance(hk.sf, streets[nearest, ], by_element = TRUE) / 1000)


# -------------------------------------------------------------------------
# noise 
nearest_noisebar <- st_nearest_feature(hk.sf, noise_bar)
hk.sf$distance_noise_barrier <- as.numeric(st_distance(hk.sf, noise_bar[nearest_noisebar, ], by_element = TRUE) / 1000)s

# -------------------------------------------------------------------------

# closest corridor
ordering_corridor <- apply(st_distance(hk.sf, main_tracks), 1, which.min)
hk.sf$closest_corridor <- main_tracks$corridor[ordering_corridor]


# -------------------------------------------------------------------------
# law dummies

# construct dummy for the months the law is in force (but not punishable)
hk.sf$law_inprogress <- ifelse(test = hk.sf$year_mon >= "2017-07" & hk.sf$year_mon <= "2020-11",
                             yes = 1,
                             no = 0)

# construct dummy for the months the law is in force (i.e ban is in force)
hk.sf$law_established <- ifelse(test = hk.sf$year_mon >= "2020-12",
                              yes = 1,
                              no = 0)



############################################################
# export                                                   #
############################################################

saveRDS(hk.sf, file.path(dataPath, "housing/Temp/hk_buffered.rds"))
