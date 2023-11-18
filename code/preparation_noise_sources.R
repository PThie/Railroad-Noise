###############################################################
# load data                                                   #
###############################################################


# air ---------------------------------------------------------------------
# load airport data
air_large_lden <- st_read(file.path(dataPath, "umgebungslaerm/flug/Umgebungslaerm_Grossflughaefen/Lden/Mair_Lden_17.shp"))

# load additional airports
air_add_lden <- st_read(file.path(dataPath, "umgebungslaerm/flug/Ballungsraeume/Lden/Aggair_Lden_17.shp"))


# industry ----------------------------------------------------------------
ind_lden <- st_read(file.path(dataPath, "umgebungslaerm/industrie/Ballungsraeume/Lden/Aggind_Lden_17.shp"))



# street ------------------------------------------------------------------
# load streets
str_lden <- st_read(file.path(dataPath, "umgebungslaerm/strasse/Hauptverkehrsstrassen/Lden/Mroad_Lden.gdb/a00000009.gdbtable"))

# additional street data
str_add_lden <- st_read(file.path(dataPath, "umgebungslaerm/strasse/Ballungsraeume/Lden/Aggroad_Lden_17.shp"))

###############################################################
# preparation airport noise                                   #
###############################################################

# preparation -------------------------------------------------------------

# restrict to relevant variables
air_large_lden <- air_large_lden[, c("ICAO", "DB_Low", "DB_High", "OBJECTID", "geometry")]

# add ICAO to additional airports
air_add_lden$ICAO <- NA
air_add_lden$ICAO[air_add_lden$Agglomerat == "Essen"] <- "EDLE"
air_add_lden$ICAO[air_add_lden$Agglomerat == "Mülheim an der Ruhr"] <- "EDLE"
air_add_lden$ICAO[air_add_lden$Agglomerat == "Mannheim"] <- "EDFM"
air_add_lden$ICAO[air_add_lden$Agglomerat == "Dortmund"] <- "EDLW"
air_add_lden$ICAO[air_add_lden$Agglomerat == "Bremen"] <- "EDDW"
air_add_lden$ICAO[air_add_lden$Agglomerat == "Mainz"] <- "EDFZ"
air_add_lden$ICAO[air_add_lden$Agglomerat == "Dresden"] <- "EDDC"

air_add_lden <- air_add_lden[, c("ICAO", "DB_Low", "DB_High", "OBJECTID", "geometry")]

# combine both airport "types"
air_lden_total <- rbind(air_large_lden, air_add_lden)

# transform CRS
air_lden_total <- st_transform(air_lden_total, crs = 32632)

# make union
air_lden_total_unions <- air_lden_total %>% group_by(ICAO) %>% summarise(geometry = st_union(geometry))

# export ------------------------------------------------------------------

saveRDS(air_lden_total_unions, file.path(dataPath, "umgebungslaerm/air_noise_combinded.rds"))


###############################################################
# preparation industry noise                                  #
###############################################################


# preparation -------------------------------------------------------------

# drop unneeded variables
ind_lden <- ind_lden[, c(3, 4, 5, 9)]

# transform CRS
ind_lden <- st_transform(ind_lden, crs = 32632)

ind_lden_valid <- st_make_valid(ind_lden)

# make union
ind_lden_union <- ind_lden_valid %>% group_by(Agglomerat) %>% summarise(geometry = st_union(geometry))

# export ------------------------------------------------------------------

saveRDS(ind_lden_union, file.path(dataPath, "umgebungslaerm/ind_noise_combinded.rds"))

###############################################################
# preparation street noise                                    #
###############################################################

# transform
str_add_lden <- st_transform(str_add_lden, crs = 32632)

# rename and drop
str_lden <- str_lden[, c("DB_Low", "DB_High", "SHAPE")]
colnames(str_lden) <- c("db_low_str", "db_high_str", "geometry")

str_add_lden <- str_add_lden[, c("DB_Low", "DB_High", "geometry")]
colnames(str_add_lden) <- c("db_low_str", "db_high_str", "geometry")

# define as sf and transform
str_lden <- st_set_geometry(str_lden, str_lden$geometry)
str_lden$SHAPE <- NULL
str_lden <- st_transform(str_lden, crs = 32632)

# check for valid geometries
str_lden$check <- st_is_valid(str_lden)
str_add_lden$check <- st_is_valid(str_add_lden)

# extract only valid geometries
str_lden_valid <- subset(str_lden, str_lden$check == TRUE)
str_lden_valid$check <- NULL
str_lden_valid$SHAPE <- NULL
str_lden_valid <- st_set_geometry(str_lden_valid, str_lden_valid$geometry)
str_lden_valid <- st_transform(str_lden_valid, crs = 32632)

str_add_lden_valid <- subset(str_add_lden, str_add_lden$check == TRUE)
str_add_lden_valid$check <- NULL
str_add_lden_valid <- st_set_geometry(str_add_lden_valid, str_add_lden_valid$geometry)
str_add_lden_valid <- st_transform(str_add_lden_valid, crs = 32632)

# make union
str_lden_union <- str_lden_valid %>% summarise(geometry = st_make_valid(st_union(geometry)))
str_lden_add_union <- str_add_lden_valid %>% summarise(geometry = st_make_valid(st_union(geometry)))

# combine both
str_lden_complete_union <- rbind(str_lden_union, str_add_lden_union)


# export ------------------------------------------------------------------

saveRDS(str_lden_complete_union, file.path(dataPath, "umgebungslaerm/str_noise_combinded.rds"))
saveRDS(str_lden_valid, file.path(dataPath, "umgebungslaerm/str_noise.rds"))
