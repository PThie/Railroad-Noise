############################################################
# description                                              #
############################################################

# prepares regional types

############################################################
# load data                                                #
############################################################

# data with regional types and settlement density
regional_types <- xlsx::read.xlsx(file.path(dataPath, "raumtyp/raumtyp_siedlungsdichte_nach_gemeinde.xlsx"), sheetName = "Daten")

# data with regional center
regional_center <- read.xlsx(file.path(dataPath, "raumzentren/raumzentren_nach_gemeinde.xlsx"), sheetName = "Daten")

# municipality information
gem <- st_read(file.path(dataGebiete, "Gemeinde/2017/VG250_GEM.shp"))

############################################################
# prepare                                                  #
############################################################

# delete first row
regional_types <- regional_types[2:nrow(regional_types), ]
regional_center <- regional_center[2:nrow(regional_center), ]

# delete "aggregat"
regional_types$Aggregat <- NULL
regional_center$Aggregat <- NULL

# rename
colnames(regional_types) <- c("ags", "municipality", "regional_type", "settlement_density")
colnames(regional_center) <- c("ags", "municipality", "center_identifier")

# merge geo info to regional center
gem <- gem[, c("AGS_0", "geometry")]
regional_center <- merge(regional_center, gem, by.x = "ags", by.y = "AGS_0", all.x = TRUE)
regional_center <- st_set_geometry(regional_center, regional_center$geometry)

# calculate centroid for each region (in center data)
centroids <- st_centroid(regional_center)

# drop and rename 
centroids <- centroids[, c("ags", "geometry")]
regional_center <- st_drop_geometry(regional_center)

# unique
centroids <- centroids[!duplicated(centroids$ags), ]
regional_center <- regional_center[!duplicated(regional_center$ags), ]

# merge
regional_center <- merge(regional_center, centroids, by = "ags")


############################################################
# export                                                   #
############################################################

write.fst(regional_types, file.path(dataPath, "raumtyp/raumtyp_siedlungsdicht_nach_gemeinde_prep.fst"))
saveRDS(regional_center, file.path(dataPath, "raumzentren/raumzentren_nach_gemeinde_prep.rds"))
