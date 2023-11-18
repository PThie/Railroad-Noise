############################################################
# Description                                              #
############################################################

# prepares the station data and the noise barriers

############################################################
# Load Data                                                #
############################################################

# load geographic info of noise barriers (orginal CRS: 4326)
ssw <- st_read(file.path(dataPath, "Schallschutzwand/schallschutzwand.shp"))

# load stations
betriebsstellen <- read.xlsx(file.path(dataPath, "Betriebsstellen/Excel/betriebsstellen_open_data.xlsx"), encoding = "UTF-8", sheetIndex = 1)

# load grid information (1km x 1km grid for Germany)
grid <- readRDS("N:/UBA_Soziale_Aspekte/Daten/Laerm/Grid/grid_ger.rds")

############################################################
# Preparation noise barriers                               #
############################################################

# drop irrelevant variables for noise barriers
ssw <- ssw[, c("schallschu", "wandhoehe_", "geometry")]

# rename variables for noise barriers data
colnames(ssw) <- c("id_ssw", "height_ssw", "geometry")

# change CRS of noise barrier data
ssw <- st_transform(ssw, crs = 32632)

# export
saveRDS(ssw, file.path(dataPath, "Schallschutzwand/ssw_prep.rds"))

############################################################
# Preparation stations                                     #
############################################################

# filter for public stations (Bf and Bft)
stations <- betriebsstellen[betriebsstellen$STELLE_ART == "Bf" | betriebsstellen$STELLE_ART == "Bft",]

# drop duplicates
stations <- stations[!duplicated(stations$BEZEICHNUNG),]

# drop not needed variables
stations <- stations[, c(5, 10, 11)]
# rename
colnames(stations) <- c("stations_name", "lat_stations", "lon_stations")

# export
saveRDS(stations, file.path(dataPath, "Betriebsstellen/stations_prep.rds"))

############################################################
# Merge noise barriers and grids                           #
############################################################

# # identify grids with noise barrier in place
# ssw_grid <- st_join(grid, ssw)
# 
# # for those grid cells which have more than one noise barrier select only the first one
# # reasoning: because aim is to construct a dummy whether there is some noise barrier oin place (does not matter which exact one)
# # drop geometry for mutate command
# ssw_grid <- st_drop_geometry(ssw_grid)
# 
# ssw_grid <- ssw_grid %>% group_by(idm) %>% summarise(idm = first(idm),
#                                                   id_ssw = first(id_ssw),
#                                                   height_ssw = mean(height_ssw, na.rm = TRUE))
# 
# # contruct dummy which is one of there is some noise barrier present in this particular grid
# ssw_grid$noise_barrier <- 0
# ssw_grid$noise_barrier[!is.na(ssw_grid$id_ssw)] <- 1
# 
# # drop id_ssw (not needed anymore)
# ssw_grid$id_ssw <- NULL
# 
# # merge back geographic information of the grids
# ssw_grid <- merge(ssw_grid, grid, by = "idm")
# ssw_grid <- st_set_geometry(ssw_grid, ssw_grid$geometry)
# 
# # export
# saveRDS(ssw_grid, file = file.path(dataPath, "ssw_grid.rds"))


