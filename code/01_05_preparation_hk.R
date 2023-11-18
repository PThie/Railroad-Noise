############################################################
# Load Data                                                #
############################################################

# housing data
red_org <- haven::read_dta("M:/_FDZ/RWI-GEO/RWI-GEO-red/daten/On-site/v5.1/HK_allVersions_ohneText.dta")

# load noise barrier grid info
noibar <- readRDS(file.path(dataPath, "Schallschutzwand/ssw_grid.rds"))

# load motorway junctions
mw_junc <- readRDS(file.path(dataPath, "motorway_junction/ger_motorway_junctions.rds"))

# load stations
stations <- readRDS(file.path(dataPath, "Betriebsstellen/stations_prep.rds"))

############################################################
# Basic preparation                                        #
############################################################

# copy
red <- red_org

# main variables --------------------------------------------------------------------
# drop unrelevant variables and adjust the format of the remaining variables

red <- red %>% mutate(
  # object characteristics
  obid = unclass(obid),
  kaufpreis = unclass(kaufpreis),
  ajahr = unclass(ajahr),
  amonat = unclass(amonat),
  jahr = unclass(ejahr),
  emonat = unclass(emonat),
  spell = unclass(spell),
  
  # housing characteristics
  baujahr = unclass(baujahr),
  wohnflaeche = unclass(wohnflaeche),
  grundstuecksflaeche = unclass(grundstuecksflaeche),
  nutzflaeche = unclass(nutzflaeche),
  anzahletagen = unclass(anzahletagen),
  zimmeranzahl = unclass(zimmeranzahl),
  badezimmer = unclass(badezimmer),
  aufzug = unclass(aufzug),
  gaestewc = unclass(gaestewc),
  keller = unclass(keller),
  bauphase = unclass(bauphase),
  ausstattung = unclass(ausstattung),
  heizungsart = unclass(heizungsart),
  kategorie_Haus = unclass(kategorie_Haus),
  objektzustand = unclass(objektzustand),
  laufzeittage = unclass(laufzeittage),
  hits = unclass(hits),
  
  # geographic characteristics
  amr = unclass(erg_amd),
  kid2019 = unclass(kid2019),
  kid2019_gen = unclass(kid2019_gen),
  r1_id = unclass(ergg_1km),
  blid = unclass(blid),
  gid2019 = unclass(gid2019),
  gid2019_gen = unclass(gid2019_gen),
  plz = unclass(plz),
  lat_gps = unclass(lat_gps),
  lon_gps = unclass(lon_gps),
  
  .keep = "none")


# duplicates --------------------------------------------------------------------
# remove duplicates and use the last spell

red$n <- ave(1:length(red$obid), red$obid, FUN = length)

red <- red[red$n == red$spell, ] 

sum(duplicated(red$obid))

red$n <- NULL

###############################################################
# restrict years                                              #
###############################################################

red <- red[red$ajahr >= 2013, ]

###############################################################
# prepare independent variables                               #
###############################################################

# kaufpreis ---------------------------------------------------------------
# restrict kaufpreis to reasonable range (cut of 1% and 99% percentiles)
red$kaufpreis[red$kaufpreis < 0] <- NA

breaks_quantile <- as.numeric(quantile(red$kaufpreis, c(0.01, 0.99), na.rm = TRUE))
breaks_quantile

red <- red[red$kaufpreis >= breaks_quantile[1] & red$kaufpreis <= breaks_quantile[2], ]


# zimmeranzahl ------------------------------------------------------------
# restrict to reasonable range
red$zimmeranzahl[red$zimmeranzahl < 0] <- NA

breaks_quantile <- as.numeric(quantile(red$zimmeranzahl, c(0.01, 0.99), na.rm = TRUE))
breaks_quantile

red <- red[red$zimmeranzahl >= 1 & red$zimmeranzahl <= breaks_quantile[2], ]


# wohnflaeche -------------------------------------------------------------
# restrict to reasonable range
red$wohnflaeche[red$wohnflaeche < 0] <- NA

breaks_quantile <- as.numeric(quantile(red$wohnflaeche, c(0.01, 0.99), na.rm = TRUE))
breaks_quantile

red <- red[red$wohnflaeche >= breaks_quantile[1] & red$wohnflaeche <= breaks_quantile[2], ]

# creating a squared version
red$wohnflaeche_squ <- red$wohnflaeche^2


# grundstueckflaeche ------------------------------------------------------
# restrict to reasonable range
red$grundstuecksflaeche[red$grundstuecksflaeche < 0] <- NA

breaks_quantile <- as.numeric(quantile(red$grundstuecksflaeche, c(0.01, 0.99), na.rm = TRUE))
breaks_quantile

red <- red[red$grundstuecksflaeche >= breaks_quantile[1] & red$grundstuecksflaeche <= breaks_quantile[2], ]

# creating a squared version
red$grundstuecksflaeche_squ <- red$grundstuecksflaeche^2


# house types -------------------------------------------------------------
# dummy for single houses (1 == Einfamilienhaus (freistehend), 2 == Einfamilienhaus)
red$single_house <- if_else(red$kategorie_Haus == 1 | red$kategorie_Haus == 2, true = 1, false = 0, missing = NULL)

# dummy for semidetached houses (3 == Doppelhaushälfte)
red$semi_house <- if_else(red$kategorie_Haus == 3, true = 1, false = 0, missing = NULL)

# dummy for serial houes (4 == Reihenhaus, 5 == Reihenmittelhaus)
red$serial_house <- if_else(red$kategorie_Haus == 4 | red$kategorie_Haus == 5, true = 1, false = 0, missing = NULL)


# dummy for first occupancy -----------------------------------------------
# objektzustand == 1 represents exactly first occupancy
red$first_occupancy <- 0
red$first_occupancy[red$objektzustand == 1] <- 1


# baujahr -----------------------------------------------------------------
# redefine the missings
red$baujahr[red$baujahr <= 0] <- NA

# redefine baujahr if < 1500 (because unrealistic value)
red$baujahr[red$baujahr < 1500] <- NA


# baujahr categories ------------------------------------------------------
red$baujahr_cat <- NA # unknown values (including missings)
red$baujahr_cat[red$baujahr >= 1500 & red$baujahr <= 1900] <- 1 # before 1900
red$baujahr_cat[red$baujahr >= 1901 & red$baujahr <= 1945] <- 2 # 1900-1945
red$baujahr_cat[red$baujahr >= 1946 & red$baujahr <= 1959] <- 3 # 1946-1959
red$baujahr_cat[red$baujahr >= 1960 & red$baujahr <= 1969] <- 4 # 1960-1969
red$baujahr_cat[red$baujahr >= 1970 & red$baujahr <= 1979] <- 5 # 1970-1979
red$baujahr_cat[red$baujahr >= 1980 & red$baujahr <= 1989] <- 6 # 1980-1989
red$baujahr_cat[red$baujahr >= 1990 & red$baujahr <= 1999] <- 7 # 1990-1999
red$baujahr_cat[red$baujahr >= 2000 & red$baujahr <= 2009] <- 8 # 2000-2009
red$baujahr_cat[red$baujahr >= 2010] <- 9 # 2010-2020


# im bau ------------------------------------------------------------------
# construct dummy which is equal to one if the building is under construction (2 == bauphase, "house in process of building")
red$in_bau <- if_else(red$bauphase == 2, true = 1, false = 0, missing = NULL)


# kreis ID ----------------------------------------------------------------
# reassign the missings in kid2019
red$kid2019_gen[is.na(red$kid2019_gen)] <- 0

# dropping those variables where you dont have a Kreis ID
red <- red[red$kid2019_gen > 0, ]


# missing values ----------------------------------------------------------
# re-assign the remaining missing values
red[red < 0] <- NA


# date --------------------------------------------------------------------
# constructing date variable (year and month)
# starting date
red$date <- ymd(paste(red$ajahr, red$amonat, "01", sep = "-"))
red$year_mon <- format(as.Date(red$date), "%Y-%m")


# UNBEKANNT values --------------------------------------------------------
red$baujahr_catUNBEKANNT <- 0
red$baujahr_catUNBEKANNT[is.na(red$baujahr)] <- 1

red$nutzflaecheUNBEKANNT <- 0
red$nutzflaecheUNBEKANNT[is.na(red$nutzflaeche)] <- 1

red$anzahletagenUNBEKANNT <- 0
red$anzahletagenUNBEKANNT[is.na(red$anzahletagen)] <- 1

red$badezimmerUNBEKANNT <- 0
red$badezimmerUNBEKANNT[is.na(red$badezimmer)] <- 1

red$gaestewcUNBEKANNT <- 0
red$gaestewcUNBEKANNT[is.na(red$gaestewc)] <- 1

red$kellerUNBEKANNT <- 0
red$kellerUNBEKANNT[is.na(red$keller)] <- 1

red$bauphaseUNBEKANNT <- 0
red$bauphaseUNBEKANNT[is.na(red$bauphase)] <- 1

red$ausstattungUNBEKANNT <- 0
red$ausstattungUNBEKANNT[is.na(red$ausstattung)] <- 1

red$heizungsartUNBEKANNT <- 0
red$heizungsartUNBEKANNT[is.na(red$heizungsart)] <- 1

red$objektzustandUNBEKANNT <- 0
red$objektzustandUNBEKANNT[is.na(red$objektzustand)] <- 1


# reassigning missing values ----------------------------------------------
# main idea: when there is no value then it simply was not specified because there is no such feature or just one (e.g. number of bathrooms)

red$nutzflaeche[is.na(red$nutzflaeche)] <- 0
red$anzahletagen[is.na(red$anzahletagen)] <- 1 # implausible to have no floors (there must be at least one floor)
red$badezimmer[is.na(red$badezimmer)] <- 1 # implausible that a house does not have a bathroom
red$ausstattung[is.na(red$ausstattung)] <- 2 # assume "normal" ausstattung if not further specified
red$heizungsart[is.na(red$heizungsart)] <- 13 # assume central heating (Zentralheizung) if not further specified (most comman type of heating)
red$objektzustand[is.na(red$objektzustand)] <- 7 # assume "gepflegt (well kempt)"


# age ---------------------------------------------------------------------
# construct a variables which specifies the age of the building
red$alter <- NA
red$alter <- red$jahr - red$baujahr
red$alter[red$alter <= 0] <- NA

# if age us unknown specfied as median age
red$alter[is.na(red$alter)] <- median(red$alter, na.rm = TRUE)

# construct squared age (to capture non-linear trend)
red$alter_squ <- NA
red$alter_squ <- red$alter^2

# construct UNBEKANNT variable
red$alterUNBEKANNT <- 0
red$alterUNBEKANNT[is.na(red$alter)] <- 1


###############################################################
# prepare dependent variables                                 #
###############################################################

# price per square meters -------------------------------------------------
red$price_sqmeter <- red$kaufpreis / red$wohnflaeche

# generate log of price per square meters (natural logarithm)
red$ln_price_sqmeter <- log(red$price_sqmeter)

# houseprice log ----------------------------------------------------------
red$ln_houseprice <- log(red$kaufpreis)


###############################################################
# merge noise barriers                                        #
###############################################################

noibar <- st_drop_geometry(noibar)
red <- merge(red, noibar, by.x = "r1_id", by.y = "idm", all.x = TRUE)

###############################################################
# observation period                                          #
###############################################################

# June 2013 to June 2021
red <- red[red$year_mon >= "2013-06", ]

# create months as factor
red$monts <- as.factor(red$year_mon)


###############################################################
# treatment periods                                           #
###############################################################

# construct dummy for the months the law is in force (but not punishable)
red$law_inprogress <- ifelse(test = red$year_mon >= "2017-07" & red$year_mon <= "2020-11",
                                       yes = 1,
                                       no = 0)

# construct dummy for the months the law is in force (i.e ban is in force)
red$law_established <- ifelse(test = red$year_mon >= "2020-12",
                                        yes = 1,
                                        no = 0)

###############################################################
# prepare coordinates                                         #
###############################################################

# drop objects which do not have a geographic information
red_geo <- red
red_geo <- red_geo[!is.na(red_geo$lat_gps), ]
red_geo <- red_geo[!is.na(red_geo$lon_gps), ]

# define as sf
red_geo <- st_as_sf(red_geo, coords = c("lon_gps", "lat_gps"), crs = 4326, remove = FALSE)
red_geo <- st_transform(red_geo, crs = 32632)

# intermediate save
# save with coordinates
saveRDS(red_geo, file.path(dataPath, "housing/Temp/hk_geo_basic.rds"))


###############################################################
# distance to station                                         #
###############################################################

# define stations as sf
stations.sf <- st_as_sf(stations, coords = c("lon_stations", "lat_stations"), crs = 4326)

# change crs of stations
stations.sf <- st_transform(stations.sf, crs = 32632)

# create subsets by years (otherwise the next operations do not work because of 
# limited CPU)
red_1314 <- red_geo[red_geo$ajahr == 2013 | red_geo$ajahr == 2014, ]
red_1516 <- red_geo[red_geo$ajahr == 2015 | red_geo$ajahr == 2016, ]
red_1718 <- red_geo[red_geo$ajahr == 2017 | red_geo$ajahr == 2018, ]
red_1920 <- red_geo[red_geo$ajahr == 2019 | red_geo$ajahr == 2020, ]
red_21 <- red_geo[red_geo$ajahr == 2021,]

ptm <- proc.time() # takes the time
# name of the nearest station ---------------------------------------------

ordering1314 <- apply(st_distance(red_1314, stations.sf), 1, which.min)
ordering1516 <- apply(st_distance(red_1516, stations.sf), 1, which.min)
ordering1718 <- apply(st_distance(red_1718, stations.sf), 1, which.min)
ordering1920 <- apply(st_distance(red_1920, stations.sf), 1, which.min)
ordering21 <- apply(st_distance(red_21, stations.sf), 1, which.min)

red_1314$closest_station <- stations.sf$stations_name[ordering1314]
red_1516$closest_station <- stations.sf$stations_name[ordering1516]
red_1718$closest_station <- stations.sf$stations_name[ordering1718]
red_1920$closest_station <- stations.sf$stations_name[ordering1920]
red_21$closest_station <- stations.sf$stations_name[ordering21]

# distance to nearest station ---------------------------------------------
red_1314$distance_station <- as.numeric(apply(st_distance(red_1314, stations.sf), 1, min))/1000
red_1516$distance_station <- as.numeric(apply(st_distance(red_1516, stations.sf), 1, min))/1000
red_1718$distance_station <- as.numeric(apply(st_distance(red_1718, stations.sf), 1, min))/1000
red_1920$distance_station <- as.numeric(apply(st_distance(red_1920, stations.sf), 1, min))/1000
red_21$distance_station <- as.numeric(apply(st_distance(red_21, stations.sf), 1, min))/1000


###############################################################
# distance to motorway junction                               #
###############################################################

# clean motorway junctions
mw_junc <- mw_junc[, c(1, 2, 5)]

# drop duplicates
mw_junc <- mw_junc[!duplicated(mw_junc$osm_id), ]

# make sf
mw_junc.sf <- st_set_geometry(mw_junc, mw_junc$geometry)
mw_junc.sf <- st_transform(mw_junc.sf, crs = 32632)

# name of the nearest motorway junction -----------------------------------

ordering1314 <- apply(st_distance(red_1314, mw_junc.sf), 1, which.min)
ordering1516 <- apply(st_distance(red_1516, mw_junc.sf), 1, which.min)
ordering1718 <- apply(st_distance(red_1718, mw_junc.sf), 1, which.min)
ordering1920 <- apply(st_distance(red_1920, mw_junc.sf), 1, which.min)
ordering21 <- apply(st_distance(red_21, mw_junc.sf), 1, which.min)

red_1314$closest_junction <- mw_junc.sf$name[ordering1314]
red_1516$closest_junction <- mw_junc.sf$name[ordering1516]
red_1718$closest_junction <- mw_junc.sf$name[ordering1718]
red_1920$closest_junction <- mw_junc.sf$name[ordering1920]
red_21$closest_junction <- mw_junc.sf$name[ordering21]

# distance to the nearest station -----------------------------------------
# calculate distance to nearest motorway junction
red_1314$distance_junction <- as.numeric(apply(st_distance(red_1314, mw_junc.sf), 1, min))/1000
red_1516$distance_junction <- as.numeric(apply(st_distance(red_1516, mw_junc.sf), 1, min))/1000
red_1718$distance_junction <- as.numeric(apply(st_distance(red_1718, mw_junc.sf), 1, min))/1000
red_1920$distance_junction <- as.numeric(apply(st_distance(red_1920, mw_junc.sf), 1, min))/1000
red_21$distance_junction <- as.numeric(apply(st_distance(red_21, mw_junc.sf), 1, min))/1000

# merge back together -----------------------------------------------------
red_geo <- rbind(red_1314, red_1516, red_1718, red_1920, red_21)

proc.time() - ptm # takes the time

###############################################################
# export                                                      #
###############################################################

# save basic data set without any additional geographic computations (i.e. distances)
saveRDS(red, file.path(dataPath, "housing/Temp/hk_basic_prep.rds"))

# save data with all the additional geographic information
saveRDS(red_geo, file.path(dataPath, "housing/Temp/hk_complete_prep.rds"))
