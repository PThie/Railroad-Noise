############################################################
# Load Data                                                #
############################################################

# housing data
hk <- haven::read_dta("M:/_FDZ/RWI-GEO/RWI-GEO-hk17/daten/On-site/v4/HK_allVersions_ohneText.dta")


############################################################
# Buffering                                                #
############################################################

# buffer 
bf25 <- st_buffer(main_tracks, dist = 25)
bf50 <- st_buffer(main_tracks, dist = 50)
bf500 <- st_buffer(main_tracks, dist = 500)
bf200 <- st_buffer(main_tracks, dist = 200)

# build union for each buffer
uni25 <- st_union(bf25)
uni50 <- st_union(bf50)
uni500 <- st_union(bf500)
uni200 <- st_union(bf200)

############################################################
# Preparation Housing data                                 #
############################################################

# restrict to 2017
hk17 <- hk[hk$ajahr == 2017, ]

# main variables --------------------------------------------------------------------
# drop unrelevant variables and adjust the format of the remaining variables

hk17 <- hk17 %>% mutate(
  # object characteristics
  obid = unclass(obid),
  kaufpreis = unclass(kaufpreis),
  mieteinnahmenpromonat = unclass(mieteinnahmenpromonat),
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
  denkmalobjekt = unclass(denkmalobjekt),
  einliegerwohnung = unclass(einliegerwohnung),
  ferienhaus = unclass(ferienhaus),
  gaestewc = unclass(gaestewc),
  keller = unclass(keller),
  parkplatz = unclass(parkplatz),
  kaufvermietet = unclass(kaufvermietet),
  bauphase = unclass(bauphase),
  ausstattung = unclass(ausstattung),
  heizungsart = unclass(heizungsart),
  kategorie_Haus = unclass(kategorie_Haus),
  objektzustand = unclass(objektzustand),
  
  # geographic characteristics
  amr = unclass(erg_amd),
  gid2015 = unclass(gid2015),
  kid2019 = unclass(kid2019),
  blid = unclass(blid),
  gid2019 = unclass(gid2019),
  plz = unclass(plz),
  lat_utm = unclass(lat_utm),
  lon_utm = unclass(lon_utm),
  lat = unclass(lat),
  lon = unclass(lon),
  
  .keep = "none")


# duplicates --------------------------------------------------------------------
# remove duplicates and use the last spell

hk17$n <- ave(1:length(hk17$obid), hk17$obid, FUN = length)

hk17 <- hk17[hk17$n == hk17$spell, ] 

sum(duplicated(hk17$obid))

hk17$n <- NULL

###############################################################
# prepare independent variables                               #
###############################################################


# kaufpreis ---------------------------------------------------------------
# restrict kaufpreis to reasonable range
hk17 <- hk17[hk17$kaufpreis >= 1000, ]
hk17 <- hk17[hk17$kaufpreis <= 10000000, ]
hk17$kaufpreis[hk17$kaufpreis < 0 ] <- NA

# zimmeranzahl ------------------------------------------------------------
# restrict to reasonable range
hk17 <- hk17[hk17$zimmeranzahl <= 11 & hk17$zimmeranzahl > 0, ]


# wohnflaeche -------------------------------------------------------------
# restrict to reasonable range
hk17 <- hk17[hk17$wohnflaeche >= 25 & hk17$wohnflaeche <= 500, ]
hk17$wohnflaeche[hk17$wohnflaeche < 0] <- NA

# creating a squahk17 version
hk17$wohnflaeche_squ <- hk17$wohnflaeche^2


# grundstueckflaeche ------------------------------------------------------
# restrict to reasonable range
hk17 <- hk17[hk17$grundstuecksflaeche >= 50 & hk17$grundstuecksflaeche <= 10000, ]
hk17$grundstuecksflaeche[hk17$grundstuecksflaeche < 0] <- NA

# creating a squahk17 version
hk17$grundstuecksflaeche_squ <- hk17$grundstuecksflaeche^2


# house types -------------------------------------------------------------
# dummy for single houses (1 == Einfamilienhaus (freistehend), 2 == Einfamilienhaus)
hk17$single_house <- if_else(hk17$kategorie_Haus == 1 | hk17$kategorie_Haus == 2, true = 1, false = 0, missing = NULL)

# dummy for semidetached houses (3 == Doppelhaushälfte)
hk17$semi_house <- if_else(hk17$kategorie_Haus == 3, true = 1, false = 0, missing = NULL)

# dummy for serial houes (4 == Reihenhaus, 5 == Reihenmittelhaus)
hk17$serial_house <- if_else(hk17$kategorie_Haus == 4 | hk17$kategorie_Haus == 5, true = 1, false = 0, missing = NULL)


# dummy for first occupancy -----------------------------------------------
hk17$first_occupancy <- 0
hk17$first_occupancy[hk17$objektzustand == 1] <- 1

# baujahr -----------------------------------------------------------------
# hk17efine baujahr if < 1500 (because unrealistic value)
hk17$baujahr[hk17$baujahr < 1500] <- 0

# hk17efine the missings
hk17$baujahr[hk17$baujahr <= 0] <- NA


# baujahr categories ------------------------------------------------------
hk17$baujahr_cat <- NA # unknown values (including missings)
hk17$baujahr_cat[hk17$baujahr > 0 & hk17$baujahr <= 1900] <- 1 # before 1900
hk17$baujahr_cat[hk17$baujahr >= 1901 & hk17$baujahr <= 1945] <- 2 # 1900-1945
hk17$baujahr_cat[hk17$baujahr >= 1946 & hk17$baujahr <= 1959] <- 3 # 1946-1959
hk17$baujahr_cat[hk17$baujahr >= 1960 & hk17$baujahr <= 1969] <- 4 # 1960-1969
hk17$baujahr_cat[hk17$baujahr >= 1970 & hk17$baujahr <= 1979] <- 5 # 1970-1979
hk17$baujahr_cat[hk17$baujahr >= 1980 & hk17$baujahr <= 1989] <- 6 # 1980-1989
hk17$baujahr_cat[hk17$baujahr >= 1990 & hk17$baujahr <= 1999] <- 7 # 1990-1999
hk17$baujahr_cat[hk17$baujahr >= 2000 & hk17$baujahr <= 2009] <- 8 # 2000-2009
hk17$baujahr_cat[hk17$baujahr >= 2010] <- 9 # 2010-2020


# im bau ------------------------------------------------------------------
# construct dummy which is equal to one if the building is under construction (2 == bauphase)
hk17$in_bau <- if_else(hk17$bauphase == 2, true = 1, false = 0, missing = NULL)


# kreis ID ----------------------------------------------------------------
# reassign the missings in kid2019
hk17$kid2019[is.na(hk17$kid2019)] <- 0

# dropping those variables where you dont have a Kreis ID
hk17 <- hk17[hk17$kid2019 > 0, ]


# missing values ----------------------------------------------------------
# re-assign the remaining missing values
hk17[hk17 < 0] <- NA


# date --------------------------------------------------------------------
# constructing date variable (year and month)
# starting date
hk17$date <- ymd(paste(hk17$ajahr, hk17$amonat, "01", sep = "-"))
hk17$year_mon <- format(as.Date(hk17$date), "%Y-%m")

# UNBEKANNT values --------------------------------------------------------
hk17$mieteinnahmenpromonatUNBEKANNT <- 0
hk17$mieteinnahmenpromonatUNBEKANNT[is.na(hk17$mieteinnahmenpromonat)] <- 1

hk17$baujahr_catUNBEKANNT <- 0
hk17$baujahr_catUNBEKANNT[is.na(hk17$baujahr)] <- 1

hk17$nutzflaecheUNBEKANNT <- 0
hk17$nutzflaecheUNBEKANNT[is.na(hk17$nutzflaeche)] <- 1

hk17$anzahletagenUNBEKANNT <- 0
hk17$anzahletagenUNBEKANNT[is.na(hk17$anzahletagen)] <- 1

hk17$badezimmerUNBEKANNT <- 0
hk17$badezimmerUNBEKANNT[is.na(hk17$badezimmer)] <- 1

hk17$denkmalobjektUNBEKANNT <- 0
hk17$denkmalobjektUNBEKANNT[is.na(hk17$denkmalobjekt)] <- 1

hk17$einliegerwohnungUNBEKANNT <- 0
hk17$einliegerwohnungUNBEKANNT[is.na(hk17$einliegerwohnung)] <- 1

hk17$ferienhausUNBEKANNT <- 0
hk17$ferienhausUNBEKANNT[is.na(hk17$ferienhaus)] <- 1

hk17$gaestewcUNBEKANNT <- 0
hk17$gaestewcUNBEKANNT[is.na(hk17$gaestewc)] <- 1

hk17$kaufvermietetUNBEKANNT <- 0
hk17$kaufvermietetUNBEKANNT[is.na(hk17$kaufvermietet)] <- 1

hk17$kellerUNBEKANNT <- 0
hk17$kellerUNBEKANNT[is.na(hk17$keller)] <- 1

hk17$bauphaseUNBEKANNT <- 0
hk17$bauphaseUNBEKANNT[is.na(hk17$bauphase)] <- 1

hk17$ausstattungUNBEKANNT <- 0
hk17$ausstattungUNBEKANNT[is.na(hk17$ausstattung)] <- 1

hk17$heizungsartUNBEKANNT <- 0
hk17$heizungsartUNBEKANNT[is.na(hk17$heizungsart)] <- 1

hk17$objektzustandUNBEKANNT <- 0
hk17$objektzustandUNBEKANNT[is.na(hk17$objektzustand)] <- 1


# reassigning missing values ----------------------------------------------
# main idea: when there is no value then it simply was not specified because there is no such feature or just one (e.g. number of bathrooms)

hk17$mieteinnahmenpromonat[is.na(hk17$mieteinnahmenpromonat)] <- 0
hk17$nutzflaeche[is.na(hk17$nutzflaeche)] <- 0
hk17$anzahletagen[is.na(hk17$anzahletagen)] <- 1 # implausible to have no floors (there must be at least one floor)
hk17$badezimmer[is.na(hk17$badezimmer)] <- 1 # implausible that a house does not have a bathroom
hk17$denkmalobjekt[is.na(hk17$denkmalobjekt)] <- 0
hk17$einliegerwohnung[is.na(hk17$einliegerwohnung)] <- 0
hk17$ausstattung[is.na(hk17$ausstattung)] <- 2 # assume "normal" ausstattung if not further specified
hk17$heizungsart[is.na(hk17$heizungsart)] <- 13 # assume central heating (Zentralheizung) if not further specified (most comman type of heating)
hk17$objektzustand[is.na(hk17$objektzustand)] <- 7 # assume "gepflegt"


# age ---------------------------------------------------------------------
# construct a variables which specifies the age of the building
hk17$alter <- NA
hk17$alter <- hk17$jahr - hk17$baujahr
hk17$alter[hk17$alter <= 0] <- NA

# if age us unknown specfied as median age
hk17$alter[is.na(hk17$alter)] <- median(hk17$alter, na.rm = TRUE)

# construct squahk17 age (to capture non-linear trend)
hk17$alter_squ <- NA
hk17$alter_squ <- hk17$alter^2

# construct UNBEKANNT variable
hk17$alterUNBEKANNT <- 0
hk17$alterUNBEKANNT[is.na(hk17$alter)] <- 1



###############################################################
# prepare dependent variables                                 #
###############################################################


# price per square meters -------------------------------------------------

hk17$price_sqmeter <- hk17$kaufpreis / hk17$wohnflaeche

# generate log of price per square meters (natural logarithm)
hk17$ln_price_sqmeter <- log(hk17$price_sqmeter)


# houseprice log ----------------------------------------------------------

hk17$ln_houseprice <- log(hk17$kaufpreis)





# -------------------------------------------------------------------------
# drop NAs in coordinates
hk17 <- hk17[!is.na(hk17$lat), ]
hk17 <- hk17[!is.na(hk17$lon), ]
hk17 <- hk17[hk17$lat > 0, ]
hk17 <- hk17[hk17$lon > 0, ]

hk.sf <- st_as_sf(hk17, coords = c("lat", "lon"), crs = 4326)
hk.sf <- st_transform(hk.sf, crs = 32632)


# intersect objects with buffers
int_bf25 <- st_intersec

int_bf50 <- st_intersects(hk.sf, uni50)
hk.sf$bf50 <- lengths(int_bf50)
#hk.sf$bf50[hk.sf$bf50 != 0] <- 1

int_bf200 <- st_intersects(hk.sf, uni200)
hk.sf$bf200 <- lengths(int_bf200)
hk.sf$bf200[hk.sf$bf200 != 0] <- 1
hk.sf$bf200[hk.sf$bf50 == 1] <- 0

int_bf500 <- st_intersects(hk.sf, uni500)
hk.sf$bf500 <- lengths(int_bf500)
hk.sf$bf500[hk.sf$bf500 != 0] <- 1
hk.sf$bf500[hk.sf$bf50 == 1 | hk.sf$bf200 == 1] <- 0

treat50 <- hk.sf[hk.sf$bf50 == 1,]
treat500 <- hk.sf[hk.sf$bf500 == 1,]

tm_shape(treat50)+ 
  tm_dots(col = "green")+
  tm_shape(treat500)+
  tm_dots(col = "red")+
  tm_shape(uni50)+
  tm_polygons(col = "grey80", alpha = 0.5)+
  tm_shape(uni500)+
  tm_polygons(col = "grey90", alpha = 0.3)


# merge grid info
grid <- readRDS("N:/UBA_Soziale_Aspekte/Daten/Laerm/Grid/grid_ger.rds")
hk.sf <- st_join(hk.sf, grid, left = TRUE)

# merge muncipality info
zuord <- read.fst("N:/UBA_Soziale_Aspekte/Daten/Laerm/zuordnung.fst")
hk.sf <- merge(hk.sf, zuord, by.x = "idm", by.y = "r1_id", all.x = TRUE)

hk.est <- hk.sf
hk.est <- st_drop_geometry(hk.sf)


fe1 <- feols(ln_houseprice ~  in_bau + alter + alter_squ + alterUNBEKANNT + wohnflaeche + wohnflaeche_squ +
               grundstuecksflaeche + grundstuecksflaeche_squ + 
               anzahletagen + anzahletagenUNBEKANNT + badezimmer + badezimmerUNBEKANNT + as.factor(heizungsart) + heizungsartUNBEKANNT +
               as.factor(ausstattung) + ausstattungUNBEKANNT + keller + kellerUNBEKANNT +
               zimmeranzahl + bf50 + bf200 + bf500 | year_mon + AGS, data = hk.est)

etable(fe1, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10))


