############################################################
# Load Data                                                #
############################################################

# load main tracks
main_tracks <- st_read(file.path(dataPath, "main_tracks/tracks5805.shp"))

# load buffered houses from previous step
hk <- readRDS(file.path(dataPath, "housing/temp_hk_buffered.rds"))


############################################################
# function                                                 #
############################################################

bf.intersection.fun <- function(df){
  
  ##### documentation
  #' Intersection function with buffers
  #'
  #' \code{bf.intersection.func} returns indicators for housing objects being close to the tracks (based on distance buffers)
  #'
  #' @param df specifies the input data frame (has to be sf format)
  #'
  #' @return Returns the input data frame with distance indicators included
  #'
  #' @export
  
  ##### function itself
  # intersections -----------------------------------------------------------
  int_bf25 <- st_intersects(df, bf25)
  int_bf50 <- st_intersects(df, bf50)
  int_bf100 <- st_intersects(df, bf100)
  int_bf250 <- st_intersects(df, bf250)
  int_bf500 <- st_intersects(df, bf500)
  int_bf750 <- st_intersects(df, bf750)
  int_bf1000 <- st_intersects(df, bf1000)
  int_bf2000 <- st_intersects(df, bf2000)
  int_bf3000 <- st_intersects(df, bf3000)
  int_bf5000 <- st_intersects(df, bf5000)
  
  # add to data -------------------------------------------------------------
  df %>% mutate(# for 25 meters
    bf25_simpltra = lengths(int_bf25),
    bf25_simpltra = replace(bf25_simpltra, bf25_simpltra != 0, 1),
    bf25_simpltra = replace(bf25_simpltra, bf25_simpltra != 1, 0),
    
    # for 50 meters
    bf50_simpltra = lengths(int_bf50),
    bf50_simpltra = replace(bf50_simpltra, bf25_simpltra == 1, 0),
    bf50_simpltra = replace(bf50_simpltra, bf50_simpltra != 0, 1),
    bf50_simpltra = replace(bf50_simpltra, bf50_simpltra != 1, 0), 
    
    # for up to 50 meters (0-50)
    bf50_only_simpltra = 0,
    bf50_only_simpltra = replace(bf50_only_simpltra, bf25_simpltra == 1, 1),
    bf50_only_simpltra = replace(bf50_only_simpltra, bf50_simpltra == 1, 1),
    
    # for 100 meters
    bf100_simpltra = lengths(int_bf100),
    bf100_simpltra = replace(bf100_simpltra, bf25_simpltra == 1 | 
                      bf50_simpltra == 1, 0),
    bf100_simpltra = replace(bf100_simpltra, bf100_simpltra != 0, 1),
    bf100_simpltra = replace(bf100_simpltra, bf100_simpltra != 1, 0),
    
    # for 250 meters
    bf250_simpltra = lengths(int_bf250),
    bf250_simpltra = replace(bf250_simpltra, bf25_simpltra == 1 | 
                      bf50_simpltra == 1 |
                      bf100_simpltra == 1, 0),
    bf250_simpltra = replace(bf250_simpltra, bf250_simpltra != 0, 1),
    bf250_simpltra = replace(bf250_simpltra, bf250_simpltra != 1, 0),
    
    # for 500 meters
    bf500_simpltra = lengths(int_bf500),
    bf500_simpltra = replace(bf500_simpltra, bf25_simpltra == 1 | 
                      bf50_simpltra == 1 |
                      bf100_simpltra == 1 |
                      bf250_simpltra == 1, 0),
    bf500_simpltra = replace(bf500_simpltra, bf500_simpltra != 0, 1),
    bf500_simpltra = replace(bf500_simpltra, bf500_simpltra != 1, 0),
    
    # for 750 meters
    bf750_simpltra = lengths(int_bf750),
    bf750_simpltra = replace(bf750_simpltra, bf25_simpltra == 1 | 
                      bf50_simpltra == 1 |
                      bf100_simpltra == 1 |
                      bf250_simpltra == 1 |
                      bf500_simpltra == 1, 0),
    bf750_simpltra = replace(bf750_simpltra, bf750_simpltra != 0, 1),
    bf750_simpltra = replace(bf750_simpltra, bf750_simpltra != 1, 0),
    
    # for 1000 meters
    bf1000_simpltra = lengths(int_bf1000),
    bf1000_simpltra = replace(bf1000_simpltra, bf25_simpltra == 1 | 
                       bf50_simpltra == 1 |
                       bf100_simpltra == 1 |
                       bf250_simpltra == 1 |
                       bf500_simpltra == 1 |
                       bf750_simpltra == 1, 0),
    bf1000_simpltra = replace(bf1000_simpltra, bf1000_simpltra != 0, 1),
    bf1000_simpltra = replace(bf1000_simpltra, bf1000_simpltra != 1, 0),
    
    # for 2000 meters
    bf2000_simpltra = lengths(int_bf2000),
    bf2000_simpltra = replace(bf2000_simpltra, bf25_simpltra == 1 | 
                       bf50_simpltra == 1 |
                       bf100_simpltra == 1 |
                       bf250_simpltra == 1 |
                       bf500_simpltra == 1 |
                       bf750_simpltra == 1 |
                       bf1000_simpltra == 1, 0),
    bf2000_simpltra = replace(bf2000_simpltra, bf2000_simpltra != 0, 1),
    bf2000_simpltra = replace(bf2000_simpltra, bf2000_simpltra != 1, 0),
    
    # for 3000 meters
    bf3000_simpltra = lengths(int_bf3000),
    bf3000_simpltra = replace(bf3000_simpltra, bf25_simpltra == 1 | 
                       bf50_simpltra == 1 |
                       bf100_simpltra == 1 |
                       bf250_simpltra == 1 |
                       bf500_simpltra == 1 |
                       bf750_simpltra == 1 |
                       bf1000_simpltra == 1 |
                       bf2000_simpltra == 1, 0),
    bf3000_simpltra = replace(bf3000_simpltra, bf3000_simpltra != 0, 1),
    bf3000_simpltra = replace(bf3000_simpltra, bf3000_simpltra != 1, 0),
    
    # for 5000 meters
    bf5000_simpltra = lengths(int_bf5000),
    bf5000_simpltra = replace(bf5000_simpltra, bf25_simpltra == 1 | 
                       bf50_simpltra == 1 |
                       bf100_simpltra == 1 |
                       bf250_simpltra == 1 |
                       bf500_simpltra == 1 |
                       bf750_simpltra == 1 |
                       bf1000_simpltra == 1 |
                       bf2000_simpltra == 1 |
                       bf3000_simpltra == 1, 0),
    bf5000_simpltra = replace(bf5000_simpltra, bf5000_simpltra != 0, 1),
    bf5000_simpltra = replace(bf5000_simpltra, bf5000_simpltra != 1, 0),
    
    
    # for 500 meters (but just 1 and 0)
    bf500_only_simpltra = lengths(int_bf500),
    bf500_only_simpltra = replace(bf500_only_simpltra, bf500_only_simpltra != 0, 1),
    bf500_only_simpltra = replace(bf500_only_simpltra, bf500_only_simpltra != 1, 0)
  )
}


############################################################
# preparation                                              #
############################################################


# simplify tracks ---------------------------------------------------------

# transform to UTM (because you need a CRS based on meters for distances)
main_tracks <- st_transform(main_tracks, crs = 32632)

# simplify the tracks to straight lines (based on 10km)
main_tracks_simple <- st_simplify(main_tracks, dTolerance = 25000)

# check the result
tm_shape(main_tracks)+
  tm_lines(col = "black", lwd = 1.5)
  tm_shape(main_tracks_simple)+
  tm_lines(col = "red", lwd = 1.5)


# calculate buffers for intersection --------------------------------------

# buffer 
bf25 <- st_buffer(main_tracks_simple, dist = 25)
bf50 <- st_buffer(main_tracks_simple, dist = 50)
bf100 <- st_buffer(main_tracks_simple, dist = 100)
bf250 <- st_buffer(main_tracks_simple, dist = 250)
bf500 <- st_buffer(main_tracks_simple, dist = 500)
bf750 <- st_buffer(main_tracks_simple, dist = 750)
bf1000 <- st_buffer(main_tracks_simple, dist = 1000)
bf2000 <- st_buffer(main_tracks_simple, dist = 2000)
bf3000 <- st_buffer(main_tracks_simple, dist = 3000)
bf5000 <- st_buffer(main_tracks_simple, dist = 5000)

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

############################################################
# Correlation                                              #
############################################################
##### preparation function

prep_data <- function(data_prepared){

  # drop geometry
  data_prepared <- st_drop_geometry(data_prepared)
  
  # restrict the data set 
  # to municipalities that are crossed by tracks
  data_prepared <- data_prepared[data_prepared$affected_munic == 1, ]
  
  data_prepared$law_complete <- 0
  data_prepared$law_complete[data_prepared$law_established == 1 | data_prepared$law_inprogress == 1] <- 1
  
  # return
  return(data_prepared)
}

tst_data_prep <- prep_data(hk.sf)
tst_data_prep <- tst_data_prep[!is.na(tst_data_prep$bf500_only), ]

cor(tst_data_prep$bf500_only, tst_data_prep$bf500_only_simpltra)



tst_est <- lm(bf500_only ~ bf500_only_simpltra, data = tst_data_prep)

summary(tst_est)

# store predicted values
tst_data_prep$bf500_only_pred <- tst_est$fitted.values




# define variables and formula --------------------------------------------

##### dependent variable
dep_hk <- "ln_houseprice"


##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT", 
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter",
                   "distance_airport", "distance_industry")


##### interactions (both event times included)
int_indep <- c("bf500_only_pred * law_inprogress", "bf500_only_pred * law_established")
int_indep <- c("bf500_only_pred * law_complete")

#dist_indep <- c("bf500_only_pred")

##### combine all independent variables
indep_hk <- c(char_indep_hk, int_indep)



##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))

# estimation --------------------------------------------------------------

##### HK
# estimation 
basemodel_hk <- feols(form_hk, data = tst_data_prep, fixef = c("months", "r1_id"), se = "hetero")

# show results
etable(basemodel_hk, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


library(ivreg)

tst_mod <- feols(ln_houseprice ~ in_bau + alter + alter_squ + wohnflaeche + wohnflaeche_squ + grundstuecksflaeche + grundstuecksflaeche_squ +
                   anzahletagen + anzahletagenUNBEKANNT + badezimmer + badezimmerUNBEKANNT + as.factor(heizungsart) + heizungsartUNBEKANNT +
                   as.factor(ausstattung) + ausstattungUNBEKANNT + zimmeranzahl + as.factor(objektzustand) + objektzustandUNBEKANNT + distance_station +
                   distance_junction + distance_largcenter + distance_medcenter + distance_airport + distance_industry | 
                   bf500_only * law_inprogress + bf500_only * law_established ~ bf500_only_simpltra * law_inprogress + bf500_only_simpltra * law_established,
                 data = tst_data_prep,
                 fixef = c("months", "r1_id"), se = "hetero")

etable(tst_mod, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


tst_mod <- feols(ln_houseprice ~ in_bau + alter + alter_squ + wohnflaeche + wohnflaeche_squ + grundstuecksflaeche + grundstuecksflaeche_squ +
                   anzahletagen + anzahletagenUNBEKANNT + badezimmer + badezimmerUNBEKANNT + as.factor(heizungsart) + heizungsartUNBEKANNT +
                   as.factor(ausstattung) + ausstattungUNBEKANNT + zimmeranzahl + as.factor(objektzustand) + objektzustandUNBEKANNT + distance_station +
                   distance_junction + distance_largcenter + distance_medcenter + distance_airport + distance_industry +
                   bf500_only_pred * law_inprogress + bf500_only_pred * law_established,
                 data = tst_data_prep,
                 fixef = c("months", "r1_id"), se = "hetero")

etable(tst_mod, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")
