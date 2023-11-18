############################################################
# Load Data                                                #
############################################################

hk <- readRDS(file.path(dataPath, "housing/temp_hk_buffered.rds"))

street_noise <- readRDS(file.path(dataPath, "umgebungslaerm/str_noise.rds"))

############################################################
# preparation                                              #
############################################################

##### preparation function
prep_data <- function(df){
  # drop geometry
  #data_prepared <- st_drop_geometry(df)
  data_prepared <- df
  
  # restrict the data set 
  # to municipalities that are crossed by tracks
  data_prepared <- data_prepared[data_prepared$affected_munic == 1, ]
  
  # as factor
  data_prepared$blid <- as.factor(data_prepared$blid)
  
  # return
  data_prepared
}


##### apply function
hk_affected <- prep_data(hk)

# make union
street_union <- st_union(street_noise)


# make sf
hk_affected <- hk_affected[!is.na(hk_affected$lat_gps), ]
hk_affected.sf <- st_as_sf(hk_affected, coords = c("lon_gps", "lat_gps"), crs = 4326)
hk_affected.sf <- st_transform(hk_affected.sf, crs = 32632)

# take 10% sample
sam <- hk_affected.sf %>% sample_frac(size = 0.1, replace = FALSE)

# calculate distance
sam$distance_street <- as.numeric(apply(st_distance(sam, street_union), 1, min, na.rm = TRUE)) / 1000

# export by hand
saveRDS(sam, file.path(dataPath, "housing/Temp/sample_streets_5.rds"))

# -------------------------------------------------------------------------

# table labels ------------------------------------------------------------
tablabel_objchar <- c("alter" = "Age", "alter_squ" = "Age$^2$", "wohnflaeche" = "Living space", "wohnflaeche_squ" = "Living space$^2$", "grundstuecksflaeche" = "Plot area",
                      "grundstuecksflaeche_squ" = "Plot area$^2$", "anzahletagen" = "Number floors", "anzahletagenUNBEKANNT" = "Number floors (unknown)", "keller" = "Basement",
                      "as.factor(objektzustand)2" = "Condition: First occupancy after reconstruction", "as.factor(objektzustand)3" = "Condition: Like new", "as.factor(objektzustand)4" = "Condition: Reconstructed",
                      "as.factor(objektzustand)5" = "Condition: Modernised", "as.factor(objektzustand)6" = "Condition: Completely renovated", "as.factor(objektzustand)7" = "Condition: Well kempt",
                      "as.factor(objektzustand)8" = "Condition: Needs renovation", "as.factor(objektzustand)9" = "Condition: By arrangement", "as.factor(objektzustand)10" = "Condition: Dilapidated", "objektzustandUNBEKANNT" = "Condition (unknown)",
                      "as.factor(heizungsart)2" = "Heating: Electric heating", "as.factor(heizungsart)3" = "Heating: Self-contained central heating", "as.factor(heizungsart)4" = "Heating: District heating", 
                      "as.factor(heizungsart)5" = "Heating: Floor heating", "as.factor(heizungsart)6" = "Heating: Gas heating", "as.factor(heizungsart)7" = "Heating: Wood pellet heating", 
                      "as.factor(heizungsart)8" = "Heating: Night storage heating", "as.factor(heizungsart)9" = "Heating: Heating by stove", "as.factor(heizungsart)10" = "Heating: Oil heating",
                      "as.factor(heizungsart)11" = "Heating: Solar heating", "as.factor(heizungsart)12" = "Heating: Thermal heat pump", "as.factor(heizungsart)13" = "Heating: Central heating",
                      "heizungsartUNBEKANNT" = "Heating (unknown)", "as.factor(ausstattung)2" = "Endowment: Normal", "as.factor(ausstattung)3" = "Endowment: Sophisticated", "as.factor(ausstattung)4" = "Endowment: Deluxe",
                      "ausstattungUNBEKANNT" = "Endowment (unknown)", "badezimmer" = "Number bathrooms", "badezimmerUNBEKANNT" = "Number bathrooms (unknown)",
                      "etage" = "Floor", "balkon" = "Balcony", "einbaukueche" = "Built-in kitchen", "garten" = "Garden", "keller" = "basement", "kellerUNBEKANNT" = "Basement (unknown)")



# -------------------------------------------------------------------------

sam_est <- sam
sam_est <- st_drop_geometry(sam_est)

##### dependent variable
dep_hk <- "ln_houseprice"


##### object characteristics
char_indep_hk_streets <- c("in_bau", "alter", "alter_squ", "alterUNBEKANNT","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter",
                   "distance_airport", "distance_industry", "distance_street")

char_indep_hk <- c("in_bau", "alter", "alter_squ", "alterUNBEKANNT","wohnflaeche", "wohnflaeche_squ",
                           "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                           "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT",
                           "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                           "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter",
                           "distance_airport", "distance_industry")


##### distance buffers
dist_indep <- c("bf500_only")


##### interactions (both event times included)
int_indep <- c("bf500_only * law_inprogress", "bf500_only * law_established")


##### combine all independent variables
indep_hk <- c(char_indep_hk, dist_indep, int_indep)
indep_hk_streets <- c(char_indep_hk_streets, dist_indep, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))

form_hk_streets <- as.formula(paste(dep_hk,
                            paste(indep_hk_streets, collapse = " + "),
                            sep = "~"))

# estimation --------------------------------------------------------------


##### HK
# estimation
basemodel_hk_sample <- feols(form_hk, se = "hetero" , data = sam_est, fixef = c("months", "r1_id"))
basemodel_hk_sample_streets <- feols(form_hk_streets, se = "hetero" , data = sam_est, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk_sample, basemodel_hk_sample_streets, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_hk_sample, basemodel_hk_sample_streets, file = file.path(outputPath, "regression/sample_basemodel_hk_street_5.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "sample 5 distance street")

