############################################################
# Description                                              #
############################################################



############################################################
# Load Data                                                #
############################################################

hk <- readRDS(file.path(dataPath, "housing/temp_hk_buffered.rds"))

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


##################################### PLACEBO TEST 1 ############################################################################

# restrict data to observations that are close to airports
# close means within 3 km

# subset
hk_airports <- hk_affected[hk_affected$distance_airport <= 3, ]

# check for number of observations treated by periods
length(which(hk_airports$bf500_only == 1 & hk_airports$law_established == 1))
length(which(hk_airports$bf500_only == 1 & hk_airports$law_inprogress == 1))


# define variables and formula --------------------------------------------

##### dependent variable
dep_hk <- "ln_houseprice"


##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ", "alterUNBEKANNT","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT", "keller", "kellerUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter",
                   "distance_airport", "distance_industry")


##### distance buffers
dist_indep <- c("bf500_only")


##### interactions (both event times included)
int_indep <- c("bf500_only * law_inprogress", "bf500_only * law_established")


##### combine all independent variables
indep_hk <- c(char_indep_hk, dist_indep, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))

# estimation --------------------------------------------------------------

##### HK
# estimation
airports_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_airports, fixef = c("months", "r1_id"))

# show results
etable(airports_basemodel_hk, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(airports_basemodel_hk, file = file.path(outputPath, "regression/placebo_basemodel_hk_airports.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "placebo model hk airports sample")





##################################### PLACEBO TEST 3 ############################################################################

control_period <- hk_affected[hk_affected$year_mon <= "2017-06", ]

control_period <- control_period %>% mutate(new_law_inprogress = case_when(year_mon >= "2015-07" & year_mon <= "2016-11" ~ 1,
                                                                     year_mon < "2015-07" | year_mon > "2016-11" ~ 0),
                                      new_law_established = case_when(year_mon >= "2016-12" & year_mon <= "2017-06" ~ 1,
                                                                      year_mon < "2016-12" ~ 0))


##### interactions (both event times included)
int_indep_new <- c("bf500_only * new_law_inprogress", "bf500_only * new_law_established")




##### combine all independent variables
indep_hk <- c(char_indep_hk, dist_indep, int_indep_new)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))

# estimation --------------------------------------------------------------

##### HK
# estimation
shift_basemodel_hk <- feols(form_hk, se = "hetero" , data = control_period, fixef = c("months", "r1_id"))

# show results
etable(shift_basemodel_hk, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", drop = "blid")




##################################### PLACEBO TEST 3 ############################################################################

hk_affected$law_complete <- 0
hk_affected$law_complete[hk_affected$law_established == 1 | hk_affected$law_inprogress == 1] <- 1



length(which(hk_affected$law_complete == 1))
length(which(hk_affected$law_established == 1))
length(which(hk_affected$law_inprogress == 1))




# define variables and formula --------------------------------------------

##### dependent variable
dep_hk <- "ln_houseprice"


##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ", "alterUNBEKANNT","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT", "keller", "kellerUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter",
                   "distance_airport", "distance_industry")


##### distance buffers
dist_indep <- c("bf500_only")


##### interactions (both event times included)
int_indep <- c("bf500_only * law_complete")


##### combine all independent variables
indep_hk <- c(char_indep_hk, dist_indep, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))

# estimation --------------------------------------------------------------

##### HK
# estimation
complete_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_affected, fixef = c("months", "r1_id"))

# show results
etable(complete_basemodel_hk, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")






control_period <- hk_affected[hk_affected$year_mon <= "2017-06", ]


# back one year
control_period$law_complete_backone <- 0
control_period$law_complete_backone[control_period$year_mon >= "2016-07"] <- 1

# back two years
control_period$law_complete_backtwo <- 0
control_period$law_complete_backtwo[control_period$year_mon >= "2015-07"] <- 1

# back three years
control_period$law_complete_backthree <- 0
control_period$law_complete_backthree[control_period$year_mon >= "2014-07"] <- 1



# define variables and formula --------------------------------------------

##### dependent variable
dep_hk <- "ln_houseprice"


##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ", "alterUNBEKANNT","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT", "keller", "kellerUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter",
                   "distance_airport", "distance_industry")


##### distance buffers
dist_indep <- c("bf500_only")


##### interactions (both event times included)
int_indep <- c("bf500_only * law_complete_backtwo")


##### combine all independent variables
indep_hk <- c(char_indep_hk, dist_indep, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))

# estimation --------------------------------------------------------------

##### HK
# estimation
complete_basemodel_hk_backone <- feols(form_hk, se = "hetero" , data = control_period, fixef = c("months", "r1_id"))

# show results
etable(complete_basemodel_hk_backone, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


# export
esttex(complete_basemodel_hk_backone, file = file.path(outputPath, "regression/15122021/full/placebo_basemodel_hk_backone.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "placebo model hk control period back one")
esttex(complete_basemodel_hk_backone, file = file.path(outputPath, "regression/15122021/placebo_basemodel_hk_backone.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "placebo model hk control period back one", drop = "amr")
