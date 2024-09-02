############################################################
# Load Data                                                #
############################################################

hk <- readRDS(file.path(dataPath, "housing/Temp/hk_buffered.rds"))

############################################################
# preparation                                              #
############################################################

##### preparation function
prep_data <- function(data_prepared){
  # drop geometry
  data_prepared <- st_drop_geometry(data_prepared)

  # restrict the data set 
  # to municipalities that are crossed by tracks
  data_prepared <- data_prepared[data_prepared$affected_munic == 1, ]
  
  # months
  data_prepared$months <- as.factor(data_prepared$year_mon)
  
  # return
  return(data_prepared)
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
                      "etage" = "Floor", "balkon" = "Balcony", "einbaukueche" = "Built-in kitchen", "garten" = "Garden")


############################################################
# baseline estimation                                      #
############################################################

# baseline estimation 
# 500m as treated group 
# the rest in control group (restricted to municipalities with tracks)

# define variables and formula --------------------------------------------

##### dependent variable
dep_hk <- "ln_houseprice"


##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ",
           "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
           "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT", 
           "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
           "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter", "distance_smalcenter",
           "distance_airport", "distance_industry", "distance_streets")


##### distance buffers
#dist_indep <- c("bf500_only")


##### interactions (both event times included)
int_indep <- c("law_inprogress * bf500_only", "law_established * bf500_only")


##### combine all independent variables
indep_hk <- c(char_indep_hk, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                         paste(indep_hk, collapse = " + "),
                         sep = "~"))

# estimation --------------------------------------------------------------

##### HK
# estimation 
basemodel_hk <- feols(form_hk, se = "hetero", data = hk_affected, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_hk, file = file.path(outputPath, "regression/basemodel_hk.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "basemodel hk")

############################################################
# east and west germany                                    #
############################################################

# split the sample
# East: Brandenburg, Mecklenburg-Vorpommern, Sachsen, Sachsen-Anhalt und Thüringen
# Berlin constitutes a special case

east_houses <- hk_affected |>
  dplyr::filter(blid %in% c(12, 13, 14, 15, 16))

west_houses <- hk_affected |>
  dplyr::filter(!blid %in% c(12, 13, 14, 15, 16, 11))

# estimation

basemodel_hk_east <- feols(form_hk, se = "hetero", data = east_houses, fixef = c("months", "r1_id"))
basemodel_hk_west <- feols(form_hk, se = "hetero", data = west_houses, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk_east, basemodel_hk_west, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

esttex(basemodel_hk_east, basemodel_hk_west, file = file.path(outputPath, "regression/basemodel_hk_east_west.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "basemodel hk east west")



# NOT USED ----------------------------------------------------------------


############################################################
# money terms                                              #
############################################################


# setup -------------------------------------------------------------------

# observations dropped in regression (because of NAs)
obs_removed <- basemodel_hk$obsRemoved

# subset
hk_money <- hk_affected[-obs_removed, c("obid", "kaufpreis", "wohnflaeche", "ln_houseprice", "year_mon", "bf500_only", "law_established", "law_inprogress")]

# merge fitted values
hk_money$fitted <- basemodel_hk$fitted.values

# transform log back into prices
hk_money$fitted_kaufpreis <- exp(hk_money$fitted)

# calculate sq meter price
hk_money$price_sqmeter_fitted <- hk_money$fitted_kaufpreis / hk_money$wohnflaeche

# diff-in-diff: announcement effect ---------------------------------------

##### price per sq meter
# treated before and after law
treated_after <- hk_money %>% filter(law_inprogress == 1 & bf500_only == 1) %>% summarise(mean(price_sqmeter_fitted))
treated_before <- hk_money %>% filter(law_established == 0 & law_inprogress == 0 & bf500_only == 1) %>% summarise(mean(price_sqmeter_fitted))

# control before and after law
control_after <- hk_money %>% filter(law_inprogress == 1 & bf500_only == 0) %>% summarise(mean(price_sqmeter_fitted))
control_before <- hk_money %>% filter(law_established == 0 & law_inprogress == 0 & bf500_only == 0) %>% summarise(mean(price_sqmeter_fitted))

diffdiff_AE_sqprice <- round((treated_after-treated_before) - (control_after-control_before), digits = 2)
diffdiff_AE_sqprice


##### total price
treated_after <- hk_money %>% filter(law_inprogress == 1 & bf500_only == 1) %>% summarise(mean(fitted_kaufpreis))
treated_before <- hk_money %>% filter(law_established == 0 & law_inprogress == 0 & bf500_only == 1) %>% summarise(mean(fitted_kaufpreis))

# control before and after law
control_after <- hk_money %>% filter(law_inprogress == 1 & bf500_only == 0) %>% summarise(mean(fitted_kaufpreis))
control_before <- hk_money %>% filter(law_established == 0 & law_inprogress == 0 & bf500_only == 0) %>% summarise(mean(fitted_kaufpreis))

diffdiff_AE_price <- round((treated_after-treated_before) - (control_after-control_before), digits = 2)
diffdiff_AE_price

# diff-in-diff: treatment effect ------------------------------------------

##### price per sq meter
# treated before and after law
treated_after <- hk_money %>% filter(law_established == 1 & bf500_only == 1) %>% summarise(mean(price_sqmeter_fitted))
treated_before <- hk_money %>% filter(law_established == 0 & law_inprogress == 0 & bf500_only == 1) %>% summarise(mean(price_sqmeter_fitted))

# control before and after law
control_after <- hk_money %>% filter(law_established == 1 & bf500_only == 0) %>% summarise(mean(price_sqmeter_fitted))
control_before <- hk_money %>% filter(law_established == 0 & law_inprogress == 0 & bf500_only == 0) %>% summarise(mean(price_sqmeter_fitted))

diffdiff_TE_sqprice <- round((treated_after-treated_before) - (control_after-control_before), digits = 2)
diffdiff_TE_sqprice

##### total price
treated_after <- hk_money %>% filter(law_established == 1 & bf500_only == 1) %>% summarise(mean(fitted_kaufpreis))
treated_before <- hk_money %>% filter(law_established == 0 & law_inprogress == 0 & bf500_only == 1) %>% summarise(mean(fitted_kaufpreis))

# control before and after law
control_after <- hk_money %>% filter(law_established == 1 & bf500_only == 0) %>% summarise(mean(fitted_kaufpreis))
control_before <- hk_money %>% filter(law_established == 0 & law_inprogress == 0 & bf500_only == 0) %>% summarise(mean(fitted_kaufpreis))

diffdiff_TE_price <- round((treated_after-treated_before) - (control_after-control_before), digits = 2)
diffdiff_TE_price
