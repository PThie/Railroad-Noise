############################################################
# Description                                              #
############################################################

# performs different robustness test by rerunning regression under different settings
# Robustness tests:
  # 1) Change of control group (to 3000 meters)
  # 2) Change of regional FE (to grid level)
  # 3) Pre-trends
  # 4) Change of dependent variable to laufzeittage
  # 5) State time trend
  # 6) One treatment period (total effect)
  # 7) control period and middle (placebo)
  # 8) Exclusion of big cities 500k population
  # 9) Exclusion of big cities 100k population
  # 10) Drop mixed months
  # 11) Drop Neutral Zone
  # 12) LOOE States
  # 13) Alternative Railroads
  # 14) LOOE Corridors

############################################################
# Load Data                                                #
############################################################

hk <- readRDS(file.path(dataPath, "housing/Temp/hk_buffered.rds"))

# house data with alternative railroads
hk_alt_rail <- readRDS(file.path(dataPath, "housing/Temp/hk_buffered_alt_rail.rds"))

# alternative railroads
alt_rail <- st_read(file.path(dataPath, "umgebungslaerm/schiene/Basisdaten/Mrail_Source_17.shp"))
alt_rail <- st_transform(alt_rail, crs = 32632)

# freight train corridors
main_tracks <- st_read(file.path(dataPath, "main_tracks/tracks5805.shp"))
main_tracks <- st_transform(main_tracks, crs = 32632)

# state borders
bula <- st_read(file.path(dataGebiete, "Bundesland/2019/VG250_LAN.shp"))
bula <- st_transform(bula, crs = 32632)


############################################################
# preparation - general                                    #
############################################################

##### preparation function
prep_data <- function(data_prepared){
  # drop geometry
  data_prepared <- st_drop_geometry(data_prepared)

  # restrict the data set 
  # to municipalities that are crossed by tracks
  data_prepared <- data_prepared[data_prepared$affected_munic == 1, ]
  
  # as factor
  data_prepared$blid <- as.factor(data_prepared$blid)
  
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



##################################### ROBUSTNESS TEST 1 ############################################################################

############################################################
# preparation - different control group                    #
############################################################

##### description
# set the control group to 3000 meter and see whether this impacts the results


##### preparation function
prep_data_diff_cg <- function(data_prepared){
  # drop geometry
  data_prepared <- st_drop_geometry(data_prepared)

  # restrict the data set 
  # to municipalities that are crossed by tracks
  data_prepared <- data_prepared[data_prepared$bf50_only == 1 |
                                   data_prepared$bf100 == 1 |
                                   data_prepared$bf250 == 1 |
                                   data_prepared$bf500 == 1 |
                                   data_prepared$bf750 == 1 |
                                   data_prepared$bf1000 == 1 |
                                   data_prepared$bf2000 == 1 |
                                   data_prepared$bf3000 == 1, ]
  
  # as factor: states
  data_prepared$blid <- as.factor(data_prepared$blid)
  
  # as factor: months
  data_prepared$months <- as.factor(data_prepared$year_mon)
  
  # return
  return(data_prepared)
}


##### apply function
hk_affected3000 <- prep_data_diff_cg(hk)


# descriptives ------------------------------------------------------------
# to show that treated and control more similar than in the unrestricted case (beyond 3km)

# remove empty rows
# for some reason there are rows which are completely empty (only NAs) -> delete them
hk_affected_des <- hk_affected[rowSums(is.na(hk_affected)) != ncol(hk_affected), ]
hk_affected3000_des <- hk_affected3000[rowSums(is.na(hk_affected3000)) != ncol(hk_affected3000), ]

# summary function
sum_fun <- function(dataframe, dataframe_name){
  des <- dataframe %>% group_by(bf500_only) %>% summarise(mean_price = mean(kaufpreis, na.rm = TRUE),
                                                          mean_age = mean(alter, na.rm = TRUE),
                                                          mean_space = mean(wohnflaeche, na.rm = TRUE),
                                                          mean_plot = mean(grundstuecksflaeche, na.rm = TRUE),
                                                          mean_endowment = mean(ausstattung, na.rm = TRUE),
                                                          mean_rooms = mean(zimmeranzahl, na.rm = TRUE),
                                                          mean_station = mean(distance_station, na.rm = TRUE),
                                                          mean_largcenter = mean(distance_largcenter, na.rm = TRUE),
                                                          mean_streets = mean(distance_streets, na.rm = TRUE))
  
  # add group difference (control - treated)
  des <- data.frame(rbind(des, des[1,] - des[2,]))
  
  # rename groups
  des$bf500_only <- c(paste0("control_", dataframe_name), paste0("treated_", dataframe_name), paste0("difference_", dataframe_name))
  
  # return
  return(des)
}

# main descriptives
des_hk <- sum_fun(dataframe = hk_affected_des, dataframe_name = "normal")
des_hk3000 <- sum_fun(dataframe = hk_affected3000_des, dataframe_name = "3km")

# combine
des <- data.frame(rbind(des_hk, des_hk3000))

# export
write.xlsx(des, file.path(outputPath, "descriptives/summary_difference_3km_restriction.xlsx"), row.Names = FALSE)

############################################################
# different control group: repeat baseline estimation      #
############################################################

# baseline estimation 
# 500m as treated group 
# the rest in control group (restricted to 3000 meters)

# define variables and formula --------------------------------------------

##### dependent variable
dep_hk <- "ln_houseprice"

##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
                   "distance_airport", "distance_industry", "distance_streets")


##### interactions (both event times included)
int_indep <- c("bf500_only * law_inprogress", "bf500_only * law_established")


##### combine all independent variables
indep_hk <- c(char_indep_hk, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))



# estimation --------------------------------------------------------------

##### HK
# estimation
basemodel_hk_3000k <- feols(form_hk, se = "hetero" , data = hk_affected3000, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk_3000k, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_hk_3000k, file = file.path(outputPath, "regression/robust_basemodel_hk_3k.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk 3km control")


##################################### ROBUSTNESS TEST 2 ############################################################################

# implementing regional fixed effects on a zip-code level (instead of grid level)

############################################################
# different regional FE: repeat baseline estimation        #
############################################################

# estimation --------------------------------------------------------------
# same estimation as for the baseline (just change the regional FE)

##### HK
# estimation
basemodel_hk_regFE <- feols(form_hk, se = "hetero" , data = hk_affected, fixef = c("months", "plz"))

# show results
etable(basemodel_hk_regFE, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_hk_regFE, file = file.path(outputPath, "regression/robust_basemodel_hk_regFE.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk with regional FE on zip-code")


##################################### ROBUSTNESS TEST 3 ############################################################################

# pre-trend analysis
# split the control period into three periods


# function ----------------------------------------------------------------
# to generate a list of months per period

month_generator <- function(year){
  
  # define list of possible months
  x <- c(1:12)
  
  # initialise vector
  list_of_months <- c()
  
  # loop over possible months
  for (val in x){
    if (val <= 9){
      list_of_months[val] <- paste0(year, "-0", val)
    }
    else{
      list_of_months[val] <- paste0(year, "-", val)
    }
  }
  
  # return 
  list_of_months
}


# preparation -------------------------------------------------------------

# apply function for each year
lom_2013 <- month_generator(year = 2013)
lom_2014 <- month_generator(year = 2014)
lom_2015 <- month_generator(year = 2015)
lom_2016 <- month_generator(year = 2016)
lom_2017 <- month_generator(year = 2017)
lom_2018 <- month_generator(year = 2018)
lom_2019 <- month_generator(year = 2019)
lom_2020 <- month_generator(year = 2020)
lom_2021 <- month_generator(year = 2021)

# create list of months according to:
#     t-4: June 2013 to June 2014
#     t-3: July 2014 to June 2015
#     t-2: July 2015 to June 2016
#     t-1: July 2016 to June 2017
#     t: July 2017
#     t+1: August 2017 to November 2020
#     t+2: December 2020 to June 2021

lom_1314 <- c(lom_2013[6:12], lom_2014[1:6])
lom_1415 <- c(lom_2014[7:12], lom_2015[1:6])
lom_1516 <- c(lom_2015[7:12], lom_2016[1:6])
lom_1617 <- c(lom_2016[7:12], lom_2017[1:6])

lom_17 <- c(lom_2017[7])

lom_1718 <- c(lom_2017[8:12], lom_2018[1:7])
lom_1819 <- c(lom_2018[8:12], lom_2019[1:7])
lom_1920 <- c(lom_2019[8:12], lom_2020[1:7])
lom_2021_new <- c(lom_2020[12], lom_2021[1:6])

lom_1720 <- c(lom_2017[8:12], lom_2018, lom_2019, lom_2020[1:11])

# create variable for periods according to list of months
hk_affected$periods[hk_affected$year_mon %in% lom_1314] <- "t-4"
hk_affected$periods[hk_affected$year_mon %in% lom_1415] <- "t-3"
hk_affected$periods[hk_affected$year_mon %in% lom_1516] <- "t-2"
hk_affected$periods[hk_affected$year_mon %in% lom_1617] <- "t-1"

hk_affected$periods[hk_affected$year_mon %in% lom_17] <- "t"

hk_affected$periods[hk_affected$year_mon %in% lom_1720] <- "t+1"
hk_affected$periods[hk_affected$year_mon %in% lom_2021_new] <- "t+2"

# make "periods" as factor
hk_affected$periods <- as.factor(hk_affected$periods)



# estimation --------------------------------------------------------------

# model
basemodel_hk_pretrend <- feols(ln_houseprice ~ in_bau + alter + alter_squ + 
               wohnflaeche + wohnflaeche_squ + grundstuecksflaeche + grundstuecksflaeche_squ + 
               anzahletagen + anzahletagenUNBEKANNT + badezimmer + badezimmerUNBEKANNT + 
               as.factor(heizungsart) + heizungsartUNBEKANNT + as.factor(ausstattung) + 
               ausstattungUNBEKANNT + zimmeranzahl + as.factor(objektzustand) + 
               objektzustandUNBEKANNT + distance_station + distance_junction + 
               distance_largcenter + distance_medcenter + distance_smalcenter +
               distance_airport + distance_industry + distance_streets +
               bf500_only +
               i(periods, bf500_only, ref = "t"), se = "hetero" , data = hk_affected, fixef = c("months", "r1_id"))


# show results
etable(basemodel_hk_pretrend, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_hk_pretrend, file = file.path(outputPath, "regression/robust_basemodel_hk_pretrend.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk pre-trends")

# confidence interval -----------------------------------------------------

coefplot(basemodel_hk_pretrend, se = "hetero", drop = c("wohnflaeche", "grundstuecksflaeche", "anzahletagen", "badezimmer",
                                                        "in_bau", "alter", "ausstattung", "heizungsart", "objektzustand", "zimmeranzahl", "distance", "^bf500_only$"),
         ci_level = 0.9, dict = c("i(var = bf500_only, f = periods, ref = \"t\")t-1" = "t-1",
                                  "i(var = bf500_only, f = periods, ref = \"t\")t-2" = "t-2",
                                  "i(var = bf500_only, f = periods, ref = \"t\")t-3" = "t-3",
                                  "i(var = bf500_only, f = periods, ref = \"t\")t-4" = "t-4",
                                  "i(var = bf500_only, f = periods, ref = \"t\")t+1" = "t+1",
                                  "i(var = bf500_only, f = periods, ref = \"t\")t+2" = "t+2"),
         order = c("t-4", "t-3", "t-2", "t-1", "t+1", "t+2"),
         lwd = 1.5, pt.lwd = 2, grid = FALSE, main = "", lab.cex = 1.2, pt.cex = 1.5)

# export
# via "Export" function in plot window (size 488x618)

##### ggplot graph
# get coefficients
coef <- as.data.frame(cbind(summary(basemodel_hk_pretrend)$coefficients, summary(basemodel_hk_pretrend)$se))
coef$vars <- rownames(coef)
names(coef) <- c("coef", "se", "vars")

# select only coefficients of interest
coef_interest <- coef[49:54, ]

# change row names
rownames(coef_interest) <- seq(1:nrow(coef_interest))

coef_interest$period <- c("-1", "-2", "-3", "-4", 
                          "1", "2")

# confidence interval min and max (90% interval)
coef_interest <- coef_interest %>% mutate(conf_min = coef - (1.645 * se),
                                          conf_max = coef + (1.645 * se))

# reorder
coef_interest <- coef_interest[c(rev(1:4), 5:6),]
coef_interest$order_periods <- c(1:4, 6:7)

# add reference period (t)
reference <- as.data.frame(cbind(coef = c(coef_interest$coef, 0), order_periods = c(coef_interest$order_periods, 5)))
reference <- reference[order(reference$order_periods), ]

# plot
pretrends_plot <- ggplot(data = coef_interest, 
       mapping = aes(x = order_periods, y = coef))+
  geom_point(size = 2.4)+
  geom_pointrange(mapping = aes(x = order_periods, ymin = conf_min, ymax = conf_max),
                position = position_dodge(width = 0.4), size = 0.6)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 5)+
  geom_point(mapping = aes(x = 5, y = 0), size = 2.4)+
  labs(x = "", y = "Coefficients and 90% CI")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.key = element_blank())

pretrends_plot <- pretrends_plot+
  geom_line(data = reference, mapping = aes(y = coef, x = order_periods, group = 1), linetype = "dashed", size = 1)+
  scale_x_continuous(breaks = c(1:7),
                     labels = c("t-4", "t-3", "t-2", "t-1", "t", "t+1", "t+2"))
  
pretrends_plot
ggsave(plot = pretrends_plot, file.path(outputPath, "graphs/pretrends_plot.png"), height = 7, width = 8)




##################################### ROBUSTNESS TEST 4 ############################################################################

############################################################
# change dependent variable to laufzeittage                #
############################################################


##### dependent variable
dep_hk_hits <- "laufzeittage"

##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT", 
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter", "distance_smalcenter",
                   "distance_airport", "distance_industry", "distance_streets")


##### interactions (both event times included)
int_indep <- c("bf500_only * law_inprogress", "bf500_only * law_established")


##### combine all independent variables
indep_hk <- c(char_indep_hk, int_indep)


##### define estimation formula
form_hk_hits <- as.formula(paste(dep_hk_hits,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))



# estimation --------------------------------------------------------------

##### HK
# estimation
basemodel_hk_duration <- feols(form_hk_hits, se = "hetero" , data = hk_affected, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk_duration, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


# export
esttex(basemodel_hk_duration, file = file.path(outputPath, "regression/robust_basemodel_hk_duration.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk duration", drop = "blid")




##################################### ROBUSTNESS TEST 5 ############################################################################

############################################################
# add state time trend                                     #
############################################################

##### dependent variable
dep_hk <- "ln_houseprice"

##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT", 
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter", "distance_smalcenter",
                   "distance_airport", "distance_industry", "distance_streets", "i(blid, months)")


##### interactions (both event times included)
int_indep <- c("bf500_only * law_inprogress", "bf500_only * law_established")


##### combine all independent variables
indep_hk <- c(char_indep_hk, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))



# -------------------------------------------------------------------------


##### HK
# estimation
basemodel_hk_trend <- feols(form_hk, se = "hetero" , data = hk_affected, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk_trend, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", drop = "blid")

# export
esttex(basemodel_hk_trend, file = file.path(outputPath, "regression/robust_basemodel_hk_trend.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk trend", drop = "blid")


##################################### ROBUSTNESS TEST 6 ############################################################################

# combining both treatment periods into one

hk_affected$law_complete <- 0
hk_affected$law_complete[hk_affected$law_established == 1 | hk_affected$law_inprogress == 1] <- 1



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

##### interactions (both event times included)
int_indep <- c("bf500_only * law_complete")


##### combine all independent variables
indep_hk <- c(char_indep_hk, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))

# estimation --------------------------------------------------------------

##### HK
# estimation
complete_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_affected, fixef = c("months", "r1_id"))

# show results
etable(complete_basemodel_hk, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


# export
esttex(complete_basemodel_hk, file = file.path(outputPath, "regression/robust_basemodel_hk_complete.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk complete treatment period")



##################################### ROBUSTNESS TEST 7 ############################################################################

# restricting to control period 
# shifting treatment period to the middle


##### subset
control_period <- hk_affected[hk_affected$year_mon <= "2017-06", ]


##### treatment to the middle
control_period$law_complete_backtwo <- 0
control_period$law_complete_backtwo[control_period$year_mon >= "2015-07"] <- 1

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

##### interactions (both event times included)
int_indep <- c("bf500_only * law_complete_backtwo")


##### combine all independent variables
indep_hk <- c(char_indep_hk, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))

# estimation --------------------------------------------------------------

##### HK
# estimation
complete_basemodel_hk_backtwo <- feols(form_hk, se = "hetero" , data = control_period, fixef = c("months", "r1_id"))

# show results
etable(complete_basemodel_hk_backtwo, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


# export
esttex(complete_basemodel_hk_backtwo, file = file.path(outputPath, "regression/robust_basemodel_hk_controlperiod.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robust model hk control period")



##################################### ROBUSTNESS TEST 8 ############################################################################

############################################################
# big cities                                               #
############################################################
# exclude 15 largest cities because they might drive the effect (around 500.000 population)
# and there are more noise sources
# rerun baseline regression

# cities are: Berlin (11000000), Hamburg (02000000), München (09162000), Köln (05315000),
#       Frankfurt (06412000), Stuttgart (08111000), Düsseldorf (05111000), Leipzig (14713000),
#       Dortmund (05913000), Essen (05113000), Bremen (04011000), Dresden (14612000),
#       Hannover (03241001), Nürnberg (09564000), Duisburg (05112000)

##### function
exclude_bigseven <- function(df){
  # dummy for 15 largest cities
  bigseven_excluded <- df %>% mutate(bigseven = case_when(gid2019_gen == 11000000 | gid2019_gen == 2000000 |
                                                            gid2019_gen == 9162000 | gid2019_gen == 5315000 |
                                                            gid2019_gen == 6412000 | gid2019_gen == 8111000 |
                                                            gid2019_gen == 5111000 | gid2019_gen == 14713000 | 
                                                            gid2019_gen == 5913000 | gid2019_gen == 5113000 |
                                                            gid2019_gen == 4011000 | gid2019_gen == 14612000 |
                                                            gid2019_gen == 3241001 | gid2019_gen == 9564000 |
                                                            gid2019_gen == 5112000 ~ 1))
  
  bigseven_excluded$bigseven[is.na(bigseven_excluded$bigseven)] <- 0
  
  # restrict data by dropping Big 7
  bigseven_excluded <- bigseven_excluded[bigseven_excluded$bigseven == 0, ]
  
  # return
  return(bigseven_excluded)
}

with_bigseven <- function(df){
  # dummy for 15 largest cities
  bigseven_excluded <- df %>% mutate(bigseven = case_when(gid2019_gen == 11000000 | gid2019_gen == 2000000 |
                                                            gid2019_gen == 9162000 | gid2019_gen == 5315000 |
                                                            gid2019_gen == 6412000 | gid2019_gen == 8111000 |
                                                            gid2019_gen == 5111000 | gid2019_gen == 14713000 | 
                                                            gid2019_gen == 5913000 | gid2019_gen == 5113000 |
                                                            gid2019_gen == 4011000 | gid2019_gen == 14612000 |
                                                            gid2019_gen == 3241001 | gid2019_gen == 9564000 |
                                                            gid2019_gen == 5112000 ~ 1))
  
  bigseven_excluded$bigseven[is.na(bigseven_excluded$bigseven)] <- 0
  
  # restrict to Big 7
  bigseven_excluded <- bigseven_excluded[bigseven_excluded$bigseven == 1, ]
  
  # return
  return(bigseven_excluded)
}

##### apply function
hk_wo_bigcit <- exclude_bigseven(hk_affected)
hk_w_bigcit <- with_bigseven(hk_affected)

# rerun basemodel ---------------------------------------------------------


##### dependent variable
dep_hk <- "ln_houseprice"


##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter", "distance_smalcenter",
                   "distance_airport", "distance_industry", "distance_streets")


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
basemodel_hk_wobigcit <- feols(form_hk, se = "hetero" , data = hk_wo_bigcit, fixef = c("months", "r1_id"))
basemodel_hk_bigcit <- feols(form_hk, se = "hetero" , data = hk_w_bigcit, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk_wobigcit, basemodel_hk_bigcit, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_hk_wobigcit, file = file.path(outputPath, "regression/robust_basemodel_hk_wobigcit.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robust basemodel hk without big cities")
esttex(basemodel_hk_bigcit, file = file.path(outputPath, "regression/robust_basemodel_hk_bigcit.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robust basemodel hk with big cities")

##################################### ROBUSTNESS TEST 9 ############################################################################
# excluding largest cities (Großstädte) (100k)

gemeindetyp <- read.xlsx(file.path(dataPath, "gemeindetyp/gemeindetyp.xlsx"), sheet = 1)

# drop NAs
gemeindetyp <- gemeindetyp %>% filter(!is.na(Aggregat))

# clean
gemeindetyp <- gemeindetyp %>% select(Kennziffer, `Stadt-/Gemeindetyp`)
colnames(gemeindetyp) <- c("ags", "city_type")

# adjust AGS 
gemeindetyp$ags <- as.numeric(gemeindetyp$ags)

# merge to houses
hk_affected <- merge(hk_affected, gemeindetyp, by.x = "gid2019_gen", by.y = "ags", all.x = TRUE)


rural <- hk_affected %>% filter(city_type != 10)
metro <- hk_affected %>% filter(city_type == 10)

# estimation --------------------------------------------------------------

basemodel_hk_rural <- feols(form_hk, se = "hetero" , data = rural, fixef = c("months", "r1_id"))
basemodel_hk_metro <- feols(form_hk, se = "hetero" , data = metro, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk_rural, basemodel_hk_metro, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_hk_rural, file = file.path(outputPath, "regression/robust_basemodel_hk_rural.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robust basemodel hk without largest cities")
esttex(basemodel_hk_metro, file = file.path(outputPath, "regression/robust_basemodel_hk_metro.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robust basemodel hk with largest cities")

##################################### ROBUSTNESS TEST 10 ############################################################################
# exlude the months that could be partially treated and control

hk_affected_month_drop <- hk_affected %>% filter(year_mon != "2017-07" & year_mon != "2020-12") 

# define variables and formula --------------------------------------------

##### dependent variable
dep_hk <- "ln_houseprice"

##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
                   "distance_airport", "distance_industry", "distance_streets")


##### interactions (both event times included)
int_indep <- c("bf500_only * law_inprogress", "bf500_only * law_established")


##### combine all independent variables
indep_hk <- c(char_indep_hk, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))



# estimation --------------------------------------------------------------

##### HK
# estimation
basemodel_hk_mixedmonths <- feols(form_hk, se = "hetero" , data = hk_affected_month_drop, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk_mixedmonths, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_hk_mixedmonths, file = file.path(outputPath, "regression/robust_basemodel_hk_mixedmonths.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk mixed months")


##################################### ROBUSTNESS TEST 11 ############################################################################
# neutral zone

# remove objects between 500 and 1000 meters
hk_affected_nz <- hk_affected %>% filter(bf750 != 1 & bf1000 != 1)

##### HK
# estimation
basemodel_hk_neutralzone <- feols(form_hk, se = "hetero" , data = hk_affected_nz, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk_neutralzone, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_hk_neutralzone, file = file.path(outputPath, "regression/robust_basemodel_hk_neutralzone.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk neutral zone")




##################################### combined export ############################################################################

##### table 1
# show results
etable(basemodel_hk_3000k, basemodel_hk_wobigcit, basemodel_hk_rural, basemodel_hk_neutralzone, basemodel_hk_regFE, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", drop = "blid")

# export
esttex(basemodel_hk_3000k, basemodel_hk_wobigcit, basemodel_hk_rural, basemodel_hk_neutralzone, basemodel_hk_regFE, file = file.path(outputPath, "regression/robust_basemodel_hk_combined_table1.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk combined table 1", drop = "blid")


##### table 2
etable(basemodel_hk_trend, basemodel_hk_pretrend, complete_basemodel_hk_backtwo, complete_basemodel_hk, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", drop = "blid")

# export
esttex(basemodel_hk_trend, basemodel_hk_pretrend, complete_basemodel_hk_backtwo, complete_basemodel_hk, file = file.path(outputPath, "regression/robust_basemodel_hk_combined_table2.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk combined table 2", drop = "blid")



##################################### ROBUSTNESS TEST 12 ############################################################################
# leave one out estimation: states


# subset ------------------------------------------------------------------
# subset for states
# 1	Schleswig-Holstein
# 2	Freie und Hansestadt Hamburg
# 3	Niedersachsen
# 4	Freie Hansestadt Bremen
# 5	Nordrhein-Westfalen
# 6	Hessen
# 7	Rheinland-Pfalz
# 8	Baden-Württemberg
# 9	Freistaat Bayern
# 10	Saarland
# 11	Berlin
# 12	Brandenburg
# 13	Mecklenburg-Vorpommern
# 14	Freistaat Sachsen
# 15	Sachsen-Anhalt
# 16	Freistaat Thüringen

##### subset and leave one out
states_sh <- hk_affected %>% filter(blid != 1)
states_hh <- hk_affected %>% filter(blid != 2)
states_ni <- hk_affected %>% filter(blid != 3)
states_hb <- hk_affected %>% filter(blid != 4)
states_nw <- hk_affected %>% filter(blid != 5)
states_he <- hk_affected %>% filter(blid != 6)
states_rp <- hk_affected %>% filter(blid != 7)
states_bw <- hk_affected %>% filter(blid != 8)
states_by <- hk_affected %>% filter(blid != 9)
states_sl <- hk_affected %>% filter(blid != 10)
states_be <- hk_affected %>% filter(blid != 11)
states_bb <- hk_affected %>% filter(blid != 12)
states_mv <- hk_affected %>% filter(blid != 13)
states_sn <- hk_affected %>% filter(blid != 14)
states_st <- hk_affected %>% filter(blid != 15)
states_th <- hk_affected %>% filter(blid != 16)

##### make list
states_subsets <- list(states_sh, states_hh, states_ni, states_hb, states_nw, states_he, states_rp, states_bw,
                       states_by, states_sl, states_be, states_bb, states_mv, states_sn, states_st, states_th)

# rename list elements
names(states_subsets) <- c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW",
                           "BY", "SL", "BE", "BB", "MV", "SN", "ST", "TH")

##### formula
##### dependent variable
dep_hk <- "ln_houseprice"


##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter", "distance_smalcenter",
                   "distance_airport", "distance_industry", "distance_streets")


##### interactions (both event times included)
int_indep <- c("law_inprogress * bf500_only", "law_established * bf500_only")


##### combine all independent variables
indep_hk <- c(char_indep_hk, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))


# estimation --------------------------------------------------------------

# estimation function
est_fm <- function(df, fm){
  feols(fml = fm, data = df, se = "hetero", fixef = c("months", "r1_id"))
}

# estimate
looe_states <- lapply(states_subsets, est_fm, form_hk)

etable(looe_states, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(looe_states, file = file.path(outputPath, "regression/robust_basemodel_hk_looe_states.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk LOOE states", drop = "blid")


##################################### ROBUSTNESS TEST 13 ############################################################################


# -------------------------------------------------------------------------
##### preparation function
prep_data <- function(data_prepared){

  # restrict the data set 
  # to municipalities that are crossed by tracks
  data_prepared <- data_prepared[data_prepared$affected_munic == 1, ]
  
  # construct dummy for the months the law is in force (but not punishable)
  data_prepared$law_inprogress <- ifelse(test = data_prepared$year_mon >= "2017-07" & data_prepared$year_mon <= "2020-11",
                                         yes = 1,
                                         no = 0)
  
  # construct dummy for the months the law is in force (i.e ban is in force)
  data_prepared$law_established <- ifelse(test = data_prepared$year_mon >= "2020-12",
                                          yes = 1,
                                          no = 0)
  
  # make months factor
  data_prepared$months <- as.factor(data_prepared$year_mon)
  
  # return
  return(data_prepared)
}


##### apply function
hk_alt_rail_affected <- prep_data(hk_alt_rail)

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
basemodel_hk <- feols(form_hk, se = "hetero", data = hk_alt_rail_affected, fixef = c("months", "r1_id"))

# show results
etable(basemodel_hk, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_hk, file = file.path(outputPath, "regression/robust_hk_alt_rail.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robust with alternative railroads")


# plot railrods -----------------------------------------------------------

##### plot
# get color
library(MetBrewer)
color <- met.brewer(name = "Greek", n = 5, type = "discrete")

alt_rail_map <-   tm_shape(bula)+
  tm_borders(col = "gray70")+
  tm_shape(alt_rail)+
  tm_lines(col = color[3], lwd = 3)+
  tm_shape(main_tracks)+
  tm_lines(col = "black", lwd = 2)+
  tm_add_legend(type = "line",
                labels = c("Freight train corridors", "Main railroads"),
                col = c("Freight train corridors" = "black",
                        "Main railroads" = color[3]),
                lwd = 3)+
  tm_layout(legend.position = c("right", 0.915),
            legend.frame = TRUE)

alt_rail_map
tmap_save(alt_rail_map, file.path(outputPath, "graphs/alt_railroads.png"), width = 10, height = 13, units = "cm")


##################################### ROBUSTNESS TEST 14 ############################################################################
# leave out estimation: corridors

##### subset and leave one out
corridors_scanmed <- hk_affected %>% filter(closest_corridor != "scanmed")
corridors_orient <- hk_affected %>% filter(closest_corridor != "orient")
corridors_northsea <- hk_affected %>% filter(closest_corridor != "northsea")
corridors_rine <- hk_affected %>% filter(closest_corridor != "rine")
corridors_danube <- hk_affected %>% filter(closest_corridor != "danube")
corridors_atlantic <- hk_affected %>% filter(closest_corridor != "atlantic")

##### make list
corridors_subsets <- list(corridors_scanmed, corridors_orient, corridors_northsea, 
                          corridors_rine, corridors_danube, corridors_atlantic)
names(corridors_subsets) <- c("wo_scanmed", "wo_orient", "wo_northsea", "wo_rine", "wo_danube", "wo_atlantic")

# estimation --------------------------------------------------------------

# estimation function
est_fm <- function(df, fm){
  feols(fml = fm, data = df, se = "hetero", fixef = c("months", "r1_id"))
}

# estimate
looe_corridors <- lapply(corridors_subsets, est_fm, form_hk)

# show results
etable(looe_corridors, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(looe_corridors, file = file.path(outputPath, "regression/robust_basemodel_hk_looe_corridors.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk LOOE corridors", drop = "blid",
       headers = c("wo_scanmed", "wo_orient", "wo_northsea", "wo_rine", "wo_danube", "wo_atlantic"))



# plot corridors ----------------------------------------------------------

pal <- RColorBrewer::brewer.pal(n = 6, name = "Dark2")

plot_corr <- tm_shape(bula)+
  tm_borders(col = "gray70")+
  tm_shape(main_tracks)+
  tm_lines(col = "corridor", 
           title.col = "Corridor",
           lwd = 3,
           palette = pal,
           labels = c("atlantic" = "Atlantic",
                      "danube" = "Rhine-Danube",
                      "northsea" = "North Sea-Baltic",
                      "orient" = "Orient/East-Med",
                      "rine" = "Rhine-Alpine",
                      "scanmed" = "ScanMed"))+
  tm_layout(legend.outside = TRUE,
            legend.position = c(0.1, "center"),
            legend.text.size = 0.8,
            legend.title.size = 1.1)

plot_corr
tmap_save(plot_corr, file.path(outputPath, "graphs/tracks_corridors.png"))  


##################################### ROBUSTNESS TEST 15 ############################################################################


# define variables and formula --------------------------------------------

##### dependent variable
dep_hk <- "ln_houseprice"

##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
                   "distance_airport", "distance_industry", "distance_streets", "distance_noise_barrier")


##### interactions (both event times included)
int_indep <- c("bf500_only * law_inprogress", "bf500_only * law_established")


##### combine all independent variables
indep_hk <- c(char_indep_hk, int_indep)


##### define estimation formula
form_hk <- as.formula(paste(dep_hk,
                            paste(indep_hk, collapse = " + "),
                            sep = "~"))



# estimation --------------------------------------------------------------

##### HK
# estimation
basemodel_noisebar <- feols(form_hk, se = "hetero" , data = hk_affected, fixef = c("months", "r1_id"))

# show results
etable(basemodel_noisebar, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(basemodel_noisebar, file = file.path(outputPath, "regression/robust_basemodel_hk_noisebar.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "robustness basemodel hk noisebar")
