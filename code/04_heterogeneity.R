############################################################
# Description                                              #
############################################################



############################################################
# Load Data                                                #
############################################################

hk <- readRDS(file.path(dataPath, "housing/Temp/hk_buffered.rds"))

# grid data
microm <- haven::read_dta("M:/_FDZ/RWI-GEO/RWI-GEO-GRID/daten/Original/Stata16/microm_panel_05-19.dta")

# zuordnung
zuord <- fread(file.path(dataGebiete, "Zuordnung/_Gemeinde/2019_Grids_Municipality_Exact_unambiguous.csv"))

# shapes
krs <- st_read(file.path(dataGebiete, "Kreis/2019/VG250_KRS.shp"))

# load main tracks
main_tracks <- st_read(file.path(dataPath, "main_tracks/tracks5805.shp"))

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
                      "etage" = "Floor", "balkon" = "Balcony", "einbaukueche" = "Built-in kitchen", "garten" = "Garden", "keller" = "basement", "kellerUNBEKANNT" = "Basement (unknown)")



##################################### HETEROGENEITY TEST 1 ############################################################################


############################################################
# heterogeneity analysis: extended model                   #
############################################################

# split treated zones in 6 buffers

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
int_indep_ext <- c("bf50_only * law_inprogress", "bf50_only * law_established",
                   "bf100 * law_inprogress", "bf100 * law_established",
                   "bf250 * law_inprogress", "bf250 * law_established",
                   "bf500 * law_inprogress", "bf500 * law_established",
                   "bf750 * law_inprogress", "bf750 * law_established",
                   "bf1000 * law_inprogress", "bf1000 * law_established")


##### combine all independent variables
indep_hk_ext <- c(char_indep_hk, int_indep_ext)


##### define estimation formula
form_hk_ext <- as.formula(paste(dep_hk,
                                paste(indep_hk_ext, collapse = " + "),
                                sep = "~"))


# estimation --------------------------------------------------------------

##### HK
# estimation
extended_hk <- feols(form_hk_ext, se = "hetero" , data = hk_affected, fixef = c("months", "r1_id"))

# show results
etable(extended_hk, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(extended_hk, file = file.path(outputPath, "regression/hetero_extended_hk.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "extended model hk")


# plot --------------------------------------------------------------------

# show confidence interval
confint(extended_hk)

# get coefficients
coef <- as.data.frame(cbind(summary(extended_hk)$coefficients, summary(extended_hk)$se))
coef$vars <- rownames(coef)
names(coef) <- c("coef", "se", "vars")

# select only coefficients of interest
coef_interest <- coef[54:65, ]

# indicator for odd rows (announcement period = odd rows)
row_odd <- seq_len(nrow(coef_interest)) %% 2

# coefficients for in progress
coef_inprogress <- coef_interest[row_odd == 1, ]
coef_inprogress$buffer <- c("50", "100", "250", "500", "750", "1000")
coef_inprogress$group <- "inp"

# coefficients for law established
coef_established <- coef_interest[row_odd == 0, ]
coef_established$buffer <- c("50", "100", "250", "500", "750", "1000")
coef_established$group <- "est"

# combine again
coefs <- rbind(coef_inprogress, coef_established)

# confidence interval
conf = 1.65

##### plotting
extended_plot <- ggplot(data = coefs, 
       mapping = aes(x = factor(buffer, levels = c("50", "100", "250", "500", "750", "1000")), y = coef, col = factor(group, levels = c("inp", "est"))))+
  geom_point(mapping = aes(x = factor(buffer, levels = c("50", "100", "250", "500", "750", "1000")), y = coef), 
             position = position_dodge(width = 0.5), size = 2)+
  geom_pointrange(mapping = aes(x = buffer, ymin = (coef - (conf * se)), ymax = coef + (conf * se)),
                position = position_dodge(width = 0.5), size = 0.7, show.legend = FALSE)+
  scale_color_manual(values = c("inp" = "darkorange2", "est" = "royalblue4"),
                     labels = c("inp" = "LawPassed", "est" = "LawInForce"),
                     name = "")+
  geom_hline(yintercept = 0)+
  labs(x = "Distance to tracks (m)", y = "Estimate and 90% CI")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 13),
        legend.key = element_blank())

extended_plot
ggsave(plot = extended_plot, file.path(outputPath, "graphs/extended_model_plot.png"))


# -------------------------------------------------------------------------
# plot for UBA in German

extended_plot_german <- ggplot(data = coefs, 
                        mapping = aes(x = factor(buffer, levels = c("50", "100", "250", "500", "750", "1000")), y = coef*100, col = factor(group, levels = c("inp", "est"))))+
  geom_point(position = position_dodge(width = 0.5), size = 2)+
  geom_pointrange(mapping = aes(x = buffer, ymin = (coef - (conf * se))*100, ymax = (coef + (conf * se))*100),
                  position = position_dodge(width = 0.5), size = 0.7, show.legend = FALSE)+
  scale_color_manual(values = c("inp" = "darkorange2", "est" = "royalblue4"),
                     labels = c("inp" = "Verabschiedung", "est" = "Umsetzung"),
                     name = "")+
  geom_hline(yintercept = 0)+
  labs(x = "Entfernung Gleise (m)", y = "Effekt und 90% KI (in %)")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 13),
        legend.key = element_blank())

extended_plot_german
ggsave(plot = extended_plot_german, file.path(outputPath, "graphs/extended_model_plot_german.png"))


# test robustness of the extended model -----------------------------------

# with 3km restriction
extended_hk_3km <- feols(form_hk_ext, se = "hetero" , data = hk_affected3000, fixef = c("months", "r1_id"))

# without 500k cities
extended_hk_500k <- feols(form_hk_ext, se = "hetero" , data = hk_wo_bigcit, fixef = c("months", "r1_id"))

# without 100k cities
extended_hk_100k <- feols(form_hk_ext, se = "hetero" , data = rural, fixef = c("months", "r1_id"))


# show results
etable(extended_hk, extended_hk_3km, extended_hk_500k, extended_hk_100k, 
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3",
       headers = c("original ext.", "ext. & 3km", "ext. & 500k", "ext. & 100k"))

# export
# export
esttex(extended_hk, extended_hk_3km, extended_hk_500k, extended_hk_100k, file = file.path(outputPath, "regression/hetero_extended_hk_robustcheck.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "extended model hk robust",
       headers = c("original ext.", "ext. & 3km", "ext. & 500k", "ext. & 100k"))

Ù##################################### HETEROGENEITY TEST 2 ############################################################################

############################################################
# heterogeneity analysis: regional types                   #
############################################################

# regional types
hk_affected$regional_type <- as.numeric(hk_affected$regional_type)

regional_type1 <- hk_affected[hk_affected$regional_type == 1, ]
regional_type2 <- hk_affected[hk_affected$regional_type == 2, ]
regional_type3 <- hk_affected[hk_affected$regional_type == 3, ]
regional_type4 <- hk_affected[hk_affected$regional_type == 4, ]


# -------------------------------------------------------------------------

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



# -------------------------------------------------------------------------

baseline_region1 <- feols(form_hk, se = "hetero" , data = regional_type1, fixef = c("months", "r1_id"))
baseline_region2 <- feols(form_hk, se = "hetero" , data = regional_type2, fixef = c("months", "r1_id"))
baseline_region3 <- feols(form_hk, se = "hetero" , data = regional_type3, fixef = c("months", "r1_id"))
baseline_region4 <- feols(form_hk, se = "hetero" , data = regional_type4, fixef = c("months", "r1_id"))

# look at results
etable(baseline_region1, baseline_region2, baseline_region3, baseline_region4, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(baseline_region1, baseline_region2, baseline_region3, baseline_region4, file = file.path(outputPath, "regression/hetero_basemodel_hk_regtypes.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel hk regional types",
       headers = c("high_cent", "cent", "periph", "high_periph"))



##################################### HETEROGENEITY TEST 3 ############################################################################

# rich vs. poor

############################################################
# preparation                                              #
############################################################


# microm ------------------------------------------------------------------

# keep relevant variables
microm_prep <- microm[, c("r1_id", "r1_kkr_w_summe", "r1_mba_a_haushalt", "year")]

# keep only year 2019
microm_prep <- microm_prep[microm_prep$year == 2019, ]

# set NAs
microm_prep$r1_kkr_w_summe[microm_prep$r1_kkr_w_summe <= 0] <- NA
microm_prep$r1_mba_a_haushalt[microm_prep$r1_mba_a_haushalt <= 0] <- NA

# merge municipality info
zuord$share <- NULL
zuord$AGS <- as.character(zuord$AGS)
zuord$AGS <- ifelse(test = nchar(zuord$AGS) == 7,
                    yes = paste0("0",  zuord$AGS),
                    no = paste0(zuord$AGS))
zuord$AGS_krs <- substring(zuord$AGS, first = 1, last = 5)
microm_prep <- merge(microm_prep, zuord, by = "r1_id")

# drop 
microm_prep$year <- NULL

# calculate average household purchasing power by grid
microm_prep <- microm_prep %>% mutate(household_pp = r1_kkr_w_summe / r1_mba_a_haushalt)

# average household purchasing power by district
sum_microm_gem <- microm_prep %>% group_by(AGS) %>% summarise(mean_hh_pp_gem = mean(household_pp, na.rm = TRUE))
sum_microm_krs <- microm_prep %>% group_by(AGS_krs) %>% summarise(mean_hh_pp_krs = mean(household_pp, na.rm = TRUE))

# merge back
microm_prep <- merge(microm_prep, sum_microm_krs, by = "AGS_krs", all.x = TRUE)

# add difference of grid household pp to district household pp
microm_prep <- microm_prep %>% mutate(diff_pp = round(((household_pp - mean_hh_pp_krs) / mean_hh_pp_krs) * 100, digits = 3))

# define purchasing power groups using quintiles
quant_pp <- as.numeric(quantile(microm_prep$diff_pp, probs = seq(0, 1, 0.25), na.rm = TRUE))
quant_pp

# export group border
write.xlsx(data.frame(quant_pp), file.path(outputPath, "descriptives/pp_categories.xlsx"), rowNames = FALSE)

# define categorical variable
microm_prep <- microm_prep %>% mutate(purchasing_power_cat = case_when(diff_pp >= quant_pp[1] & diff_pp < quant_pp[2] ~ 1,
                                                                       diff_pp >= quant_pp[2] & diff_pp < quant_pp[3] ~ 2,
                                                                       diff_pp >= quant_pp[3] & diff_pp < quant_pp[4] ~ 3,
                                                                       diff_pp >= quant_pp[4] & diff_pp <= quant_pp[5] ~ 4))

# clean
microm_prep <- microm_prep[, c("r1_id", "purchasing_power_cat", "diff_pp")]


# setup -------------------------------------------------------------------

# merge
hk <- merge(hk, microm_prep, by = "r1_id", all.x = TRUE)

# apply function
hk_affected <- prep_data(hk)

# subset for different purchasing power groups
hk_pp1 <- hk_affected[hk_affected$purchasing_power_cat == 1, ]
hk_pp2 <- hk_affected[hk_affected$purchasing_power_cat == 2, ]
hk_pp3 <- hk_affected[hk_affected$purchasing_power_cat == 3, ]
hk_pp4 <- hk_affected[hk_affected$purchasing_power_cat == 4, ]

# check number of observations
num_obs <- function(df){
  x <- length(which(df$bf500_only == 1 & df$law_established == 1))
  y <- length(which(df$bf500_only == 1 & df$law_inprogress == 1))
  
  return(list(x, y))
}

lapply(list(hk_pp1, hk_pp2, hk_pp3, hk_pp4), num_obs)

############################################################
# estimation                                               #
############################################################


# subset estimation -------------------------------------------------------


# estimation
pp1_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_pp1, fixef = c("months", "r1_id"))
pp2_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_pp2, fixef = c("months", "r1_id"))
pp3_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_pp3, fixef = c("months", "r1_id"))
pp4_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_pp4, fixef = c("months", "r1_id"))


# show results
etable(pp1_basemodel_hk, pp2_basemodel_hk, pp3_basemodel_hk, pp4_basemodel_hk, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(pp1_basemodel_hk, pp2_basemodel_hk, pp3_basemodel_hk, pp4_basemodel_hk, file = file.path(outputPath, "regression/hetero_basemodel_hk_richpoor.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel hk rich vs poor",
       headers = c("low", "low-medium", "medium-high", "high"))


# median estimation -------------------------------------------------------

# split data
pp_below_median <- hk_affected %>% filter(diff_pp < median(microm_prep$diff_pp, na.rm = TRUE))
pp_above_median <- hk_affected %>% filter(diff_pp >= median(microm_prep$diff_pp, na.rm = TRUE))

# estimation
est_pp_below_med <- feols(form_hk, se = "hetero" , data = pp_below_median, fixef = c("months", "r1_id"))
est_pp_above_med <- feols(form_hk, se = "hetero" , data = pp_above_median, fixef = c("months", "r1_id"))

# show results
etable(est_pp_below_med, est_pp_above_med, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(est_pp_below_med, est_pp_above_med, file = file.path(outputPath, "regression/hetero_basemodel_hk_richpoor_median.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel hk rich vs poor", 
       headers = c("below_med", "above_med"))

############################################################
# plotting districts                                       #
############################################################

# transform projection
krs <- st_transform(krs, crs = 32632)

# add geometry
sum_microm_krs.sf <- merge(as.data.frame(sum_microm_krs), krs, by.x = "AGS_krs", by.y = "AGS", all.x = TRUE)
sum_microm_krs.sf <- st_set_geometry(sum_microm_krs.sf, sum_microm_krs.sf$geometry)
sum_microm_krs.sf <- st_transform(sum_microm_krs.sf$geometry, crs = 32632)

# bring to same projection
main_tracks <- st_transform(main_tracks, crs = 32632)

# plot
krs_pp_map <- tm_shape(sum_microm_krs.sf)+
  tm_polygons(col = "mean_hh_pp_krs",
              palette = "-magma",
              style = "cont",
              border.col = "black",
              title = "in Euro")+
  tm_shape(main_tracks)+
  tm_lines(lwd = 2, col = "green")+
  tm_layout(legend.outside = TRUE,
            legend.position = c(0.1, "center"),
            legend.text.size = 0.6,
            legend.title.size = 0.8)

krs_pp_map
tmap_save(krs_pp_map, file.path(outputPath, "graphs/district_purchasing_power.png"), width = 10, height = 13, units = "cm")

##################################### HETEROGENEITY TEST 4 ############################################################################
# settlement density

# as numeric
hk_affected$settlement_density <- as.numeric(hk_affected$settlement_density)

# define categories
breaks_settden <- as.numeric(quantile(hk_affected$settlement_density, probs = seq(0, 1, 0.25), na.rm = TRUE))
breaks_settden

# export group border
write.xlsx(data.frame(breaks_settden), file.path(outputPath, "descriptives/settlement_density_categories.xlsx"), rowNames = FALSE)

# assign housing objects 
hk_affected <- hk_affected %>% mutate(settden_cat = case_when(settlement_density >= breaks_settden[1] & settlement_density < breaks_settden[2] ~ 1,
                                                              settlement_density >= breaks_settden[2] & settlement_density < breaks_settden[3] ~ 2,
                                                              settlement_density >= breaks_settden[3] & settlement_density < breaks_settden[4] ~ 3,
                                                              settlement_density >= breaks_settden[4] & settlement_density <= breaks_settden[5] ~ 4))

# subset by group
hk_settden1 <- hk_affected %>% filter(settden_cat == 1)
hk_settden2 <- hk_affected %>% filter(settden_cat == 2)
hk_settden3 <- hk_affected %>% filter(settden_cat == 3)
hk_settden4 <- hk_affected %>% filter(settden_cat == 4)



############################################################
# estimation                                               #
############################################################

# subset estimation -------------------------------------------------------

settden1_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_settden1, fixef = c("months", "r1_id"))
settden2_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_settden2, fixef = c("months", "r1_id"))
settden3_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_settden3, fixef = c("months", "r1_id"))
settden4_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_settden4, fixef = c("months", "r1_id"))


# show results
etable(settden1_basemodel_hk, settden2_basemodel_hk, settden3_basemodel_hk, settden4_basemodel_hk, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(settden1_basemodel_hk, settden2_basemodel_hk, settden3_basemodel_hk, settden4_basemodel_hk, file = file.path(outputPath, "regression/hetero_basemodel_hk_settlement_density.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel hk settlement density",
       headers = c("low", "low-med", "med-high", "high"))

# median estimation -------------------------------------------------------

# split data
settden_below_median <- hk_affected %>% filter(settlement_density < median(settlement_density, na.rm = TRUE))
settden_above_median <- hk_affected %>% filter(settlement_density >= median(settlement_density, na.rm = TRUE))

# estimation
est_settden_below_med <- feols(form_hk, se = "hetero" , data = settden_below_median, fixef = c("months", "r1_id"))
est_settden_above_med <- feols(form_hk, se = "hetero" , data = settden_above_median, fixef = c("months", "r1_id"))

# show results
etable(est_settden_below_med, est_settden_above_med, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", headers = c("below", "above"))

# export
esttex(est_settden_below_med, est_settden_above_med, file = file.path(outputPath, "regression/hetero_basemodel_hk_settlement_density_median.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel hk settlement density", 
       headers = c("below", "above"))



##################################### HETEROGENEITY TEST 5 ############################################################################
# distances to other noise sources

##### model
##### dependent variable
dep_hk <- "ln_houseprice"


##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl",
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


# extreme cases -----------------------------------------------------------

####### estimation #######

# distances
dist_extreme <- as.data.frame(cbind(source = c("airport", "industry", "streets"), 
                                    first_quart = c(as.numeric(quantile(hk_affected$distance_airport, probs = 0.25, na.rm = TRUE)),
                                                    as.numeric(quantile(hk_affected$distance_industry, probs = 0.25, na.rm = TRUE)),
                                                    as.numeric(quantile(hk_affected$distance_streets, probs = 0.25, na.rm = TRUE))),
                                    second_quart = c(as.numeric(quantile(hk_affected$distance_airport, probs = 0.5, na.rm = TRUE)),
                                                     as.numeric(quantile(hk_affected$distance_industry, probs = 0.5, na.rm = TRUE)),
                                                     as.numeric(quantile(hk_affected$distance_streets, probs = 0.5, na.rm = TRUE)))))

# export distances
write.xlsx(dist_extreme, file.path(outputPath, "descriptives/distances_extreme.xlsx"), rowNames = FALSE)

# subset below 1st quartile
extreme_quarter <- hk_affected %>% filter(distance_airport < as.numeric(quantile(distance_airport, probs = 0.25, na.rm = TRUE)) &
                                            distance_industry < as.numeric(quantile(distance_industry, probs = 0.25, na.rm = TRUE)) &
                                            distance_streets < as.numeric(quantile(distance_streets, probs = 0.25, na.rm = TRUE)))

# subset up to 2nd quartile
extreme_median <- hk_affected %>% filter(distance_airport >= as.numeric(quantile(distance_airport, probs = 0.25, na.rm = TRUE)) &
                                           distance_airport <= as.numeric(quantile(distance_airport, probs = 0.5, na.rm = TRUE)) &
                                           distance_industry >= as.numeric(quantile(distance_industry, probs = 0.25, na.rm = TRUE)) &
                                           distance_industry <= as.numeric(quantile(distance_industry, probs = 0.5, na.rm = TRUE)) &
                                           distance_streets >= as.numeric(quantile(distance_streets, probs = 0.25, na.rm = TRUE)) &
                                           distance_streets <= as.numeric(quantile(distance_streets, probs = 0.5, na.rm = TRUE)))

# subset above median
extreme_top <- hk_affected %>% filter(distance_airport > as.numeric(quantile(distance_airport, probs = 0.5, na.rm = TRUE)) &
                                        distance_industry > as.numeric(quantile(distance_industry, probs = 0.5, na.rm = TRUE)) &
                                        distance_streets > as.numeric(quantile(distance_streets, probs = 0.5, na.rm = TRUE)))

##### estimation
extreme_est_quarter <- feols(form_hk, se = "hetero" , data = extreme_quarter, fixef = c("months", "r1_id"))
extreme_est_median <- feols(form_hk, se = "hetero" , data = extreme_median, fixef = c("months", "r1_id"))
extreme_est_top <- feols(form_hk, se = "hetero" , data = extreme_top, fixef = c("months", "r1_id"))

etable(extreme_est_quarter, extreme_est_median, extreme_est_top, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

##### export
esttex(extreme_est_quarter, extreme_est_median, extreme_est_top, 
       file = file.path(outputPath, "regression/hetero_basemodel_hk_extreme_noises.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel hk extreme noises", 
       headers = c("quarter", "median", "top"))

####### plotting #######

##### prepare
# extract data
coef_table_extreme <- rbind(as.data.frame(cbind(extreme_est_quarter$coeftable[39:40, 1:2], vars = c("law_passed", "law_inforce"), group = "first_quart")),
      as.data.frame(cbind(extreme_est_median$coeftable[39:40, 1:2], vars = c("law_passed", "law_inforce"), group = "second_quart")),
      as.data.frame(cbind(extreme_est_top$coeftable[39:40, 1:2], vars = c("law_passed", "law_inforce"), group = "top_quart")))

# rename columns and rows
rownames(coef_table_extreme) <- seq(1, nrow(coef_table_extreme), 1)
colnames(coef_table_extreme) <- c("coef", "se", "vars", "group")

# make numeric
coef_table_extreme$coef <- as.numeric(coef_table_extreme$coef)
coef_table_extreme$se <- as.numeric(coef_table_extreme$se)

# confidence interval
conf <- 1.65
coef_table_extreme <- coef_table_extreme %>% mutate(conf_min = coef - (se * conf),
                                conf_max = coef + (se * conf))

##### plot
noises_extreme_plot <- ggplot(data = coef_table_extreme, 
                          mapping = aes(x = factor(group, levels = c("first_quart", "second_quart", "top_quart")), y = coef, col = factor(vars, levels = c("law_passed", "law_inforce"))))+
  geom_point(size = 2, position = position_dodge(width = 0.5))+
  geom_pointrange(mapping = aes(ymin = conf_min, ymax = conf_max),
                  position = position_dodge(width = 0.5), size = 0.7, show.legend = FALSE)+
  scale_color_manual(values = c("law_passed" = "darkorange2",
                                "law_inforce" = "royalblue4"),
                     labels = c("law_passed" = "LawPassed",
                                "law_inforce" = "LawInForce"),
                     name = "")+
  geom_hline(yintercept = 0)+
  labs(x = "", y = "Coefficients and 90% CI")+
  scale_x_discrete(labels = c("first_quart" = "High \n exposure",
                              "second_quart" = "  Medium \n exposure",
                              "top_quart" = "Low \n exposure"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_text(size = 13, colour = "black"),
        axis.title = element_text(size = 14),
        legend.key = element_blank(),
        legend.text = element_text(size = 14.5))

noises_extreme_plot
ggsave(plot = noises_extreme_plot, file.path(outputPath, "graphs/noise_extreme.png"))


# median estimation -------------------------------------------------------
#### help function
coeftable <- function(model_below, model_above, source, conf){
  # extract info
  model_below_df <- as.data.frame(cbind(model_below$coeftable[39:40, 1:2], vars = c("law_passed", "law_inforce"), group = "below_med", source = source), row.names = c(1:2))
  model_above_df <- as.data.frame(cbind(model_above$coeftable[39:40, 1:2], vars = c("law_passed", "law_inforce"), group = "above_med", source = source), row.names = c(1:2))
  
  # combine
  model_df <- rbind(model_below_df, model_above_df)
  
  # rename
  names(model_df) <- c("coef", "se", "vars", "group", "source")
  
  # make numeric
  model_df$coef <- as.numeric(model_df$coef)
  model_df$se <- as.numeric(model_df$se)
  
  # calculate confidence interval
  model_df <- model_df %>% mutate(conf_min = coef - (se * conf),
                                  conf_max = coef + (se * conf))
  
  # return
  return(model_df)
}

# confidence interval
conf = 1.65

####### Airports #######

##### subset
dist_airport_belmed <- hk_affected %>% filter(distance_airport < median(distance_airport, na.rm = TRUE))
dist_airport_abomed <- hk_affected %>% filter(distance_airport >= median(distance_airport, na.rm = TRUE))

##### estimation
dist_airport_belmed_est <- feols(form_hk, se = "hetero" , data = dist_airport_belmed, fixef = c("months", "r1_id"))
dist_airport_abomed_est <- feols(form_hk, se = "hetero" , data = dist_airport_abomed, fixef = c("months", "r1_id"))

##### store coefficients
coef_air <- coeftable(model_below = dist_airport_belmed_est, model_above = dist_airport_abomed_est, source = "airport", conf = conf)

##### show estimation results
etable(dist_airport_belmed_est, dist_airport_abomed_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


####### Industry #######

##### subset industry
dist_industry_belmed <- hk_affected %>% filter(distance_industry < median(distance_industry, na.rm = TRUE))
dist_industry_abomed <- hk_affected %>% filter(distance_industry >= median(distance_industry, na.rm = TRUE))

##### estimation
dist_industry_belmed_est <- feols(form_hk, se = "hetero" , data = dist_industry_belmed, fixef = c("months", "r1_id"))
dist_industry_abomed_est <- feols(form_hk, se = "hetero" , data = dist_industry_abomed, fixef = c("months", "r1_id"))

##### store coefficients
coef_ind <- coeftable(model_below = dist_industry_belmed_est, model_above = dist_industry_abomed_est, source = "industry", conf = conf)

##### show estimation results
etable(dist_industry_belmed_est, dist_industry_abomed_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


####### Streets #######

##### subset streets
dist_streets_belmed <- hk_affected %>% filter(distance_streets < median(distance_streets, na.rm = TRUE))
dist_streets_abomed <- hk_affected %>% filter(distance_streets >= median(distance_streets, na.rm = TRUE))

##### estimation
dist_streets_belmed_est <- feols(form_hk, se = "hetero" , data = dist_streets_belmed, fixef = c("months", "r1_id"))
dist_streets_abomed_est <- feols(form_hk, se = "hetero" , data = dist_streets_abomed, fixef = c("months", "r1_id"))

##### store coefficients
coef_str <- coeftable(model_below = dist_streets_belmed_est, model_above = dist_streets_abomed_est, source = "streets", conf = conf)

##### show estimation results
etable(dist_streets_belmed_est, dist_streets_abomed_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

####### combined results #######
etable(dist_airport_belmed_est, dist_airport_abomed_est, 
       dist_industry_belmed_est, dist_industry_abomed_est, 
       dist_streets_belmed_est, dist_streets_abomed_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

# export
esttex(dist_airport_belmed_est, dist_airport_abomed_est, 
       dist_industry_belmed_est, dist_industry_abomed_est, 
       dist_streets_belmed_est, dist_streets_abomed_est, 
       file = file.path(outputPath, "regression/hetero_basemodel_hk_noises_belabov_median.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel noises belabov median", 
       headers = c("bel_air", "abo_air", "bel_ind", "abo_ind", "bel_str", "abo_str"))


####### Plotting #######

##### combine coefficient tables
coef_table <- rbind(coef_air, coef_ind, coef_str)


##### plot
noises_med_plot <- ggplot(data = coef_table, 
       mapping = aes(x = factor(vars, levels = c("law_passed", "law_inforce")), y = coef, shape = factor(source, levels = c("airport", "industry", "streets"))))+
  geom_point(position = position_dodge(width = 0.7), size = 2.3)+
  geom_pointrange(mapping = aes(ymin = conf_min, ymax = conf_max),
                position = position_dodge(width = 0.7), size = 0.6, show.legend = FALSE)+
  facet_wrap(~ factor(group, levels = c("below_med", "above_med")), scales = "fixed", labeller = as_labeller(c("below_med" = "Below median",
                                                                                                   "above_med" = "Above median"), ))+
  scale_shape_manual(values = c("airport" = 15, 
                                "industry" = 17,
                                "streets" = 19),
                     name = "Noise sources", labels = c("airport" = "Airports",
                                                        "industry" = "Ind. Plants",
                                                        "streets" = "Streets"),
                     guide = guide_legend(override.aes = list(size = 3)))+
  geom_hline(yintercept = 0)+
  labs(x = "", y = "Coefficients and 90% CI")+
  scale_x_discrete(labels = c("law_passed" = "LawPassed",
                              "law_inforce" = "LawInForce"),
                   expand = c(0.1, 0.1))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14.5))

noises_med_plot
ggsave(plot = noises_med_plot, file.path(outputPath, "graphs/noise_belabov_median.png"))



# distance estimation -----------------------------------------------------


#### help function
coeftable_dist <- function(model_high, model_med, model_low, source, conf){
  # extract info
  model_high_df <- as.data.frame(cbind(model_high$coeftable[39:40, 1:2], vars = c("law_passed", "law_inforce"), group = "high", source = source), row.names = c(1:2))
  model_med_df <- as.data.frame(cbind(model_med$coeftable[39:40, 1:2], vars = c("law_passed", "law_inforce"), group = "medium", source = source), row.names = c(1:2))
  model_low_df <- as.data.frame(cbind(model_low$coeftable[39:40, 1:2], vars = c("law_passed", "law_inforce"), group = "low", source = source), row.names = c(1:2))
  
  # combine
  model_df <- rbind(model_high_df, model_med_df, model_low_df)
  
  # rename
  names(model_df) <- c("coef", "se", "vars", "group", "source")
  
  # make numeric
  model_df$coef <- as.numeric(model_df$coef)
  model_df$se <- as.numeric(model_df$se)
  
  # calculate confidence interval
  model_df <- model_df %>% mutate(conf_min = coef - (se * conf),
                                  conf_max = coef + (se * conf))
  
  # return
  return(model_df)
}

####### Airports #######

##### subset
dist_airport_high <- hk_affected %>% filter(distance_airport < as.numeric(quantile(distance_airport, probs = 0.25, na.rm = TRUE)))
dist_airport_medium <- hk_affected %>% filter(distance_airport >= as.numeric(quantile(distance_airport, probs = 0.25, na.rm = TRUE)) &
                                                distance_airport <= median(distance_airport, na.rm = TRUE))
dist_airport_low <- hk_affected %>% filter(distance_airport > median(distance_airport, na.rm = TRUE))

##### estimation
dist_airport_high_est <- feols(form_hk, se = "hetero" , data = dist_airport_high, fixef = c("months", "r1_id"))
dist_airport_medium_est <- feols(form_hk, se = "hetero" , data = dist_airport_medium, fixef = c("months", "r1_id"))
dist_airport_low_est <- feols(form_hk, se = "hetero" , data = dist_airport_low, fixef = c("months", "r1_id"))

##### store coefficients
coef_air <- coeftable_dist(model_high = dist_airport_high_est, model_med = dist_airport_medium_est, model_low = dist_airport_low_est, source = "airport", conf = conf)

##### show estimation results
etable(dist_airport_high_est, dist_airport_medium_est, dist_airport_low_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

##### export
esttex(dist_airport_high_est, dist_airport_medium_est, dist_airport_low_est,
       file = file.path(outputPath, "regression/hetero_basemodel_hk_airport_dist.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel airport distance", 
       headers = c("high_exp", "med_exp", "low_exp"))


####### Industry #######

##### subset
dist_industry_high <- hk_affected %>% filter(distance_industry < as.numeric(quantile(distance_industry, probs = 0.25, na.rm = TRUE)))
dist_industry_medium <- hk_affected %>% filter(distance_industry >= as.numeric(quantile(distance_industry, probs = 0.25, na.rm = TRUE)) &
                                                 distance_industry <= median(distance_industry, na.rm = TRUE))
dist_industry_low <- hk_affected %>% filter(distance_industry > median(distance_industry, na.rm = TRUE))

##### estimation
dist_industry_high_est <- feols(form_hk, se = "hetero" , data = dist_industry_high, fixef = c("months", "r1_id"))
dist_industry_medium_est <- feols(form_hk, se = "hetero" , data = dist_industry_medium, fixef = c("months", "r1_id"))
dist_industry_low_est <- feols(form_hk, se = "hetero" , data = dist_industry_low, fixef = c("months", "r1_id"))

##### store coefficients
coef_ind <- coeftable_dist(model_high = dist_industry_high_est, model_med = dist_industry_medium_est, model_low = dist_industry_low_est, source = "industry", conf = conf)

##### show estimation results
etable(dist_industry_high_est, dist_industry_medium_est, dist_industry_low_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

##### export
esttex(dist_industry_high_est, dist_industry_medium_est, dist_industry_low_est,
       file = file.path(outputPath, "regression/hetero_basemodel_hk_industry_dist.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel industry distance", 
       headers = c("high_exp", "med_exp", "low_exp"))


####### Streets #######

##### subset
dist_streets_high <- hk_affected %>% filter(distance_streets < as.numeric(quantile(distance_streets, probs = 0.25, na.rm = TRUE)))
dist_streets_medium <- hk_affected %>% filter(distance_streets >= as.numeric(quantile(distance_streets, probs = 0.25, na.rm = TRUE)) &
                                                distance_streets <= median(distance_streets, na.rm = TRUE))
dist_streets_low <- hk_affected %>% filter(distance_streets > median(distance_streets, na.rm = TRUE))

##### estimation
dist_streets_high_est <- feols(form_hk, se = "hetero" , data = dist_streets_high, fixef = c("months", "r1_id"))
dist_streets_medium_est <- feols(form_hk, se = "hetero" , data = dist_streets_medium, fixef = c("months", "r1_id"))
dist_streets_low_est <- feols(form_hk, se = "hetero" , data = dist_streets_low, fixef = c("months", "r1_id"))

##### store coefficients
coef_str <- coeftable_dist(model_high = dist_streets_high_est, model_med = dist_streets_medium_est, model_low = dist_streets_low_est, source = "streets", conf = conf)

##### show estimation results
etable(dist_streets_high_est, dist_streets_medium_est, dist_streets_low_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

##### export
esttex(dist_streets_high_est, dist_streets_medium_est, dist_streets_low_est,
       file = file.path(outputPath, "regression/hetero_basemodel_hk_street_dist.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel streets distance", 
       headers = c("high_exp", "med_exp", "low_exp"))


##### combine coefficient tables
coef_table <- rbind(coef_air, coef_ind, coef_str)


##### plot
noises_dist_plot <- ggplot(data = coef_table, 
                          mapping = aes(x = factor(vars, levels = c("law_passed", "law_inforce")), y = coef, shape = factor(source, levels = c("airport", "industry", "streets"))))+
  geom_point(position = position_dodge(width = 0.9), size = 2.3)+
  geom_pointrange(mapping = aes(ymin = conf_min, ymax = conf_max),
                  position = position_dodge(width = 0.9), size = 0.6, show.legend = FALSE)+
  facet_wrap(~ factor(group, levels = c("high", "medium", "low")), scales = "fixed", labeller = as_labeller(c("high" = "High exposure",
                                                                                                              "medium" = "Medium exposure",
                                                                                                              "low" = "Low exposure")))+
  scale_shape_manual(values = c("airport" = 15, 
                                "industry" = 17,
                                "streets" = 19),
                     name = "Noise sources", labels = c("airport" = "Airports",
                                                        "industry" = "Ind. Plants",
                                                        "streets" = "Streets"),
                     guide = guide_legend(override.aes = list(size = 3)))+
  geom_hline(yintercept = 0)+
  labs(x = "", y = "Coefficients and 90% CI")+
  scale_x_discrete(labels = c("law_passed" = "LawPassed",
                              "law_inforce" = "LawInForce"),
                   expand = c(0.1, 0.1))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14.5))

noises_dist_plot
ggsave(plot = noises_dist_plot, file.path(outputPath, "graphs/noise_distances.png"), width = 8, height = 7)

