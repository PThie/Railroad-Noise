##################################### HETEROGENEITY TEST 5 ############################################################################
# house type (NOT INCLUDED IN PAPER)

singlhous <- hk_affected %>% filter(single_house == 1)
semihous <- hk_affected %>% filter(semi_house == 1)
serialhous <- hk_affected %>% filter(serial_house == 1)

singlhous_basemodel_hk <- feols(form_hk, se = "hetero" , data = singlhous, fixef = c("months", "r1_id"))
semihous_basemodel_hk <- feols(form_hk, se = "hetero" , data = semihous, fixef = c("months", "r1_id"))
serialhous_basemodel_hk <- feols(form_hk, se = "hetero" , data = serialhous, fixef = c("months", "r1_id"))

etable(singlhous_basemodel_hk, semihous_basemodel_hk, serialhous_basemodel_hk, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


##################################### HETEROGENEITY TEST 8 ############################################################################
# house condition (NOT INCLUDED IN PAPER)

# category 1: Erstbezug (= 1), Neuwertig (= 3), Modernisiert (= 5)
# category 2: Erstbezug nach Sanierung (= 2), Saniert (= 4), Vollstaendig renoviert (= 6), Gepflegt (= 7)
# category 3: Renovierungsbedürftig (= 8), Nach Vereinbarung (= 9), Abbruchreif (= 10)
hk_affected <- hk_affected %>% mutate(condition_cat = case_when(objektzustand == 1 | objektzustand == 3 | objektzustand == 5 ~ 1,
                                                                objektzustand == 2 | objektzustand == 4 | objektzustand == 6 | objektzustand == 7 ~ 2,
                                                                objektzustand == 8 | objektzustand == 9 | objektzustand == 10 ~ 3))

# subset
condition_1 <- hk_affected %>% filter(condition_cat == 1)
condition_2 <- hk_affected %>% filter(condition_cat == 2)
condition_3 <- hk_affected %>% filter(condition_cat == 3)

# model

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

# estimation
condition_1_basemodel_hk <- feols(form_hk, se = "hetero" , data = condition_1, fixef = c("months", "r1_id"))
condition_2_basemodel_hk <- feols(form_hk, se = "hetero" , data = condition_2, fixef = c("months", "r1_id"))
condition_3_basemodel_hk <- feols(form_hk, se = "hetero" , data = condition_3, fixef = c("months", "r1_id"))

etable(condition_1_basemodel_hk, condition_2_basemodel_hk, condition_3_basemodel_hk, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


##################################### HETEROGENEITY TEST 9 ############################################################################
# distance to station (NOT INCLUDED IN PAPER)

dist_station_500 <- hk_affected %>% filter(distance_station <= 1.5)
dist_station_1500 <- hk_affected %>% filter(distance_station > 1.5 & distance_station <= 3)
dist_station_4000 <- hk_affected %>% filter(distance_station > 2 & distance_station <= 3)
dist_station_above_4000 <- hk_affected %>% filter(distance_station > 3)

# estimation
dist_station_500_est <- feols(form_hk, se = "hetero" , data = dist_station_500, fixef = c("months", "r1_id"))
dist_station_1500_est <- feols(form_hk, se = "hetero" , data = dist_station_1500, fixef = c("months", "r1_id"))
dist_station_above_4000_est <- feols(form_hk, se = "hetero" , data = dist_station_above_4000, fixef = c("months", "r1_id"))

etable(dist_station_500_est, dist_station_1500_est, dist_station_above_4000_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


# median estimation -------------------------------------------------------


dist_station_belmed <- hk_affected %>% filter(distance_station < median(distance_station, na.rm = TRUE))
dist_station_abomed <- hk_affected %>% filter(distance_station >= median(distance_station, na.rm = TRUE))

dist_station_belmed_est <- feols(form_hk, se = "hetero" , data = dist_station_belmed, fixef = c("months", "r1_id"))
dist_station_abomed_est <- feols(form_hk, se = "hetero" , data = dist_station_abomed, fixef = c("months", "r1_id"))

etable(dist_station_belmed_est, dist_station_abomed_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


##################################### HETEROGENEITY TEST 10 ############################################################################
# distance to airports (NOT INCLUDED IN PAPER)
quantile(hk_affected$distance_airport, probs = seq(0, 1, 0.1), na.rm = T)

dist_airport_1 <- hk_affected %>% filter(distance_airport <= as.numeric(quantile(distance_airport, probs = 0.1, na.rm = TRUE)))
dist_airport_2 <- hk_affected %>% filter(distance_airport > as.numeric(quantile(distance_airport, probs = 0.1, na.rm = TRUE)) & distance_airport <= as.numeric(quantile(distance_airport, probs = 0.2, na.rm = TRUE)))
dist_airport_3 <- hk_affected %>% filter(distance_airport > as.numeric(quantile(distance_airport, probs = 0.2, na.rm = TRUE)) & distance_airport <= as.numeric(quantile(distance_airport, probs = 0.3, na.rm = TRUE)))
dist_airport_4 <- hk_affected %>% filter(distance_airport > as.numeric(quantile(distance_airport, probs = 0.3, na.rm = TRUE)) & distance_airport <= as.numeric(quantile(distance_airport, probs = 0.4, na.rm = TRUE)))
dist_airport_5 <- hk_affected %>% filter(distance_airport > as.numeric(quantile(distance_airport, probs = 0.4, na.rm = TRUE)) & distance_airport <= median(distance_airport, na.rm = TRUE))
dist_airport_6 <- hk_affected %>% filter(distance_airport > median(distance_airport, na.rm = TRUE))


# estimation
dist_airport_1_est <- feols(form_hk, se = "hetero" , data = dist_airport_1, fixef = c("months", "r1_id"))
dist_airport_2_est <- feols(form_hk, se = "hetero" , data = dist_airport_2, fixef = c("months", "r1_id"))
dist_airport_3_est <- feols(form_hk, se = "hetero" , data = dist_airport_3, fixef = c("months", "r1_id"))
dist_airport_4_est <- feols(form_hk, se = "hetero" , data = dist_airport_4, fixef = c("months", "r1_id"))
dist_airport_5_est <- feols(form_hk, se = "hetero" , data = dist_airport_5, fixef = c("months", "r1_id"))
dist_airport_6_est <- feols(form_hk, se = "hetero" , data = dist_airport_6, fixef = c("months", "r1_id"))


etable(dist_airport_1_est, dist_airport_2_est, dist_airport_3_est, dist_airport_4_est, dist_airport_5_est, dist_airport_6_est,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


##################################### HETEROGENEITY TEST 11 ############################################################################
# distance to industry (NOT INCLUDED IN PAPER)
quantile(hk_affected$distance_industry, probs = seq(0, 1, 0.1), na.rm = T)

dist_industry_1 <- hk_affected %>% filter(distance_industry <= as.numeric(quantile(distance_industry, probs = 0.1, na.rm = TRUE)))
dist_industry_2 <- hk_affected %>% filter(distance_industry > as.numeric(quantile(distance_industry, probs = 0.1, na.rm = TRUE)) & distance_industry <= as.numeric(quantile(distance_industry, probs = 0.2, na.rm = TRUE)))
dist_industry_3 <- hk_affected %>% filter(distance_industry > as.numeric(quantile(distance_industry, probs = 0.2, na.rm = TRUE)) & distance_industry <= as.numeric(quantile(distance_industry, probs = 0.3, na.rm = TRUE)))
dist_industry_4 <- hk_affected %>% filter(distance_industry > as.numeric(quantile(distance_industry, probs = 0.3, na.rm = TRUE)) & distance_industry <= as.numeric(quantile(distance_industry, probs = 0.4, na.rm = TRUE)))
dist_industry_5 <- hk_affected %>% filter(distance_industry > as.numeric(quantile(distance_industry, probs = 0.4, na.rm = TRUE)) & distance_industry <= median(distance_industry, na.rm = TRUE))
dist_industry_6 <- hk_affected %>% filter(distance_industry > median(distance_industry, na.rm = TRUE))

# estimation
dist_industry_1_est <- feols(form_hk, se = "hetero" , data = dist_industry_1, fixef = c("months", "r1_id"))
dist_industry_2_est <- feols(form_hk, se = "hetero" , data = dist_industry_2, fixef = c("months", "r1_id"))
dist_industry_3_est <- feols(form_hk, se = "hetero" , data = dist_industry_3, fixef = c("months", "r1_id"))
dist_industry_4_est <- feols(form_hk, se = "hetero" , data = dist_industry_4, fixef = c("months", "r1_id"))
dist_industry_5_est <- feols(form_hk, se = "hetero" , data = dist_industry_5, fixef = c("months", "r1_id"))
dist_industry_6_est <- feols(form_hk, se = "hetero" , data = dist_industry_6, fixef = c("months", "r1_id"))

etable(dist_industry_1_est, dist_industry_2_est, dist_industry_3_est, 
       dist_industry_4_est, dist_industry_5_est, dist_industry_6_est,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

##################################### HETEROGENEITY TEST 11 ############################################################################
# distance to streets (NOT INCLUDED IN PAPER)
quantile(hk_affected$distance_streets, probs = seq(0, 1, 0.1), na.rm = T)


# interval estimation -----------------------------------------------------

# subset
dist_streets_1 <- hk_affected %>% filter(distance_streets <= as.numeric(quantile(distance_streets, probs = 0.1, na.rm = TRUE)))
dist_streets_2 <- hk_affected %>% filter(distance_streets > as.numeric(quantile(distance_streets, probs = 0.1, na.rm = TRUE)) & distance_streets <= as.numeric(quantile(distance_streets, probs = 0.2, na.rm = TRUE)))
dist_streets_3 <- hk_affected %>% filter(distance_streets > as.numeric(quantile(distance_streets, probs = 0.2, na.rm = TRUE)) & distance_streets <= as.numeric(quantile(distance_streets, probs = 0.3, na.rm = TRUE)))
dist_streets_4 <- hk_affected %>% filter(distance_streets > as.numeric(quantile(distance_streets, probs = 0.3, na.rm = TRUE)) & distance_streets <= as.numeric(quantile(distance_streets, probs = 0.4, na.rm = TRUE)))
dist_streets_5 <- hk_affected %>% filter(distance_streets > as.numeric(quantile(distance_streets, probs = 0.4, na.rm = TRUE)) & distance_streets <= median(distance_streets, na.rm = TRUE))
dist_streets_6 <- hk_affected %>% filter(distance_streets > median(distance_streets, na.rm = TRUE))



# estimation
dist_streets_1_est <- feols(form_hk, se = "hetero" , data = dist_streets_1, fixef = c("months", "r1_id"))
dist_streets_2_est <- feols(form_hk, se = "hetero" , data = dist_streets_2, fixef = c("months", "r1_id"))
dist_streets_3_est <- feols(form_hk, se = "hetero" , data = dist_streets_3, fixef = c("months", "r1_id"))
dist_streets_4_est <- feols(form_hk, se = "hetero" , data = dist_streets_4, fixef = c("months", "r1_id"))
dist_streets_5_est <- feols(form_hk, se = "hetero" , data = dist_streets_5, fixef = c("months", "r1_id"))
dist_streets_6_est <- feols(form_hk, se = "hetero" , data = dist_streets_6, fixef = c("months", "r1_id"))

etable(dist_streets_1_est, dist_streets_2_est, dist_streets_3_est, 
       dist_streets_4_est, dist_streets_5_est, dist_streets_6_est,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")
