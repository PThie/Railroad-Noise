############################################################
# Description                                              #
############################################################



############################################################
# Load Data                                                #
############################################################

hk <- readRDS(file.path(dataPath, "housing/temp_hk_buffered.rds"))
microm <- haven::read_dta("M:/_FDZ/RWI-GEO/RWI-GEO-GRID/daten/Original/Stata16/microm_panel_05-19.dta")
zuord <- fread("M:/_FDZ/interne Daten/Gebietseinheit/Zuordnung/_Gemeinde/2019_Grids_Municipality_Exact_unambiguous.csv")

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
microm_prep <- merge(microm_prep, zuord, by = "r1_id")

# drop 
microm_prep$share <- NULL
microm_prep$year <- NULL

# calculate average household purchasing power by grid
microm_prep <- microm_prep %>% mutate(household_pp = r1_kkr_w_summe / r1_mba_a_haushalt)

# # generate district AGS
# microm_prep$AGS_str <- as.character(microm_prep$AGS)
# microm_prep$AGS_str <- ifelse(nchar(microm_prep$AGS_str) == 7,
#                               yes = paste0("0", microm_prep$AGS_str),
#                               no = paste0(microm_prep$AGS_str))
# microm_prep$kid2019_gen <- substr(microm_prep$AGS_str, start = 1, stop = 5)
# microm_prep$kid2019_gen <- as.numeric(microm_prep$kid2019_gen)
# microm_prep$AGS_str <- NULL

# average household purchasing power by municipality
sum_microm_gem <- microm_prep %>% group_by(AGS) %>% summarise(mean_hh_pp_gem = mean(household_pp, na.rm = TRUE))

# average household purchasing power by district
#sum_microm_krs <- microm_prep %>% group_by(kid2019_gen) %>% summarise(mean_hh_pp_krs =)

# merge back
microm_prep <- merge(microm_prep, sum_microm_gem, by = "AGS", all.x = TRUE)

# add difference of grid household pp to municipality household pp
microm_prep <- microm_prep %>% mutate(diff_pp = round(((household_pp - mean_hh_pp_gem) / mean_hh_pp_gem) * 100, digits = 3))

# define purchasing power groups using quintiles
quant_pp <- as.numeric(quantile(microm_prep$diff_pp, probs = seq(0, 1, 0.25), na.rm = TRUE))
quant_pp

microm_prep <- microm_prep %>% mutate(purchasing_power_cat = case_when(diff_pp >= quant_pp[1] & diff_pp < quant_pp[2] ~ 1,
                                                                       diff_pp >= quant_pp[2] & diff_pp < quant_pp[3] ~ 2,
                                                                       diff_pp >= quant_pp[3] & diff_pp < quant_pp[4] ~ 3,
                                                                       diff_pp >= quant_pp[4] & diff_pp <= quant_pp[5] ~ 4))

#clean
microm_prep <- microm_prep[, c("r1_id", "purchasing_power_cat")]







############### TEST

hk <- merge(hk, microm_prep, by = "r1_id", all.x = TRUE)


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



# subset for different purchasing power groups

hk_pp1 <- hk_affected[hk_affected$purchasing_power_cat == 1, ]
hk_pp2 <- hk_affected[hk_affected$purchasing_power_cat == 2, ]
hk_pp3 <- hk_affected[hk_affected$purchasing_power_cat == 3, ]
hk_pp4 <- hk_affected[hk_affected$purchasing_power_cat == 4, ]

num_obs <- function(df){
  x <- length(which(df$bf500_only == 1 & df$law_established == 1))
  y <- length(which(df$bf500_only == 1 & df$law_inprogress == 1))
  
  return(list(x, y))
}

lapply(list(hk_pp1, hk_pp2, hk_pp3, hk_pp4), num_obs)


##### dependent variable
dep_hk <- "ln_houseprice"


##### object characteristics
char_indep_hk <- c("in_bau", "alter", "alter_squ", "alterUNBEKANNT","wohnflaeche", "wohnflaeche_squ",
                   "grundstuecksflaeche", "grundstuecksflaeche_squ", "anzahletagen", "anzahletagenUNBEKANNT",
                   "badezimmer", "badezimmerUNBEKANNT", "as.factor(heizungsart)", "heizungsartUNBEKANNT", "keller", "kellerUNBEKANNT",
                   "as.factor(ausstattung)", "ausstattungUNBEKANNT", "zimmeranzahl", "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                   "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter",
                   "distance_airport", "distance_industry", "i(blid, year_mon)")


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
pp1_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_pp1, fixef = c("months", "r1_id"))
pp2_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_pp2, fixef = c("months", "r1_id"))
pp3_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_pp3, fixef = c("months", "r1_id"))
pp4_basemodel_hk <- feols(form_hk, se = "hetero" , data = hk_pp4, fixef = c("months", "r1_id"))


# show results
etable(pp1_basemodel_hk, pp2_basemodel_hk, pp3_basemodel_hk, pp4_basemodel_hk, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", drop = "blid")

# export
esttex(pp1_basemodel_hk, pp2_basemodel_hk, pp3_basemodel_hk, pp4_basemodel_hk, file = file.path(outputPath, "regression/full/hetero_basemodel_hk_richpoor.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel hk rich vs poor")
esttex(pp1_basemodel_hk, pp2_basemodel_hk, pp3_basemodel_hk, pp4_basemodel_hk, file = file.path(outputPath, "regression/hetero_basemodel_hk_richpoor.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
       signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), title = "heterogeneity basemodel hk rich vs poor", drop = "blid")
