library(multcomp)

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
tablabel_objchar <- c(
    "alter" = "Age", "alter_squ" = "Age$^2$", "wohnflaeche" = "Living space", "wohnflaeche_squ" = "Living space$^2$", "grundstuecksflaeche" = "Plot area",
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
char_indep_hk <- c(
    "in_bau", "alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ",
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
form_hk <- as.formula(
    paste(dep_hk,
    paste(indep_hk, collapse = " + "),
    sep = "~")
)

# estimation --------------------------------------------------------------

##### HK
# estimation
basemodel_hk <- feols(form_hk, se = "hetero", data = hk_affected, fixef = c("months", "r1_id"))

# show results
etable(
    basemodel_hk,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r6",
    coefstat = "confint"
)

# export
esttex(
    basemodel_hk, file = file.path(outputPath, "regression/disputation/basemodel_hk_CI.tex"), replace = TRUE, digits = "r3", dict = tablabel_objchar,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    title = "basemodel hk",
    coefstat = "confint",
    se = "hetero"
)

#--------------------------------------------------
# testing whether the noise effect (buffer500) is off-set by the different RNPA
# periods
# H0: beta(bf500_only) + beta(law_inprogress:bf500_only) = 0 means that both
# coefficients are equal to zero

# test for adoption period
noise_and_adoption <- multcomp::glht(
    basemodel_hk,
    linfct = c(
        "bf500_only + law_inprogress:bf500_only = 0"
    )
)

# test for actual implementation period
noise_and_implementation <- multcomp::glht(
    basemodel_hk,
    linfct = c(
        "bf500_only + bf500_only:law_established = 0"
    )
)

# extract the summary of the test and add confidence intervals
extract_test <- function(test_result = NA) {
    # extract the test results
    test <- summary(test_result)$test

    # extract the confidence intervals
    confint <- confint(test_result)$confint

    # construct data frame
    dta <- as.data.frame(
        cbind(
            coef = test$coefficients,
            se = test$sigma,
            p_value = test$pvalues,
            ci_lower = confint[1, "lwr"],
            ci_upper = confint[1, "upr"]
        )
    )

    # add test specification
    dta$test <- rownames(dta)
    rownames(dta) <- seq(1, nrow(dta))

    # move test to the first column
    dta <- dta |>
        dplyr::relocate(test)

    return(dta)
}


test_results_adoption <- extract_test(noise_and_adoption)
test_results_implementation <- extract_test(noise_and_implementation)

# combine both tests
test_results <- rbind(
    test_results_adoption,
    test_results_implementation
)

# export
openxlsx::write.xlsx(
    test_results,
    file.path(
        outputPath,
        "regression",
        "disputation",
        "joined_significance_tests.xlsx"
    ),
    rowNames = FALSE
)
