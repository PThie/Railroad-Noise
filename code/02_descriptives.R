############################################################
# Load Data                                                #
############################################################

hk <- readRDS(file.path(dataPath, "housing/Temp/hk_buffered.rds"))

# regional centers
regional_center <- readRDS(file.path(dataPath, "raumzentren/raumzentren_nach_gemeinde_prep.rds"))

# regional types
regional_types <- read.fst(file.path(dataPath, "raumtyp/raumtyp_siedlungsdicht_nach_gemeinde_prep.fst"))

# municipality information
gem <- st_read(file.path(dataGebiete, "Gemeinde/2019/VG250_GEM.shp"))
gem <- st_transform(gem, crs = 32632)

gem17 <- st_read(file.path(dataGebiete, "Gemeinde/2017/VG250_GEM.shp"))
gem17 <- st_transform(gem17, crs = 32632)

# freight corridors
main_tracks <- st_read(file.path(dataPath, "main_tracks/tracks5805.shp"))
main_tracks <- st_transform(main_tracks, crs = 32632)

############################################################
# preparation                                              #
############################################################

##### preparation function
prep_data <- function(data_prepared){
  # drop geometry
  data_prepared <- st_drop_geometry(data_prepared)
  
  ##### law implementation process (for descriptives by groups)
  # create variable
  data_prepared$law_implementation <- 0
  
  # law implemented
  data_prepared$law_implementation[data_prepared$year_mon >= "2017-07" & data_prepared$year_mon <= "2020-11"] <- 1
  
  # law in force
  data_prepared$law_implementation[data_prepared$year_mon >= "2020-12"] <- 2
  
  
  ##### time variables
  # add plot date
  data_prepared$plot_date <- as.yearmon(data_prepared$year_mon)
  
  # restrict the data set 
  # to municipalities that are crossed by tracks
  data_prepared <- data_prepared[data_prepared$affected_munic == 1, ]
  
  # return
  return(data_prepared)
}


##### apply function
hk_affected <- prep_data(hk)


# remove empty rows -------------------------------------------------------
# for some reason there are rows which are completely empty (only NAs) -> delete them

hk_affected <- hk_affected[rowSums(is.na(hk_affected)) != ncol(hk_affected), ]


############################################################
# General descriptives                                     #
############################################################


# overall descriptives HK --------------------------------------------------

# define the variables used in regression and some additional variables
hk_main_varlist <- c("ln_houseprice", "kaufpreis", "price_sqmeter", "in_bau", "alter", "wohnflaeche", "grundstuecksflaeche",
                 "badezimmer", "heizungsart", "ausstattung", "zimmeranzahl", "anzahletagen", "keller", "objektzustand", 
                 "distance_station", "distance_junction", "distance_largcenter", "distance_medcenter", "distance_smalcenter",
                 "distance_airport", "distance_industry", "distance_streets",
                 "year_mon", "ajahr", "bf50_only", "bf100", "bf250", "bf500", "bf750", "bf1000", "bf500_only", "law_implementation")

hk_main_var <- hk_affected[, hk_main_varlist]


# main descriptives
des_hk <- round(describe(hk_main_var, fast = TRUE, quant = c(0.25, 0.75), ranges = FALSE, na.rm = TRUE), digits = 3)

# drop year_mon and rings
des_hk <- des_hk[1:22, ]

# export the results
write.xlsx(des_hk, file = file.path(outputPath, "descriptives/descriptives_hk_overall.xlsx"), sheetName = "House Purchases")


############################################################
# descriptives by buffer                                   #
############################################################

# calculates the descriptive summary for each buffer ring 
# for each period of the law implementation process (no law, law implemented and law in force)

# function ----------------------------------------------------------------

ringtime_descriptives <- function(df){
  # descriptive by group 
  des <- describeBy(df, group = df$law_implementation, mat = TRUE, digits = 3)
  
  # drop rows that are not needed
  des <- des[1:66, ]
  
  # drop unneeded descriptive statistics
  des$trimmed <- NULL
  des$mad <- NULL
  des$range <- NULL
  des$skew <- NULL
  des$kurtosis <- NULL
  des$se <- NULL
  des$item <- NULL
  
  # add rownames as variable names
  des$variable <- rownames(des)
  
  # return
  return(des)
}

# descriptives by ring and time -------------------------------------------

##### subset
# subset for the different rings
hk_main_var_bf50only <- subset(hk_main_var, hk_main_var$bf50_only == 1)
hk_main_var_bf100 <- subset(hk_main_var, hk_main_var$bf100 == 1)
hk_main_var_bf250 <- subset(hk_main_var, hk_main_var$bf250 == 1)
hk_main_var_bf500 <- subset(hk_main_var, hk_main_var$bf500 == 1)
hk_main_var_bf750 <- subset(hk_main_var, hk_main_var$bf750 == 1)
hk_main_var_bf1000 <- subset(hk_main_var, hk_main_var$bf1000 == 1)
hk_main_var_control <- subset(hk_main_var, hk_main_var$bf50_only != 1 &
                                hk_main_var$bf100 != 1 &
                                hk_main_var$bf250 != 1 &
                                hk_main_var$bf500 != 1 &
                                hk_main_var$bf750 != 1 &
                                hk_main_var$bf1000 != 1)  

hk_main_var_bf500only <- subset(hk_main_var, hk_main_var$bf500_only == 1)
hk_main_var_bf500only_control <- subset(hk_main_var, hk_main_var$bf500_only != 1)


##### descriptives
des_hk_bf50only_time <- ringtime_descriptives(hk_main_var_bf50only)
des_hk_bf100_time <- ringtime_descriptives(hk_main_var_bf100)
des_hk_bf250_time <- ringtime_descriptives(hk_main_var_bf250)
des_hk_bf500_time <- ringtime_descriptives(hk_main_var_bf500)
des_hk_bf750_time <- ringtime_descriptives(hk_main_var_bf750)
des_hk_bf1000_time <- ringtime_descriptives(hk_main_var_bf1000)
des_hk_control_time <- ringtime_descriptives(hk_main_var_control)

des_hk_bf500only_time <- ringtime_descriptives(hk_main_var_bf500only)
des_hk_bf500only_control_time <- ringtime_descriptives(hk_main_var_bf500only_control)


##### export
write.xlsx(des_hk_bf50only_time, file = file.path(outputPath, "descriptives/descriptives_hk_bf50only.xlsx"), sheetName = "House Purchases 50m only")
write.xlsx(des_hk_bf100_time, file = file.path(outputPath, "descriptives/descriptives_hk_bf100.xlsx"), sheetName = "House Purchases 100m")
write.xlsx(des_hk_bf250_time, file = file.path(outputPath, "descriptives/descriptives_hk_bf250.xlsx"), sheetName = "House Purchases 250m")
write.xlsx(des_hk_bf500_time, file = file.path(outputPath, "descriptives/descriptives_hk_bf500.xlsx"), sheetName = "House Purchases 500m")
write.xlsx(des_hk_bf750_time, file = file.path(outputPath, "descriptives/descriptives_hk_bf750.xlsx"), sheetName = "House Purchases 750m")
write.xlsx(des_hk_bf1000_time, file = file.path(outputPath, "descriptives/descriptives_hk_bf1000.xlsx"), sheetName = "House Purchases 1000m")
write.xlsx(des_hk_control_time, file = file.path(outputPath, "descriptives/descriptives_hk_control.xlsx"), sheetName = "House Purchases control")

write.xlsx(des_hk_bf500only_time, file = file.path(outputPath, "descriptives/descriptives_hk_bf500only.xlsx"), sheetName = "HousePurchases 500monly")
write.xlsx(des_hk_bf500only_control_time, file = file.path(outputPath, "descriptives/descriptives_hk_bf500only_control.xlsx"), sheetName = "HousePurchases 500monly control")


# combine control and treatment descriptives ------------------------------

# merge both tables
des_hk_control_treat <- merge(des_hk_bf500only_time, des_hk_bf500only_control_time, by = "variable")

# delete unwanted columns
des_hk_control_treat <- des_hk_control_treat |> 
  select(variable, group1.x, n.x, mean.x, n.y, mean.y)

# rename variables
colnames(des_hk_control_treat) <- c("variable", "group", "obs_treat", "mean_treat", "obs_control", "mean_control")

# redefine group variable
des_hk_control_treat <- des_hk_control_treat |> 
  mutate(group = case_when(group == "0" ~ "control",
                           group == "1" ~ "adoption",
                           group == "2" ~ "treatment"))

# clean variable names
des_hk_control_treat <- des_hk_control_treat |> 
  mutate(variable = substr(variable, start = 1, stop = nchar(variable) - 1))

# make wide table
des_hk_control_treat_wide <- des_hk_control_treat |> 
  select(!c(obs_treat, obs_control)) |> 
  pivot_wider(names_from = "group", values_from = c("mean_treat", "mean_control"))

# drop unwanted rows
des_hk_control_treat_wide <- des_hk_control_treat_wide |> 
  filter(!variable %in% c("keller", "price_sqmeter"))

# reorder column
des_hk_control_treat_wide$variable <- factor(des_hk_control_treat_wide$variable, 
                                             levels = c("ln_houseprice", "kaufpreis", "zimmeranzahl", "alter", "anzahletagen", "ausstattung",
                                                        "badezimmer", "grundstuecksflaeche", "heizungsart", "in_bau", "wohnflaeche", "objektzustand",
                                                        "distance_largcenter", "distance_medcenter", "distance_smalcenter", "distance_airport",
                                                        "distance_industry", "distance_streets", "distance_junction", "distance_station"))

des_hk_control_treat_wide <- des_hk_control_treat_wide[order(des_hk_control_treat_wide$variable), ]

# reorder columns by law implementation
des_hk_control_treat_wide <- des_hk_control_treat_wide |> 
  select(variable, mean_treat_control, mean_control_control,
         mean_treat_adoption, mean_control_adoption,
         mean_treat_treatment, mean_control_treatment)

##### differences via regression

reg_uncond_did <- function(data, depvar){
  # make law implementation a factor
  # to get the difference for both periods (relative to control period)
  regdata <- data
  regdata$law_implementation <- factor(regdata$law_implementation)
  
  depvar_unstringed <- as.name(depvar)
  # run regression
  est_mod <- feols(depvar_unstringed ~ bf500_only * law_implementation, data = regdata, se = "hetero")
  
  # extract interaction terms (i.e. unconditional DiD)
  est_mod_df <- as.data.frame(est_mod$coeftable[c(5, 6), c("Estimate", "Std. Error")])
  
  # rename rows and columns
  rownames(est_mod_df) <- seq(1, nrow(est_mod_df), by = 1)
  colnames(est_mod_df) <- c("estimate", "std_error")
  
  # add variable name and implementation phase
  est_mod_df <- est_mod_df |> 
    mutate(implementation = c("adoption", "actual_treat"),
           variable = depvar)
  
  # reorder
  est_mod_df <- est_mod_df |> 
    select(variable, implementation, estimate, std_error)
  
  # round estimates and SE
  est_mod_df <- est_mod_df |> 
    mutate(estimate = round(estimate, digits = 3),
           std_error = round(std_error, digits = 3))
  
  # return output
  return(est_mod_df)
}

# get variable for regression
variable_names <- as.character(unique(des_hk_control_treat_wide$variable))

# define list for storage
reg_output_uncond_did_list <- list()

# run regressions for each variable
for(variable_name in variable_names){
  reg_out <- reg_uncond_did(data = hk_main_var, depvar = variable_name)
  reg_output_uncond_did_list[[variable_name]] <- reg_out
}

# convert list into data frame
reg_output_uncond_did_df <- do.call(rbind.data.frame, reg_output_uncond_did_list)

# adjust row names
rownames(reg_output_uncond_did_df) <- seq(1, nrow(reg_output_uncond_did_df), by = 1)

# make wide table
reg_output_wide <- reg_output_uncond_did_df |> 
  pivot_wider(names_from = implementation, values_from = c("estimate", "std_error"))

# reorder
reg_output_wide <- reg_output_wide |> 
  select(variable, estimate_adoption, std_error_adoption, estimate_actual_treat, std_error_actual_treat)

# merge to other descriptive table
des_hk_control_treat_final <- merge(des_hk_control_treat_wide, reg_output_wide, by = "variable")

# reorder rows
des_hk_control_treat_final <- des_hk_control_treat_final[order(des_hk_control_treat_final$variable), ]

# add number of observations for entire table
num_obs <- hk_main_var |> group_by(bf500_only, law_implementation) |> summarise(N = n()) |> as.data.frame()
num_obs <- num_obs |> pivot_wider(values_from = N, names_from = law_implementation) |> as.data.frame()
colnames(num_obs) <- c("group", "control", "adoption", "actual_treat")
num_obs <- num_obs |> 
  pivot_wider(values_from = c("control", "adoption", "actual_treat"), names_from = group) |> as.data.frame()
num_obs <- num_obs |> select(control_1, control_0,
                             adoption_1, adoption_0,
                             actual_treat_1, actual_treat_0)
num_obs <- num_obs |> mutate(uncond_did_adoption_N = control_1 + control_0 + adoption_1 + adoption_0,
                             uncond_did_adoption_sd_N = control_1 + control_0 + adoption_1 + adoption_0,
                             uncond_did_actual_treat_N = control_1 + control_0 + actual_treat_1 + actual_treat_0,
                             uncond_did_actual_treat_sd_N = control_1 + control_0 + actual_treat_1 + actual_treat_0)

num_obs$variable <- "observations"
num_obs <- num_obs[, c(ncol(num_obs), seq(1, ncol(num_obs)-1, 1))]
colnames(num_obs) <- names(des_hk_control_treat_final)

des_hk_control_treat_final <- rbind(des_hk_control_treat_final, num_obs)

# export
write.xlsx(des_hk_control_treat_final, file.path(outputPath, "descriptives/des_treat_control_combined.xlsx"))

############################################################
# average price per sq. meter over time and buffer         #
############################################################


# function ----------------------------------------------------------------

##### mean sq. meter price per month
# takes the subset per buffer data set
# summarizes the price per sq. meter for each month

# function
sum_price_month <- function(df, variable, name){
  sum_df <- df %>% group_by(year_mon) %>% summarise(mean_price_sqmeter = mean({{variable}}, na.rm = TRUE),
                                                    year_mon = first(year_mon))
  
  # rename
  colnames(sum_df) <- c("year_mon", paste0("mean_price_sqmeter_", name))
  
  # return
  return(sum_df)
}


# -------------------------------------------------------------------------
# summarise by month
sum_bf50only_month <- sum_price_month(hk_main_var_bf50only, variable = price_sqmeter, name = "bf50only")
sum_bf100_month <- sum_price_month(hk_main_var_bf100, variable = price_sqmeter, name = "bf100")
sum_bf250_month <- sum_price_month(hk_main_var_bf250, variable = price_sqmeter, name = "bf250")
sum_bf500_month <- sum_price_month(hk_main_var_bf500, variable = price_sqmeter, name = "bf500")
sum_bf750_month <- sum_price_month(hk_main_var_bf750, variable = price_sqmeter, name = "bf750")
sum_bf1000_month <- sum_price_month(hk_main_var_bf1000, variable = price_sqmeter, name = "bf1000")
sum_control_month <- sum_price_month(hk_main_var_control, variable = price_sqmeter, name = "control")

sum_bf500only_month <- sum_price_month(hk_main_var_bf500only, variable = price_sqmeter, name = "bf500only")
sum_bf500only_control_month <- sum_price_month(hk_main_var_bf500only_control, variable = price_sqmeter, name = "bf500only_control")

# merge all ---------------------------------------------------------------
sum_buffer_month_aux1 <- merge(sum_bf50only_month, sum_bf100_month, by = "year_mon")
sum_buffer_month_aux2 <- merge(sum_bf250_month, sum_bf500_month, by = "year_mon")
sum_buffer_month_aux3 <- merge(sum_bf750_month, sum_bf1000_month, by = "year_mon")
sum_buffer_month_aux4 <- merge(sum_buffer_month_aux1, sum_buffer_month_aux2, by = "year_mon")

sum_buffer_month_aux5 <- merge(sum_buffer_month_aux4, sum_buffer_month_aux3, by = "year_mon")
sum_buffer_month_aux6 <- merge(sum_buffer_month_aux5, sum_control_month, by = "year_mon")

sum_buffer_month_aux7 <- merge(sum_bf500only_month, sum_bf500only_control_month, by = "year_mon")
sum_buffer_month <- merge(sum_buffer_month_aux6, sum_buffer_month_aux7, by = "year_mon")

# add plot date
sum_buffer_month$plot_date <- as.yearmon(sum_buffer_month$year_mon)



# export ------------------------------------------------------------------
write.xlsx(sum_buffer_month, file.path(outputPath, "descriptives/avg_price_buffer.xlsx"), rowNames = FALSE)


############################################################
# average price difference over periods and buffer         #
############################################################


# function ----------------------------------------------------------------

##### function
# summarize the average price difference (per sq. meter) by periods
sum_periods_month <- function(df, variable, name){
  # average by period
  avg_control_period <- df %>% filter(year_mon <= "2017-06") %>% summarise(mean_diff = mean({{variable}}))
  avg_law_passed <- df %>% filter(year_mon >= "2017-07" & year_mon <= "2020-11") %>% summarise(mean_diff = mean({{variable}}))
  avg_law_in_force <- df %>% filter(year_mon >= "2020-12") %>% summarise(mean_diff = mean({{variable}}))
  
  # combine
  avg_differences_month <- data.frame(cbind(c("2013 Jan - 2017 Jun", "2017 Jul - 2020 Nov", "2020 Dec - 2021 Jun"),
                                            c(avg_control_period, avg_law_passed, avg_law_in_force)))
  
  # rename
  colnames(avg_differences_month) <- c("period", paste0("avg_difference_", name))
  rownames(avg_differences_month) <- seq(1, nrow(avg_differences_month))
  
  # return
  return(avg_differences_month)
}

# -------------------------------------------------------------------------
# calculate average difference for of monthly data for 3 periods: control period (2013 Jan), law passed (2017 Jul), law in force (2020 Dec)

##### apply function
avg_differences_bf50only <- sum_periods_month(df = sum_buffer_month, variable = diff_bf50only_control, name = "bf50only")
avg_differences_bf100 <- sum_periods_month(df = sum_buffer_month, variable = diff_bf100_control, name = "bf100")
avg_differences_bf250 <- sum_periods_month(df = sum_buffer_month, variable = diff_bf250_control, name = "bf250")
avg_differences_bf500 <- sum_periods_month(df = sum_buffer_month, variable = diff_bf500_control, name = "bf500")
avg_differences_bf750 <- sum_periods_month(df = sum_buffer_month, variable = diff_bf750_control, name = "bf750")
avg_differences_bf1000 <- sum_periods_month(df = sum_buffer_month, variable = diff_bf1000_control, name = "bf1000")
avg_differences_bf500only <- sum_periods_month(df = sum_buffer_month, variable = diff_bf500only_control, name = "bf500only")

##### merge
avg_differences <- cbind(avg_differences_bf50only, avg_differences_bf100[2], avg_differences_bf250[2], avg_differences_bf500[2],
                         avg_differences_bf750[2], avg_differences_bf1000[2], avg_differences_bf500only[2]) 

##### export
write.xlsx(avg_differences, file.path(outputPath, "descriptives/avg_differences_price_buffer.xlsx"), rowNames = FALSE)


# absolute prices ---------------------------------------------------------

sum_bf500only_month_price <- sum_price_month(hk_main_var_bf500only, variable = kaufpreis, name = "bf500only")
sum_bf500only_control_month_price <- sum_price_month(hk_main_var_bf500only_control, variable = kaufpreis, name = "bf500only_control")

# merge all ---------------------------------------------------------------

sum_buffer_month_price <- merge(sum_bf500only_month_price, sum_bf500only_control_month_price, by = "year_mon")

# rename
names(sum_buffer_month_price) <- c("year_mon", "mean_price_bf500only", "mean_price_bf500only_control")

# difference between buffers and control ----------------------------------

sum_buffer_month_price <- sum_buffer_month_price %>% mutate(diff_bf500only_control_price = mean_price_bf500only - mean_price_bf500only_control)

# summarise difference for periods
sum_buffer_month_price <- sum_buffer_month_price %>% mutate(law_indicator = case_when(year_mon < "2017-07" ~ 0,
                                                                                      year_mon >= "2017-07" & year_mon <= "2020-11" ~ 1,
                                                                                      year_mon >= "2020-12" ~ 2))

sum_diff <- sum_buffer_month_price %>% group_by(law_indicator) %>% summarise(mean_diff = mean(diff_bf500only_control_price))


# export
write.xlsx(sum_diff, file.path(outputPath, "descriptives/summary_diff_abs_price.xlsx"), rowNames = FALSE)

############################################################
# plotting                                                 #
############################################################

# grouping
sum_buffer_month <- sum_buffer_month %>% mutate(cutoff = case_when(year_mon >= "2013-06" & year_mon <= "2017-06" ~ "control",
                                                                   year_mon >= "2017-07" & year_mon <= "2020-11" ~ "passed",
                                                                   year_mon >= "2020-12" ~ "inforce"))

# -------------------------------------------------------------------------
# month labels
month_labels <- c("Jun 2013", "Dec 2013", "Jun 2014", "Dec 2014", "Jun 2015", "Dec 2015", "Jun 2016", "Dec 2016", "Jun 2017", "Dec 2017",
                  "Jun 2018", "Dec 2018", "Jun 2019", "Dec 2019", "Jun 2020", "Dec 2020", "Jun 2021")


# theme -------------------------------------------------------------------

mytheme <- theme(panel.background = element_blank(),
                 axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
                 axis.text.y = element_text(size = 11),
                 axis.line = element_line(colour = "black"),
                 legend.text = element_text(size = 10),
                 axis.ticks.length = unit(0.25, "cm"),
                 legend.key = element_blank())

linethick <- 1

# -------------------------------------------------------------------------

##### bf500only, bf500only_control
plot1 <- ggplot(data = sum_buffer_month, mapping = aes(x = plot_date, group = 1))+
  geom_line(mapping = aes(y = mean_price_sqmeter_bf500only_control, linetype = "mean_price_sqmeter_bf500only_control"), lwd = linethick)+
  geom_line(mapping = aes(y = mean_price_sqmeter_bf500only, linetype = "mean_price_sqmeter_bf500only"), lwd = linethick)+
  scale_linetype_manual(values = c("mean_price_sqmeter_bf500only_control" = "solid",
                                   "mean_price_sqmeter_bf500only" = "twodash"),
                     labels = c("mean_price_sqmeter_bf500only_control" = "beyond 500m (control)",
                                "mean_price_sqmeter_bf500only" = "0-to-500m buffer (treated)"),
                     name = "")+
  scale_x_yearmon(breaks = seq(min(sum_buffer_month$plot_date), max(sum_buffer_month$plot_date), 0.5),
                  labels = month_labels)+
  scale_y_continuous(breaks = seq(1600, 3400, 200),
                     labels = scales::comma)+
  labs(x = "", y = expression(paste("Price per sq. meter [€/", m^{2}, "]")))+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(group = cutoff, x = plot_date, y = mean_price_sqmeter_bf500only_control), col = "grey60")+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(group = cutoff, x = plot_date, y = mean_price_sqmeter_bf500only), col = "grey60")+
  mytheme

# add text for law passed (2017-07)
plot2 <- plot1+
  geom_segment(aes(x = 2017.500, xend = 2017.500, y = 1700, yend = 3500), linetype = 3)+
  geom_text(aes(x = 2017.500, y = 3600, label = "Law passed"), size = 4)

# add text for law in force (2020-12)
month_plot <- plot2+
  geom_segment(aes(x = 2020.917, xend = 2020.917, y = 1700, yend = 3500), linetype = 3)+
  geom_text(aes(x = 2020.917, y = 3600, label = "Law in force"), size = 4)

month_plot
ggsave(plot = month_plot, file.path(outputPath, "graphs/plot_price_sqmeter_500only_control.png"), width = 7.5, height = 5, dpi = 800)



# -------------------------------------------------------------------------

##### bf50only, bf100, control
plot1 <- ggplot(data = sum_buffer_month, mapping = aes(x = plot_date, group = 1))+
  geom_line(mapping = aes(y = mean_price_sqmeter_bf50only, linetype = "mean_price_sqmeter_bf50only"), lwd = linethick)+
  geom_line(mapping = aes(y = mean_price_sqmeter_bf100, linetype = "mean_price_sqmeter_bf100"), lwd = linethick)+
  geom_line(mapping = aes(y = mean_price_sqmeter_control, linetype = "mean_price_sqmeter_control"), lwd = linethick)+
  scale_linetype_manual(values = c("mean_price_sqmeter_bf50only" = "twodash",
                                   "mean_price_sqmeter_bf100" = "dotted",
                                   "mean_price_sqmeter_control" = "solid"),
                        labels = c("mean_price_sqmeter_bf50only" = "0-to-50m buffer",
                                   "mean_price_sqmeter_bf100" = "50-to-100m buffer",
                                   "mean_price_sqmeter_control" = "beyond 1000m (control)"),
                        name = "", 
                        breaks = c("mean_price_sqmeter_bf50only", "mean_price_sqmeter_bf100", "mean_price_sqmeter_control"))+
  scale_x_yearmon(breaks = seq(min(sum_buffer_month$plot_date), max(sum_buffer_month$plot_date), 0.5),
                  labels = month_labels)+
  scale_y_continuous(breaks = seq(600, 4400, 200))+
  labs(x = "", y = expression(paste("Price per sq. meter [€/", m^{2}, "]")))+
  mytheme

# add text for law passed (2017-07)
plot2 <- plot1+
  geom_segment(aes(x = 2017.500, xend = 2017.500, y = 600, yend = 4200), linetype = 2)+
  geom_text(aes(x = 2017.500, y = 4300, label = "Law passed"), size = 3.5)

# add text for law in force (2020-12)
month_plot <- plot2+
  geom_segment(aes(x = 2020.917, xend = 2020.917, y = 600, yend = 4200), linetype = 2)+
  geom_text(aes(x = 2020.917, y = 4300, label = "Law in force"), size = 3.5)

month_plot
ggsave(plot = month_plot, file.path(outputPath, "graphs/plot_price_sqmeter_50_100_control.png"), width = 7, height = 5)



# -------------------------------------------------------------------------

##### bf250, bf500, control
plot1 <- ggplot(data = sum_buffer_month, mapping = aes(x = plot_date, group = 1))+
  geom_line(mapping = aes(y = mean_price_sqmeter_bf250, linetype = "mean_price_sqmeter_bf250"), lwd = linethick)+
  geom_line(mapping = aes(y = mean_price_sqmeter_bf500, linetype = "mean_price_sqmeter_bf500"), lwd = linethick)+
  geom_line(mapping = aes(y = mean_price_sqmeter_control, linetype = "mean_price_sqmeter_control"), lwd = linethick)+
  scale_linetype_manual(values = c("mean_price_sqmeter_bf250" = "dashed",
                                   "mean_price_sqmeter_bf500" = "dotted",
                                   "mean_price_sqmeter_control" = "solid"),
                        labels = c("mean_price_sqmeter_bf250" = "100-to-250m buffer",
                                   "mean_price_sqmeter_bf500" = "250-to-500m buffer",
                                   "mean_price_sqmeter_control" = "beyond 1000m (control)"),
                        name = "")+
  scale_x_yearmon(breaks = seq(min(sum_buffer_month$plot_date), max(sum_buffer_month$plot_date), 0.5),
                  labels = month_labels)+
  scale_y_continuous(breaks = seq(1400, 4000, 200))+
  labs(x = "", y = expression(paste("Price per sq. meter [€/", m^{2}, "]")))+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.line = element_line(colour = "black"))

# add text for law passed (2017-07)
plot2 <- plot1+
  geom_segment(aes(x = 2017.500, xend = 2017.500, y = 1400, yend = 4000), linetype = 2)+
  geom_text(aes(x = 2017.500, y = 4100, label = "Law passed"), size = 3.5)

# add text for law in force (2020-12)
month_plot <- plot2+
  geom_segment(aes(x = 2020.917, xend = 2020.917, y = 1400, yend = 4000), linetype = 2)+
  geom_text(aes(x = 2020.917, y = 4100, label = "Law in force"), size = 3.5)

month_plot
ggsave(plot = month_plot, file.path(outputPath, "graphs/plot_price_sqmeter_250_500_control.png"), width = 7, height = 5)



# -------------------------------------------------------------------------

##### bf750, bf1000, control
plot1 <- ggplot(data = sum_buffer_month, mapping = aes(x = plot_date, group = 1))+
  geom_line(mapping = aes(y = mean_price_sqmeter_bf750, linetype = "mean_price_sqmeter_bf750"), lwd = linethick)+
  geom_line(mapping = aes(y = mean_price_sqmeter_bf1000, linetype = "mean_price_sqmeter_bf1000"), lwd = linethick)+
  geom_line(mapping = aes(y = mean_price_sqmeter_control, linetype = "mean_price_sqmeter_control"), lwd = linethick)+
  scale_linetype_manual(values = c("mean_price_sqmeter_bf750" = "dashed",
                                   "mean_price_sqmeter_bf1000" = "dotted",
                                   "mean_price_sqmeter_control" = "solid"),
                        labels = c("mean_price_sqmeter_bf750" = "500-to-750m buffer",
                                   "mean_price_sqmeter_bf1000" = "750-to-1000m buffer",
                                   "mean_price_sqmeter_control" = "beyond 1000m (control)"),
                        name = "")+
  scale_x_yearmon(breaks = seq(min(sum_buffer_month$plot_date), max(sum_buffer_month$plot_date), 0.5),
                  labels = month_labels)+
  scale_y_continuous(breaks = seq(1400, 4000, 200))+
  labs(x = "", y = expression(paste("Price per sq. meter [€/", m^{2}, "]")))+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.line = element_line(colour = "black"))

# add text for law passed (2017-07)
plot2 <- plot1+
  geom_segment(aes(x = 2017.500, xend = 2017.500, y = 1400, yend = 4000), linetype = 2)+
  geom_text(aes(x = 2017.500, y = 4100, label = "Law passed"), size = 3.5)

# add text for law in force (2020-12)
month_plot <- plot2+
  geom_segment(aes(x = 2020.917, xend = 2020.917, y = 1400, yend = 4000), linetype = 2)+
  geom_text(aes(x = 2020.917, y = 4100, label = "Law in force"), size = 3.5)

month_plot
ggsave(plot = month_plot, file.path(outputPath, "graphs/plot_price_sqmeter_750_1000_control.png"), width = 7, height = 5)



############################################################
# Number of Observations                                   #
############################################################


# function ----------------------------------------------------------------
# extracts the number of treated (i.e. objects that fall in a specific ring)

##### function
num_treat <- function(df){
  # count number of treated by ring
  control_period <- c(length(which(df$bf50_only == 1 & df$law_implementation == 0)),
         length(which(df$bf100 == 1 & df$law_implementation == 0)),
         length(which(df$bf250 == 1 & df$law_implementation == 0)),
         length(which(df$bf500 == 1 & df$law_implementation == 0)),
         length(which(df$bf750 == 1 & df$law_implementation == 0)),
         length(which(df$bf1000 == 1 & df$law_implementation == 0)),
         length(which(df$bf500_only == 1 & df$law_implementation == 0)))
  
  implementation_period <- c(length(which(df$bf50_only == 1 & df$law_implementation == 1)),
                             length(which(df$bf100 == 1 & df$law_implementation == 1)),
                             length(which(df$bf250 == 1 & df$law_implementation == 1)),
                             length(which(df$bf500 == 1 & df$law_implementation == 1)),
                             length(which(df$bf750 == 1 & df$law_implementation == 1)),
                             length(which(df$bf1000 == 1 & df$law_implementation == 1)),
                             length(which(df$bf500_only == 1 & df$law_implementation == 1)))
  
  inforce_period <- c(length(which(df$bf50_only == 1 & df$law_implementation == 2)),
                             length(which(df$bf100 == 1 & df$law_implementation == 2)),
                             length(which(df$bf250 == 1 & df$law_implementation == 2)),
                             length(which(df$bf500 == 1 & df$law_implementation == 2)),
                             length(which(df$bf750 == 1 & df$law_implementation == 2)),
                             length(which(df$bf1000 == 1 & df$law_implementation == 2)),
                             length(which(df$bf500_only == 1 & df$law_implementation == 2)))

  # combine
  obs <- data.frame(cbind(control_period, implementation_period, inforce_period))
  
  # total number of observations
  overall <- nrow(df)
  obs <- cbind(obs, overall)
  
  # return 
  return(obs)
}


# apply function ----------------------------------------------------------
num_treated <- num_treat(hk_affected)

# clean -------------------------------------------------------------------
# rename rows
rownames(num_treated) <- c("50m", "100m", "250m", "500m", "750m", "1000m", "500m_only")

# export ------------------------------------------------------------------
write.xlsx(num_treated, file.path(outputPath, "descriptives/num_treated.xlsx"), rowNames = TRUE)


############################################################
# Plot regional centers                                    #
############################################################

regional_center <- st_set_geometry(regional_center, regional_center$geometry)
regional_center_top3 <- regional_center %>% filter(center_identifier >= 1)
regional_center_top3$center_identifier <- as.numeric(regional_center_top3$center_identifier)

regional_center_top3 <- st_transform(regional_center_top3, crs = 4326)
coords <- as.data.frame(st_coordinates(regional_center_top3))
colnames(coords) <- c("lon", "lat")
regional_center_top3 <- cbind(regional_center_top3, coords)

mytheme <- theme(panel.background = element_blank(),
                 panel.border = element_rect(fill = NA, size = 1, colour = "black"),
                 legend.text = element_text(size = 10),
                 axis.title = element_blank(),
                 axis.ticks = element_blank(),
                 axis.text = element_blank())

ggplot(regional_center_top3)+
  geom_sf()+
  geom_point(mapping = aes(x = lon, y = lat, 
                           col = center_identifier, 
                           shape = center_identifier,
                           fill = center_identifier, 
                           size = center_identifier))+
  scale_color_manual(values = c("black", "black", "black"), 
                     name = "", 
                     labels = c("1" = "Large RC",
                                "2" = "Medium RC",
                                "3" = "Small RC"))+
  scale_fill_manual(values = c("red", "green", "blue"), 
                    name = "", 
                    labels = c("1" = "Large RC",
                               "2" = "Medium RC",
                               "3" = "Small RC"))+
  scale_shape_manual(values = c(20, 22, 24), 
                     name = "", 
                     labels = c("1" = "Large RC",
                                "2" = "Medium RC",
                                "3" = "Small RC"))+
  scale_size_manual(values = c(4, 1 ,0.5),
                    name = "", 
                    labels = c("1" = "Large RC",
                               "2" = "Medium RC",
                               "3" = "Small RC"))+
  mytheme

############################################################
# Plot regional types                                      #
############################################################


# -------------------------------------------------------------------------
# clean municipality information

# keep relevant variables
gem17 <- gem17 %>% select(AGS_0, geometry)

# merge to regional types data
regional_types <- merge(regional_types, gem17, by.x = "ags", by.y = "AGS_0", all.x = TRUE)

# set as sf
regional_types <- st_set_geometry(regional_types, regional_types$geometry)
regional_types <- st_make_valid(regional_types)

# -------------------------------------------------------------------------
# plot regional types

pal <- RColorBrewer::brewer.pal(n = 4, name = "Paired")
pal <- pal[c(2, 1, 3, 4)]

map_reg <- tm_shape(regional_types)+
  tm_polygons(col = "regional_type",
              border.alpha = 0.3,
              palette = pal,
              style = "cat",
              title = "Regional types",
              labels = c("1" = "Highly central",
                         "2" = "Central",
                         "3" = "Peripheral",
                         "4" = "Highly peripheral"),
              textNA = "Not assigned",
              colorNA = "grey")+
  tm_shape(main_tracks)+
  tm_lines(lwd = 2, col = "black")+
  tm_layout(legend.outside = TRUE,
            legend.position = c(0.1, "center"),
            legend.text.size = 0.7,
            legend.title.size = 0.9)

map_reg
tmap_save(map_reg, file.path(outputPath, "graphs/map_regional_types.png"), width = 11, height = 13, units = "cm")


# -------------------------------------------------------------------------

# as numeric
regional_types$settlement_density <- as.numeric(regional_types$settlement_density)

# define categories
breaks_settden <- as.numeric(quantile(regional_types$settlement_density, probs = seq(0, 1, 0.25), na.rm = TRUE))
breaks_settden

# assign housing objects 
regional_types <- regional_types %>% mutate(settden_cat = case_when(settlement_density >= breaks_settden[1] & settlement_density < breaks_settden[2] ~ 1,
                                                              settlement_density >= breaks_settden[2] & settlement_density < breaks_settden[3] ~ 2,
                                                              settlement_density >= breaks_settden[3] & settlement_density < breaks_settden[4] ~ 3,
                                                              settlement_density >= breaks_settden[4] & settlement_density <= breaks_settden[5] ~ 4))

map_density <- tm_shape(regional_types)+
  tm_polygons(col = "settden_cat",
              style = "cat",
              border.alpha = 0.2,
              palette = "Blues",
              title = "Settlement density",
              labels = c("1" = "Highly sparse",
                         "2" = "Sparse",
                         "3" = "Dense",
                         "4" = "Highly dense"),
              showNA = FALSE)+
  tm_shape(main_tracks)+
  tm_lines(lwd = 2, col = "black")+
  tm_layout(legend.outside = TRUE,
            legend.position = c(0.1, "center"),
            legend.text.size = 0.8,
            legend.title.size = 1)


# labels = c("1" = paste0(breaks_settden[1], "-", breaks_settden[2]-1),
#            "2" = paste0(breaks_settden[2], "-", (breaks_settden[3]-1)),
#            "3" = paste0(breaks_settden[3], "-", (breaks_settden[4]-1)),
#            "4" = paste0(breaks_settden[4], "-", (breaks_settden[5]))))
  
map_density
tmap_save(map_density, file.path(outputPath, "graphs/settlement_density.png"), height = 13, width = 11, units = "cm")
