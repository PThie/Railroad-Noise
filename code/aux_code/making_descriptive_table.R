###### description
# generate the descriptive statistic table for 500m group (treated) and control group

##### load
des_hk_bf500only_time <- read.xlsx(file.path(outputPath, "descriptives/descriptives_hk_bf500only.xlsx"), sheetIndex = 1)
des_hk_bf500only_control_time <- read.xlsx(file.path(outputPath, "descriptives/descriptives_hk_bf500only_control.xlsx"), sheetIndex = 1)

##### function
des_table <- function(group){
  
  # subset
  treat <- des_hk_bf500only_time[des_hk_bf500only_time$group1 == group, c("variable", "mean", "sd", "n")]
  control <- des_hk_bf500only_control_time[des_hk_bf500only_control_time$group1 == group, c("variable", "mean", "sd", "n")]
  
  # rename
  colnames(treat) <- c("variable", paste0("mean_treat_", group), paste0("sd_treat_", group), paste0("n_treat_", group))
  colnames(control) <- c("variable", paste0("mean_control_", group), paste0("sd_control_", group), paste0("n_control_", group))
  
  # merge
  combined <- merge(treat, control, by = "variable")
  
  # clean variable label
  combined$variable <- substr(combined$variable, start = 1, stop = nchar(combined$variable) - 1)
  
  # return
  combined
}



# subset and prepare ------------------------------------------------------

##### law not introduced (control period)

des_period0 <- des_table(group = 0)

##### law implemented (2017-07 to 2020-11)

des_period1 <- des_table(group = 1)

##### law in force (2020-12 and later)

des_period2 <- des_table(group = 2)

##### merge all
des_time_periods_aux <- merge(des_period0, des_period1, by = "variable")
des_time_periods <- merge(des_time_periods_aux, des_period2, by = "variable")



# export ------------------------------------------------------------------

write.xlsx(des_time_periods, file.path(outputPath, "descriptives/descriptives_hk_500_control_combinded.xlsx"), row.names = FALSE)
