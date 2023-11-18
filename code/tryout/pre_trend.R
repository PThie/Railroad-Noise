hk_affected <- hk_affected[hk_affected$ajahr != 2013, ]

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

lom_2013 <- month_generator(year = 2013)
lom_2014 <- month_generator(year = 2014)
lom_2015 <- month_generator(year = 2015)
lom_2016 <- month_generator(year = 2016)
lom_2017 <- month_generator(year = 2017)
lom_2018 <- month_generator(year = 2018)
lom_2019 <- month_generator(year = 2019)
lom_2020 <- month_generator(year = 2020)
lom_2021 <- month_generator(year = 2021)

lom_1314 <- c(lom_2013[7:12], lom_2014[1:6])
lom_1415 <- c(lom_2014[7:12], lom_2015[1:6])
lom_1516 <- c(lom_2015[7:12], lom_2016[1:6])
lom_1617 <- c(lom_2016[7:12], lom_2017[1:6])

lom_17 <- c(lom_2017[7])

lom_1718 <- c(lom_2017[8:12], lom_2018[1:7])
lom_1819 <- c(lom_2018[8:12], lom_2019[1:7])
lom_1920 <- c(lom_2019[8:12], lom_2020[1:7])
lom_2021_new <- c(lom_2020[12], lom_2021[1:6])


lom_1720 <- c(lom_2017[8:12], lom_2018, lom_2019, lom_2020[1:11])


hk_affected$periods[hk_affected$year_mon %in% lom_1314] <- "t-4"
hk_affected$periods[hk_affected$year_mon %in% lom_1415] <- "t-3"
hk_affected$periods[hk_affected$year_mon %in% lom_1516] <- "t-2"
hk_affected$periods[hk_affected$year_mon %in% lom_1617] <- "t-1"

hk_affected$periods[hk_affected$year_mon %in% lom_17] <- "t"

hk_affected$periods[hk_affected$year_mon %in% lom_1720] <- "t+1"
hk_affected$periods[hk_affected$year_mon %in% lom_2021_new] <- "t+2"



hk_affected$periods <- as.factor(hk_affected$periods)



tst <- feols(ln_houseprice ~ in_bau + alter + alter_squ + alterUNBEKANNT + 
                        wohnflaeche + wohnflaeche_squ + grundstuecksflaeche + grundstuecksflaeche_squ + 
                        anzahletagen + anzahletagenUNBEKANNT + badezimmer + badezimmerUNBEKANNT + 
                        as.factor(heizungsart) + heizungsartUNBEKANNT + as.factor(ausstattung) + 
                        ausstattungUNBEKANNT + zimmeranzahl + as.factor(objektzustand) + 
                        objektzustandUNBEKANNT + distance_station + distance_junction + 
                        settlement_density + distance_largcenter + distance_medcenter + 
                        distance_airport + distance_industry + kid2019 * year_mon +
                        bf500_only +
               i(bf500_only, periods, ref = "t"), se = "hetero" , data = hk_affected, fixef = c("month_dummy", "plz"))

etable(tst, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")

confint(tst, se = "hetero", level = 0.9)



# before and after
hk_affected$beforeafter[]

