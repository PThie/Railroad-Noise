#############################################################
# load data                                                 #
#############################################################

# loads all the data from the excel into into list object (take only sheets 2 to 17 because 1 are only explanations)
data_list <- import_list(file.path(dataPath, "Laermstatistik/Laermstatistik_BL_Hauptstrecken.xlsx"), setclass = "tbl", which = c(2:17))

# load muncipality info
munc <- st_read(file.path(dataGebiete, "Gemeinde/2020/VG250_GEM.shp"))
bula <- st_read(file.path(dataGebiete, "Bundesland/2019/VG250_LAN.shp"))

#############################################################
# functions                                                 #
#############################################################
mapping.lkz <- function(df.sf, mapvar){
  tm_shape(df.sf)+
  tm_polygons(col =  mapvar, 
              palette = "-viridis",
              style = "cont",
              border.alpha = 0.3,
              textNA = "Not affected",
              colorNA = "gray90",
              title = "Noise indicator",
              breaks = seq(1, 9, 1),
              labels = c("0 - 5,000", "5,001 - 10,000", "10,001 - 15,000", "15,001 - 20,000",
                         "20,001 - 25,000", "25,001 - 30,000", "30,001 - 35,000", "35,001 - 40,000", "> 40,000"))+
  tm_layout(legend.outside = TRUE, 
            legend.position = c(0.1, 0.5))
}


#############################################################
# preparation                                               #
#############################################################

# transform CRS
munc <- st_transform(munc, crs = 32632)
bula <- st_transform(bula, crs = 32632)

# transform the list object into data frame
laermstatistik <- plyr::ldply(data_list, data.frame)

# rename and drop variables
laermstatistik <- laermstatistik[,c(1:15, 28, 29)]

names(laermstatistik)[names(laermstatistik) == ".id"] <- "bundesland"
names(laermstatistik)[names(laermstatistik) == "NAME"] <- "gemeinde"

# restrict muncipality info to important variables
munc <- munc[, c("AGS_0", "geometry")]
munc <- munc[!duplicated(munc$AGS_0), ]

# calculate total LDEN and LN by municipality
laermstatistik <- laermstatistik %>% rowwise(gemeinde) %>% mutate(total_lden_munc = sum(EW_LDEN_55_60, EW_LDEN_60_65, EW_LDEN_65_70,
                                                                                        EW_LDEN_70_75, EW_LDEN_75),
                                                                  total_ln_munc = sum(EW_LN_45_50, EW_LN_50_55, EW_LN_55_60,
                                                                                      EW_LN_60_65, EW_LN_65_70, EW_LN_70))
#############################################################
# overview table                                            #
#############################################################
# making a overview table with the total numbers

# total (across all municipalities)
# total_ew <- laermstatistik %>% ungroup() %>%  summarise(total_ew = sum(EWZ),
#                                          total_lden_55_60 = sum(EW_LDEN_55_60),
#                                          total_lden_60_65 = sum(EW_LDEN_60_65),
#                                          total_lden_65_70 = sum(EW_LDEN_65_70),
#                                          total_lden_70_75 = sum(EW_LDEN_70_75),
#                                          total_lden_75 = sum(EW_LDEN_75),
#                                          total_ln_45_50 = sum(EW_LN_45_50),
#                                          total_ln_50_55 = sum(EW_LN_50_55),
#                                          total_ln_55_60 = sum(EW_LN_55_60),
#                                          total_ln_60_65 = sum(EW_LN_60_65),
#                                          total_ln_65_70 = sum(EW_LN_65_70),
#                                          total_ln_70 = sum(EW_LN_70),
#                                          total_lden_ew = sum(first(total_lden_55_60), first(total_lden_60_65),
#                                                              first(total_lden_65_70), first(total_lden_70_75),
#                                                              first(total_lden_75)),
#                                          total_ln_ew = sum(first(total_ln_45_50), first(total_ln_50_55),
#                                                            first(total_ln_55_60), first(total_ln_60_65),
#                                                            first(total_ln_65_70), first(total_ln_70)),
#                                          total_lden_55_60_perc = (total_lden_55_60 / total_lden_ew) * 100,
#                                          total_lden_60_65_perc = (total_lden_60_65 / total_lden_ew) * 100,
#                                          total_lden_65_70_perc = (total_lden_65_70 / total_lden_ew) * 100,
#                                          total_lden_70_75_perc = (total_lden_70_75 / total_lden_ew) * 100,
#                                          total_lden_75_perc = (total_lden_75 / total_lden_ew) * 100,
#                                          total_ln_45_50_perc = (total_ln_45_50 / total_ln_ew) * 100,
#                                          total_ln_50_55_perc = (total_ln_50_55 / total_ln_ew) * 100,
#                                          total_ln_55_60_perc = (total_ln_55_60 / total_ln_ew) * 100,
#                                          total_ln_60_65_perc = (total_ln_60_65 / total_ln_ew) * 100,
#                                          total_ln_65_70_perc = (total_ln_65_70 / total_ln_ew) * 100,
#                                          total_ln_70_perc = (total_ln_70 / total_ln_ew) * 100)

# population Germany (31.03.2021)
# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/liste-zensus-geschlecht-staatsangehoerigkeit.html
popGER <- 83121363

total_ew <- laermstatistik %>% ungroup() %>%  summarise(total_ew = sum(EWZ),
                                                        total_lden_55_60 = sum(EW_LDEN_55_60),
                                                        total_lden_60_65 = sum(EW_LDEN_60_65),
                                                        total_lden_65_70 = sum(EW_LDEN_65_70),
                                                        total_lden_70_75 = sum(EW_LDEN_70_75),
                                                        total_lden_75 = sum(EW_LDEN_75),
                                                        total_ln_45_50 = sum(EW_LN_45_50),
                                                        total_ln_50_55 = sum(EW_LN_50_55),
                                                        total_ln_55_60 = sum(EW_LN_55_60),
                                                        total_ln_60_65 = sum(EW_LN_60_65),
                                                        total_ln_65_70 = sum(EW_LN_65_70),
                                                        total_ln_70 = sum(EW_LN_70),
                                                        total_lden_ew = sum(first(total_lden_55_60), first(total_lden_60_65),
                                                                            first(total_lden_65_70), first(total_lden_70_75),
                                                                            first(total_lden_75)),
                                                        total_ln_ew = sum(first(total_ln_45_50), first(total_ln_50_55),
                                                                          first(total_ln_55_60), first(total_ln_60_65),
                                                                          first(total_ln_65_70), first(total_ln_70)),
                                                        total_lden_55_60_perc = (total_lden_55_60 / popGER) * 100,
                                                        total_lden_60_65_perc = (total_lden_60_65 / popGER) * 100,
                                                        total_lden_65_70_perc = (total_lden_65_70 / popGER) * 100,
                                                        total_lden_70_75_perc = (total_lden_70_75 / popGER) * 100,
                                                        total_lden_75_perc = (total_lden_75 / popGER) * 100,
                                                        total_lden_perc = (total_lden_ew / popGER) * 100,
                                                        total_ln_45_50_perc = (total_ln_45_50 / popGER) * 100,
                                                        total_ln_50_55_perc = (total_ln_50_55 / popGER) * 100,
                                                        total_ln_55_60_perc = (total_ln_55_60 / popGER) * 100,
                                                        total_ln_60_65_perc = (total_ln_60_65 / popGER) * 100,
                                                        total_ln_65_70_perc = (total_ln_65_70 / popGER) * 100,
                                                        total_ln_70_perc = (total_ln_70 / popGER) * 100,
                                                        total_ln_perc = (total_ln_ew / popGER) * 100)

# transpose
total_ew <- as.data.frame(t(total_ew))
colnames(total_ew) <- "n"

# export
write.xlsx(total_ew, file = file.path(outputPath, "overview_noise_affected.xlsx"), row.names = TRUE)

#############################################################
# LKZ                                                       #
#############################################################

# add categories for LKZs
laermstatistik$lkz_lden_cat <- NA
laermstatistik$lkz_lden_cat[laermstatistik$LKZ_LDEN <= 5000] <- 1
laermstatistik$lkz_lden_cat[laermstatistik$LKZ_LDEN > 5000 & laermstatistik$LKZ_LDEN <= 10000] <- 2
laermstatistik$lkz_lden_cat[laermstatistik$LKZ_LDEN > 10000 & laermstatistik$LKZ_LDEN <= 15000] <- 3
laermstatistik$lkz_lden_cat[laermstatistik$LKZ_LDEN > 15000 & laermstatistik$LKZ_LDEN <= 20000] <- 4
laermstatistik$lkz_lden_cat[laermstatistik$LKZ_LDEN > 20000 & laermstatistik$LKZ_LDEN <= 25000] <- 5
laermstatistik$lkz_lden_cat[laermstatistik$LKZ_LDEN > 25000 & laermstatistik$LKZ_LDEN <= 30000] <- 6
laermstatistik$lkz_lden_cat[laermstatistik$LKZ_LDEN > 30000 & laermstatistik$LKZ_LDEN <= 35000] <- 7
laermstatistik$lkz_lden_cat[laermstatistik$LKZ_LDEN > 35000 & laermstatistik$LKZ_LDEN <= 40000] <- 8
laermstatistik$lkz_lden_cat[laermstatistik$LKZ_LDEN > 40000] <- 9

laermstatistik$lkz_ln_cat <- NA
laermstatistik$lkz_ln_cat[laermstatistik$LKZ_LNIGHT <= 5000] <- 1
laermstatistik$lkz_ln_cat[laermstatistik$LKZ_LNIGHT > 5000 & laermstatistik$LKZ_LNIGHT <= 10000] <- 2
laermstatistik$lkz_ln_cat[laermstatistik$LKZ_LNIGHT > 10000 & laermstatistik$LKZ_LNIGHT <= 15000] <- 3
laermstatistik$lkz_ln_cat[laermstatistik$LKZ_LNIGHT > 15000 & laermstatistik$LKZ_LNIGHT <= 20000] <- 4
laermstatistik$lkz_ln_cat[laermstatistik$LKZ_LNIGHT > 20000 & laermstatistik$LKZ_LNIGHT <= 25000] <- 5
laermstatistik$lkz_ln_cat[laermstatistik$LKZ_LNIGHT > 25000 & laermstatistik$LKZ_LNIGHT <= 30000] <- 6
laermstatistik$lkz_ln_cat[laermstatistik$LKZ_LNIGHT > 30000 & laermstatistik$LKZ_LNIGHT <= 35000] <- 7
laermstatistik$lkz_ln_cat[laermstatistik$LKZ_LNIGHT > 35000 & laermstatistik$LKZ_LNIGHT <= 40000] <- 8
laermstatistik$lkz_ln_cat[laermstatistik$LKZ_LNIGHT > 40000] <- 9

# merge geo info of municipalities
laerm.sf <- merge(laermstatistik, munc, by.x = "AGS", by.y = "AGS_0", all.y = TRUE)
laerm.sf <- st_set_geometry(laerm.sf, laerm.sf$geometry)

# LDEN
gem.map.lden <- mapping.lkz(laerm.sf, mapvar = "lkz_lden_cat")

gem.map.lden

tmap_save(gem.map.lden, file.path(outputPath, "map_lkz_lden.png"))

# LN
gem.map.ln <- mapping.lkz(laerm.sf, mapvar = "lkz_ln_cat")

gem.map.ln

tmap_save(gem.map.ln, file.path(outputPath, "map_lkz_ln.png"))


#############################################################
# export                                                    #
#############################################################

saveRDS(laermstatistik, file.path(dataPath, "Laermstatistik/laermstatistik_prep.rds"))
