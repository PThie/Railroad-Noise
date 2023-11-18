##### interactions (both event times included)
hk_affected$gid2019_gen <- as.factor(hk_affected$gid2019_gen)


tst_mod <- feols(ln_houseprice ~ law_inprogress * bf500_only, se = "hetero", data = hk_affected, fixef = c("months", "r1_id"))

etable(tst_mod, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3")


hk_buffer_org <- hk_buffer_org %>% select(obid, distance_streets)

hk_alt_rail <- merge(hk_alt_rail, hk_buffer_org, by = "obid", all.x = TRUE)




lom13H1 <- c(lom_2013[6:11])
lom13H2 <- c(lom_2013[12], lom_2014[1:5])
lom14H1 <- c(lom_2014[6:11])
lom14H2 <- c(lom_2014[12], lom_2015[1:5])
lom15H1 <- c(lom_2015[6:11])
lom15H2 <- c(lom_2015[12], lom_2016[1:5])
lom16H1 <- c(lom_2016[6:11])
lom16H2 <- c(lom_2016[12], lom_2017[1:6])


lom17H2 <- c(lom_2017[8:12], lom_2018[1])
lom18H1 <- c(lom_2018[2:7])
lom18H2 <- c(lom_2018[8:12], lom_2019[1])

lom19H1 <- c(lom_2019[2:7])
lom19H2 <- c(lom_2019[8:12], lom_2020[1])

lom20H1 <- c(lom_2020[2:7])
lom20H2 <- c(lom_2020[8:12], lom_2021[1])

lom21H1 <- c(lom_2021[2:6])



# lom_1314 <- c(lom_2013[6:12], lom_2014[1:6])
# lom_1415 <- c(lom_2014[7:12], lom_2015[1:6])
# lom_1516 <- c(lom_2015[7:12], lom_2016[1:6])
# lom_1617 <- c(lom_2016[7:12], lom_2017[1:6])

lom_17 <- c(lom_2017[7])

lom_1718 <- c(lom_2017[8:12], lom_2018[1:7])
lom_1819 <- c(lom_2018[8:12], lom_2019[1:7])
lom_1920 <- c(lom_2019[8:12], lom_2020[1:7])
lom_2021_new <- c(lom_2020[12], lom_2021[1:6])

lom_1720 <- c(lom_2017[8:12], lom_2018, lom_2019, lom_2020[1:11])

# create variable for periods according to list of months
hk_affected$periods[hk_affected$year_mon %in% lom13H1] <- "t-8"
hk_affected$periods[hk_affected$year_mon %in% lom13H2] <- "t-7"
hk_affected$periods[hk_affected$year_mon %in% lom14H1] <- "t-6"
hk_affected$periods[hk_affected$year_mon %in% lom14H2] <- "t-5"

hk_affected$periods[hk_affected$year_mon %in% lom15H1] <- "t-4"
hk_affected$periods[hk_affected$year_mon %in% lom15H2] <- "t-3"
hk_affected$periods[hk_affected$year_mon %in% lom16H1] <- "t-2"
hk_affected$periods[hk_affected$year_mon %in% lom16H2] <- "t-1"


hk_affected$periods[hk_affected$year_mon %in% lom_17] <- "t"

hk_affected$periods[hk_affected$year_mon %in% lom17H2] <- "t+1"
hk_affected$periods[hk_affected$year_mon %in% lom18H1] <- "t+2"

hk_affected$periods[hk_affected$year_mon %in% lom18H2] <- "t+3"
hk_affected$periods[hk_affected$year_mon %in% lom19H1] <- "t+4"
hk_affected$periods[hk_affected$year_mon %in% lom19H2] <- "t+5"
hk_affected$periods[hk_affected$year_mon %in% lom20H1] <- "t+6"
hk_affected$periods[hk_affected$year_mon %in% lom20H2] <- "t+7"
hk_affected$periods[hk_affected$year_mon %in% lom21H1] <- "t+8"

# make "periods" as factor
hk_affected$periods <- as.factor(hk_affected$periods)


# get coefficients
coef <- as.data.frame(cbind(summary(basemodel_hk_pretrend)$coefficients, summary(basemodel_hk_pretrend)$se))
coef$vars <- rownames(coef)
names(coef) <- c("coef", "se", "vars")

# select only coefficients of interest
coef_interest <- coef[49:64, ]

coef_interest$period <- c("-1", "-2", "-3", "-4", "-5", "-6", "-7", "-8", 
                          "1", "2", "3", "4", "5", "6", "7", "8")

# confidence interval min
coef_interest <- coef_interest %>% mutate(conf_min = coef - (1.96 * se),
                                          conf_max = coef + (1.96 * se))

# reorder
coef_interest <- coef_interest[c(rev(1:8), 9:16),]

ggplot(data = coef_interest, 
       mapping = aes(x = factor(period, levels = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", 
                                                   "1", "2", "3", "4", "5", "6", "7", "8")), y = coef))+
  geom_point(size = 2)+
  geom_line(mapping = aes(y = coef, group = 1))+
  geom_line(mapping = aes(y = conf_min, group = 1), linetype = "dashed")+
  geom_line(mapping = aes(y = conf_max, group = 1), linetype = "dashed")+
  geom_hline(yintercept = 0)+
  labs(x = "Periods", y = "Estimate and 95% CI")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.key = element_blank())


# leave post-treatment unchanged
coef_interest <- coef[49:58, ]

coef_interest$period <- c("-1", "-2", "-3", "-4", "-5", "-6", "-7", "-8", 
                          "1", "2")

# confidence interval min
coef_interest <- coef_interest %>% mutate(conf_min = coef - (1.96 * se),
                                          conf_max = coef + (1.96 * se))

# reorder
coef_interest <- coef_interest[c(rev(1:8), 9:10),]

ggplot(data = coef_interest, 
       mapping = aes(x = factor(period, levels = c("-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", 
                                                   "1", "2")), y = coef))+
  geom_point(size = 2)+
  geom_line(mapping = aes(y = coef, group = 1))+
  geom_line(mapping = aes(y = conf_min, group = 1), linetype = "dashed")+
  geom_line(mapping = aes(y = conf_max, group = 1), linetype = "dashed")+
  geom_hline(yintercept = 0)+
  labs(x = "Periods", y = "Estimate and 95% CI")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.key = element_blank())






coefplot(basemodel_hk_pretrend, se = "hetero", drop = c("wohnflaeche", "grundstuecksflaeche", "anzahletagen", "badezimmer",
                                                        "in_bau", "alter", "ausstattung", "heizungsart", "objektzustand", "zimmeranzahl", "distance", "^bf500_only$"),
         ci_level = 0.95, 
         order = c("t-8", "t-7", "t-6", "t-5", "t-4", "t-3", "t-2", "t-1", "t+1", "t+2", "t+3", "t+4", "t+5", "t+6", "t+7", "t+8"),
         lwd = 1.5, pt.lwd = 2, grid = FALSE, main = "", lab.cex = 1.2, pt.cex = 1.5)
