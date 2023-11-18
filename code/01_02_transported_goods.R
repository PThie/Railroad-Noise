#############################################################
# load data                                                 #
#############################################################

good_trans <- read_excel(file.path(dataPath, "gueterverkehr_statistik/46131-0002_prep.xlsx"), sheet = 2)
befoerderungsleistung <- read.xlsx(file.path(dataPath, "gueterverkehr_statistik/befoerderungsleistung_verkehrstraeger_jahre.xlsx"), sheetIndex = 1)

#############################################################
# preparation                                               #
#############################################################


# transported goods -------------------------------------------------------
# create date variable
good_trans$date <- ymd(paste(good_trans$year, good_trans$month, "01", sep = "-"))
good_trans$date <- format(as.Date(good_trans$date), "%Y-%m")

good_trans$date <- as.Date.character(good_trans$date)

# calculate the difference to the previous month
good_trans <- good_trans %>% mutate(transported_goods_prev_month = ((transported_goods - lag(transported_goods, n = 1)) /lag(transported_goods, n = 1)) * 100)



# befoerderungsleistung ---------------------------------------------------

# drop shipping by ship (no data)
befoerderungsleistung$Seeverkehr <- NULL

# drop 2000
befoerderungsleistung <- befoerderungsleistung[befoerderungsleistung$Jahr != 2000, ]

# calculate share relative to total
befoerderungsleistung <- befoerderungsleistung %>% mutate(Eisenbahnverkehr_share = round((Eisenbahnverkehr / Insgesamt) * 100, digits = 2),
                                                          Straßenverkehr_share = round((Straßenverkehr / Insgesamt) * 100, digits = 2),
                                                          Binnenschifffahrt_share = round((Binnenschifffahrt / Insgesamt) * 100, digits = 2),
                                                          Rohölrohrleistung_share = round((Rohölrohrleistung / Insgesamt) * 100, digits = 2),
                                                          Luftverkehr_share = round((Luftverkehr / Insgesamt) * 100, digits = 2))

#############################################################
# plotting                                                  #
#############################################################


# transported goods -------------------------------------------------------


plot1 <- ggplot(data = good_trans)+
  geom_line(mapping = aes(y = transported_goods_perc, x = date, group = 1), size = 1)+
  geom_hline(yintercept = 0)+
  scale_x_discrete(breaks = good_trans$date[seq(1, length(good_trans$date), by = 12)])+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 0.8, size = 12),
        axis.text.y = element_text(size = 15), axis.ticks.length = unit(0.2, "cm"),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1), legend.title = element_blank(),
        plot.title = element_text(size = 30), legend.text = element_text(size = 20), axis.title.y = element_text(size = 20))+
  xlab(label = "")+
  ylab(label = "Change to previous year (%)")
  
plot1  

ggsave(plot = plot1, file.path(outputPath, "graph_transported_goods.png"))



# befoerderungsleistung ---------------------------------------------------

bef_plot <- ggplot(befoerderungsleistung)+
  geom_bar(mapping = aes(x = Jahr, y = Eisenbahnverkehr_share), stat = "identity")+
  scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20))+
  scale_x_continuous(breaks = seq(2005, 2020, 1))+
  labs(x = "", y = "Share railroad freight traffic (in %)")+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.line = element_line(colour = "black"))

bef_plot
ggsave(plot = bef_plot, file.path(outputPath, "graph_share_railroad_freight_traffic.png"))
