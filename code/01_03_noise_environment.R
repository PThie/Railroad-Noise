#############################################################
# load data                                                 #
#############################################################

# freight corridors
main_tracks <- st_read(file.path(dataPath, "main_tracks/tracks5805.shp"))
main_tracks <- st_transform(main_tracks, crs = 32632)

# states
bula <- st_read(file.path(dataGebiete, "Bundesland/2019/VG250_LAN.shp"))
bula <- st_transform(bula, crs = 32632)

# municipality information
gem <- st_read(file.path(dataGebiete, "Gemeinde/2019/VG250_GEM.shp"))

# whisper brakes
whisbrak <- read.xlsx(
    file.path(dataPath, "db_bremsen/db_fluesterbremsen.xlsx"),
    sheet = "Tabelle1"
)

#############################################################
# preparation Noise data                                    #
#############################################################

##### function to read and prepare noise data
read_noise <- function(measuring_time, data_folder, noise_type, length_identifier){
    setwd(
        paste0(
            "N:/Schienenlaerm/data/Laermmessungen/stations/",
            measuring_time,
            "/",
            data_folder
        )
    )
    file.list <- list.files(
        path = paste0(
            "N:/Schienenlaerm/data/Laermmessungen/stations/",
            measuring_time,
            "/",
            data_folder
        ),
        pattern = "*.xlsx",
        full.names = TRUE
    )
    df.list <- lapply(file.list, readxl::read_excel)

    # prepare noise data ------------------------------------------------------

    # extract the station names from the file list
    stations_names <- substring(file.list, first = length_identifier, last = nchar(file.list) - 5)

    # assign names to the data list
    names(df.list) <- stations_names

    # rowbind all list elements and make data frame
    noise_data <- bind_rows(df.list, .id = "id")

    # rename columns
    names(noise_data) <- c("station", "date", noise_type, "trains", "freight_trains")

    # transform date variable
    noise_data$date <- ymd(noise_data$date)

    # return
    noise_data
}

##### apply function
noise_lden <- read_noise(
    measuring_time = "24h",
    data_folder = "uptoMar22",
    noise_type = "noise_lden",
    length_identifier = 61
)
noise_day <- read_noise(
    measuring_time = "day",
    data_folder = "uptoMar22",
    noise_type = "noise_day",
    length_identifier = 61
)
noise_night <- read_noise(
    measuring_time = "night",
    data_folder = "uptoMar22",
    noise_type = "noise_night",
    length_identifier = 63
)

##### merge
noise_levels <- merge(noise_lden, noise_day, by = c("date", "station"))
noise_levels <- merge(noise_levels, noise_night, by = c("date", "station"))

# rename
colnames(noise_levels) <- c(
    "date", "station", "noise_lden", "num_pass_lden", "num_freight_lden",
    "noise_day", "num_pass_day", "num_freight_day", 
    "noise_night", "num_pass_night", "num_freight_night"
)

#############################################################
# preparation train passing                                 #
#############################################################

##### function to read train passing data
read_passing <- function(data_folder){
    setwd("N:/Schienenlaerm/data/zugvorbeifahrten/")
    file.list <- list.files(
        path = paste0(
            "N:/Schienenlaerm/data/zugvorbeifahrten/",
            data_folder
        ),
        pattern = "*.xlsx",
        full.names = TRUE
    )
    df.list <- lapply(file.list, readxl::read_excel)
    
    # prepare train passing ---------------------------------------------------
    
    # extract the station names from the file list
    stations_names <- substring(file.list, first = 50, last = nchar(file.list) - 5)
    
    # assign names to the data list
    names(df.list) <- stations_names
    
    # rowbind all list elements and make data frame
    train_pass <- bind_rows(df.list, .id = "id")
    
    # return
    train_pass
}

##### apply function
passing_oct21 <- read_passing(data_folder = "uptoOct21")
passing_sep21 <- read_passing(data_folder = "uptoSep21")
passing_jul21 <- read_passing(data_folder = "uptoJul21")

##### preparation

# new passings
new_passings_oct_sep <- passing_oct21[
    !(passing_oct21$Einfahrtszeit %in% passing_sep21$Einfahrtszeit),
]
new_passings_sep_aug <- passing_sep21[
    !(passing_sep21$Einfahrtszeit %in% passing_jul21$Einfahrtszeit),
]

# merge old and new
train_passings <- as.data.frame(
    rbind(passing_jul21, new_passings_sep_aug, new_passings_oct_sep)
)

# extract date
train_passings$date <- as.character(train_passings$Einfahrtszeit)
train_passings$date <- substring(train_passings$date, first = 1, last = 10)
train_passings$date <- ymd(train_passings$date)

# drop variables
train_passings$Einfahrtszeit <- NULL
train_passings$Gleis <- NULL
train_passings$Richtung <- NULL
train_passings$Info <- NULL

# rename variables
names(train_passings) <- c(
    "station", "train_type", "duration_sec", "length_m", "speed_kmh",
    "noise_max_db", "noise_tel_db", "date"
)

# reorder
train_passings <- train_passings[, c(8, 1:7)]

# adjust train type
train_passings$train_type[train_passings$train_type == "GZ"] <- "freight_train"
train_passings$train_type[train_passings$train_type == "PZ"] <- "passenger_train"

# keep only freight and passenger trains
train_passings <- train_passings[train_passings$train_type == "freight_train" | train_passings$train_type == "passenger_train", ]

# sort
train_passings <- train_passings[order(train_passings$date), ]

##### clean

# drop unplausble values
# durantion cannot be negative
train_passings <- train_passings[train_passings$duration_sec > 0, ]

#############################################################
# plot freight tracks                                       #
#############################################################

# -------------------------------------------------------------------------
# map the corridors (colored version)
# map_freight <-  tm_shape(bula)+
#   tm_borders(col = "gray70")+
#   tm_shape(main_tracks)+
#   tm_lines(col = "corridor", title.col = "Corridor", lwd = 3, palette = "Dark2", 
#            labels = c("atlantic" ="Atlantic", 
#                       "danube" = "Rhine-Danube", 
#                       "northsea" = "North Sea-Baltic", 
#                       "orient" = "Orient/ East-Med", 
#                       "rine" = "Rhine-Alpine", 
#                       "scanmed" = "ScanMed"))+
#   tm_layout(legend.outside = TRUE,
#             legend.position = c(0.1, "center"))
# 
# map_freight
# tmap_save(map_freight, file.path(outputPath, "graphs/map_freight_tracks.png"))

# map corridors (black-white version)
map_freight <-  tm_shape(bula)+
    tm_borders(col = "gray70")+
    tm_shape(main_tracks)+
    tm_lines(col = "black", lwd = 3)+
    tm_layout(
        legend.outside = TRUE,
        legend.position = c(0.1, "center"),
        frame = FALSE
    )

tmap_save(
    map_freight,
    file.path(
        outputPath,
        "graphs/map_freight_tracks.png"
    )
)

# -------------------------------------------------------------------------
# map treated municipalities that are crossed by tracks

##### prepare
# transform municipality CRS
gem <- st_transform(gem, crs = 32632)

# determine municipalities which are crossed by tracks
int <- st_intersects(gem, main_tracks)
gem$logical <- lengths(int)
gem$logical[gem$logical != 0] <- 1

# split up municipalities into treated and non-treated
gem_treated <- gem[gem$logical == 1, ]
gem_untreated <- gem[gem$logical == 0, ]

##### map the municipalities that are crossed by tracks
map_treat_munc <- tm_shape(gem_treated)+
    tm_polygons(col = "darkblue", alpha = 0.7, border.col = "gray80")+
    tm_shape(gem_untreated)+
    tm_borders(col = "gray80")+
    tm_layout(frame = FALSE)

tmap_save(
    map_treat_munc,
    file.path(
        outputPath,
        "graphs/map_freight_tracks_treated_munic.png"
    )
)

# combine tracks and treated municipalities -------------------------------

# select major cities (for reference on map)
# Berlin, Hamburg, Frankfurt aM, Munich, Cologne, Stuttgart
major_cities <- gem |>
    dplyr::filter(AGS %in% c(
        "11000000", "02000000", "06412000", "09162000",
        "05315000", "08111000"
    )) |>
    # drop Hamburg's islands
    dplyr::filter(logical == 1) |>
    sf::st_centroid() |>
    dplyr::mutate(
        GEN = stringi::stri_trans_general(GEN, "de-ASCII; Latin-ASCII"),
        # add city labels and translate to English
        munic_label = GEN,
        munic_label = dplyr::case_when(
            munic_label == "Koeln" ~ "Cologne",
            munic_label == "Muenchen" ~ "Munich",
            munic_label == "Frankfurt am Main" ~ "Frankfurt",
            TRUE ~ munic_label
        ),
        lon = sf::st_coordinates(geometry)[, 1],
        lat = sf::st_coordinates(geometry)[, 2]
    ) |>
    sf::st_drop_geometry()

map_tracks_treated <- ggplot()+
    geom_sf(
        data = gem_untreated,
        aes(geometry = geometry),
        col = "grey80"
    )+
    geom_sf(
        data = gem_treated,
        aes(geometry = geometry),
        fill = "grey70"
    )+
    geom_sf(
        data = main_tracks,
        aes(geometry = geometry),
        col = "black",
        lwd = 1.2
    )+
    geom_point(
        data = major_cities,
        aes(x = lon, y = lat),
        col = "black",
        shape = 15
    )+
    geom_label(
        data = major_cities,
        aes(x = lon, y = lat, label = munic_label),
        size = 5,
        hjust = -0.1,
        vjust = 0.5,
        label.padding = unit(0.15, "lines"),
        label.size = 0.25, 
        color = "black",
        fill = "white"
    )+
    theme_void()

ggsave(
    plot = map_tracks_treated,
    file.path(
        outputPath,
        "graphs",
        "map_freight_tracks_municipalities.png"
    ),
    dpi = 400
)

# -------------------------------------------------------------------------
# map 3km buffer (as alternative control group)

##### prepare
# buffer 3km
bf3000 <- st_buffer(main_tracks, dist = 3000)

# form union
uni <- st_union(bf3000)

##### map
map_buffer3km <- tm_shape(uni)+
    tm_polygons(col = "black", border.alpha = 0)+ 
    tm_shape(gem_treated)+
    tm_polygons(col = "blue", alpha = 0.5, border.alpha = 0)+
    tm_shape(gem)+
    tm_borders(alpha = 0.4)+
    tm_layout(frame = FALSE)

tmap_save(
    map_buffer3km,
    file.path(
        outputPath,
        "graphs/map_freight_tracks_buffer3km.png"
    )
)

#############################################################
# train passings                                            #
#############################################################

#### NOTE
# TEL stands for Transit Exposure Level. gives the mean level of a train passing ("Der Vorbeifahrtexpositionspegel (engl. Transit Exposure Level, TEL) 
# gibt den mittleren Schalldruckpegel während einer einzelnen Vorbeifahrt an. Die Länge des Zuges hat kaum Einfluss." (Jahresbericht Lärmmonitoring 2020))

# MAXL stands for maximum level of a train passing ("Der Maximalpegel gibt den größten Pegelwert während einer Zugvorbeifahrt an." (Jahresbericht Lärmmonitoring 2020))
####

# summary by group (train type) -------------------------------------------

group_summary <- train_passings %>% 
    group_by(train_type) %>% 
    summarise(
        mean_duration = mean(duration_sec, na.rm = TRUE),
        mean_length = mean(length_m, na.rm = TRUE),
        mean_speed = mean(speed_kmh, na.rm = TRUE),
        mean_noise_tel = mean(noise_tel_db, na.rm = TRUE),
        mean_noise_max = mean(noise_max_db, na.rm = TRUE),
        num_trains = n()
    ) %>% 
    as.data.frame()

# difference between the types
diff <- group_summary[1, 2:7]-group_summary[2, 2:7]
diff$train_type <- "difference"
diff <- diff[, c(7, 1:6)]

# calculate overall mean

total_mean <- train_passings %>% 
    summarise(
        train_type = "overall",
        mean_duration = mean(duration_sec, na.rm = TRUE),
        mean_length = mean(length_m, na.rm = TRUE),
        mean_speed = mean(speed_kmh, na.rm = TRUE),
        mean_noise_tel = mean(noise_tel_db, na.rm = TRUE),
        mean_noise_max = mean(noise_max_db, na.rm = TRUE),
        num_trains = n()
    ) %>% 
    as.data.frame()

group_summary <- rbind(group_summary, diff, total_mean)

# info
txt <- "Period covered: 28.02.2021 to 30.10.2021. Total number of train passings: 895,303"

# export
openxlsx::write.xlsx(
    group_summary,
    file.path(
        outputPath,
        "descriptives/group_summary_train_passing.xlsx"
    ),
    row.names = FALSE,
    sheetName = "results"
)
openxlsx::write.xlsx(
    txt,
    file.path(
        outputPath,
        "descriptives/group_summary_train_passing.xlsx"
    ),
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE,
    sheetName = "info"
)

# summary by day and type of train
day_summary <- train_passings %>% 
    group_by(train_type, date) %>% 
    summarise(
        mean_duration = mean(duration_sec, na.rm = TRUE),
        mean_length = mean(length_m, na.rm = TRUE),
        mean_speed = mean(speed_kmh, na.rm = TRUE),
        mean_noise_tel = mean(noise_tel_db, na.rm = TRUE),
        mean_noise_max = mean(noise_max_db, na.rm = TRUE)
    ) %>% 
    as.data.frame()

# add moving average
day_summary <- day_summary %>%
    group_by(train_type) %>%
    mutate(
        mov_avg = rollmean(mean_noise_tel, k = 14, fill = NA)
    )

# plot
##### owntheme
owntheme <- theme(
    axis.text.x = element_text(size = 25, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 27, vjust = 2),
    axis.text.y = element_text(size = 25),
    panel.background = element_rect(colour = "white", fill = "white"),
    axis.line = element_line(size = 0.5, linetype = "solid", color = "black"),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 25),
    axis.ticks.length = unit(0.25, "cm"),
    legend.key = element_blank(),
    legend.position = "bottom"
)

noise_train_passing_type <- ggplot(day_summary)+
    geom_line(aes(x = date, y = mean_noise_tel, group = train_type, col = train_type), size = 1)+
    geom_line(aes(x = date, y = mov_avg, group = train_type), size = 1)+
    scale_x_date(breaks = "7 day")+
    scale_y_continuous(breaks = seq(82, 86, 1))+
    scale_color_manual(
        name = "Train Type",
        values = c(
            "freight_train" = "darkorange2",
            "passenger_train" = "darkblue"
        ),
        labels = c(
            "freight_train" = "Freight train",
            "passenger_train" = "Passenger train"
        )
    )+
    labs(x = "", y = "TEL Noise Level (in dB)")+
    owntheme

ggsave(
    plot = noise_train_passing_type,
    file.path(
        outputPath,
        "graphs/noise_level_by_type_trainpassing.png"
    ),
    height = 10,
    width = 13
)

# summary for freight trains ----------------------------------------------

# subset
freight_train_passings <- train_passings[train_passings$train_type == "freight_train", ]

# descriptives
des_freight_passings <- describe(freight_train_passings, na.rm = TRUE, fast = TRUE)

# clean
des_freight_passings$vars <- row.names(des_freight_passings)
des_freight_passings <- des_freight_passings[4:nrow(des_freight_passings), 1:6]
row.names(des_freight_passings) <- seq(1, nrow(des_freight_passings), 1)

# export
write.xlsx(
    des_freight_passings,
    file.path(
        outputPath,
        "descriptives/freight_train_passings.xlsx"
    ),
    row.names = FALSE
)

#############################################################
# noise measures                                            #
#############################################################

# add weekday
noise_levels$weekday <- weekdays(as.Date(noise_levels$date))

# add month
noise_levels$year_mon <- format(as.Date(noise_levels$date), "%Y-%m")

# summarise noise types across stations -----------------------------------
##### summary by day
noise_summary <- noise_levels %>%
    group_by(date) %>%
    summarise(
        mean_noise_lden = mean(noise_lden, na.rm = TRUE),
        mean_num_pass_lden = mean(num_pass_lden, na.rm = TRUE),
        mean_num_freight_lden = mean(num_freight_lden, na.rm = TRUE),
        # DAY
        mean_noise_day = mean(noise_day, na.rm = TRUE),
        mean_num_pass_day = mean(num_pass_day, na.rm = TRUE),
        mean_num_freight_day = mean(num_freight_day, na.rm = TRUE),
        # NIGHT
        mean_noise_night = mean(noise_night, na.rm = TRUE),
        mean_num_pass_night = mean(num_pass_night, na.rm = TRUE),
        mean_num_freight_night = mean(num_freight_night, na.rm = TRUE)
    ) %>% 
    as.data.frame()

##### summary by weekday
noise_summary_weekday <- noise_levels %>%
    group_by(weekday) %>%
    summarise(
        mean_noise_lden = mean(noise_lden, na.rm = TRUE),
        mean_num_pass_lden = mean(num_pass_lden, na.rm = TRUE),
        mean_num_freight_lden = mean(num_freight_lden, na.rm = TRUE),
        # DAY
        mean_noise_day = mean(noise_day, na.rm = TRUE),
        mean_num_pass_day = mean(num_pass_day, na.rm = TRUE),
        mean_num_freight_day = mean(num_freight_day, na.rm = TRUE),
        # NIGHT
        mean_noise_night = mean(noise_night, na.rm = TRUE),
        mean_num_pass_night = mean(num_pass_night, na.rm = TRUE),
        mean_num_freight_night = mean(num_freight_night, na.rm = TRUE)
    ) %>% 
    as.data.frame()

noise_summary_weekday <- noise_summary_weekday[c(5, 1, 4, 2, 3, 6, 7),]

##### summary by month
noise_summary_month <- noise_levels %>%
    group_by(year_mon) %>%
    summarise(
        mean_noise_lden = mean(noise_lden, na.rm = TRUE),
        mean_num_pass_lden = mean(num_pass_lden, na.rm = TRUE),
        mean_num_freight_lden = mean(num_freight_lden, na.rm = TRUE),
        # DAY
        mean_noise_day = mean(noise_day, na.rm = TRUE),
        mean_num_pass_day = mean(num_pass_day, na.rm = TRUE),
        mean_num_freight_day = mean(num_freight_day, na.rm = TRUE),
        # NIGHT
        mean_noise_night = mean(noise_night, na.rm = TRUE),
        mean_num_pass_night = mean(num_pass_night, na.rm = TRUE),
        mean_num_freight_night = mean(num_freight_night, na.rm = TRUE)
    ) %>% 
    as.data.frame()

# rolling average ---------------------------------------------------------

# for monthly data
noise_summary_month <- noise_summary_month %>%
    mutate(
        mov_avg_lden = rollmean(mean_noise_lden, k = 3, fill = NA),
        mov_avg_day = rollmean(mean_noise_day, k = 3, fill = NA),
        mov_avg_night = rollmean(mean_noise_night, k = 3, fill = NA)
    )

# export
write.fst(
    noise_levels,
    file.path(
        dataPath,
        "Laermmessungen/stations/noise_levels_prep.fst"
    )
)
write.fst(
    noise_summary,
    file.path(
        dataPath,
        "Laermmessungen/stations/noise_levels_summary_day.fst"
    )
)
write.fst(
    noise_summary_weekday,
    file.path(
        dataPath,
        "Laermmessungen/stations/noise_levels_summary_weekdays.fst"
    )
)
write.xlsx(
    noise_summary_weekday,
    file.path(
        dataPath,
        "Laermmessungen/stations/noise_levels_summary_weekdays.xlsx"
    ),
    row.names = FALSE
)

# plot average noise by month ---------------------------------------------

# compare before and after treatment (mean) -------------------------------
# calculate average
##### based on daily data
noise_summary$after_treat <- ifelse(
    noise_summary$date >= "2020-12-13",
    yes = 1,
    no = 0
)

# mean by group
mean_noise_treat <- noise_summary %>% 
    group_by(after_treat) %>% 
    summarise(
        mean_lden = mean(mean_noise_lden),
        mean_day = mean(mean_noise_day),
        mean_night = mean(mean_noise_night),
        mean_num_freight = mean(mean_num_freight_lden)
    ) %>% 
    as.data.frame()


##### based on monthly data
noise_summary_month$after_treat <- ifelse(
    noise_summary_month$year_mon >= "2020-12",
    yes = 1,
    no = 0
)

# export
write.xlsx(
    mean_noise_treat,
    file.path(
        outputPath,
        "descriptives/mean_noise_levels_beforeafter.xlsx"
    ),
    row.names = FALSE
)

# plot date
noise_summary_month$plot_date <- as.yearmon(noise_summary_month$year_mon)

# month labels
month_labels <- c(
    "Apr 2019", "Aug 2019", "Dec 2019", "Apr 2020", "Aug 2020", "Dec 2020",
    "Apr 2021", "Aug 2021", "Dec 2021"
)

# ban of freight trains
ban <- 2020.917

# max date (limited to December 2021)
max_date <- 2021.917

# averages
before_day <- mean_noise_treat$mean_day[mean_noise_treat$after_treat == 0]
before_night <- mean_noise_treat$mean_night[mean_noise_treat$after_treat == 0]

after_day <- mean_noise_treat$mean_day[mean_noise_treat$after_treat == 1]
after_night <- mean_noise_treat$mean_night[mean_noise_treat$after_treat == 1]

##### LDEN
level_by_month_lden <- ggplot(noise_summary_month)+
    geom_line(
        aes(
            x = plot_date,
            y = mean_noise_lden,
            col = "mean_noise_lden",
            group = 1
        ),
        size = 1
    )+
    geom_line(
        aes(
            x = plot_date,
            y = mov_avg_lden,
            col = "mov_avg_lden",
            group = 1
        ),
        size = 1
    )+
    scale_color_manual(
        name = "",
        values = c(
            "mean_noise_lden" = "darkblue",
            "mov_avg_lden" = "darkorange3"
        ),
        labels = c(
            "mean_noise_lden" = "Monthly noise level (LDEN)",
            "mov_avg_lden" = "3-month average"
        )
    )+
    scale_y_continuous(
        breaks = seq(67, 74, 1),
        limits = c(67, 74)
    )+
    labs(
        x = "",
        y = "Average noise level (in dB)"
    )+
    scale_x_yearmon(
        breaks = seq(min(noise_summary_month$plot_date), max(noise_summary_month$plot_date), 0.34),
        limits = c(2019.250, 2021.917), labels = month_labels
    )+
    geom_segment(
        aes(
            x = ban,
            xend = ban,
            y = 67.3,
            yend = 73
        ),
        linetype = 3,
        size = 2
    )+
    geom_text(
        aes(
            x = 2020.917,
            y = 73.5,
            label = "Ban of loud freight trains"
        ),
        size = 8
    )+
    owntheme

ggsave(
    plot = level_by_month_lden,
    file.path(
        outputPath,
        "graphs/noise_level_by_month_LDEN.png"
    ),
    height = 10,
    width = 13
)

##### Day & Night
level_by_month_daynight <- ggplot(noise_summary_month)+
    geom_line(
        aes(
            x = plot_date,
            y = mean_noise_day,
            linetype = "mean_noise_day",
            group = 1
        ),
        size = 1
    )+
    geom_line(
        aes(
            x = plot_date,
            y = mean_noise_night,
            linetype = "mean_noise_night",
            group = 1
        ),
        size = 1
    )+
    scale_linetype_manual(
        name = "",
        values = c(
            "mean_noise_day" = "solid",
            "mean_noise_night" = "twodash"
        ),
        labels = c(
            "mean_noise_day" = "Noise level (Day)",
            "mean_noise_night" = "Noise level (Night)")
        )+
    scale_y_continuous(
        breaks = seq(67, 74, 1),
        limits = c(67, 74)
    )+
    labs(
        x = "",
        y = "Average noise level (in dB)"
    )+
    scale_x_yearmon(
        breaks = seq(min(noise_summary_month$plot_date), max(noise_summary_month$plot_date), 0.34),
        limits = c(2019.250, max_date),
        labels = month_labels
    )+
    geom_segment(
        aes(
            x = ban,
            xend = ban,
            y = 67,
            yend = 73.4
        ),
        linetype = "dotted",
        size = 2
    )+
    geom_segment(
        aes(
            x = as.numeric(min(plot_date)),
            xend = ban,
            y = before_day,
            yend = before_day
        ),
        linetype = "solid",
        size = 1.5
    )+
    geom_segment(
        aes(
            x = as.numeric(min(plot_date)),
            xend = ban,
            y = before_night,
            yend = before_night
        ),
        linetype = "twodash",
        size = 1.5
    )+
    geom_segment(
        aes(
            x = ban,
            xend = max_date,
            y = after_day,
            yend = after_day
        ),
        linetype = "solid",
        size = 1.5
    )+
    geom_segment(
        aes(
            x = ban,
            xend = max_date,
            y = after_night,
            yend = after_night
        ),
        linetype = "twodash",
        size = 1.5
    )+
    geom_text(
        aes(
            x = ban,
            y = 73.7,
            label = "Ban of loud freight trains"
        ),
        size = 10
    )+
    owntheme

ggsave(
    plot = level_by_month_daynight,
    file.path(
        outputPath,
        "graphs/noise_level_by_month_daynight.png"
    ),
    height = 10,
    width = 13
)

# plot average number of trains -------------------------------------------

##### LDEN
num_trains_month <- ggplot(noise_summary_month)+
    geom_line(
        aes(
            x = year_mon,
            y = mean_num_freight_lden,
            col = "freight",
            group = 1
        ),
        size = 1
    )+
    geom_line(
        aes(
            x = year_mon,
            y = mean_num_pass_lden,
            col = "passenger",
            group = 1
        ),
        size = 1
    )+
    scale_color_manual(
        name = "",
        values = c(
            "freight" = "seagreen",
            "passenger" = "firebrick2"
        ),
        labels = c(
            "freight" = "Freight trains",
            "passenger" = "Passenger trains"
        )
    )+
    scale_y_continuous(
        breaks = seq(80, 220, 20),
        limits = c(65, 220)
    )+
    labs(
        x = "",
        y = "Average number of trains"
    )+
    geom_segment(
        mapping = aes(
            x = "2020-12",
            xend = "2020-12",
            y = 68,
            yend = 220
        ),
        size = 1
    )+
    geom_text(
        x = "2020-12",
        y = 222,
        label = "Ban of loud freight trains",
        size = 6
    )+
    owntheme

ggsave(
    plot = num_trains_month,
    file.path(
        outputPath,
        "graphs/num_trains_by_month_LDEN.png"
    ),
    height = 10,
    width = 13
)

##### Passings with respect to COVID-19

# average before and after COVID-19 (March 2020 as cutoff)
avg_passing_covid <- noise_summary |>
    dplyr::filter(date < "2020-03-01" | date > "2020-03-31") |>
    dplyr::mutate(
        covid_period = dplyr::case_when(
            date < "2020-03-01" ~ "before covid",
            date > "2020-03-31" ~ "after covid"
        )
    ) |>
    dplyr::group_by(covid_period) |>
    dplyr::summarise(
        mean_num_pass = mean(mean_num_pass_lden, na.rm = TRUE),
        mean_num_freight = mean(mean_num_freight_lden, na.rm = TRUE)
    ) |>
    as.data.frame()

# export
openxlsx::write.xlsx(
    avg_passing_covid,
    file.path(
        outputPath,
        "descriptives",
        "avg_passing_covid.xlsx"
    )
)

# plot labels
breaks <- seq(as.Date("2019-04-01"), as.Date("2022-03-01"), by = "2 months")
breaks <- format(breaks, "%Y-%m")

# redo previous plot with COVID-19
num_trains_month_covid <- ggplot(noise_summary_month)+
    geom_line(
        aes(
            x = plot_date,
            y = mean_num_freight_lden,
            col = "freight",
            group = 1.2
        ),
        size = 1
    )+
    geom_line(
        aes(
            x = plot_date,
            y = mean_num_pass_lden,
            col = "passenger",
            group = 1.2
        ),
        size = 1
    )+
    scale_color_manual(
        name = "",
        values = c(
            "freight" = "seagreen",
            "passenger" = "firebrick2"
        ),
        labels = c(
            "freight" = "Freight trains",
            "passenger" = "Passenger trains"
        )
    )+
    scale_y_continuous(
        breaks = seq(0, 220, 20),
        limits = c(0, 230)
    )+
    scale_x_yearmon(
        breaks = seq(min(noise_summary_month$plot_date), max(noise_summary_month$plot_date), 0.34),
        labels = month_labels
    )+
    labs(
        x = "",
        y = "Average number of trains"
    )+
    geom_segment(
        # use as.numeric(noise_summary_month$plot_date) to find number for month
        aes(
            x = 2020.167,
            xend = 2020.167,
            y = 0,
            yend = 220
        ),
        linetype = "dashed",
        size = 1
    )+
    geom_text(
        aes(
            x = 2020.167,
            y = 224,
            label = "Start COVID-19"
        ),
        size = 8
    )+
    owntheme

ggsave(
    plot = num_trains_month_covid,
    file.path(
        outputPath,
        "graphs",
        "num_trains_by_month_LDEN_covid.png"
    ),
    height = 10,
    width = 13
)

##### LDEN Freight trains
num_freight_trains_month <- ggplot(noise_summary_month)+
    geom_line(
        aes(
            x = plot_date,
            y = mean_num_freight_lden,
            col = "freight",
            group = 1
        ),
        size = 2,
        show.legend = FALSE
    )+
    scale_color_manual(
        name = "",
        values = c("freight" = "black")
    )+
    scale_y_continuous(
        breaks = seq(65, 115, 10),
        limits = c(65, 115)
    )+
    scale_x_yearmon(
        breaks = seq(min(noise_summary_month$plot_date), max(noise_summary_month$plot_date), 0.34),
        labels = month_labels
    )+
    labs(
        x = "",
        y = "Average number of trains"
    )+
    geom_segment(
        aes(
            x = 2020.917,
            xend = 2020.917,
            y = 68,
            yend = 110
        ),
        linetype = 3,
        size = 2
    )+
    geom_text(
        aes(
            x = 2020.917,
            y = 112,
            label = "Ban of loud freight trains"
        ),
        size = 8
    )+
    owntheme

ggsave(
    plot = num_freight_trains_month,
    file.path(
        outputPath,
        "graphs/num_freight_trains_by_month_LDEN.png"
    ),
    height = 10,
    width = 13
)

###### Freight trains at day and night
num_freight_trains_day_night <- ggplot(noise_summary_month)+
    geom_line(
        aes(
            x = plot_date,
            y = mean_num_freight_day,
            linetype = "mean_num_freight_day",
            group = 1
        ),
        size = 1
    )+
    geom_line(
        aes(
            x = plot_date,
            y = mean_num_freight_night,
            linetype = "mean_num_freight_night",
            group = 1
        ),
        size = 1
    )+
    scale_linetype_manual(
        name = "",
        values = c(
            "mean_num_freight_day" = "solid",
            "mean_num_freight_night" = "twodash"
        ),
        labels = c(
            "mean_num_freight_day" = "Day",
            "mean_num_freight_night" = "Night"
        )
    )+
    scale_y_continuous(
        breaks = seq(25, 72, 5),
        limits = c(25, 73)
    )+
    labs(
        x = "",
        y = "Number of freight trains"
    )+
    scale_x_yearmon(
        breaks = seq(min(noise_summary_month$plot_date), max(noise_summary_month$plot_date), 0.34),
        limits = c(2019.250, max_date), labels = month_labels
    )+
    geom_segment(
        aes(
            x = ban,
            xend = ban,
            y = 25,
            yend = 72
        ),
        linetype = "dotted",
        size = 2
    )+
    geom_text(
        aes(
            x = ban,
            y = 73,
            label = "Ban of loud freight trains"
        ),
        size = 8
    )+
    geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        aes(
            group = after_treat,
            x = plot_date,
            y = mean_num_freight_day
        ),
        col = "grey60"
    )+
    geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        aes(
            group = after_treat,
            x = plot_date,
            y = mean_num_freight_night
        ),
        col = "grey60"
    )+
    owntheme

ggsave(
    plot = num_freight_trains_day_night,
    file.path(
        outputPath,
        "graphs/num_freight_trains_day_night.png"
    ),
    height = 10,
    width = 13
)

# averages before and after implementation --------------------------------

avg_num_freight <- noise_summary_month |> 
    group_by(after_treat) |> 
    summarise(
        mean_num_day = mean(mean_num_freight_day, na.rm = TRUE),
        mean_num_night = mean(mean_num_freight_night, na.rm = TRUE)
    ) |> 
    as.data.frame()

write.xlsx(
    avg_num_freight,
    file.path(
        outputPath,
        "descriptives/avg_num_freight_trains.xlsx"
    )
)

# compare beginning and end -----------------------------------------------

##### based on monthly data
# Day and night
mean_start <- noise_summary_month %>% 
    filter(year_mon <= "2019-09") %>% 
    summarise(
        mean_day = mean(mean_noise_day),
        mean_night = mean(mean_noise_night)
    ) %>% 
    as.data.frame()

mean_end <- noise_summary_month %>% 
    filter(year_mon >= "2021-05") %>% 
    summarise(
        mean_day = mean(mean_noise_day),
        mean_night = mean(mean_noise_night)
    ) %>% 
    as.data.frame()

# prepare
avg_values <- as.data.frame(rbind(mean_start, mean_end))

#############################################################
# whisper brakes                                            #
#############################################################

whisbrak$perz <- round(whisbrak$perz, digits = 2)

whisplot <- ggplot(whisbrak, aes(x = jahr))+
    geom_line(aes(y = perz), size = 1)+
    geom_point(aes(y = perz), size = 2)+
    geom_text(aes(y = perz, label = perz), hjust = 0.8, vjust = -2)+
    scale_x_continuous(name = "", breaks = seq(2012, 2020, 1))+
    scale_y_continuous(
        name = "Number of wagons with whisper brakes (in %)",
        breaks = seq(7000, 61000, 6000)
    )+
    theme_classic()

ggsave(
    plot = whisplot,
    file.path(
        outputPath,
        "whisper_brakes_db.png"
    ),
    width = 13,
    height = 10
)