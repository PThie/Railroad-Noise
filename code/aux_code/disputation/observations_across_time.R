############################################################
# Load Data                                                #
############################################################

hk <- readRDS(file.path(dataPath, "housing/Temp/hk_buffered.rds"))

############################################################
# Preparation                                              #
############################################################

# drop geometry
hk <- sf::st_drop_geometry(hk)

# theme
mytheme <- theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 16),
    axis.line = element_line(colour = "black"),
    legend.text = element_text(size = 15),
    axis.ticks.length = unit(0.25, "cm"),
    legend.key = element_blank(),
    legend.position = "bottom"
)

linethick <- 1

############################################################
# Number of observations                                   #
############################################################

# calculate the number of observations per year and group
obs <- hk |>
    dplyr::group_by(jahr, bf500_only) |>
    dplyr::summarise(
        n = n()
    ) |>

# create plot
ggplot()+
    geom_line(
        data = obs,
        aes(
            x = jahr,
            y = n,
            linetype = as.factor(bf500_only)
        ),
        linewidth = linethick
    )+
    scale_x_continuous(
        breaks = seq(2013, 2021, 1)
    )+
    scale_y_continuous(
        breaks = seq(0, max(obs$n), 50000),
        labels = scales::comma
    )+
    scale_linetype_manual(
        values = c("0" = "solid", "1" = "dashed"),
        labels = c("0" = "Control", "1" = "Treated")
    )+
    labs(
        x = "",
        y = "Number of Observations"
    )+
    mytheme
