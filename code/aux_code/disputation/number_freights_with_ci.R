#--------------------------------------------------
# noise levels object

# NOTE: generate in 01_03_noise_environment.R

#--------------------------------------------------
# preparation

# keep only freight train stats (number of freight trains)
freight_trains <- noise_levels |>
    dplyr::select(year_mon, num_freight_day, num_freight_night) |>
    tidyr::pivot_longer(
        !year_mon,
        names_to = "period",
        values_to = "num_freight"
    )

# add plot date
freight_trains$plot_date <- as.yearmon(freight_trains$year_mon)

# month labels
month_labels <- c(
    "Apr 2019", "Aug 2019", "Dec 2019", "Apr 2020", "Aug 2020", "Dec 2020",
    "Apr 2021", "Aug 2021", "Dec 2021"
)

# max date (limited to December 2021)
max_date <- 2021.917

# ban of freight trains
ban <- as.yearmon("2020-12")

# define own plotting theme
owntheme <- theme(
    axis.text.x = element_text(size = 25, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 27, vjust = 2),
    axis.text.y = element_text(size = 25),
    panel.background = element_rect(colour = "white", fill = "white"),
    axis.line = element_line(linewidth = 0.5, linetype = "solid", color = "black"),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 25),
    axis.ticks.length = unit(0.25, "cm"),
    legend.key = element_blank(),
    legend.position = "bottom"
)

#--------------------------------------------------
# generate plot

freight_plot <- ggplot(
    data = freight_trains,
    aes(y = num_freight, x = plot_date, group = period)
)+
    # create confidence intervals
    stat_summary(
        geom = "ribbon",
        fun.data = mean_cl_normal,
        #conf.int = 0.95,
        fill = "grey70"
    )+
    stat_summary(
        geom = "line",
        fun.y = mean,
        linetype = "dashed"
    )+
    stat_summary(
        geom = "point",
        fun.y = mean,
        color = "black"
    )+
    scale_y_continuous(
        breaks = seq(25, 72, 5)
    )+
    labs(
        x = "",
        y = "Number of freight trains"
    )+
    scale_x_yearmon(
        breaks = seq(min(freight_trains$plot_date), max(freight_trains$plot_date), 0.34),
        labels = month_labels
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
    owntheme

ggsave(
    plot = freight_plot,
    file.path(
        outputPath,
        "graphs",
        "disputation",
        "num_freight_trains_day_night_ci.png"
    )
)
