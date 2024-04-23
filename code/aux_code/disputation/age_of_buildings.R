
#--------------------------------------------------
# load housing data

red_org <- haven::read_dta("M:/_FDZ/RWI-GEO/RWI-GEO-red/daten/On-site/v9/HK_allVersions_ohneText.dta")

# copy to avoid re-loading
red <- red_org

#--------------------------------------------------
# preparation

# replace missing values with NA
red$baujahr[red$baujahr <= 0] <- NA

# redefine baujahr if < 1500 (because unrealistic value)
red$baujahr[red$baujahr < 1500] <- NA

# generate age of buildings
red$alter <- NA
red$alter <- red$ejahr - red$baujahr
red$alter[red$alter <= 0] <- NA

# calculate average age of buildings
avg_age <- red |>
    group_by(ejahr) |>
    summarise(mean = mean(alter, na.rm = TRUE))

#--------------------------------------------------
# theme
mytheme <- theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 22),
    axis.text.y = element_text(size = 22),
    axis.title = element_text(size = 23),
    axis.line = element_line(colour = "black"),
    legend.text = element_text(size = 15),
    axis.ticks.length = unit(0.25, "cm"),
    legend.key = element_blank(),
    legend.position = "bottom"
)

linethick <- 1

#--------------------------------------------------
# create plot

plot <- ggplot()+
    geom_line(
        data = avg_age,
        aes(x = ejahr, y = mean),
        linewidth = linethick
    )+
    scale_x_continuous(
        breaks = seq(2007, 2023, 1)
    )+
    labs(
        x = "",
        y = "Average Age of Buildings (in years)"
    )+
    mytheme

ggsave(
    plot = plot,
    file.path(
        outputPath,
        "graphs",
        "disputation",
        "avg_age_buildings.png"
    ),
)
