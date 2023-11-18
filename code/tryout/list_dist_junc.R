tst <- red_grid[1:20, ]

dist_junc <- function(df.sf){
  as.numeric(apply(st_distance(df.sf, mw_junc.sf), 1, min))/1000
}


# create list by year (as group variable)
tst.ls <- tst %>% group_split(ajahr)

x <- lapply(tst.ls, dist_junc)

x.df <- plyr::ldply(x, data.frame)

colnames(x.df) <- "distance_junction"

tst <- cbind(tst, x.df)

order_junc <- function(df.sf){
  apply(st_distance(df.sf, mw_junc.sf), 1, which.min)
}

ordering_junc <- lapply(tst.ls, order_junc)

# make df
ordering_junc.df <- plyr::ldply(ordering_junc, data.frame)
ordering_junc.vec <- ordering_junc.df$X..i..


tst$closest_junction <- mw_junc.sf$name[ordering_junc.vec]



colnames(ordering_junc.df) <- "osm_id"

ordering_junc.df$osm_id <- as.character(ordering_junc.df$osm_id)

# merge junciton names
ordering_junc.df <- merge(ordering_junc.df, y, by = "osm_id")

