###################################################################
# description                                                     #
###################################################################

# downloads motorway links for Germany using Open Street Map data
# prepares the files
# and outputs one complete file with all the motorway links (Autobahnauf-/abfahrten)
###################################################################
# libraries                                                       #
###################################################################

# specific to this file
library(ggmap)
library(osmdata)

###################################################################
# function                                                        #
###################################################################

# construct a query for motorway links as function 
osmfun <- function(area){
  getbb(place_name = area) %>% 
    opq(timeout = 100) %>% 
    add_osm_feature(key = "highway", value = "motorway_link") 
}

readosm <- function(area){
  # read osm data and add the area (state name)
  # then drop variables which are not needed
  st_read(file.path(dataPath, paste0("motorway_link/", area, ".osm")), 
          layer = "points") %>% 
    mutate(state = area) %>% 
    select(c(osm_id, name, highway, state, geometry))
}

###################################################################
# download motorway links by states                               #
###################################################################
# it works better than the states are split up
# probably something to do with the requests made to the server

# define states
state_names_part1 <- c("Bayern", "Baden-Württemberg",
                       "Berlin", "Bremen", 
                       "Hamburg", "Hessen", 
                       "Mecklenburg-Vorpommern", "Niedersachsen") 

state_names_part2 <- c("Nordrhein-Westfalen", "Rheinland-Pfalz",
                       "Saarland", "Sachsen", "Sachsen-Anhalt",
                       "Schleswig-Holstein")

state_names_part3 <- c("Brandenburg", "Thüringen")

# apply osm query function to each state
query.state.part1.ls <- lapply(state_names_part1, osmfun)
query.state.part2.ls <- lapply(state_names_part2, osmfun)
query.state.part3.ls <- lapply(state_names_part3, osmfun)

# replace list element names with the state names
names(query.state.part1.ls) <- c("Bayern", "Baden-Württemberg",
                                 "Berlin", "Bremen", 
                                 "Hamburg", "Hessen", 
                                 "Mecklenburg-Vorpommern", "Niedersachsen")

names(query.state.part2.ls) <- c("Nordrhein-Westfalen", "Rheinland-Pfalz",
                                 "Saarland", "Sachsen", "Sachsen-Anhalt",
                                 "Schleswig-Holstein")

names(query.state.part3.ls) <- c("Brandenburg", "Thüringen")

# export the osm data
lapply(names(query.state.part1.ls), function(x){
  f <- query.state.part1.ls[[x]]
  osmdata_xml(f, paste0(dataPath, "motorway_link/", x, ".osm"))})

lapply(names(query.state.part2.ls), function(x){
  f <- query.state.part2.ls[[x]]
  osmdata_xml(f, paste0(dataPath, "motorway_link/", x, ".osm"))})

lapply(names(query.state.part3.ls), function(x){
  f <- query.state.part3.ls[[x]]
  osmdata_xml(f, paste0(dataPath, "motorway_link/", x, ".osm"))})

###################################################################
# read the osm data back in                                       #
###################################################################
# aim: read osm data in (as sf) and clean it

# define state names (complete list of all 16)
state_names <- c("Bayern", "Baden-Württemberg",
                 "Berlin", "Bremen", 
                 "Hamburg", "Hessen", 
                 "Mecklenburg-Vorpommern", "Niedersachsen",
                 "Nordrhein-Westfalen", "Rheinland-Pfalz",
                 "Saarland", "Sachsen", "Sachsen-Anhalt",
                 "Schleswig-Holstein", "Brandenburg", "Thüringen")

# load the data (returns a list with a state per element)
motor.states.ls <- lapply(state_names, readosm)

# transform from list class into data frame
motor.states.df <- plyr::ldply(motor.states.ls, data.frame)

###################################################################
# export                                                          #
###################################################################

saveRDS(motor.states.df, file.path(dataPath, "motorway_link/ger_motorway_links.rds"))
