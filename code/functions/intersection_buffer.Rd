library(roxygen2)

bf.intersection.fun <- function(df){
  
  ##### documentation
  #' Intersection function with buffers
  #'
  #' \code{bf.intersection.func} returns indicators for housing objects being close to the tracks (based on distance buffers)
  #'
  #' @param df specifies the input data frame (has to be sf format)
  #'
  #' @return Returns the input data frame with distance indicators included
  #'
  #' @export
  
  ##### function itself
  # intersections -----------------------------------------------------------
  int_bf25 <- st_intersects(df, bf25)
  int_bf50 <- st_intersects(df, bf50)
  int_bf100 <- st_intersects(df, bf100)
  int_bf250 <- st_intersects(df, bf250)
  int_bf500 <- st_intersects(df, bf500)
  int_bf750 <- st_intersects(df, bf750)
  int_bf1000 <- st_intersects(df, bf1000)
  int_bf2000 <- st_intersects(df, bf2000)
  int_bf3000 <- st_intersects(df, bf3000)
  int_bf5000 <- st_intersects(df, bf5000)
  
  # add to data -------------------------------------------------------------
  df %>% mutate(# for 25 meters
    bf25 = lengths(int_bf25),
    bf25 = replace(bf25, bf25 != 0, 1),
    bf25 = replace(bf25, bf25 != 1, 0),
    
    # for 50 meters
    bf50 = lengths(int_bf50),
    bf50 = replace(bf50, bf25 == 1, 0),
    bf50 = replace(bf50, bf50 != 0, 1),
    bf50 = replace(bf50, bf50 != 1, 0), 
    
    # for up to 50 meters (0-50)
    bf50_only = 0,
    bf50_only = replace(bf50_only, bf25 == 1, 1),
    bf50_only = replace(bf50_only, bf50 == 1, 1),
    
    # for 100 meters
    bf100 = lengths(int_bf100),
    bf100 = replace(bf100, bf25 == 1 | 
                      bf50 == 1, 0),
    bf100 = replace(bf100, bf100 != 0, 1),
    bf100 = replace(bf100, bf100 != 1, 0),
    
    # for 250 meters
    bf250 = lengths(int_bf250),
    bf250 = replace(bf250, bf25 == 1 | 
                      bf50 == 1 |
                      bf100 == 1, 0),
    bf250 = replace(bf250, bf250 != 0, 1),
    bf250 = replace(bf250, bf250 != 1, 0),
    
    # for 500 meters
    bf500 = lengths(int_bf500),
    bf500 = replace(bf500, bf25 == 1 | 
                      bf50 == 1 |
                      bf100 == 1 |
                      bf250 == 1, 0),
    bf500 = replace(bf500, bf500 != 0, 1),
    bf500 = replace(bf500, bf500 != 1, 0),
    
    # for 750 meters
    bf750 = lengths(int_bf750),
    bf750 = replace(bf750, bf25 == 1 | 
                      bf50 == 1 |
                      bf100 == 1 |
                      bf250 == 1 |
                      bf500 == 1, 0),
    bf750 = replace(bf750, bf750 != 0, 1),
    bf750 = replace(bf750, bf750 != 1, 0),
    
    # for 1000 meters
    bf1000 = lengths(int_bf1000),
    bf1000 = replace(bf1000, bf25 == 1 | 
                       bf50 == 1 |
                       bf100 == 1 |
                       bf250 == 1 |
                       bf500 == 1 |
                       bf750 == 1, 0),
    bf1000 = replace(bf1000, bf1000 != 0, 1),
    bf1000 = replace(bf1000, bf1000 != 1, 0),
    
    # for 2000 meters
    bf2000 = lengths(int_bf2000),
    bf2000 = replace(bf2000, bf25 == 1 | 
                       bf50 == 1 |
                       bf100 == 1 |
                       bf250 == 1 |
                       bf500 == 1 |
                       bf750 == 1 |
                       bf1000 == 1, 0),
    bf2000 = replace(bf2000, bf2000 != 0, 1),
    bf2000 = replace(bf2000, bf2000 != 1, 0),
    
    # for 3000 meters
    bf3000 = lengths(int_bf3000),
    bf3000 = replace(bf3000, bf25 == 1 | 
                       bf50 == 1 |
                       bf100 == 1 |
                       bf250 == 1 |
                       bf500 == 1 |
                       bf750 == 1 |
                       bf1000 == 1 |
                       bf2000 == 1, 0),
    bf3000 = replace(bf3000, bf3000 != 0, 1),
    bf3000 = replace(bf3000, bf3000 != 1, 0),
    
    # for 5000 meters
    bf5000 = lengths(int_bf5000),
    bf5000 = replace(bf5000, bf25 == 1 | 
                       bf50 == 1 |
                       bf100 == 1 |
                       bf250 == 1 |
                       bf500 == 1 |
                       bf750 == 1 |
                       bf1000 == 1 |
                       bf2000 == 1 |
                       bf3000 == 1, 0),
    bf5000 = replace(bf5000, bf5000 != 0, 1),
    bf5000 = replace(bf5000, bf5000 != 1, 0),
    
    
    # for 500 meters (but just 1 and 0)
    bf500_only = lengths(int_bf500),
    bf500_only = replace(bf500_only, bf500_only != 0, 1),
    bf500_only = replace(bf500_only, bf500_only != 1, 0)
  )
}
