#' Creates a map
#'
#' Calls \code{mxmap_state} or \code{mxmap_municipality} to generate a static map. 
#' 
#' @param type Type of map to create (data by state or municipality)
#' @param df Data frame or vector with information to paint geography. The data must contain an "id" and a "value" column to be matched.
#' @param method Method to be used (static for ggplot2 object, leaflet for leaflet object)
#' @author Eduardo Flores
#' @return ggplot object
#' @seealso mxmap_state_p, mxmap_state_l, mxmap_state_g, mxmap_municipality_p, mxmap_municipality_l, mxmap_municipality_g
#' @examples
#' # A static state map with random data 
#' data(states_catalog, package = "mexicomapR")
#' df <- data.frame("id" = states_catalog$id, "value" = runif(n = 32, min = 1, max = 100))
#' mxmap(type = "state", df)
#' @export
#'
mxmap <- function(type, df, method = "static", vcol = "value"){
  type <- tolower(type)
  # Function designed to call others. 
  # A standard way to create many types of maps.

  if(type == "state"){
    if(method == "static"){
      message("Using mxmap_state_s")
      m <- mexicomapR::mxmap_state_s(df, vcol = vcol)  
    }else{
      if(method == "leaflet"){
        message("Using mxmap_state_l")
        m <- mexicomapR::mxmap_state_l(df, vcol = vcol)}else{
          stop("Metod not recognized. Use static, leaflet or google")}
    }
  }else{
  if(type == "municipality"){
    if(method == "static"){
      message("Using mxmap_municipality_s")
      m <- mexicomapR::mxmap_municipality_s(df, vcol = vcol)  
    }else{
      if(method == "leaflet"){
        message("Using mxmap_municipality_l")
        m <- mexicomapR::mxmap_municipality_l(df, vcol = vcol)
      }else{
        stop("Metod not recognized. Use static, leaflet or google")}
      }
    }else{stop("Geography not recognized. Use state or municipality")}
  }
  
  return(m)
}


#' Creates static state map
#'
#' Generates a static (ggplot2 object) state map. 
#' @param df Data frame or vector with information to paint geography. The data must contain an "id" and a "value" column to be matched.
#' @param vcol Value column with standard evaluation. Defaults to "value".
#' @author Eduardo Flores
#' @return ggplot object
#' @seealso mxmap
#' @examples
#' # A state map with random data 
#' utils::data(states_catalog, package = "mexicomapR")
#' df <- data.frame("id" = states_catalog$id, "value" = runif(n = 32, min = 1, max = 100))
#' mxmap_state_s(df)
#' @export
#'
mxmap_state_s <- function(df, vcol = "value"){
  # Type of map
  nobject <- 32
    
  # checks the data.frame for common mistakes. 
  # This is a function in backend_functions.R
  mexicomapR::mx_checkdf(df, vcol = vcol, nobject = nobject)
  
  # change name of value column, to be used by ggplot
  names(df)[names(df) == vcol] <- "value"
  
      # get geography to merge 
      utils::data("states", 
                  package = "mexicomapR", 
                  envir = e <- new.env())
      geo <- sp::spTransform(x = e$states, 
                         CRSobj = CRS("+proj=longlat +datum=WGS84"))
      geo <- ggplot2::fortify(geo)
      
      d <- base::merge(df, geo, by ="id")
      m <- ggplot2::ggplot(d, 
                           ggplot2::aes(long, 
                                        lat, 
                                        group = group)) +
          geom_polygon(ggplot2::aes(fill = value))
      
  return(m)
}

#' Creates static municipality map
#'
#' Generates a static (ggplot2 object) municipality map. 
#' @param df Data frame or vector with information to paint geography. The data must contain an "id" and a "value" column to be matched.
#' @param vcol Value column with standard evaluation. Defaults to "value".
#' @author Eduardo Flores
#' @return ggplot object
#' @seealso mxmap
#' @examples
#' # A municipality map with random data 
#' utils::data(municipality_catalog, package = "mexicomapR")
#' df <- data.frame("id" = municipality_catalog$id, "value" = runif(n = 2457, min = 1, max = 100))
#' mxmap_municipality_s(df)
#' @export
#'
mxmap_municipality_s <- function(df, vcol = "value"){
  # Type of map
  nobject <- 2457
  
  # checks the data.frame for common mistakes. 
  # This is a function in backend_functions.R
  mexicomapR::mx_checkdf(df, vcol = vcol, nobject = nobject)
  
  # change name of value column, to be used by ggplot
  names(df)[names(df) == vcol] <- "value"
  
  # get geography to merge 
  utils::data("municipality", 
              package = "mexicomapR", 
              envir = e <- new.env())
  geo <- sp::spTransform(x = e$municipality, 
                         CRSobj = CRS("+proj=longlat +datum=WGS84"))
  geo <- ggplot2::fortify(geo)
  
  d <- base::merge(df, geo, by ="id")
  m <- ggplot2::ggplot(d, 
                       aes(long, 
                           lat, group = group)) +
    geom_polygon(aes(fill = value))
  
  return(m)
}

#' Creates leaflet state map
#'
#' Generates a leaflet state map 
#' @param df Data frame or vector with information to paint geography. The data must contain an "id" and a "value" column to be matched.
#' @author Eduardo Flores
#' @return htmlwidget object
#' @seealso mxmap
#' @examples
#' \dontrun{
#' # change... 
#' population_choroplethr <- mxmap("municipality", data(mxpop2010))
#' }
#' @export
#'
mxmap_state_l <- function(df){
  # Type of map
  nobject <- 32
  
  # checks the data.frame for common mistakes. 
  # This is a function in backend_functions.R
  mexicomapR::mx_checkdf(df, vcol = vcol, nobject = nobject)
  
  
  
  
  
    # get geography to merge 
    # change 
    # geo <- data("mxmunicipality", package = "mexicomapR")
    
    
    m <- data.frame("long" = mxstates$long, 
                    "lat" = mxstates$lat)
    # debe haber un NA entre cada una ... 
    m <- as.matrix(m)
    
    mx <- leaflet() %>% 
      addTiles() %>% 
      setView(-101.5, 21.085, zoom = 5)
    
    #puedes poner estado, por estado... 
    #aguas con los hoyos
    m <- subset(mxstates, id == "19")
    m2 <- subset(mxstates, id == "30")
    mx %>% 
      addPolygons(lng = m$long, lat = m$lat) %>%
      addPolygons(lng = m2$long, lat = m2$lat)
  
  return(mx)
}


#' Creates municipality catalog
#'
#' Uses \code{data(mxgeography)} to generate a data.frame with two columns: id as factor and name of municipality, on which to join data. This assures the user will use the correct id's to pass to \code{mxmap()}.
#' @author Eduardo Flores
#' @return data.frame
#' @seealso mxgeography
#' @examples
#' mxmunicipality_catalog()
#' @export
#'
mxmunicipality_catalog <- function(){
  #mxgeography
}




