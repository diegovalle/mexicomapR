#' Creates a static map
#'
#' Calls \code{mxmap_state} or \code{mxmap_municipality} to generate a static map. 
#' 
#' @param type Type of map to create (data by state or municipality)
#' @param df Data frame or vector with information to paint geography. The data must contain an "id" and a "value" column to be matched.
#' @param geography Name of geography (i.e. "Aguascalientes"). Names are searched in \code{data(mxgeography)}. Default is set to = "all", to create a country-wide map.
#' @author Eduardo Flores
#' @return ggplot object
#' @seealso mxmap_state_p, mxmap_state_l, mxmap_state_g, mxmap_municipality_p, mxmap_municipality_l, mxmap_municipality_g
#' @examples
#' \dontrun{
#' population_choroplethr <- mxmap("state", data(mxpop2010))
#' }
#' @export
#'
mxmap <- function(type, df, geography = "all", method = "ggplot"){
  type <- tolower(type)

  if(type == "state"){
    if(method == "ggplot"){
      m <- mexicomapR::mxmap_state_p(df)  
    }else{
      if(method == "leaflet"){
        m <- mexicomapR::mxmap_state_l(df)
      }else{
        if(method == "google"){
          m <- mexicomapR::mxmap_state_g(df)
        }else{stop("Metod not recognized. Use gglot, leaflet or google")}
        
      }
    }
  }else{
  if(type == "municipality"){
    if(method == "ggplot"){
      m <- mexicomapR::mxmap_municipality_g(df)  
    }else{
      if(method == "leaflet"){
        m <- mexicomapR::mxmap_municipality_l(df)
      }else{
        if(method == "google"){
          m <- mexicomapR::mxmap_municipality_g(df)
        }else{stop("Metod not recognized. Use gglot, leaflet or google")}
        
      }
    }
  }else{stop("Geography not recognized. Use state or municipality")}
  }
  
  return(m)
}


#' Creates state map
#'
#' Generates a static state map 
#' @param df Data frame or vector with information to paint geography. The data must contain an "id" and a "value" column to be matched.
#' @author Eduardo Flores
#' @return ggplot object
#' @seealso mxmap
#' @examples
#' \dontrun{
#' population_choroplethr <- mxmap("state", data(mxpop2010))
#' }
#' @export
#'
mxmap_state_p <- function(df){
  
  if(class(df) == "data.frame" | 
     class(df) == "matrix"){
      # clean and fix data frame to merge
      d <- as.data.frame(df)
      names(d) <- base::tolower(names(d))
      if(!("id" %in% names(d))){stop("id column not found in data")}
      if(!("value" %in% names(d))){stop("value column not found in data")}
      
      # get geography to merge 
      geo <- data("mxstates", package = "mexicomapR")
      
      d <- base::merge(d, mxstates)
      m <- ggplot(d, 
                  aes(long, 
                      lat, group = group)) +
          geom_polygon(aes(fill = value))  
        
  }else{
   stop("df must be data.frame or matrix")
  }
  return(m)
}

#' Creates ggplot municipality map
#'
#' Generates a static state map 
#' @param df Data frame or vector with information to paint geography. The data must contain an "id" and a "value" column to be matched.
#' @author Eduardo Flores
#' @return ggplot object
#' @seealso mxmap
#' @examples
#' \dontrun{
#' population_choroplethr <- mxmap("municipality", data(mxpop2010))
#' }
#' @export
#'
mxmap_municipality_p <- function(df){
  
  if(class(df) == "data.frame" | 
     class(df) == "matrix"){
    # clean and fix data frame to merge
    d <- as.data.frame(df)
    names(d) <- base::tolower(names(d))
    if(!("id" %in% names(d))){stop("id column not found in data")}
    if(!("value" %in% names(d))){stop("value column not found in data")}
    
    # get geography to merge 
    geo <- data("mxmunicipality", package = "mexicomapR")
    
    d <- base::merge(d, geo)
    m <- ggplot(d, 
                aes(long, 
                    lat, group = group)) +
      geom_polygon(aes(fill = value))  
    
  }else{
    stop("df must be data.frame or matrix")
  }
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
  
  if(class(df) == "data.frame" | 
       class(df) == "matrix"){
    # clean and fix data frame to merge
    d <- as.data.frame(df)
    names(d) <- base::tolower(names(d))
    if(!("id" %in% names(d))){stop("id column not found in data")}
    if(!("value" %in% names(d))){stop("value column not found in data")}
    
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
    
  
    
  }else{
    stop("df must be data.frame or matrix")
  }
  return(mx)
}






#' Creates state catalog
#'
#' Uses \code{data(mxgeography)} to generate a data.frame with two columns: id as factor and name of state, on which to join data. This assures the user will use the correct id's to pass to \code{mxmap()}.
#' @author Eduardo Flores
#' @return data.frame
#' @seealso mxgeography
#' @examples
#' mxstate_catalog()
#' @export
#'
mxstate_catalog <- function(){
  #mxgeography
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




