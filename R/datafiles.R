#' Catalog of geographical names and INEGI equivalents
#'
#' A data catalog that matches some common descriptors and it's official geographical equivalents from INEGI.
#'
#' \itemize{
#'   \item mun. Name of Municipality, as defined by INEGI
#'   \item mun_id. Municipality id in mxmunicipios
#'   \item city. City as defined arbitrarily
#'   \item met. Metropolitan zone, defined arbitrarily, can be matched with names in price index of inegiR (inflacion_ciudades)
#'   \item name_2. Name of Municipality, as defined internally
#'   \item st. State name as defined by INEGI
#'   \item st_name. State namem defined arbitrarily to be matched with inegiR
#'   \item st_id. State id
#'   \item zn. Zone, as defined by INEGI: \url{http://www.inegi.org.mx/est/contenidos/proyectos/cn/itaee/default.aspx}.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name mxgeography
#' @usage data(mxgeography)
#' @format A data frame with 9 variables
NULL

#' Mexican states
#'
#' A fortified data.frame with Mexican state borders, to be passed to ggplot.
#'
#' \itemize{
#'   \item long. Longitud
#'   \item lat. Latitud
#'   \item group. Group to identify same state coordinates
#'   \item id. Unique state id to match data.frames with information
#' }
#'
#' @docType data
#' @keywords datasets
#' @name mxstates
#' @usage data(mxstates)
#' @format A data frame with 4 variables
NULL

#' Mexican municipalities
#'
#' A fortified data.frame with Mexican municipality borders, to be passed to ggplot.
#'
#' \itemize{
#'   \item long. Longitud 
#'   \item lat. Latitud
#'   \item group. Group to identify same coordinates
#'   \item id. Unique municipality id to match data.frame with information (number is a concatenation of INEGI entidad id and municipality id)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name mxmunicipality
#' @usage data(mxmunicipality)
#' @format A data frame with 9 variables
NULL