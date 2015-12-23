#' Attempts to fix INEGI number to description of geography
#'
#' Calls \code{data(mxgeography)} and attempts to translate geography names to INEGI codes. 
#' 
#' @param vector Vector of names or descriptions
#' @author Eduardo Flores
#' @examples
#' 
#' @export
#'
#'
mxfind_id <- function(vector){
  v <- tolower(vector)
  v <- unique(v)
}