#' Checks and cleans format of data.frame to be merged with fortified data
#'
#' Checks unique id's, format of value and other possible issues.  
#' 
#' @param df Data.frame with id and value columns
#' @param vcol Value column with standard evaluation. Defaults to "value".
#' @param nobject length of type of map (32 for state or 2457 for municipality)
#' @author Eduardo Flores
#' @examples
#' 
#' @export
#'
#'
mx_checkdf <- function(df, vcol = "value", nobject = 32){
 # Check if df is a data.frame and has the proper names 
  if(base::class(df) == "data.frame"){
    # clean and fix data frame
    base::names(df) <- base::tolower(names(df))
    
    if(!("id" %in% base::names(df))){stop("id column not found in data")}
    if(!(vcol %in% base::names(df))){stop("value column not found in data")}
  }
  
  # Check NA in value columns. 
  row.has.na <- apply(df, 1, function(x) {any(is.na(x))})
  if (sum(row.has.na) > 0) {r <- TRUE}else{r <- FALSE}
 
  # the object "r" has a false or true parameter...
  # now, we validate.
  if(r){stop("NAs in data.frame")}
  
  # Check if all the ids are unique 
  if(length(unique(df$id)) == length(df$id)){}else{stop("ids should be unique")}
  
  # Check length matches type of map
  if(nobject == length(df$id)){}else{
    stop("number of ids do not match map type (must have 32 for states or 2,457 for municipalities)")}

  # all checks passed...
}