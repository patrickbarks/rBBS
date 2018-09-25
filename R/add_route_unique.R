#' Add a column to a BBS table identifying unique routes survey-wide
#' 
#' Add a column to a BBS table identifying unique routes survey-wide. The
#' \code{route} column in a variety of BBS tables only identifies unique routes
#' within states. This function creates route identifiers that are unique
#' survey-wide by pasting together columns \code{country_num}, \code{state_num},
#' and  \code{route}.
#'
#' @param df A \code{data.frame} created by a \code{bbs_*} function that
#'   includes columns \code{country_num}, \code{state_num}, and \code{route}.
#' @param col_name Name for the new column. Defaults to "route_unique".
#'
#' @return
#' \code{df} with an additional column that identifies unique routes
#' survey-wide. The additional column is called \code{name} and is of class
#' \code{integer}.
#' 
#' @author Bob O'Hara
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @references Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding and M.-A.R.
#'   Hudson. 2018. North American Breeding Bird Survey Dataset 1966-2017,
#'   version 2017.0. U.S. Geological Survey, Patuxent Wildlife Research Center.
#'   \url{https://doi.org/10.5066/F76972V8}
#' 
#' @examples
#' \dontrun{
#' 
#' # load the metadata table 'weather'
#' weather <- add_route_unique(bbs_dir = ".")
#' 
#' # add a column identifying unique routes
#' weather <- add_route_unique(weather)
#' }
#' 
#' @export add_route_unique
add_route_unique <- function(df, col_name = "route_unique") {
  
  if (!("data.frame" %in% class(df) | "tbl_df" %in% class(df))) {
    stop("Expecting df to be of class data.frame (and/or tbl_df)")
  }
  if (length(col_name) != 1 | class(col_name) != 'character') {
    stop("Argument name should be a single character-string")
  }
  if (col_name %in% names(df)) {
    stop("Argument name matches an existing column of df")
  }
    
  x_req <- c("country_num", "state_num", "route")
  
  if (!all(x_req %in% names(df))) {
    stop(paste("Expecting df to have all of the following columns:",
         paste(x_req, collapse = ", ")))
  }
  
  df[[col_name]] <- as.integer(paste0(df$country_num,
                                      df$state_num,
                                      df$route))
  
  return(df)
}
