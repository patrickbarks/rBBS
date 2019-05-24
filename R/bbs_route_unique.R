#' Identify survey-wide unique BBS routes from a BBS table
#' 
#' The \code{route} column in a variety of BBS tables only identifies unique
#' routes within states. This function creates route identifiers that are unique
#' survey-wide by pasting together columns \code{country_num}, \code{state_num},
#' and \code{route} (after first adding leading zeros to ensure a constant
#' number of digits for each component column).
#'
#' @param df A \code{data.frame} created by a \code{bbs_*} function that
#'   includes columns \code{country_num}, \code{state_num}, and \code{route}.
#'
#' @return
#' An integer vector identifying unique routes survey-wide.
#' 
#' @details
#' Before the component columns are pasted together and reconverted to an
#' integer, leading zeros are added, where necessary, to ensure constant columns
#' widths (and therefore unique final codes). The number of digits for each
#' component column is:
#'  - \code{country_num}: 3 digits
#'  - \code{state_num}: 2 digits
#'  - \code{route}: 3 digits
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
#' weather <- bbs_meta_weather(bbs_dir = ".")
#' 
#' # add a column identifying unique routes
#' weather$route_unique <- bbs_route_unique(weather)
#' }
#' 
#' @export bbs_route_unique
bbs_route_unique <- function(df) {
  
  if (!("data.frame" %in% class(df) | "tbl_df" %in% class(df))) {
    stop("Expecting df to be of class data.frame (and/or tbl_df)")
  }
    
  x_req <- c("country_num", "state_num", "route")
  
  if (!all(x_req %in% names(df))) {
    stop(paste("Expecting df to have all of the following columns:",
         paste(x_req, collapse = ", ")))
  }
  
  x <- as.integer(
    paste0(
      formatC(df$country_num, width = 3, flag = "0"),
      formatC(df$state_num, width = 2, flag = "0"),
      formatC(df$route, width = 3, flag = "0")
    )
  )
  
  return(x)
}
