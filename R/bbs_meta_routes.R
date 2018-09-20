#' Get meta-data about BBS routes
#' 
#' Gets meta-data about BBS routes
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#'
#' @return
#' \code{data.frame} with the following columns:
#'   \item{country_num}{Integer code for country: 124 (Canada), 484 (Mexico), or
#'   840 (USA)}
#'   \item{state_num}{Integer code for state}
#'   \item{route}{Integer code for route}
#'   \item{route_name}{Name of route}
#'   \item{active}{Integer code for whether route is active: 1 if active (note:
#'   might be active, but not sampled)}
#'   \item{latitude}{Latitude of route starting point}
#'   \item{longitude}{Longitude of route starting point}
#'   \item{stratum}{The BBS physiographic stratum code for that route. }
#'   \item{bcr}{Bird Conservation Region}
#'   \item{route_type_id}{Route substrate; 1 = Roadside, 2 = Water, 3 = Off-road}
#'   \item{route_type_detail_id}{Indicates route length and selection criteria}
#' 
#' @details See 'bbs_dir/RouteInf.txt' for documentation.
#' @author Bob O'Hara
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#' 
#' @examples
#' routes <- bbs_meta_routes()
#' 
#' @export bbs_meta_routes
bbs_meta_routes <- function(bbs_dir = NULL) {
  
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  bbs_dir <- '~/bbs_data/'
  zip_path <- paste0(bbs_dir, "/routes.zip")

  out <- csv_unzip(paste0(bbs_dir, "/routes.zip"))
  names(out) <- tolower(names(out))
  
  return(out)
}
