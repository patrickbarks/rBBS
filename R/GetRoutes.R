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
#'   \item{countrynum}{Integer code for country: 124 (Canada), 484 (Mexico), or
#'   840 (USA)}
#'   \item{statenum}{Integer code for state}
#'   \item{Route}{Integer code for route}
#'   \item{Active}{Integer code for whether route is active: 1 if active (note:
#'   might be active, but not sampled)}
#'   \item{Lati}{Latitude}
#'   \item{Longi}{Longitude}
#'   \item{Stratum}{The BBS physiographic stratum code for that route. }
#'   \item{BCR}{Bird Conservation Region}
#'   \item{LandTypeID}{Integer for type of land that a route is located on}
#'   \item{RouteTypeID}{Route substrate; 1 = Roadside, 2 = Water, 3 = Off-road}
#'   \item{RouteTypeDetailId}{Indicates route length and selection criteria}
#'   \item{routeID}{Route ID (mainly for internal use, to make sure routes are
#'   unique)}
#' 
#' @details See 'bbs_dir/RouteInf.txt' for documentation.
#' @author Bob O'Hara
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#' 
#' @examples
#' Routes <- GetRoutes()
#' 
#' @export GetRoutes
GetRoutes <- function(bbs_dir = NULL) {
  
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  routes=GetUnzip(ZipName=paste0(bbs_dir, "routes.zip"), FileName="routes.csv")
  routes$routeID=paste(routes$statenum, routes$Route)
  routes
}
