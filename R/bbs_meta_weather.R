#' Read in weather and other meta-data
#' 
#' Reads Weather meta-data in BBS, from Weather.zip. Contains other meta-data
#' too, so is useful elsewhere
#' 
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#'
#' @return
#' \code{data.frame} with the following columns:
#'   \item{CountryNum}{Three digit code that identifies country: 124 (Canada),
#'   484 (Mexico), or 840 (USA)}
#'   \item{StateNumber}{Two digit numerical code that identifies the state,
#'   province, or territory}
#'   \item{Route}{Three digit code that identifies the route.}
#'   \item{RPID}{The run protocol identification number.}
#'   \item{Year}{The year.}
#'   \item{Month}{The month the route was surveyed (1-12).}
#'   \item{Day}{The day the route was surveyed (1-31).}
#'   \item{ObsN}{Unique observer identification number.}
#'   \item{TotalSpp}{The total number of species recorded on that run of the
#'   route.}
#'   \item{StartTemp}{The temperature recorded at the beginning of the run of
#'   the route.}
#'   \item{EndTemp}{The temperature recorded at the end of the run of the
#'   route.}
#'   \item{TempScale}{The scale in which the temperatures were recorded. F
#'   stands for Fahrenheit; C stands for Celcius.}
#'   \item{StartWind}{The Beaufort wind speed code recorded at the beginning of
#'   the run of the route.}
#'   \item{EndWind}{The Beaufort wind speed code recorded at the end of the run
#'   of the route.}
#'   \item{StartSky}{The Weather Bureau sky code recorded at the beginning of
#'   the run of the route.}
#'   \item{EndSky}{The Weather Bureau sky code recorded at the end of the run of
#'   the route.}
#'   \item{StartTime}{The time the run of the route began, recorded in 24 hour
#'   local time.}
#'   \item{EndTime}{The time the run of the route ended, recorded in 24 hour
#'   local time.}
#'   \item{Assistant}{Did someone assist? 1 if they did, otherwise 0.}
#'   \item{RunType}{If this run is acceptable by BBS standards, then 1,
#'   otherwise 0.}
#' 
#' @details See 'bbs_dir/WeatherInf.txt' for documentation.
#' @author Bob O'Hara
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#'
#' @examples
#' weather <- bbs_meta_weather()
#' 
#' @export bbs_meta_weather
bbs_meta_weather <- function(bbs_dir = NULL) {
  
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  out <- csv_unzip(paste0(bbs_dir, "/Weather.zip"))
  names(out) <- tolower(names(out))
  
  return(out)
}
