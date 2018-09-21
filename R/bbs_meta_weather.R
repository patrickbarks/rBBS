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
#'   \item{route_data_id}{Data identification number for unique combinations of
#'   country_num, state, route, rpid, and year}
#'   \item{country_num}{Three digit code that identifies country: 124 (Canada),
#'   484 (Mexico), or 840 (USA)}
#'   \item{state_num}{Two digit numerical code that identifies the state,
#'   province, or territory}
#'   \item{route}{Three digit code that identifies the route.}
#'   \item{rpid}{The run protocol identification number.}
#'   \item{year}{The year.}
#'   \item{month}{The month the route was surveyed (1-12).}
#'   \item{day}{The day the route was surveyed (1-31).}
#'   \item{obs_n}{Unique observer identification number.}
#'   \item{total_spp}{The total number of species recorded on that run of the
#'   route.}
#'   \item{start_temp}{The temperature recorded at the beginning of the run of
#'   the route.}
#'   \item{end_temp}{The temperature recorded at the end of the run of the
#'   route.}
#'   \item{temp_scale}{The scale in which the temperatures were recorded. F
#'   stands for Fahrenheit; C stands for Celcius.}
#'   \item{start_wind}{The Beaufort wind speed code recorded at the beginning of
#'   the run of the route.}
#'   \item{end_wind}{The Beaufort wind speed code recorded at the end of the run
#'   of the route.}
#'   \item{start_sky}{The Weather Bureau sky code recorded at the beginning of
#'   the run of the route.}
#'   \item{end_sky}{The Weather Bureau sky code recorded at the end of the run
#'   of the route.}
#'   \item{start_time}{The time the run of the route began, recorded in 24 hour
#'   local time.}
#'   \item{end_time}{The time the run of the route ended, recorded in 24 hour
#'   local time.}
#'   \item{assistant}{Did someone assist? 1 if they did, otherwise 0.}
#'   \item{run_type}{If this run is acceptable by BBS standards, then 1,
#'   otherwise 0.}
#' 
#' @details See 'bbs_dir/WeatherInf.txt' for documentation.
#' @author Bob O'Hara
#' @author Patrick Barks <patrick.barks@@gmail.com>
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
  out$state_num <- as.integer(out$state_num)
  out$route <- as.integer(out$route)
  
  return(out)
}
