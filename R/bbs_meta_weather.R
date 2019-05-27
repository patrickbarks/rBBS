#' Get weather and environmental metadata for North American Breeding Bird
#' Survey
#' 
#' Get a data frame with weather and environmental data from the North American
#' Breeding Bird Survey (BBS). This data comes from the BBS file
#' \emph{Weather.csv}. See also files \emph{WeatherInf.txt} and
#' \emph{weathercodes.txt} for further details.
#' 
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#'
#' @return
#' A \code{data.frame} with the following columns:
#'   \item{route_data_id}{integer code for unique combinations of country_num,
#'   state, route, rpid, and year}
#'   \item{country_num}{integer code for country: 124 (Canada), 484 (Mexico),
#'   840 (United States)}
#'   \item{state_num}{integer code for state/province/territory}
#'   \item{route}{integer code for route (unique within states)}
#'   \item{rpid}{integer code for Run Protocol ID (see BBS file
#'   \emph{RunProtocolID.txt})}
#'   \item{year}{integer survey year}
#'   \item{month}{integer survey month (1-12)}
#'   \item{day}{integer survey day (1-31)}
#'   \item{obs_n}{integer identification code for human observer}
#'   \item{total_spp}{total number of species recorded on that run of the route}
#'   \item{start_temp}{temperature recorded at the beginning of the run of the
#'   route}
#'   \item{end_temp}{temperature recorded at the end of the run of the route}
#'   \item{temp_scale}{scale in which the temperature was recorded: F
#'   (Fahrenheit), C (Celsius)}
#'   \item{start_wind}{Beaufort wind speed code recorded at the beginning of the
#'   run of the route (see BBS file \emph{weathercodes.txt})}
#'   \item{end_wind}{Beaufort wind speed code recorded at the end of the run of
#'   the route (see BBS file \emph{weathercodes.txt})}
#'   \item{start_sky}{Weather Bureau sky code recorded at the beginning of the
#'   run of the route (see BBS file \emph{weathercodes.txt})}
#'   \item{end_sky}{Weather Bureau sky code recorded at the end of the run of
#'   the route (see BBS file \emph{weathercodes.txt})}
#'   \item{start_time}{time the run of the route began (in 24 hour local time)}
#'   \item{end_time}{time the run of the route ended (in 24 hour local time)}
#'   \item{assistant}{logical indicating whether another person assisted}
#'   \item{quality_current_id}{logical indicating whether the run took place
#'   under suitable weather conditions, and within suitable time, date, and
#'   route completion criteria (see BBS file \emph{WeatherInf.txt})}
#'   \item{run_type}{integer indicating whether the run is acceptable (1) or not
#'   (0) by BBS standards (see BBS file \emph{runtype.pdf})}
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
#' # get from USGS ftp server
#' weather <- bbs_meta_weather()
#' 
#' # get from local working directory
#' weather <- bbs_meta_weather(bbs_dir = ".")
#' }
#' @export bbs_meta_weather
bbs_meta_weather <- function(bbs_dir = bbs_ftp()) {
  
  out <- csv_unzip(paste(bbs_dir, "Weather.zip", sep = "/"))
  out <- bbs_standardize(out)
  
  return(out)
}
