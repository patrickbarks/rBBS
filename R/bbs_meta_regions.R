#' Get state/province/territory metadata for North American Breeding Bird Survey
#'
#' Get a data frame with the names and codes for states/provinces/territories
#' used in North American Breeding Bird Survey (BBS), and optionally, the names
#' of the corresponding zip files containing 10-stop and 50-stop count data.
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#' @param zip_files Include names of zip files containing 10-stop and 50-stop
#'   data? Defaults to FALSE.
#' 
#' @return
#' A \code{data.frame} with the following columns:
#'   \item{country_num}{integer code for country}
#'   \item{country_name}{name of country}
#'   \item{state_num}{integer code for state/province/territory}
#'   \item{state_name}{name of state/province/territory}
#'   
#' If \code{zip_files = TRUE}, also includes the column:
#'   \item{ten_stop_file}{name of zip file with 10-stop survey data (or
#'   \code{NA} if region does not have a zip file)}
#'   \item{fifty_stop_file}{name of zip file with 50-stop survey data (or
#'   \code{NA} if region does not have a zip file)}
#' 
#' @details Note that not all regions have a zip file. As of 2017 there is no
#'   data for Mexico, Puerto Rico, or Washington D.C.
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
#' regions <- bbs_meta_regions()
#' 
#' # get from local working directory
#' regions <- bbs_meta_regions(bbs_dir = ".")
#' }
#' @importFrom  tibble add_column
#' @export bbs_meta_regions
bbs_meta_regions <- function(bbs_dir = bbs_ftp(), zip_files = FALSE) {
  
  # read RegionCodes.txt
  regions <- read_bbs_txt(paste(bbs_dir, "RegionCodes.txt", sep = "/"))
  regions <- bbs_standardize(regions)
  regions <- add_column(regions,
                        country_name = bbs_country(regions$country_num),
                        .after = 1)
  
  # add names of 10-stop and 50-stop zip archives
  if (zip_files == TRUE) {
    file_names <- bbs_files_10(bbs_dir = bbs_dir)
    m10 <- match(regions$state_name, file_names$state_name)
    regions$ten_stop_file <- file_names$ten_stop_file[m10]
    m50 <- match(tolower(regions$state_name), df_zip$state_name)
    regions$fifty_stop_file <- df_zip$fifty_stop_file[m50]
  }
  
  return(regions)
}
