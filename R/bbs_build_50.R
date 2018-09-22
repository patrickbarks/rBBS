#' Assemble North American Breeding Bird Survey 50-stop count data
#' 
#' Assemble North American Breeding Bird Survey (BBS) 50-stop count data
#'
#' @param bbs_dir Local directory from which to load BBS data.
#'
#' Must be an existing directory with the same structure as the USGS BBS
#' directory (ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/).
#'   
#' @return
#' A \code{data.frame} with the following columns:
#'   \item{route_data_id}{integer code for unique combinations of country_num,
#'   state, route, rpid, and year}
#'   \item{country_num}{integer code for country: 124 (Canada), 484 (Mexico), or
#'   840 (United States)}
#'   \item{state_num}{integer code for state/province/territory (see BBS file
#'   \emph{RegionCodes.txt})}
#'   \item{route}{integer code for survey route (unique within states)}
#'   \item{rpid}{integer Run Protocol Identification ID (see BBS file
#'   \emph{RunProtocolID.txt})}
#'   \item{year}{integer survey year}
#'   \item{aou}{integer species code from American Ornithological Union}
#'   \item{stop_1 ... stop_50}{total number of individuals of the species
#'   recorded on the given stop}
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
#' bbs_50 <- bbs_build_50(bbs_dir = '.')
#' }
#' 
#' @export bbs_build_50
bbs_build_50 <- function(bbs_dir) {
  
  fs_dir <- '/50-StopData/1997ToPresent_SurveyWide/'
  files_fs <- list.files(paste0(bbs_dir, fs_dir))
  files_fs <- files_fs[grepl('\\.zip$', files_fs)]
  
  if (length(files_fs) == 0) {
    stop(paste('No .zip files found within directory',
               '/50-StopData/1997ToPresent_SurveyWide/'))
  }
  
  paths_fs <- paste0(bbs_dir, fs_dir, files_fs)
  bbs_50_l <- lapply(paths_fs, csv_unzip)
  bbs_50 <- do.call(rbind.data.frame, bbs_50_l)
  
  return(bbs_50)
}
