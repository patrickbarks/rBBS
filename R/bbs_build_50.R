#' Build data frame with North American Breeding Bird Survey 10-stop data
#' 
#' Build data frame with North American Breeding Bird Survey 10-stop data
#'
#' @param bbs_dir Directory from which to load data. Must be a path to an
#'   existing local directory.
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
#'   \item{aou}{American Ornithological Union code number}
#'   \item{stop_1 ... stop_50}{Total individuals of the species recorded on
#'   given stop.}
#' 
#' @author Bob O'Hara
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#'   
#' @examples
#' 
#' @export bbs_build_50
bbs_build_50 <- function(bbs_dir) {
  
  fs_dir <- '/50-StopData/1997ToPresent_SurveyWide/'
  files_fs <- list.files(paste0(bbs_dir, fs_dir))
  files_fs <- files_fs[grepl('\\.zip$', files_fs)]
  
  if (length(files_fs) == 0) {
    stop('No .zip files found within <bbs_dir>/50-StopData/1997ToPresent_SurveyWide/')
  }
  
  paths_fs <- paste0(bbs_dir, fs_dir, files_fs)
  bbs_50_l <- lapply(paths_fs, csv_unzip)
  bbs_50 <- do.call(rbind.data.frame, bbs_50_l)
  
  return(bbs_50)
}
