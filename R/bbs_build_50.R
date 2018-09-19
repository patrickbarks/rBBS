#' Build data frame with North American Breeding Bird Survey 10-stop data
#' 
#' Build data frame with North American Breeding Bird Survey 10-stop data
#'
#' @param bbs_dir Directory from which to load data. Must be a path to an
#'   existing local directory.
#' 
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
  paths_fs <- paste0(bbs_dir, fs_dir, files_fs)
  
  bbs_50_l <- lapply(paths_fs, csv_unzip)
  bbs_50 <- do.call(rbind.data.frame, bbs_50_l)
  
  return(bbs_50)
}
