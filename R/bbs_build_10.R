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
#' @export bbs_build_10
bbs_build_10 <- function(bbs_dir) {
  
  ts_dir <- '/States/'
  files_ts <- list.files(paste0(bbs_dir, ts_dir))
  files_ts <- files_ts[grepl('\\.zip$', files_ts)]
  paths_ts <- paste0(bbs_dir, ts_dir, files_ts)
  
  bbs_10_l <- lapply(paths_ts, csv_unzip)
  bbs_10 <- do.call(rbind.data.frame, bbs_10_l)
  
  return(bbs_10)
}
