#' Get Bird Conservation Region data from North American Breeding Bird Survey
#' 
#' Get names and ID numbers of Bird Conservation Regions (BCRs) from the North
#' American Breeding Bird Survey (BBS). This data comes from the BBS file
#' 'BCR.txt'.
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#'
#' @return
#' A \code{data.frame} with the following columns:
#'   \item{bcr}{Integer code for BCR}
#'   \item{bcr_name}{BCR name in English}
#'   \item{bcr_name_french}{BCR name in French}
#'   \item{bcr_name_spanish}{BCR name in Spanish}
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
#' bcr <- bbs_meta_bcr()
#' 
#' # get from local working directory
#' bcr <- bbs_meta_bcr(bbs_dir = '.')
#' }
#' @export bbs_meta_bcr
bbs_meta_bcr <- function(bbs_dir = NULL) {

  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  df <- read_bbs_txt(txt_file = paste0(bbs_dir, "BCR.txt"))
  
  return(df)
}
