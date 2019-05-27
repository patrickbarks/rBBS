#' Get Bird Conservation Region metadata for North American Breeding Bird Survey
#' 
#' Get names and codes of Bird Conservation Regions (BCRs) from the North
#' American Breeding Bird Survey (BBS). This data comes from the BBS file
#' \emph{BCR.txt}.
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#'
#' @return
#' A \code{data.frame} with the following columns:
#'   \item{bcr}{integer code for BCR}
#'   \item{bcr_name}{name of BCR in English}
#'   \item{bcr_name_french}{name of BCR in French}
#'   \item{bcr_name_spanish}{name of BCR in Spanish}
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
#' bcr <- bbs_meta_bcr(bbs_dir = ".")
#' }
#' @export bbs_meta_bcr
bbs_meta_bcr <- function(bbs_dir = bbs_ftp()) {
  
  out <- read_bbs_txt(paste(bbs_dir, "BCR.txt", sep = "/"))
  out <- bbs_standardize(out)
  
  return(out)
}
