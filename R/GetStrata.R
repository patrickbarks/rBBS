#' Get geographic strata data for North American Breeding Bird Survey
#' 
#' Get names and ID numbers of geographic strata in the North American Breeding
#' Bird Survey (BBS). This data comes from the BBS file 'BBSStrata.txt'.
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#'
#' @return
#' \code{data.frame} with the following columns:
#'   \item{StratumID}{Stratum integer identifier}
#'   \item{StratumName}{Stratum name in English}
#'   \item{StratumNameFrench}{Stratum name in French}
#'   \item{StratumNameSpanish}{Stratum name in Spanish}
#' 
#' @author Bob O'Hara
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#' @examples
#' Strata <- GetStrata()
#' 
#' @export GetStrata
GetStrata <- function(bbs_dir = NULL) {

  # TODO: add Strata areas from:
  #  https://www.pwrc.usgs.gov/bbs/stratanames/index.html
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  df <- read_bbs_txt(txt_file = paste0(bbs_dir, "BBSStrata.txt"))
  
  return(df)
}
