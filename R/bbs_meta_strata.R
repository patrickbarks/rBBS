#' Get physiographic strata metadata for North American Breeding Bird Survey
#' 
#' Get a data frame with the names and codes of physiographic strata in the
#' North American Breeding Bird Survey (BBS). This data comes from the BBS file
#' \emph{BBSStrata.txt}.
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#'
#' @return
#' A \code{data.frame} with the following columns:
#'   \item{stratum}{integer code for physiographic stratum}
#'   \item{stratum_name}{name of stratum in English}
#'   \item{stratum_name_french}{name of stratum in French}
#'   \item{stratum_name_spanish}{name of stratum in Spanish}
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
#' strata <- bbs_meta_strata()
#' 
#' # get from local working directory
#' strata <- bbs_meta_strata(bbs_dir = '.')
#' }
#' @export bbs_meta_strata
bbs_meta_strata <- function(bbs_dir = NULL) {

  # TODO: add Strata areas from:
  #  https://www.pwrc.usgs.gov/bbs/stratanames/index.html
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  out <- read_bbs_txt(txt_file = paste0(bbs_dir, "BBSStrata.txt"))
  
  return(out)
}
