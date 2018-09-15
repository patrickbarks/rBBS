#' Get taxonomic data from North American Breeding Bird Survey (BBS)
#' 
#' Get data frame with species names and associated taxonomic information from
#' the North American Breeding Bird Survey (BBS). This data comes from the BBS
#' file 'SpeciesList.txt'.
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#'
#' @return
#' \code{data.frame} with the following columns:
#'   \item{Seq}{Phylogenetic sequence number}
#'   \item{AOU}{American Ornithological Union code number}
#'   \item{English_Common_Name}{English Common Name}
#'   \item{French_Common_Name}{French Common Name}
#'   \item{Spanish_Common_Name}{Spanish Common Name}
#'   \item{ORDER}{Taxonomic order}
#'   \item{Family}{Taxonomic family}
#'   \item{Genus}{Taxonomic genus}
#'   \item{Species}{Taxonomic species name}
#' 
#' @author Bob O'Hara
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#' @examples
#' Species <- GetSpNames()
#' 
#' @export GetSpNames
GetSpNames <- function(bbs_dir = NULL) {

  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  txt_file <- paste0(bbs_dir, "SpeciesList.txt")
  
  df <- read_bbs_txt(txt_file)
  df$AOU <- formatC(df$AOU, width = 5, flag = '0')
  
  return(df)
}
