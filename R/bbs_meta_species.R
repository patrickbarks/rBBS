#' Get taxonomic data from North American Breeding Bird Survey
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
#'   \item{seq}{Phylogenetic sequence number}
#'   \item{aou}{American Ornithological Union code number}
#'   \item{english_common_name}{English Common Name}
#'   \item{french_common_name}{French Common Name}
#'   \item{spanish_common_name}{Spanish Common Name}
#'   \item{order}{Taxonomic order}
#'   \item{family}{Taxonomic family}
#'   \item{genus}{Taxonomic genus}
#'   \item{species}{Taxonomic species name}
#' 
#' @author Bob O'Hara
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#' @examples
#' species <- bbs_meta_species()
#' 
#' @export bbs_meta_species
bbs_meta_species <- function(bbs_dir = NULL) {

  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  df <- read_bbs_txt(txt_file = paste0(bbs_dir, "SpeciesList.txt"))
  
  return(df)
}
