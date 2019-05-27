#' Get taxonomic metadata for North American Breeding Bird Survey
#' 
#' Get data frame with species names and associated taxonomic information from
#' the North American Breeding Bird Survey (BBS). This data comes from the BBS
#' file \emph{SpeciesList.txt}.
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#'
#' @return
#' A \code{data.frame} with the following columns:
#'   \item{seq}{phylogenetic sequence number}
#'   \item{aou}{integer species code from American Ornithological Union}
#'   \item{english_common_name}{common name in English}
#'   \item{french_common_name}{common name in French}
#'   \item{spanish_common_name}{common name in Spanish}
#'   \item{order}{taxonomic order}
#'   \item{family}{taxonomic family}
#'   \item{genus}{taxonomic genus}
#'   \item{species}{taxonomic species name}
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
#' species <- bbs_meta_species()
#' 
#' # get from local working directory
#' species <- bbs_meta_species(bbs_dir = ".")
#' }
#' @export bbs_meta_species
bbs_meta_species <- function(bbs_dir = bbs_ftp()) {
  
  out <- read_bbs_txt(paste(bbs_dir, "SpeciesList.txt", sep = "/"))
  out <- bbs_standardize(out)
  
  return(out)
}
