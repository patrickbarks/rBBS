#' Read BBS meta-data
#' 
#' Read in list of species names, from SpeciesList.txt, and then extract list of
#' where the data is kept
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#'
#' @return
#' Data frame with these columns:
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
#' @importFrom utils read.fwf
#' @export GetSpNames
GetSpNames <- function(bbs_dir = NULL) {

  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
    
  File <- paste0(bbs_dir, "SpeciesList.txt")
  
  All <- scan(File, what="character", sep="\n", encoding="latin1", quiet = TRUE)
  
  Delimiter <- grep("^-", All)
  
  ColNames <- strsplit(All[Delimiter-1], split='[[:blank:]]+')[[1]]
  Widths <- nchar(strsplit(All[Delimiter], split='[[:blank:]]+')[[1]]) + 1
  
  df <- read.fwf(File, skip = Delimiter, widths = Widths,
                 fileEncoding = 'Latin1', strip.white = TRUE,
                 stringsAsFactors = FALSE, col.names = ColNames)
  
  df$AOU <- formatC(df$AOU, width = 5, flag = '0')
  
  return(df)
}

