#' Read BBS meta-data
#' 
#' Read in list of species names, from SpeciesList.txt, and then extract list of
#' where the data is kept
#'
#' @param Dir Directory to get data. Defaults to
#'   ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/, the USGS FTP server
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
#' @export GetSpNames
GetSpNames <- function(Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
  File <- paste0(Dir, "SpeciesList.txt")
  
  All <- scan(File, what="character", sep="\n", encoding="latin1")
  Delimiter <- grep("^-", All)
  
  ColNames <- strsplit(All[Delimiter-1], split='[[:blank:]]+')[[1]]
  Widths <- nchar(strsplit(All[Delimiter], split='[[:blank:]]+')[[1]])
  
  Lines <- sapply(All[-(1:Delimiter)], function(str, w) {
    trimws(substring(str, c(1,cumsum(w[-length(w)])), cumsum(w)))
  }, w=Widths+1)
  colnames(Lines) <- NULL
  rownames(Lines) <- ColNames
  
  Lines.df <- as.data.frame(t(Lines), stringsAsFactors = FALSE)
  Lines.df$Seq <- as.numeric(Lines.df$Seq)
  Lines.df$AOU <- as.numeric(Lines.df$AOU)
  Lines.df
}


# GetSpNames <- function(Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
#   File <- paste0(Dir,"SpeciesList.txt")
#   spnames.all <- scan(File, sep="\n", what=character(), blank.lines.skip = FALSE, fileEncoding="Latin1")
#   SpCols <- spnames.all[-(1:grep("^-", spnames.all))]
#   
#   Widths <- c(1,1+cumsum(nchar(strsplit(spnames.all[8], ' ')[[1]])))
#   
#   SpData <- as.data.frame(sapply(2:length(Widths), function(wh, wid, dat) {
#     trimws(substr(dat, wid[wh-1]+1, wid[wh]))
#   }, wid=Widths, dat=SpCols), stringsAsFactors = FALSE)
#   names(SpData) <- strsplit(spnames.all[7], '[ ]{2,}')[[1]]
#   SpData$Seq <- as.numeric(SpData$Seq)
#   SpData$AOU <- as.numeric(SpData$AOU)
#   SpData
# }
