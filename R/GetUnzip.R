#' Function to extract csv data from a zipped archive
#' 
#' Code adapted from
#' http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
#'
#' @param ZipName name of zipped archive
#' @param FileName name of file in zipped archive
#'
#' @return A data frame (or whatever else read.csv wants to give)
#' 
#' @author Bob O'Hara
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#'   
#' @examples
#' ## Download woodpecker data from BBS
#' Rep <- "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/"
#' dat <- GetUnzip(ZipName = paste0(Rep, "States/Florida.zip"),
#'                 FileName="Florida.csv")
#' 
#' \dontrun{
#'   ## Download from a local repository
#'   ##   Use your own LocalRepos
#'   LocalRepos <- "~/Dropbox/"
#'   dat=GetUnzip(ZipName=paste0(LocalRepos, 
#'   "BBS/DataFiles/Species/TrogWoodp.zip"), 
#'   FileName="TrogWoodp.csv")
#' }
#' 
#' @importFrom utils download.file
#' @importFrom utils read.csv
#' @export GetUnzip
GetUnzip=function(ZipName, FileName) {
  if(grepl('^[hf]t+p', ZipName)) {
    temp <- tempfile()
    download.file(ZipName,temp, quiet=FALSE)
    data <- read.csv(unz(temp, FileName))
    unlink(temp)
  } else {
    data <- read.csv(unz(ZipName, FileName))
  }
  data
}
