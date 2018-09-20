#' Read BBS meta-data
#'
#' Read in list of states/provinces/territories, and also names of zip files
#' where the 10 stop data is kept.
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#' @param zip_files Logical: should the names of the zip files for the 10- and
#'   50-stop data be added? Defaults to FALSE.
#' 
#' @return
#' \code{data.frame} with the following columns:
#'   \item{country_num}{Integer code for country}
#'   \item{country_name}{Name of country}
#'   \item{state_num}{Integer code for state/province/territory}
#'   \item{state_name}{Name of state/province/territory}
#'   \item{ten_stop_file}{Name of zip file with 10-stop survey data}
#' 
#' @details This is meta-data collated from the full database. Note that not all
#'   regions have a zip file: in particular, there is no Mexican data, or data
#'   from Puerto Rico or Washington D.C.
#' @author Bob O'Hara
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#' 
#' @examples
#' regions <- bbs_meta_regions()
#' 
#' @importFrom  tibble as_tibble
#' @export bbs_meta_regions
bbs_meta_regions <- function(bbs_dir = NULL, zip_files = FALSE) {
  
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  # read RegionCodes.txt
  regions <- read_bbs_txt(paste0(bbs_dir, '/RegionCodes.txt'))
  regions$state_name <- bbs_stateprov(regions$state_name)
  regions$country_name <- vapply(regions$country_num, bbs_country_switch, '')
  regions <- regions[,c(1, 4, 2, 3)]
  
  if (zip_files == TRUE) {
    
    # Read README.txt
    txt_file <- paste0(bbs_dir, '/README.txt')
    
    # if txt_file path ftp or http, download
    if (grepl('^ftp:|^http:|^https:', txt_file)) {
      temp <- tempfile()
      download.file(txt_file, temp)
    } else {
      temp <- txt_file
    }
    
    # read lines
    txt_lines <- scan(temp, what = "character", sep = "\n",
                      encoding = "latin1", quiet = TRUE,
                      blank.lines.skip = FALSE)
    
    line_start <- grep("Files in the States Directory", txt_lines) + 2
    line_blank <- which(txt_lines == "")
    line_end <- min(line_blank[line_blank > line_start]) - 1
    
    txt_lines <- txt_lines[line_start:line_end]
    txt_lines <- gsub('\t', '', txt_lines)
    
    # extract state names and 10-stop file names
    ten_stop_file <- vapply(
      txt_lines,
      function(x) strsplit(x, '[[:space:]][[:space:]]+')[[1]][1],
      ''
    )
    
    state_name <- vapply(
      txt_lines,
      function(x) strsplit(x, '[[:space:]][[:space:]]+')[[1]][3],
      ''
    )
    
    # standardize state names
    state_name <- bbs_stateprov(state_name)
    
    # merge 10-stop file names with region data
    df_file_names <- data.frame(state_name, ten_stop_file,
                                stringsAsFactors = FALSE)
    
    regions <- merge(regions, df_file_names, by = 'state_name',
                     all.x = TRUE)
    regions <- regions[order(regions$country_num, regions$state_num),
                       c(2, 3, 4, 1, 5)]
  }
  
  regions <- as_tibble(regions)
  return(regions)
}
