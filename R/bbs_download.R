#' Download North American Breeding Bird Survey data via FTP
#' 
#' Download North American Breeding Bird Survey data via FTP.
#'
#' @param dest Path to an existing local directory to download files to.
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be an FTP
#'   address for an older BBS release.
#' @param stateprovs Vector of countries/states/provinces to download data for.
#'   Used only if \code{ten_stop == TRUE}. Defaults to all available regions.
#'   Case-insensitive. See Details.
#' @param meta Download top-level metadata files (T/F)? Default is \code{TRUE}.
#' @param ten_stop Download 10-stop data (T/F)? Default is \code{TRUE}.
#' @param fifty_stop Download 50-stop data (T/F)? Default is \code{FALSE}.
#' @param migrant Download data for migrant non-breeders (T/F)? Default is
#'   \code{FALSE}.
#' 
#' @details Argument \code{stateprov} may include names of countries ('Canada'
#'   or 'United States') and/or states/provinces/territories. Different
#'   geographic levels may be combined; e.g. \code{stateprov = c('Canada',
#'   'Montana', 'North Dakota')} will download data for all Canadian
#'   provinces/territories plus Montana and North Dakota.
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#'   
#' @examples
#' \dontrun{
#' # download metadata and 10-stop data for Alaska and all of Canada
#' bbs_download(dest = '~/bbs/', stateprovs = c('Alaska', 'Canada'))
#' 
#' # download metadata and 10-stop data for Gulf Coast states
#' gulf_states <- c('Alabama', 'Florida', 'Louisiana', 'Mississippi', 'Texas')
#' bbs_download(dest = '~/bbs/', stateprovs = gulf_states)
#' 
#' # download metadata and 50-stop data for all regions
#' bbs_download(dest = '~/bbs/', ten_stop = FALSE, fifty_stop = TRUE)
#' }
#' 
#' @importFrom utils download.file
#' @export bbs_download
bbs_download <- function(dest, bbs_dir = NULL, stateprovs = NULL,
                         meta = TRUE, ten_stop = TRUE, fifty_stop = FALSE,
                         migrant = FALSE) {
  
  # validate inputs
  # check whether R build has libcurl capability
  if (capabilities("libcurl") == FALSE) {
    stop('This function requires libcurl capability')
  }
  # check whether dest exists
  if (!dir.exists(dest)) {
    stop("Destination directory not found. Argument 'dest' must be a path
         to an existing local directory")
  }
  # check whether stateprovs valid
  if (!is.null(stateprovs)) {
    valid_stateprovs <- unique(c(ts_df$country, ts_df$stateprov))
    
    if (!all(tolower(stateprovs) %in% valid_stateprovs)) {
      stateprovs_inv <- stateprovs[!tolower(stateprovs) %in% valid_stateprovs]
      stop(paste('The following stateprovs could not be found:',
                 paste(stateprovs_inv, collapse = ', ')))
    }
  }
  
  # fetch bbs_dir
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  # append slash to dest and bbs_dir, for good measure
  dest <- paste0(dest, '/')
  bbs_dir <- paste0(bbs_dir, '/')
  
  ## download top-level metxadata files
  if (meta == TRUE) {
    tl <- bbs_dir
    tl_lines <- readLines(url(tl, method = 'libcurl'))
    tl_lines_split <- strsplit(tl_lines, '[[:space:]]+')
    tl_files <- vapply(tl_lines_split, function(x) x[length(x)], character(1))
    tl_files <- tl_files[grep('\\.txt$|\\.pdf$|\\.zip$', tl_files)]
    
    for(i in 1:length(tl_files)) {
      download.file(paste0(tl, tl_files[i]),
                    paste0(dest, tl_files[i]))
    }
  }
  
  ## download 10-stop files (by stateprov)
  if (ten_stop == TRUE) {
    
    # create <dest>/States/ directory if necessary
    if (!dir.exists(paste0(dest, 'States'))) {
      dir.create(paste0(dest, 'States'))
    }
    
    ts <- paste0(bbs_dir, 'States/')
    ts_lines <- readLines(url(ts, method = 'libcurl'))
    ts_lines_split <- strsplit(ts_lines, '[[:space:]]+')
    ts_files <- vapply(ts_lines_split, function(x) x[length(x)], character(1))
    ts_files <- ts_files[grep('\\.txt$|\\.pdf$|\\.zip$', ts_files)]
    
    # empty sets
    ts_files_c <- character(0)
    ts_files_s <- character(0)
    
    # subset to regions of interest
    if (!is.null(stateprovs)) {
      
      # if stateprovs includes full countries
      if (any(tolower(stateprovs) %in% unique(ts_df$country))) {
        ts_files_c <- ts_df$file[ts_df$country %in% tolower(stateprovs)]
      }
      
      # if stateprovs includes stateprovs
      if (any(tolower(stateprovs) %in% ts_df$stateprov)) {
        ts_files_s <- ts_df$file[ts_df$stateprov %in% tolower(stateprovs)]
      }
      
      # full list of ts_files to get
      ts_files <- c(ts_files_c, ts_files_s)
    }
    
    # download 10-stop files
    for(i in 1:length(ts_files)) {
      download.file(paste0(ts, ts_files[i]),
                    paste0(dest, 'States/', ts_files[i]))
    }
  }
  
  ## download 50-stop files
  if (fifty_stop == TRUE) {
    
    # create <dest>/ directories if necessary
    if (!dir.exists(paste0(dest, '50-StopData'))) {
      dir.create(paste0(dest, '50-StopData'))
    }
    if (!dir.exists(paste0(dest, '50-StopData/1997ToPresent_SurveyWide'))) {
      dir.create(paste0(dest, '50-StopData/1997ToPresent_SurveyWide'))
    }
    
    fs <- paste0(bbs_dir, '50-StopData/1997ToPresent_SurveyWide/')
    fs_lines <- readLines(url(fs, method = 'libcurl'))
    fs_lines_split <- strsplit(fs_lines, '[[:space:]]+')
    fs_files <- vapply(fs_lines_split, function(x) x[length(x)], character(1))
    fs_files <- fs_files[grep('\\.txt$|\\.pdf$|\\.zip$', fs_files)]
    
    for(i in 1:length(fs_files)) {
      download.file(paste0(fs, fs_files[i]),
                    paste0(dest, '50-StopData/1997ToPresent_SurveyWide/',
                           fs_files[i]))
    }
  }
}
