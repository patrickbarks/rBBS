#' Download North American Breeding Bird Survey data via FTP
#' 
#' Download North American Breeding Bird Survey (BBS) data via FTP.
#'
#' @param dest Path to an existing local directory to download files to.
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be an FTP
#'   address for an older BBS release.
#' @param countries Vector of countries to download 10-stop data for (e.g.
#'   \code{c('Canada', 'United States')}). Used only if \code{ten_stop = TRUE}.
#'   Defaults to all available. Case-insensitive. See Details.
#' @param states Vector of states/provinces/territories to download 10-stop data
#'   for. Used only if \code{ten_stop = TRUE}. Defaults to all available.
#'   Case-insensitive. See Details.
#' @param meta Download top-level metadata files? Default is \code{TRUE}.
#' @param ten_stop Download 10-stop data? Default is \code{TRUE}.
#' @param fifty_stop Download 50-stop data? Default is \code{FALSE}.
#' @param migrant Download data for migrant non-breeders? Default is
#'   \code{FALSE}.
#' @param overwrite Overwrite files that already exist? Default is \code{FALSE}.
#' @param verbose List files that were not re-downloaded because they already
#'   exist? Default is \code{TRUE}. Only applies if \code{overwrite = FALSE}.
#' 
#' @details
#' For names to use in arguments \code{countries} and \code{states} see
#' \code{\link{bbs_meta_regions}} or BBS file \emph{RegionCodes.txt}.
#' 
#' Note that country/state subsets are additive, so specifying
#' 
#' \code{
#' bbs_download('.', countries = 'Canada', states = 'Montana')
#' }
#' 
#' will download data for all Canadian provinces/territories plus the state of
#' Montana. The following lines will both download data for all American states:
#' 
#' \code{bbs_download('.', countries = 'United States', states = 'Florida')} \cr
#' \code{bbs_download('.', countries = 'United States')}
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
#' # download metadata and 10-stop data for Alaska and all of Canada
#' bbs_download(dest = '.', countries = 'Canada', states = c('Alaska'))
#' 
#' # download metadata and 10-stop data for Gulf Coast states
#' gulf_states <- c('Alabama', 'Florida', 'Louisiana', 'Mississippi', 'Texas')
#' bbs_download(dest = '.', states = gulf_states)
#' 
#' # download metadata and 50-stop data for all regions
#' bbs_download(dest = '.', ten_stop = FALSE, fifty_stop = TRUE)
#' }
#' 
#' @export bbs_download
bbs_download <- function(dest, bbs_dir = NULL, countries = NULL, states = NULL,
                         meta = TRUE, ten_stop = TRUE, fifty_stop = FALSE,
                         migrant = FALSE, overwrite = FALSE, verbose = TRUE) {
  
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
  # check whether countries argument valid
  if (!is.null(countries)) {
    
    if (!all(tolower(countries) %in% ts_df$country)) {
      countries_inv <- states[!tolower(countries) %in% ts_df$country]
      stop(paste('The following countries could not be found:',
                 paste(countries_inv, collapse = ', ')))
    }
  }
  # check whether states argument valid
  if (!is.null(states)) {
    
    if (!all(tolower(states) %in% ts_df$stateprov)) {
      states_inv <- states[!tolower(states) %in% ts_df$stateprov]
      stop(paste('The following states could not be found:',
                 paste(states_inv, collapse = ', ')))
    }
  }
  
  # fetch bbs_dir
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  # append slash to dest and bbs_dir, for good measure
  dest <- paste0(dest, '/')
  bbs_dir <- paste0(bbs_dir, '/')
  
  ## download top-level metadata files
  if (meta == TRUE) {
    tl_files <- bbs_read_dir(bbs_dir)
    
    bbs_download_util(bbs_dir, dest, subdir = '', dl_files = tl_files,
                      overwrite = overwrite, verbose = verbose)
  }
  
  ## download 10-stop files (by stateprov)
  if (ten_stop == TRUE) {
    
    # create <dest>/States/ directory if necessary
    if (!dir.exists(paste0(dest, 'States'))) {
      dir.create(paste0(dest, 'States'))
    }
    
    ts_dir <- '/States/'
    ts_files <- bbs_read_dir(paste0(bbs_dir, ts_dir))
    
    # empty sets
    ts_files_c <- character(0)
    ts_files_s <- character(0)
    
    # subset to regions of interest
    if (!is.null(states)) {
      
      # if states includes full countries
      if (any(tolower(states) %in% unique(ts_df$country))) {
        ts_files_c <- ts_df$file[ts_df$country %in% tolower(states)]
      }
      
      # if states includes states
      if (any(tolower(states) %in% ts_df$stateprov)) {
        ts_files_s <- ts_df$file[ts_df$stateprov %in% tolower(states)]
      }
      
      # full list of ts_files to get
      ts_files <- c(ts_files_c, ts_files_s)
    }
    
    # download 10-stop files
    bbs_download_util(bbs_dir, dest, subdir = ts_dir, dl_files = ts_files,
                      overwrite = overwrite, verbose = verbose)
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
    
    fs_dir <- '50-StopData/1997ToPresent_SurveyWide/'
    fs_files <- bbs_read_dir(paste0(bbs_dir, fs_dir))
    
    bbs_download_util(bbs_dir, dest, subdir = fs_dir, dl_files = fs_files,
                      overwrite = overwrite, verbose = verbose)
  }
}
