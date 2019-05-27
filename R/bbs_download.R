#' Download North American Breeding Bird Survey data via FTP
#' 
#' Download North American Breeding Bird Survey (BBS) data via FTP.
#'
#' @param dest Path to an existing local directory to download files to.
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be an FTP
#'   address for an older BBS release.
#' @param countries Vector of countries to download 10- and/or 50-stop data for
#'   (e.g. \code{c("Canada", "United States")}). Defaults to all available.
#'   Case-insensitive. See Details.
#' @param states Vector of states/provinces/territories to download 10- and/or
#'   50-stop data for. Defaults to all available. Case-insensitive. See Details.
#' @param meta Download top-level metadata files? Default is \code{TRUE}.
#' @param ten_stop Download 10-stop data? Default is \code{TRUE}.
#' @param fifty_stop Download 50-stop data? Default is \code{FALSE}.
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
#' bbs_download(".", countries = "Canada", states = "Montana")
#' }
#' 
#' will download data for all Canadian provinces/territories plus the state of
#' Montana. The following lines will both download data for all American states:
#' 
#' \code{bbs_download(".", countries = "United States", states = "Florida")} \cr
#' \code{bbs_download(".", countries = "United States")}
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
#' bbs_download(dest = ".", countries = "Canada", states = "Alaska")
#' 
#' # download metadata and 10-stop data for Gulf Coast states
#' gulf_states <- c("Alabama", "Florida", "Louisiana", "Mississippi", "Texas")
#' bbs_download(dest = ".", states = gulf_states)
#' 
#' # download metadata and 50-stop data for all regions
#' bbs_download(dest = ".", ten_stop = FALSE, fifty_stop = TRUE)
#' }
#' 
#' @export bbs_download
bbs_download <- function(dest, bbs_dir = bbs_ftp(), countries = NULL,
                         states = NULL, meta = TRUE, ten_stop = TRUE,
                         fifty_stop = FALSE, overwrite = FALSE,
                         verbose = TRUE) {
  
  # check whether dest exists
  if (!dir.exists(dest)) {
    stop("Destination directory not found. Argument 'dest' must be a path
         to an existing local directory")
  }
  # check whether countries argument valid
  if (!is.null(countries)) {
    
    if (!all(tolower(countries) %in% df_zip$country)) {
      countries_inv <- states[!tolower(countries) %in% df_zip$country]
      stop(paste("The following countries could not be found:",
                 paste(countries_inv, collapse = ", ")))
    }
  }
  # check whether states argument valid
  if (!is.null(states)) {
    
    if (!all(tolower(states) %in% df_zip$state_name)) {
      states_inv <- states[!tolower(states) %in% df_zip$state_name]
      stop(paste("The following states could not be found:",
                 paste(states_inv, collapse = ", ")))
    }
  }
  
  ## download top-level metadata files
  if (meta == TRUE) {
    tl_files <- bbs_read_dir(bbs_dir)
    
    bbs_download_util(bbs_dir, dest, subdir = "", dl_files = tl_files,
                      file_type = "metadata", overwrite = overwrite,
                      verbose = verbose)
  }
  
  ## download 10- and 50-stop data
  if (ten_stop == TRUE | fifty_stop == TRUE) {
    
    # geographic subset
    if (!is.null(countries) | !is.null(states)) {
      zip_use <- df_zip
      zip_use$use <- FALSE
      
      if (!is.null(countries)) {
        zip_use$use[zip_use$country_name %in% tolower(countries)] <- TRUE
      }
      if (!is.null(states)) {
        zip_use$use[zip_use$state_name %in% tolower(states)] <- TRUE
      }
      zip_use <- zip_use[zip_use$use == TRUE,]
    }
    
    ## download 10-stop files (by state)
    if (ten_stop == TRUE) {
      
      ts_dir <- "States/"
      
      if (!dir.exists(paste(dest, ts_dir, sep = "/"))) {
        dir.create(paste(dest, ts_dir, sep = "/"))
      }
      
      ts_files <- bbs_read_dir(paste(bbs_dir, ts_dir, sep = "/"))
      
      if (!is.null(countries) | !is.null(states)) {
        ts_files <- ts_files[ts_files %in% zip_use$ten_stop_file]
      }
      
      bbs_download_util(bbs_dir, dest, subdir = ts_dir, dl_files = ts_files,
                        file_type = "10-stop", overwrite = overwrite,
                        verbose = verbose)
    }
    
    ## download 50-stop files
    if (fifty_stop == TRUE) {
      
      fs_dir1 <- "50-StopData"
      fs_dir2 <- "1997ToPresent_SurveyWide/"
      fs_dir <- paste(fs_dir1, fs_dir2, sep = "/")
      
      if (!dir.exists(paste(dest, fs_dir1, sep = "/"))) {
        dir.create(paste(dest, fs_dir1, sep = "/"))
      }
      
      if (!dir.exists(paste(dest, fs_dir1, fs_dir2, sep = "/"))) {
        dir.create(paste(dest, fs_dir1, fs_dir2, sep = "/"))
      }
      
      fs_files <- bbs_read_dir(paste(bbs_dir, fs_dir, sep = "/"))
      
      if (!is.null(countries) | !is.null(states)) {
        fs_files <- fs_files[fs_files %in% zip_use$fifty_stop_file]
      }
      
      bbs_download_util(bbs_dir, dest, subdir = fs_dir, dl_files = fs_files,
                        file_type = "50-stop", overwrite = overwrite,
                        verbose = verbose)
    }
  }
}
