#' Assemble North American Breeding Bird Survey 50-stop count data
#'
#' Assemble North American Breeding Bird Survey 50-stop count data into a data
#' frame, optionally subset to select regions, years, and/or species. The
#' underlying data come from 10 zipped csv files within the BBS folder
#' \emph{50-StopData/}. These csv files do not include counts of zero (i.e. when
#' a species is not observed during a given survey), but counts of zero can
#' optionally be built in (see argument \code{zeros}).\cr\cr \strong{Note}:
#' Without counts of zero the full 50-stop dataset for 2017 includes 3.4 million
#' rows. With zeros it includes 47 million rows, which will take some time to
#' build and require a few gigabytes of memory. If building with zeros, consider
#' subsetting to regions, years, and/or species of interest.
#'
#' @param bbs_dir Local directory from which to load BBS data.
#'
#'   Must be an existing directory with the same structure as the USGS BBS
#'   directory (ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/).
#' @param zeros Build in counts of zero? Defaults to \code{FALSE}. (See Details)
#' @param countries Vector of country names to subset to. Defaults to all
#'   available. Case insensitive. (See section \emph{Geographic Subsetting})
#' @param states Vector of state/province/territory names to subset to. Defaults
#'   to all available. Case insensitive. (See section \emph{Geographic
#'   Subsetting})
#' @param bcr Vector of Bird Conservation Region integer codes to subset to.
#'   Defaults to all available. (See section \emph{Geographic Subsetting})
#' @param strata Vector of physiographic strata integer codes to subset to.
#'   Defaults to all available. (See section \emph{Geographic Subsetting})
#' @param aou Vector of integer species id codes to subset to. Defaults to all
#'   available. (See BBS file \emph{SpeciesList.txt})
#' @param years Vector of survey years to subset to. Defaults to all available.
#' 
#' @details 
#' If \code{zeros = TRUE}, counts of zero are built in as follows:
#' 
#' 1. Find every unique combination of route-year-rpid matching the given subset
#' arguments (or the whole dataset if no subset arguments given).
#'
#' 2. Find every unique species (aou) that was recorded on those
#' route-year-rpid (subject to aou subset argument, if given).
#'
#' 3. Create a data frame with every combination of those route-year-rpid and
#' aou, and merge with the non-zero count data. Any route-year-rpid-aou rows
#' with missing count data are then filled in with counts of zero.
#' 
#' Note that this method is potentially inefficient in that it will yield counts
#' of zero for species even in regions where they have never been observed.
#' 
#' @return
#' A \code{data.frame} with the following columns (all integer):
#'   \item{route_data_id}{code for unique combinations of country_num, state,
#'   route, rpid, and year}
#'   \item{country_num}{code for country: 124 (Canada), 484 (Mexico), or 840
#'   (United States)}
#'   \item{state_num}{code for state/province/territory (see BBS file
#'   \emph{RegionCodes.txt})}
#'   \item{route}{code for route (unique within states)}
#'   \item{rpid}{code for Run Protocol ID (see BBS file
#'   \emph{RunProtocolID.txt})}
#'   \item{year}{survey year}
#'   \item{aou}{species code from American Ornithological Union}
#'   \item{stop_1 ... stop_50}{total individuals of the species recorded on
#'   stops 1-10}
#' 
#' @section Geographic Subsetting:
#' For country and state names see \code{\link{bbs_meta_regions}}. For integer
#' bcr codes see \code{\link{bbs_meta_bcr}}, and for integer strata codes see
#' \code{\link{bbs_meta_strata}}.
#' 
#' Geographic subsetting may done by countries and/or states OR bcr and/or
#' strata, but not by both. Subsets are additive, so specifying
#' 
#' \code{
#' bbs_build_50('.', countries = 'Canada', states = 'Montana')
#' }
#' 
#' will return data for all Canadian provinces/territories plus the state of
#' Montana. Likewise, the following lines are equivalent, and will both return
#' data for all American states:
#' 
#' \code{bbs_build_50('.', countries = 'United States', states = 'Florida')} \cr
#' \code{bbs_build_50('.', countries = 'United States')}
#' 
#' @author Bob O'Hara
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @references Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding and M.-A.R.
#'   Hudson. 2018. North American Breeding Bird Survey Dataset 1966-2017,
#'   version 2017.0. U.S. Geological Survey, Patuxent Wildlife Research Center.
#'   \url{https://doi.org/10.5066/F76972V8}
#' @seealso \code{\link{bbs_download}}
#'   
#' @examples
#' \dontrun{
#' 
#' # build whole dataset excluding counts of zero
#' bbs <- bbs_build_50(bbs_dir = '.')
#' 
#' # build with Canadian routes only, including counts of zero
#' bbs <- bbs_build_50(bbs_dir = '.', zeros = TRUE, countries = 'Canada')
#' 
#' # build for species Gray Jay (aou 4840), including counts of zero
#' bbs <- bbs_build_50(bbs_dir = '.', zeros = TRUE, aou = 4840)
#' }
#' 
#' @importFrom  tibble tibble
#' @importFrom  tibble as_tibble
#' @export bbs_build_50
bbs_build_50 <- function(bbs_dir, zeros = FALSE, countries = NULL,
                         states = NULL, bcr = NULL, strata = NULL,
                         aou = NULL, years = NULL) {

  if ((!is.null(countries) | !is.null(states)) &
      (!is.null(bcr) | !is.null(strata))) {
    stop('Cannot subset for both country/states AND bcr/strata.')
  }
  
  fs_dir <- '50-StopData/1997ToPresent_SurveyWide/'
  files_fs <- list.files(paste(bbs_dir, fs_dir, sep = '/'))
  files_fs <- files_fs[grepl('\\.zip$', files_fs)]
  
  if (length(files_fs) == 0) {
    stop(paste('No .zip files found within directory', fs_dir))
  }
  
  # subset based on geography
  if (!is.null(countries) | !is.null(states) | !is.null(bcr) |
      !is.null(strata) | zeros == TRUE) {
    
    zip_use <- df_zip
    zip_use$use <- FALSE
    
    if (!all(files_fs %in% zip_use$file_50)) {
      warning(paste('One or more zip files within the 50-stop directory is not',
                    'recognized (i.e. not one of Fifty1.zip-Fifty10.zip)'))
    }
    
    if (!is.null(countries)) {
      zip_use$use[zip_use$country_name %in% tolower(countries)] <- TRUE
    }
    if (!is.null(states)) {
      zip_use$use[zip_use$state_name %in% tolower(states)] <- TRUE
    }
    
    if (!is.null(bcr) | !is.null(strata)) { 
      routes <- bbs_meta_routes(bbs_dir = bbs_dir)
      routes$use <- FALSE
      
      if (!is.null(bcr)) {
        routes$use[routes$bcr %in% bcr] <- TRUE
      }
      if (!is.null(strata)) {
        routes$use[routes$stratum %in% strata] <- TRUE
      }
      
      routes <- routes[routes$use == TRUE,]
      zip_use$use[zip_use$state_num %in% routes$state_num] <- TRUE
    }
    
    # if zeros == TRUE but no geographic subset, use all zip_use
    if (is.null(countries) & is.null(states) & is.null(bcr) &
        is.null(strata)) {
      zip_use$use <- TRUE
    }
    
    zip_use <- zip_use[zip_use$use == TRUE,]
    
    if (!all(zip_use$file_50 %in% files_fs)) {
      n_zip <- length(unique(zip_use$file_50))
      n_avail <- length(which(files_fs %in% zip_use$file_50))
      zip_use <- zip_use[zip_use$file_50 %in% files_fs,]
      warning(paste('The 50-state directory only contains', n_avail, 'of the',
                    n_zip, 'zip files that match the given geographic subset.',
                    'Data returned is limited to available files.'))
    }
    
    files_fs <- unique(zip_use$file_50)
  }
  
  paths_fs <- paste(bbs_dir, fs_dir, files_fs, sep = '/')
  bbs_l <- lapply(paths_fs, csv_unzip)
  bbs <- do.call(rbind.data.frame, bbs_l)
  
  bbs$aou <- as.integer(bbs$aou)
  bbs$state_num <- as.integer(bbs$state_num)
  bbs$route <- as.integer(bbs$route)
  
  # subset based on bcr and/or strata
  if (!is.null(bcr) | !is.null(strata)) {
    routes$rid <- with(routes, as.integer(paste0(country_num, state_num, route)))
    bbs_rid <- with(bbs, as.integer(paste0(country_num, state_num, route)))
    bbs <- bbs[bbs_rid %in% routes$rid,]
  }
  
  # subset based on years
  if (!is.null(years)) {
    bbs <- bbs[bbs$year %in% as.integer(years),]
  }
  
  # subset based on aou
  if (!is.null(aou)) {
    bbs <- bbs[bbs$aou %in% as.integer(aou),]
  }
  
  if (zeros == TRUE) {
    
    # get weather metadata
    wx <- bbs_meta_weather(bbs_dir = bbs_dir)
    wx$rid <- with(wx, as.integer(paste0(country_num, state_num, route)))
    
    # for 50-stop data, subset to 1997
    # note there is actually scattered 50-stop data prior to 1997...
    wx <- wx[wx$year >= 1997,]
    
    
    # geographic subsets
    # nb to limit to states with zip files to prevent erroneous zeros
    wx <- wx[wx$state_num %in% zip_use$state_num,]
    
    if (!is.null(bcr) | !is.null(strata)) {
      wx <- wx[wx$rid %in% routes$rid,]
    }
    if (!is.null(years)) {
      wx <- wx[wx$year %in% years,]
    }
    
    # create all combinations of aou and route_year_rpid
    if (!is.null(aou)) {
      aou_unique <- as.integer(aou)
    } else {
      # means will only create 0s for species observed in given subset of bbs
      aou_unique <- sort(unique(bbs$aou))
    }
    
    # create route-year-rpid column
    wx$ryr <- paste0(wx$country_num, wx$state_num, wx$route, wx$year, wx$rpid)
    
    n_aou <- length(aou_unique)
    n_wx <- nrow(wx)
    
    # all combinations of aou and route_year_rpid
    all_rows <- tibble(
      ryr = rep(wx$ryr, n_aou),
      route_data_id = rep(wx$route_data_id, n_aou),
      country_num = rep(wx$country_num, n_aou),
      state_num = rep(wx$state_num, n_aou),
      route = rep(wx$route, n_aou),
      rpid = rep(wx$rpid, n_aou),
      year = rep(wx$year, n_aou),
      aou = sort(rep(aou_unique, n_wx))
    )
    
    # create indices to match on (route-year-rpid-aou)
    all_rows_j <- as.numeric(paste0(all_rows$ryr, all_rows$aou))
    
    bbs_j <- as.numeric(paste0(bbs$country_num, bbs$state_num,
                               bbs$route, bbs$year, bbs$rpid,
                               bbs$aou))
    
    # match indices of all_rows and bbs
    m <- match(all_rows_j, bbs_j)
    m_i_not_na <- !is.na(m)
    m_not_na <- m[m_i_not_na]
    
    # create matrix of counts (stop_* columns)
    count_mat <- matrix(0L, nrow = nrow(all_rows), ncol = 50)
    count_col_names <- paste('stop', 1:50, sep = '_')
    count_col_bbs <- match(count_col_names, names(bbs))
    
    for(i in seq_along(count_col_bbs)) {
      count_mat[m_i_not_na,i] <- unlist(bbs[m_not_na, count_col_bbs[i]])
    }
    
    colnames(count_mat) <- count_col_names
    bbs <- as_tibble(cbind.data.frame(all_rows[,-1], count_mat))
    # -1 to remove ryr column
  }
  
  return(bbs)
}
