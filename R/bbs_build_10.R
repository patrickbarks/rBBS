#' Build data frame with North American Breeding Bird Survey 10-stop data
#' 
#' Build data frame with North American Breeding Bird Survey 10-stop data
#'
#' @param bbs_dir Directory from which to load data. Must be a path to an
#'   existing local directory.
#'   
#' @return
#' \code{data.frame} with the following columns:
#'   \item{route_data_id}{Data identification number for unique combinations of
#'   country_num, state, route, rpid, and year}
#'   \item{country_num}{Three digit code that identifies country: 124 (Canada),
#'   484 (Mexico), or 840 (USA)}
#'   \item{state_num}{Two digit numerical code that identifies the state,
#'   province, or territory}
#'   \item{route}{Three digit code that identifies the route.}
#'   \item{rpid}{The run protocol identification number.}
#'   \item{year}{The year.}
#'   \item{aou}{American Ornithological Union code number}
#'   \item{count_10}{Total individuals of the species recorded on stops 1-10.}
#'   \item{count_20}{Total individuals of the species recorded on stops 11-20.}
#'   \item{count_30}{Total individuals of the species recorded on stops 21-30.}
#'   \item{count_40}{Total individuals of the species recorded on stops 31-40.}
#'   \item{count_50}{Total individuals of the species recorded on stops 41-50.}
#'   \item{stop_total}{Total number of stops out of 50 on which the species was
#'   recorded.}
#'   \item{species_total}{The total number of species recorded on that run of
#'   the route.}
#' 
#' @author Bob O'Hara
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#'   
#' @examples
#' 
#' @export bbs_build_10
bbs_build_10 <- function(bbs_dir) {
  
  ts_dir <- '/States/'
  files_ts <- list.files(paste0(bbs_dir, ts_dir))
  files_ts <- files_ts[grepl('\\.zip$', files_ts)]
  paths_ts <- paste0(bbs_dir, ts_dir, files_ts)
  
  bbs_10_l <- lapply(paths_ts, csv_unzip)
  bbs_10 <- do.call(rbind.data.frame, bbs_10_l)
  
  return(bbs_10)
}
