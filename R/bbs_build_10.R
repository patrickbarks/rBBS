#' Assemble North American Breeding Bird Survey 10-stop count data
#'
#' Assemble North American Breeding Bird Survey 10-stop count data into a data
#' frame, optionally subset to select regions, years, and/or species. The
#' underlying data come from state/province-specific zipped csv files within the
#' BBS folder \emph{/States/}. These csv files do not include counts of zero
#' (i.e. when a species is not observed during a given survey), but counts of
#' zero can optionally be built in (see argument \code{zeros}).\cr\cr
#' \strong{Note}: Without counts of zero the full 10-stop dataset for 2017
#' includes 6.4 million rows. With zeros it includes 90 million rows, which will
#' take some time to build and require a few gigabytes of memory. If building
#' with zeros, consider subsetting to regions, years, and/or species of
#' interest.
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
#'   \item{count_10}{total individuals of the species recorded on stops 1-10}
#'   \item{count_20}{total individuals of the species recorded on stops 11-20}
#'   \item{count_30}{total individuals of the species recorded on stops 21-30}
#'   \item{count_40}{total individuals of the species recorded on stops 31-40}
#'   \item{count_50}{total individuals of the species recorded on stops 41-50}
#'   \item{stop_total}{number of stops out of 50 on which the species was
#'   recorded}
#'   \item{species_total}{total individuals recorded on that run of the route
#'   (sum from all stops)}
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
#' bbs_build_10('.', countries = 'Canada', states = 'Montana')
#' }
#' 
#' will return data for all Canadian provinces/territories plus the state of
#' Montana. Likewise, the following lines are equivalent, and will both return
#' data for all American states:
#' 
#' \code{bbs_build_10('.', countries = 'United States', states = 'Florida')} \cr
#' \code{bbs_build_10('.', countries = 'United States')}
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
#' bbs <- bbs_build_10(bbs_dir = ".")
#' 
#' # build with Canadian routes only, including counts of zero
#' bbs <- bbs_build_10(bbs_dir = ".", zeros = TRUE, countries = 'Canada')
#' 
#' # build for species Gray Jay (aou 4840), including counts of zero
#' bbs <- bbs_build_10(bbs_dir = ".", zeros = TRUE, aou = 4840)
#' }
#' 
#' @export bbs_build_10
bbs_build_10 <- function(bbs_dir, zeros = FALSE, countries = NULL,
                         states = NULL, bcr = NULL, strata = NULL,
                         aou = NULL, years = NULL) {
  
  bbs_build(bbs_dir = bbs_dir, zeros = zeros, countries = countries,
            states = states, bcr = bcr, strata = strata, aou = aou,
            years = years, type = "10")
}
