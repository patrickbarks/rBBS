#' Get the names of state-specific zip files containing 10-stop count data from
#' README.txt
#'
#' Get the names of state-specific zip files containing 10-stop count data from
#' the BBS file \emph{README.txt}.
#'
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#' 
#' @return
#' A \code{data.frame} with the following columns:
#'   \item{state_name}{name of state/province/territory}
#'   \item{ten_stop_file}{name of zip file with 10-stop survey data}
#' 
#' @details Note that not all regions have a zip file. As of 2017 there is no
#'   data for Mexico, Puerto Rico, or Washington D.C.
#' @author Bob O'Hara
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @references Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding and M.-A.R.
#'   Hudson. 2018. North American Breeding Bird Survey Dataset 1966-2017,
#'   version 2017.0. U.S. Geological Survey, Patuxent Wildlife Research Center.
#'   \url{https://doi.org/10.5066/F76972V8}
#' 
#' @importFrom tibble tibble
#' @export bbs_files_10
bbs_files_10 <- function(bbs_dir = NULL) {
  
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
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
    txt_lines, function(x) strsplit(x, '[[:space:]][[:space:]]+')[[1]][1],
    '', USE.NAMES = FALSE
  )
  
  state_name <- vapply(
    txt_lines, function(x) strsplit(x, '[[:space:]][[:space:]]+')[[1]][3],
    '', USE.NAMES = FALSE
  )
  
  # standardize state names
  state_name <- bbs_state(state_name)
  
  return(tibble(state_name, ten_stop_file))
}
