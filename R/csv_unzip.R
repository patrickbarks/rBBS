#' Extract a csv file from a zipped archive of the same name
#' 
#' Extract a csv file from a zipped archive of the same name (except for the
#' file extension).
#'
#' @param zip_path Path to zipped archive
#'
#' @return
#' A \code{data.frame} returned by \code{read.csv}
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
#' @importFrom utils download.file
#' @importFrom readr read_csv
#' @export csv_unzip
csv_unzip <- function(zip_path) {

  zip_path_split <- strsplit(zip_path, '/+')[[1]]
  file_zip <- zip_path_split[length(zip_path_split)]
  file_csv <- gsub('\\.zip', '\\.csv', file_zip)
  
  if(grepl('^http:|^ftp:', zip_path)) {
    temp_file <- tempfile()
    download.file(zip_path, temp_file)
    temp_dir <- tempdir()
    unzip(temp_file, exdir = temp_dir)
    temp_dir_files <- list.files(temp_dir)
    file_csv_get <- temp_dir_files[tolower(temp_dir_files) == tolower(file_csv)]
    dat <- suppressMessages(read_csv(paste0(temp_dir, '/', file_csv_get),
                                     na = c('NA', 'NULL'),
                                     progress = FALSE))
    unlink(temp_file)
    unlink(temp_dir)
  } else {
    temp_dir <- tempdir()
    unzip(zip_path, exdir = temp_dir)
    temp_dir_files <- list.files(temp_dir)
    file_csv_get <- temp_dir_files[tolower(temp_dir_files) == tolower(file_csv)]
    dat <- suppressMessages(read_csv(paste0(temp_dir, '/', file_csv_get),
                                     na = c('NA', 'NULL'),
                                     progress = FALSE))
    unlink(temp_dir)
  }
  
  return(dat)
}

