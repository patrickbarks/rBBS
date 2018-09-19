
#' @noRd
bbs_ftp <- function() {
  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/"
}

#' @noRd
bbs_read_dir <- function(dir) {
  bbs_con <- url(dir, method = 'libcurl')
  dir_lines <- readLines(bbs_con)
  close(bbs_con)
  dir_lines_split <- strsplit(dir_lines, '[[:space:]]+')
  dir_files <- vapply(dir_lines_split, function(x) x[length(x)], character(1))
  dir_files <- dir_files[grep('\\.txt$|\\.pdf$|\\.zip$', dir_files)]
  return(dir_files)
}


#' @importFrom utils read.fwf
#' @importFrom utils download.file
#' @noRd
read_bbs_txt <- function(txt_file) {
  
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
  
  # extract column names and widths
  delim <- grep("^-", txt_lines)
  col_names <- strsplit(txt_lines[delim - 1], split = '[[:blank:]]+')[[1]]
  col_dashes <- strsplit(txt_lines[delim], split = '[[:blank:]]+')[[1]]
  col_widths <- nchar(col_dashes) + 1
  
  # read table component of txt_file as data.frame
  read.fwf(temp, skip = delim, widths = col_widths,
           fileEncoding = 'Latin1', strip.white = TRUE,
           stringsAsFactors = FALSE, col.names = col_names,
           na.strings = c('N/A', 'NULL', 's/o', 'N/D'))
}
