
bbs_ftp <- function() {
  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/"
}


#' @importFrom utils read.fwf
read_bbs_txt <- function(txt_file) {
  
  # if txt_file ftp/http, download
  if (grepl('ftp:|http:|https:', txt_file)) {
    temp <- tempfile()
    download.file(txt_file, temp, quiet = TRUE)
  } else {
    temp <- txt_file
  }
  
  # read lines
  txt_lines <- scan(temp, what = "character", sep = "\n",
                    encoding = "latin1", quiet = TRUE)
  
  # extract column names and widths
  delim <- grep("^-", txt_lines)
  col_names <- strsplit(txt_lines[delim - 1], split = '[[:blank:]]+')[[1]]
  col_dashes <- strsplit(txt_lines[delim], split = '[[:blank:]]+')[[1]]
  col_widths <- nchar(col_dashes) + 1

  # read table component of txt_file as data.frame
  read.fwf(temp, skip = delim, widths = col_widths,
           fileEncoding = 'Latin1', strip.white = TRUE,
           stringsAsFactors = FALSE, col.names = col_names)
}
