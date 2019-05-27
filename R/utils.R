
#' @importFrom curl curl
#' @noRd
bbs_read_dir <- function(dir) {
  bbs_con <- curl(dir)
  dir_lines <- readLines(bbs_con)
  close(bbs_con)
  dir_lines_split <- strsplit(dir_lines, "[[:space:]]+")
  dir_files <- vapply(dir_lines_split, function(x) x[length(x)], character(1))
  dir_files <- dir_files[grep("\\.txt$|\\.pdf$|\\.zip$", dir_files)]
  return(dir_files)
}


#' @importFrom curl curl_download
#' @noRd
bbs_download_util <- function(bbs_dir, dest, subdir, dl_files, file_type,
                              overwrite, verbose) {
  
  no_overwrite <- character(0)
  dest_path <- paste(dest, subdir, dl_files, sep = "/")
  down_path <- paste(bbs_dir, subdir, dl_files, sep = "/")
  
  for(i in seq_along(dl_files)) {
    if (overwrite == FALSE & file.exists(dest_path[i])) {
      no_overwrite <- append(no_overwrite, dl_files[i])
    } else {
      message(paste("Downloading", file_type, "file:", dl_files[i]))
      curl_download(down_path[i], dest_path[i])
    }
  }
  
  if (verbose == TRUE & length(no_overwrite) > 0) {
    message(paste0("The following files already exist and were not ",
                   "re-downloaded:\n", paste(no_overwrite, collapse = ", ")),
            appendLF = TRUE)
  }
}



#' @importFrom curl curl_download
#' @importFrom readr read_csv
#' @noRd
csv_unzip <- function(zip_path) {

  zip_path_split <- strsplit(zip_path, "/+")[[1]]
  file_zip <- zip_path_split[length(zip_path_split)]

  # if zip_path ftp or http, download
  if (grepl("^ftp:|^http:|^https:", zip_path)) {
    temp <- tempdir()
    local_zip_path <- paste(temp, file_zip, sep = "/")
    curl_download(zip_path, local_zip_path, quiet = FALSE)
    zip_path <- local_zip_path
  }
  
  dat <- suppressMessages(
    read_csv(zip_path, na = c("NA", "NULL", "N"), progress = FALSE)
  )
  
  return(dat)
}


#' @importFrom utils read.fwf
#' @importFrom curl curl_download
#' @importFrom tibble as_tibble
#' @noRd
read_bbs_txt <- function(txt_file) {
  
  # if txt_file path ftp or http, download
  if (grepl("^ftp:|^http:|^https:", txt_file)) {
    temp <- tempfile()
    curl_download(txt_file, temp, quiet = FALSE)
  } else {
    temp <- txt_file
  }
  
  # read lines
  txt_lines <- scan(temp, what = "character", sep = "\n",
                    encoding = "latin1", quiet = TRUE,
                    blank.lines.skip = FALSE)
  
  # extract column names and widths
  delim <- grep("^-", txt_lines)
  if (length(delim) == 2) { delim <- delim[2] } # for RegionCodes.txt
  col_names <- strsplit(txt_lines[delim - 1], split = "[[:blank:]]+")[[1]]
  col_dashes <- strsplit(txt_lines[delim], split = "[[:blank:]]+")[[1]]
  col_widths <- nchar(col_dashes) + 1
  
  # read table component of txt_file as data.frame
  df <- read.fwf(temp, skip = delim, widths = col_widths,
                 fileEncoding = "Latin1", strip.white = TRUE,
                 stringsAsFactors = FALSE, col.names = col_names,
                 na.strings = c("N/A", "NULL", "s/o", "N/D"))
  
  # convert to tibble
  return(as_tibble(df))
}



#' Get the names of state-specific zip files containing 10-stop count data from
#' README.txt
#' @importFrom tibble tibble
#' @importFrom curl curl_download
#' @noRd
bbs_files_10 <- function(bbs_dir = bbs_ftp()) {
  
  # Read README.txt
  txt_file <- paste0(bbs_dir, "/README.txt")
  
  # if txt_file path ftp or http, download
  if (grepl("^ftp:|^http:|^https:", txt_file)) {
    temp <- tempfile()
    curl_download(txt_file, temp, quiet = FALSE)
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
    txt_lines, function(x) strsplit(x, "[[:space:]][[:space:]]+")[[1]][1],
    "", USE.NAMES = FALSE
  )
  
  state_name <- vapply(
    txt_lines, function(x) strsplit(x, "[[:space:]][[:space:]]+")[[1]][3],
    "", USE.NAMES = FALSE
  )
  
  # standardize state names
  state_name <- bbs_state(state_name)
  
  return(tibble(state_name, ten_stop_file))
}

