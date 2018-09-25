
#' @importFrom curl curl
#' @noRd
bbs_read_dir <- function(dir) {
  bbs_con <- curl(dir)
  dir_lines <- readLines(bbs_con)
  close(bbs_con)
  dir_lines_split <- strsplit(dir_lines, '[[:space:]]+')
  dir_files <- vapply(dir_lines_split, function(x) x[length(x)], character(1))
  dir_files <- dir_files[grep('\\.txt$|\\.pdf$|\\.zip$', dir_files)]
  return(dir_files)
}


#' @importFrom curl curl_download
#' @noRd
bbs_download_util <- function(bbs_dir, dest, subdir, dl_files, file_type,
                              overwrite, verbose) {
  
  no_overwrite <- character(0)
  
  for(i in 1:length(dl_files)) {
    if (overwrite == FALSE & file.exists(paste(dest, subdir, dl_files[i], sep = '/'))) {
      no_overwrite <- append(no_overwrite, dl_files[i])
    } else {
      message(paste('Downloading', file_type, 'file:', dl_files[i]))
      curl_download(paste(bbs_dir, subdir, dl_files[i], sep = '/'),
                    paste(dest, subdir, dl_files[i], sep = '/'))
    }
  }
  
  if (verbose == TRUE & length(no_overwrite) > 0) {
    message(paste0('The following files already exist and were not ',
                   're-downloaded:\n', paste(no_overwrite, collapse = ', ')),
            appendLF = TRUE)
  }
}


#' @importFrom utils unzip
#' @importFrom curl curl_download
#' @importFrom readr read_csv
#' @noRd
csv_unzip <- function(zip_path) {
  
  zip_path_split <- strsplit(zip_path, '/+')[[1]]
  file_zip <- zip_path_split[length(zip_path_split)]
  file_csv <- gsub('\\.zip', '\\.csv', file_zip)
  temp_dir <- paste0(tempdir(), '/', paste(sample(letters, 6), collapse = ''))
  dir.create(temp_dir)
  
  if(grepl('^http:|^ftp:', zip_path)) {
    temp_file <- tempfile()
    curl_download(zip_path, temp_file, quiet = FALSE)
    unzip(temp_file, exdir = temp_dir)
    unlink(temp_file)
  } else {
    unzip(zip_path, exdir = temp_dir)
  }
  
  file_csv <- list.files(temp_dir)
  file_csv <- file_csv[grep('\\.csv$', file_csv)]
  
  if (length(file_csv) > 1) {
    stop('Zip archive appears to contain more than one csv file')
  }
  
  dat <- suppressMessages(read_csv(paste(temp_dir, file_csv, sep = '/'),
                                   na = c('NA', 'NULL'),
                                   progress = FALSE))
  
  names(dat) <- bbs_col(names(dat))
  
  unlink(temp_dir)
  return(dat)
}


#' @importFrom utils read.fwf
#' @importFrom curl curl_download
#' @importFrom tibble as_tibble
#' @noRd
read_bbs_txt <- function(txt_file) {
  
  # if txt_file path ftp or http, download
  if (grepl('^ftp:|^http:|^https:', txt_file)) {
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
  col_names <- strsplit(txt_lines[delim - 1], split = '[[:blank:]]+')[[1]]
  col_names <- bbs_col(col_names)
  col_dashes <- strsplit(txt_lines[delim], split = '[[:blank:]]+')[[1]]
  col_widths <- nchar(col_dashes) + 1
  
  # read table component of txt_file as data.frame
  df <- read.fwf(temp, skip = delim, widths = col_widths,
                 fileEncoding = 'Latin1', strip.white = TRUE,
                 stringsAsFactors = FALSE, col.names = col_names,
                 na.strings = c('N/A', 'NULL', 's/o', 'N/D'))
  
  # convert to tibble
  return(as_tibble(df))
}


#' Get the names of state-specific zip files containing 10-stop count data from
#' README.txt
#' @importFrom tibble tibble
#' @importFrom curl curl_download
#' @noRd
bbs_files_10 <- function(bbs_dir = NULL) {
  
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  # Read README.txt
  txt_file <- paste0(bbs_dir, '/README.txt')
  
  # if txt_file path ftp or http, download
  if (grepl('^ftp:|^http:|^https:', txt_file)) {
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



#' @noRd
bbs_col_switch <- function(x) {
  
  switch(x,
         'bcrid' = 'bcr',
         'bcrname' = 'bcr_name',
         'bcrnamefrench' = 'bcr_name_french',
         'bcrnamespanish' = 'bcr_name_spanish',
         'stratumid' = 'stratum',
         'stratumname' = 'stratum_name',
         'stratumnamefrench' = 'stratum_name_french',
         'stratumnamespanish' = 'stratum_name_spanish',
         'countrynum' = 'country_num',
         'regioncode' = 'state_num',
         'state/prov/terrname' = 'state_name',
         'countryname' = 'country_name',
         'statenum' = 'state_num',
         'routename' = 'route_name',
         'routetypeid' = 'route_type_id',
         'routetypedetailid' = 'route_type_detail_id',
         'routedataid' = 'route_data_id',
         'obsn' = 'obs_n',
         'totalspp' = 'total_spp',
         'starttemp' = 'start_temp',
         'endtemp' = 'end_temp',
         'tempscale' = 'temp_scale',
         'startwind' = 'start_wind',
         'endwind' = 'end_wind',
         'startsky' = 'start_sky',
         'endsky' = 'end_sky',
         'starttime' = 'start_time',
         'endtime' = 'end_time',
         'qualitycurrentid' = 'quality_current_id',
         'runtype' = 'run_type',
         'stoptotal' = 'stop_total',
         'speciestotal' = 'species_total',
         x)
}

#' @noRd
bbs_col <- function(z) {
  z <- tolower(z)
  z <- gsub('^count(?=[[:digit:]])', 'count_', z, perl = TRUE)
  z <- gsub('^stop(?=[[:digit:]])', 'stop_', z, perl = TRUE)
  z <- vapply(z, bbs_col_switch, '', USE.NAMES = FALSE)
  return(z)
}


#' @noRd
title_case <- function(x) {
  s <- strsplit(x, "[[:space:]]")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}

#' @noRd
bbs_state_switch <- function(x) {
  switch(x,
         'Newfoundland And Labrador' = 'Newfoundland',
         'North West Territories' = 'Northwest Territories',
         x)
}

#' @noRd
bbs_state <- function(z) {
  z <- vapply(z, title_case, '', USE.NAMES = FALSE)
  z <- vapply(z, bbs_state_switch, '', USE.NAMES = FALSE)
  return(z)
}


#' @noRd
bbs_country_switch <- function(x) {
  switch(as.character(x),
         '124' = 'Canada',
         '484' = 'Mexico',
         '840' = 'United States',
         stop('country_num not found'))
}

#' @noRd
bbs_country <- function(z) {
  z <- vapply(z, bbs_country_switch, '', USE.NAMES = FALSE)
  return(z)
}

