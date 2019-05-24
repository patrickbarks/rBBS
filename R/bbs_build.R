
#' @noRd
#' @importFrom tibble add_column
bbs_build <- function(bbs_dir, zeros = FALSE, countries = NULL,
                      states = NULL, bcr = NULL, strata = NULL,
                      aou = NULL, years = NULL, type) {
  
  if ((!is.null(countries) | !is.null(states)) &
      (!is.null(bcr) | !is.null(strata))) {
    stop("Cannot subset for both country/states AND bcr/strata")
  }
  
  type <- match.arg(type, c("10", "50"))
  
  zip_dir <- switch(as.character(type),
                    "10" = "States/",
                    "50" = "50-StopData/1997ToPresent_SurveyWide/")
  
  zip_files <- list.files(paste(bbs_dir, zip_dir, sep = "/"))
  zip_files <- zip_files[grepl("\\.zip$", zip_files)]
  
  if (length(zip_files) == 0) {
    stop(paste("No .zip files found within directory", zip_dir))
  }
  
  # subset based on geography
  if (!is.null(countries) | !is.null(states) | !is.null(bcr) |
      !is.null(strata) | zeros == TRUE) {
    
    zip_use <- bbs_meta_regions(bbs_dir, zip_files = TRUE)
    
    file_col <- switch(as.character(type),
                       "10" = "ten_stop_file",
                       "50" = "fifty_stop_file")
    
    zip_use <- zip_use[!is.na(zip_use[[file_col]]),]
    zip_use$country_name <- tolower(zip_use$country_name)
    zip_use$state_name <- tolower(zip_use$state_name)
    zip_use$use <- FALSE
    
    if (!all(zip_files %in% unique(zip_use[[file_col]]))) {
      warning(paste("One or more zip files within the",
                    paste0(type, "-stop"), "directory is not recognized"))
    }
    
    if (!is.null(countries)) {
      zip_use$use[zip_use$country_name %in% tolower(countries)] <- TRUE
    }
    if (!is.null(states)) {
      zip_use$use[zip_use$state_name %in% tolower(states)] <- TRUE
    }
    
    routes <- bbs_meta_routes(bbs_dir = bbs_dir)
    routes$route_unique <- bbs_route_unique(routes)
    
    if (!is.null(bcr) | !is.null(strata)) { 
      
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
    
    if (is.null(countries) & is.null(states) & is.null(bcr) &
        is.null(strata)) {
      # if zeros == TRUE but no geographic subset use all
      zip_use$use <- TRUE
    }
    
    zip_use <- zip_use[zip_use$use == TRUE,]
    
    if (!all(unique(zip_use[[file_col]]) %in% zip_files)) {
      zip_expected <- unique(zip_use[[file_col]])
      n_expected <- length(zip_expected)
      n_avail <- length(which(zip_files %in% zip_expected))
      zip_use <- zip_use[zip_use[[file_col]] %in% zip_files,]
      warning(paste("The", paste0(type, "-state"), "directory only contains",
                    n_avail, "of the", n_expected, "zip files that match the",
                    "given geographic subset. Data returned are limited to",
                    "available files."))
    }
    
    zip_files <- unique(zip_use[[file_col]])
  }
  
  zip_paths <- paste(bbs_dir, zip_dir, zip_files, sep = "/")
  bbs_l <- lapply(zip_paths, csv_unzip)
  bbs <- do.call(rbind.data.frame, bbs_l)
  bbs <- bbs_standardize(bbs)
  
  if (!is.null(bcr) | !is.null(strata)) {
    route_unique <- bbs_route_unique(bbs)
    bbs <- bbs[route_unique %in% routes$route_unique,]
  }
  
  if (!is.null(years)) {
    bbs <- bbs[bbs$year %in% as.integer(years),]
  }
  
  if (!is.null(aou)) {
    bbs <- bbs[bbs$aou %in% as.integer(aou),]
  }
  
  # add route_data_id col if necessary (older BBS releases include it in
  #  the weather table but not the 10- or 50-stop files)
  if (!"route_data_id" %in% names(bbs)) {
    wx <- bbs_meta_weather(bbs_dir = bbs_dir)
    wx_route_unique <- bbs_route_unique(wx)
    bbs_route_unique <- bbs_route_unique(bbs)
    m <- match(bbs_route_unique, wx_route_unique)
    bbs <- add_column(bbs, route_data_id = wx$route_data_id[m], .before = 1)
  }
  
  if (zeros == TRUE) {
    bbs <- bbs_build_zeros(bbs_dir = bbs_dir, zeros = zeros,
                           countries = countries, states = states, bcr = bcr,
                           strata = strata, aou = aou, years = years,
                           type = type, zip_use = zip_use, routes = routes,
                           bbs = bbs)
  }
  
  return(bbs)
}




#' @noRd
#' @importFrom tibble as_tibble
bbs_build_zeros <- function(bbs_dir, zeros, countries, states, bcr, strata, aou,
                            years, type, zip_use, routes, bbs) {
  
  # get weather metadata
  wx <- bbs_meta_weather(bbs_dir = bbs_dir)
  
  # for 50-stop data, subset to 1997+
  if (type == "50") { wx <- wx[wx$year >= 1997,] }
  
  # nb to limit to states with zip files to prevent erroneous zeros
  wx <- wx[wx$state_num %in% zip_use$state_num,]
  
  wx$route_unique <- bbs_route_unique(wx)
  
  if (!is.null(bcr) | !is.null(strata)) {
    wx <- wx[wx$route_unique %in% routes$route_unique,]
  }
  if (!is.null(years)) {
    wx <- wx[wx$year %in% years,]
  }
  
  if (!is.null(aou)) {
    aou_unique <- as.integer(aou)
  } else {
    # means will only create 0s for species observed in given subset of bbs
    aou_unique <- sort(unique(bbs$aou))
  }
  
  n_aou <- length(aou_unique)
  n_wx <- nrow(wx)
  
  # all combinations of aou and route_year_rpid
  out <- data.frame(
    route_data_id = rep(wx$route_data_id, n_aou),
    country_num = rep(wx$country_num, n_aou),
    state_num = rep(wx$state_num, n_aou),
    route = rep(wx$route, n_aou),
    rpid = rep(wx$rpid, n_aou),
    year = rep(wx$year, n_aou),
    aou = unlist(lapply(sort(aou_unique), rep, times = n_wx)),
    stringsAsFactors = FALSE
  )
  
  # create indices to match on (route-year-rpid-aou)
  out_j <- paste0(out$route_data_id, out$aou)
  bbs_j <- paste0(bbs$route_data_id, bbs$aou)
  
  # match indices of out and bbs
  m <- match(out_j, bbs_j)
  m_i_not_na <- !is.na(m)
  m_not_na <- m[m_i_not_na]
  
  # bird count columns
  count_col_names <- switch(
    as.character(type),
    "10" = c(paste('count', seq(10, 50, 10), sep = "_"),
             "stop_total", "species_total"),
    "50" = paste("stop", seq(1, 50, 1), sep = "_"))
  
  # create matrix of counts
  count_mat <- matrix(0L, nrow = nrow(out), ncol = length(count_col_names))
  count_col_bbs <- match(count_col_names, names(bbs))
  
  for(i in seq_along(count_col_bbs)) {
    count_mat[m_i_not_na,i] <- as.integer(
      unlist(bbs[m_not_na, count_col_bbs[i]])
    )
  }
  
  colnames(count_mat) <- count_col_names
  bbs <- as_tibble(cbind.data.frame(out, count_mat, stringsAsFactors = FALSE))
  
  return(bbs)
}

