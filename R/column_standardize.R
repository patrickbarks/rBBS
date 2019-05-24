
#' Standardize column names and types
#' @noRd
bbs_standardize <- function(df) {
  
  if (!("data.frame" %in% class(df) | "tbl_df" %in% class(df))) {
    stop("Expecting df to be of class data.frame (and/or tbl_df)")
  }
  
  # standardize column names
  names(df) <- bbs_column_names(names(df))
  
  # standardize state names
  if ("state_name" %in% names(df)) {
    df$state_name <- bbs_state(df$state_name)
  }
  
  # integerize select columns
  cols_int <- c("aou", "country_num", "state_num", "route", "rpid",
                "year", "stratum", "bcr", "assistant", "run_type", "active",
                "route_type_id", "route_type_detail_id", "quality_current_id",
                "stop_total", "species_total", "route_data_id",
                "month", "day", "obs_n", "total_spp", "start_temp", "end_temp",
                "start_wind", "end_wind", "start_sky", "end_sky",
                "start_time", "end_time",
                paste("count", seq(10, 50, 10), sep = "_"),
                paste("stop", seq(1, 50, 1), sep = "_"))
  
  if (any(names(df) %in% cols_int)) {
    df <- integerize(df, intersect(names(df), cols_int))
  }
  
  return(df)
}



#' @noRd
bbs_column_name_switch <- function(x) {
  
  switch(x,
         "bcrid" = "bcr",
         "bcrname" = "bcr_name",
         "bcrnamefrench" = "bcr_name_french",
         "bcrnamespanish" = "bcr_name_spanish",
         "stratumid" = "stratum",
         "stratumname" = "stratum_name",
         "stratumnamefrench" = "stratum_name_french",
         "stratumnamespanish" = "stratum_name_spanish",
         "countrynum" = "country_num",
         "regioncode" = "state_num",
         "state/prov/terrname" = "state_name",
         "state.prov.terrname" = "state_name",
         "countryname" = "country_name",
         "statenum" = "state_num",
         "routename" = "route_name",
         "routetypeid" = "route_type_id",
         "routetypedetailid" = "route_type_detail_id",
         "routedataid" = "route_data_id",
         "obsn" = "obs_n",
         "totalspp" = "total_spp",
         "starttemp" = "start_temp",
         "endtemp" = "end_temp",
         "tempscale" = "temp_scale",
         "startwind" = "start_wind",
         "endwind" = "end_wind",
         "startsky" = "start_sky",
         "endsky" = "end_sky",
         "starttime" = "start_time",
         "endtime" = "end_time",
         "qualitycurrentid" = "quality_current_id",
         "runtype" = "run_type",
         "stoptotal" = "stop_total",
         "speciestotal" = "species_total",
         "landtypeid" = "land_type_id",
         x)
}



#' @noRd
bbs_column_names <- function(z) {
  z <- tolower(z)
  z <- gsub("^count(?=[[:digit:]])", "count_", z, perl = TRUE)
  z <- gsub("^stop(?=[[:digit:]])", "stop_", z, perl = TRUE)
  z <- vapply(z, bbs_column_name_switch, "", USE.NAMES = FALSE)
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
         "Newfoundland And Labrador" = "Newfoundland",
         "North West Territories" = "Northwest Territories",
         x)
}



#' @noRd
bbs_state <- function(z) {
  z <- vapply(z, title_case, "", USE.NAMES = FALSE)
  z <- vapply(z, bbs_state_switch, "", USE.NAMES = FALSE)
  return(z)
}



#' @noRd
bbs_country_switch <- function(x) {
  switch(as.character(x),
         "124" = "Canada",
         "484" = "Mexico",
         "840" = "United States",
         stop("country_num not found"))
}



#' @noRd
bbs_country <- function(z) {
  z <- vapply(z, bbs_country_switch, "", USE.NAMES = FALSE)
  return(z)
}


#' Convert all columns to integer
#' @noRd
integerize <- function(df, cols = names(df)) {
  for(i in cols) { df[[i]] <- as.integer(df[[i]]) }
  return(df)
}

