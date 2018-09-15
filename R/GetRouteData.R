#' Function to query 10 or 50 stop data for species, year, and states
#' 
#' This function imports the 10 or 50 stop species data from either the USGS BBS
#' ftp server or another repository
#' 
#' @param bbs_dir Directory from which to get data. Defaults to the USGS FTP
#'   directory for the most recent BBS release. May alternatively be a path to a
#'   local directory, or ftp address for an older BBS release.
#' @param AOU Vector of species' AOU code
#' @param countrynum Vector of country codes, either 124 (Canada), 484 (Mexico),
#'   or 840 (USA).
#' @param states Vector of state codes. If they are unique to one country,
#'   countrynum can be NULL.
#' @param year Year(s) for which data is wanted: must be after 1997 (pre-1997
#'   data not included yet)
#' @param weather Data frame with Weather data. Can be NULL, then function will
#'   extract the data
#' @param routes Data frame with routes data. Can be NULL, then function will
#'   extract the data
#' @param Zeroes Logical: if TRUE (the default) will return all routes sampled
#'   in relevant years, otherwise only the routes where the species was
#'   observed.
#' @param TenStops Logical: if TRUE (the default) get 10-stop data. If false,
#'   get 50-stop data.
#' 
#' @return Data frame with the following columns for all data:
#'   \item{countrynum}{The three digit identification code for country. See
#'   RegionCodes.txt file for key.}
#'   \item{statenum}{The two digit numerical code that identifies the state,
#'   province or territory where the route was run.  See RegionCodes.txt file
#'   for key.}
#'   \item{routeID}{character code for route. Should be unique. made of paste
#'   of state & route IDs}
#'   \item{route}{The three digit code that identifies the route - unique
#'   within states.}
#'   \item{rpid}{Three digit run protocol identification number.  See
#'   RunProtocolID.txt for key.}
#'   \item{aou}{The five digit species identification code.}
#'   \item{Year}{The year}
#'   \item{Month}{The month}
#'   \item{Day}{The day}
#'   \item{RunType}{If this run is acceptable by BBS standards, then this column
#'   is 1, otherwise it is 0.}
#'   \item{Latitude}{The latitude of the route start point in decimal degrees.}
#'   \item{Longitude}{The longitude of the route start point in decimal
#'   degrees.}
#'   \item{SumCount}{Total number of individuals of that species counted}
#' 
#' For 10 stop data, these columns in addition:
#'   \item{count10}{Total individuals of the species recorded on stops 1-10.}
#'   \item{count20}{Total individuals of the species recorded on stops 11-20.}
#'   \item{count30}{Total individuals of the species recorded on stops 21-30.}
#'   \item{count40}{Total individuals of the species recorded on stops 31-40.}
#'   \item{count50}{Total individuals of the species recorded on stops 41-50.}
#'   \item{stoptotal}{Total number of stops out of 50 on which the species was
#'   recorded.}
#'   \item{speciestotal}{The total number of species recorded on that run of
#'   the route.}
#' 
#' For 10 stop data, these columns in addition:
#'   \item{stop1 ... stop50}{Total individuals of the species recorded on that
#'   stop.}
#' 
#' @details The pre-1997 data from Canada is not included.
#' @author Bob O'Hara
#' @references Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J.
#'   Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird
#'   Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent
#'   Wildlife Research Center, Laurel, MD
#' 
#' @examples 
#' ## Get data for pileated woodpecker & roadrunner for some states
#' Data <- GetRouteData(AOU = c(4050, 3850), countrynum = NULL,
#'                      states = c(89, 40:45), year = 2010, TenStops = TRUE,
#'                      Zeroes=FALSE)
#'
#' @importFrom RCurl getURL
#' @importFrom plyr ldply
#' @export GetRouteData
GetRouteData <- function(bbs_dir = NULL, AOU=NULL, countrynum=NULL, states=NULL,
                         year, weather=NULL, routes=NULL, Zeroes=TRUE,
                         TenStops = TRUE) {
  
  if (is.null(bbs_dir)) {
    bbs_dir <- bbs_ftp()
  }
  
  if(TenStops) {
    DirData <- paste0(bbs_dir, "States/")
    CountString <- "^count"
  } else {
    if(any(year<1997)) stop("Data only available from 1997: pre-1997 data not integrated into this function for 50 stop data (yet)")
    DirData <- paste0(bbs_dir, "50-StopData/1997ToPresent_SurveyWide/")
    CountString <- "^stop"
  }
  if(!is.null(countrynum) & any(!(countrynum%in%c(124, 484, 840)))) stop("countrynum should be either 124 (Canada), 484 (Mexico), or 840 (USA)")
  
  GetDat <- function(file, dir, year, AOU, countrynum, states) {
    dat <- GetUnzip(ZipName=paste0(dir, file), FileName=gsub("^Fifty", "fifty", gsub("zip", "csv", file)))
    names(dat) <- tolower(names(dat))
    if(is.null(year)) {  UseYear <- TRUE  } else {  UseYear <- dat$year%in%year  }
    if(is.null(AOU)) {  UseAOU <- TRUE  } else {  UseAOU <- dat$aou%in%AOU  }
    if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- dat$countrynum%in%countrynum  }
    if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- dat$statenum%in%states  }
    Use <- UseYear & UseAOU & UseCountry & UseState
    if(sum(Use)>0) {
      dat$routeID <- paste(dat$statenum, dat[,grep("^[Rr]oute$", names(dat))])
      dat <- subset(dat, subset=Use)
      return(dat)      
    } else return(NULL)
  }
  
# Only use the files we want
  CountriesToUse <- if(!is.null(countrynum)) {
    RegionsForZipFiles$countrynum%in%countrynum 
  } else {
    TRUE
  }
  StatesToUse <- if(!is.null(states)) {
    RegionsForZipFiles$RegionCode%in%states 
  } else {
    TRUE
  }
  ToUse <- CountriesToUse & StatesToUse
  if(TenStops) {
    Files <- RegionsForZipFiles$FileName10stop[ToUse]
    Missing <- ToUse & is.na(RegionsForZipFiles$FileName10stop)
  } else { # 50 stop
    Files <- RegionsForZipFiles$FileName50stop[ToUse]
    Missing <- ToUse & is.na(RegionsForZipFiles$FileName50stop)
  }
  
  if(length(Files)==0) stop("No data for the states specified")
  if(any(is.na(Files))) warning(paste0("No data for these states: ", paste(RegionsForZipFiles$'State/Prov/TerrName'[Missing], collapse=", ")))
  
  Data.lst <- sapply(Files[!is.na(Files)], GetDat, dir=DirData, year=year, AOU=AOU, countrynum=countrynum, states=states, simplify=FALSE)
  
  if(all(unlist(lapply(Data.lst, is.null)))) {
    warning("no data, sorry")
    AllData <- NULL
  } else {
    Data <- ldply(Data.lst)
# Get route data for all routes, and annual data
    if(is.null(weather)) weather <-GetWeather(bbs_dir)
    if(is.null(year)) {  UseYear <- TRUE  } else {  UseYear <- weather$Year%in%year  }
    if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- weather$countrynum%in%countrynum  }
    if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- weather$statenum%in%states  }
    UseWeather <- UseYear & UseCountry & UseState
    
    if(is.null(routes)) routes <- GetRoutes(bbs_dir)
    if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- routes$countrynum%in%countrynum  }
    if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- routes$statenum%in%states  }
    UseRoutes <- UseCountry & UseState
    
    CommonNames <- names(Data)[names(Data)%in%names(weather)]
    CommonNames <- CommonNames[CommonNames%in%names(routes)]
    
    # Subset data
    # First, sites sampled in chosen year(s)
    weather <-subset(weather, subset=UseWeather, 
                     select=c(CommonNames, "Year", "Month", "Day", "RunType"))
    # Route data for sites sampled in chosen years
    routes <- subset(routes, subset=UseRoutes & routes$routeID%in%weather$routeID, 
                     select=c(CommonNames, "Latitude", "Longitude"))
    
  # merge data sets
    dat.routeID.year <- paste(Data$routeID, Data$year, sep=".")
    weather.routeID.year <- paste(weather$routeID, weather$Year, sep=".")
    
    GetID <- function(datID, otherIDs) {
      if(length(datID)!=1) stop("datID should be a scalar")
      wh <- which(datID==otherIDs)
      if(length(wh)!=1) {
        if(length(wh)==0) {
          warning("no ID, so setting to NA")
        } else {
          warning("no unique ID, so using first value")
        }
        wh <- wh[1]
      }
      wh
    }
    WeatherWhiches <- sapply(dat.routeID.year, GetID, otherIDs=weather.routeID.year)
#    WeatherWhiches <- sapply(dat.routeID.year, GetID, otherIDs=weather$routeID.year)
    RouteWhiches <- sapply(Data$routeID, GetID, otherIDs=routes$routeID)
    
    AllData <- cbind(Data, weather[WeatherWhiches, !names(weather)%in%names(Data)],
                     routes[RouteWhiches, !names(routes)%in%names(Data)])
    
  #  if(!is.na(weather)) AllData <- merge(Data, weather, all=TRUE) # by=c("routeID", "RPID"), 
  #  if(!is.na(routes))  AllData <- merge(AllData, routes, all=TRUE) # by="routeID", 
    AllData$SumCount <- apply(AllData[,grep(CountString, names(AllData))],1,sum, na.rm=TRUE)
    if(!Zeroes) AllData <- subset(AllData, AllData$SumCount>0)
    AllData <- AllData[,!names(AllData)%in%c(".id", "routedataid", "year")]
  }

  AllData
}


# Also: add a vars option, to only return some variables
#   Try <- GetRouteData(countrynum=NULL, states=c(89, 40:60), weather=NULL, routes=NULL, AOU=c(4050, 3850), year=2010, Zeroes=FALSE, Dir=NULL)
