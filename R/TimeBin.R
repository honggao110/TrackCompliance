#' Calculate Compliance Status Given a Time Bin
#'
#' This function calculates the distance between two consecutive stay events using their geolocations.
#'
#' @param df a data frame object.
#' @param coor longitude and latitude of the spatial points in the format of c("lon","lat").
#' @param groupvar grouping object to stratify time objects. Recommend to be ID for each individual.
#' If \code{groupvar} is not specified, the entire data frame will be considered as one group to
#' calculate displacement.
#'
#' @return a list of distance values. Unit is meters.
#'
#' @examples
#' data(mobility)
#' mobility_stay<- stayevent(mobility, coor = c("lon","lat"), time = "datetime", dist.threshold = 100,
#' time.threshold = 30, time.units = "mins", groupvar = "id")
#' mobility_stay<- aggregate(cbind(id, stayeventlon, stayeventlat)~stayeventgroup, mobility_stay, mean)
#' mobility_stay$dispm<- displacement(mobility_stay, coor = c("stayeventlon","stayeventlat"), groupvar = "id")
#'
#' @importFrom geosphere distVincentyEllipsoid
#'
#' @export

TimeBin<- function(df, time = NULL, timebin = NULL, groupvar = NULL) {
  if (is.atomic(df)) {
    df <- data.frame(x = df)
  }

  if (!is.POSIXct(df[time][[1]])){
    stop("Time variable must be POSIXct format.")
  }

  if (is.null(timebin)){
    stop("timebin can't be NULL.")
  }

  if (is.null(groupvar)){
    stop("groupvar can't be NULL.")
  }

  if (!is.null(groupvar)){
    df1<- df
    df1$date<- as.Date(as.character(df1[,time]), format="%Y-%m-%d")
    df1$hour<- hour(df1[,time])
    df1$timenum<- as.numeric(df1[,time])
    df1["roundi"]<- df1[time]-(df1$timenum %% (60*timebin))
    pointminute<- count(df1, c(groupvar,"date","hour","roundi"))
    pointminute<- rename(pointminute, c(freq="pointi"))
    pointminute["checki"]<- as.numeric(ifelse(pointminute["pointi"]>=1, 1, 0))
    houri<- aggregate(pointminute[,"checki"]~get(groupvar)+date+hour, pointminute, sum)
    names(houri)<-c(groupvar,"date","hour","checki")
    houri["compi"]<- as.numeric(ifelse(houri["checki"]>=(60/timebin)*0.8, 1, 0))
    dayi<- aggregate(compi~get(groupvar)+date, houri, sum)
    names(dayi)<-c(groupvar,"date","compi")
    df1<- merge(df1, dayi, by=c(groupvar, "date"))
    df1<- df1[order(df1[groupvar],df1[time]),]
    return(df1$compi)
  }
}
