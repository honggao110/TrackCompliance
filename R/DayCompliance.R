#' Calculate Within Day Compliance
#'
#' This function calculates within day compliance and returns hours completed within
#' a day and a completion value of 1 or 0 to indicate whether
#' the tracking data meets the minimum tracking requirements. Requirements are passed
#' in the parameters. For a day to be successful, hence \code{completion = 1}, there
#' must be at least one point in each time bin (i.e. 15 minutes).
#'
#' @param json a json object.
#' @param id the name of personal identifier in json.
#' @param coor longitude and latitude of tracking points in the format of c("lon","lat").
#' @param time local time variable
#' @param time.format format of \code{time} to pass to \code{as.POSIXct}
#' @param timebin time bin to test whether at least one point is recorded
#' @param hour.require the number of hours required for a day to be successful
#' @param output a string object indicating what format the output should be. Must be
#' either "json" or "dataframe".
#'
#' @return a data frame or a json
#'
#' @examples
#'
#' @importFrom jsonlite fromJSON
#'
#' @export

DayCompliance <- function(json, id = NULL, coor = NULL,
                          time = NULL, time.format = NULL,
                          timebin = NULL,
                          hour.require = NULL,
                          output = c("json", "dataframe"))
  {
  if (is.null(id)){
    stop("id can't be NULL.")
  }

  if (is.null(coor)){
    stop("coor can't be NULL.")
  }

  if (is.null(time)){
    stop("time can't be NULL.")
  }

  if (is.null(time.format)){
    stop("time.format can't be NULL.")
  }

  if (is.null(hour.require)){
    stop("hour.require can't be NULL.")
  } else if (!is.numeric(hour.require)) {
    stop("hour.require must be numeric.")
  }

  if (is.null(output)){
    stop("output should be specified ('json' or 'dataframe'). ")
  } else if (!output %in% c("json", "dataframe")){
    stop("output should be 'json' or 'dataframe' ")
  }

  track <- TrackJsonToDF(json, id = id, coor = coor, time = time,
                         time.format = time.format)
  track$datetime <- as.POSIXct(track[, time], format = time.format)
  track<- track[order(track[, id],track$datetime),]
  track$date<- as.Date(track$datetime, format="%Y-%m-%d")
  track$hourscomp<- TimeBin(track, time = "datetime", timebin = timebin, groupvar = id)
  day<- unique(track[c(id, "date", "hourscomp")])
  day$comp<- as.numeric(ifelse(day$hourscomp >= hour.require, 1, 0))

  # data output
  if (output == "json"){
    data.return <- list(DayCompliance= day)
    data.return <- jsonlite::toJSON(data.return, dataframe = "columns")
    data.return<- gsub("\\[|\\]","",data.return)
  } else {
    data.return <- day
  }

  return(data.return)
}
