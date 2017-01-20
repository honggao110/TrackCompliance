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
#'
#' @return a data frame or a json
#'
#' @examples
#'
#' @importFrom jsonlite fromJSON
#'
#' @export

DayCompliance <- function() {

}
