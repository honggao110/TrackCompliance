#' Transform Tracking Data from json to Data Frame
#'
#' This function transforms tracking data from json to data frame and rename variables to
#' desired names.
#'
#' @param json a json object.
#' @param id the name of personal identifier in json.
#' @param coor longitude and latitude of tracking points in the format of c("lon","lat").
#' @param time local time variable
#' @param time.format format of \code{time} to pass to \code{as.POSIXct}
#'
#' @return a data frame.
#'
#' @examples
#'
#' @importFrom jsonlite fromJSON
#'
#' @export

TrackJsonToDF <- function(json, id = NULL, coor = NULL, time = NULL, time.format = NULL)
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

  track.data <- jsonlite::fromJSON(json)
  track.data <- data.frame(track.data)
  track.data <- track.data[, c(id, coor, time)]
  track.data[, time] <- as.POSIXct(track.data[, time], format = time.format)
  names(track.data) <- c("id", "lon", "lat", "datetime")
  return(track.data)
}
