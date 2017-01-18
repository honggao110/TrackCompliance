

loginSurvos <- function(username, password, sslVerify = TRUE){

  # Set URL endpoint for login, accounting for potential missing trailing slash
  slash <- substr(endPoint, nchar(endPoint), nchar(endPoint))

  if(slash != "/") {
    url <- paste(endPoint, "/security/login", sep="")
  } else {
    url <- paste(endPoint,"security/login", sep="")
  }

  # Add username and password to message body
  msgBody <- NULL
  msgBody$username = username
  msgBody$password = password
  msgBody <- as.data.frame(msgBody)

  # Setup credentials
  credentials <- jsonlite::toJSON(unbox(msgBody), pretty = TRUE)

  # Parse to the API
  loginReturn <- httr::POST(url, body = credentials, encode="json",
                            add_headers('Accept' = 'application/json', 'Content-Type' = 'application/json'))

  # Out from JSON
  loginReturn <- jsonlite::fromJSON(content(loginReturn,type="text", flatten = TRUE))

  # Assigning a global variable with <<-
  accessToken <<- loginReturn$accessToken

  if(!is.null(loginReturn$code)){
    stop("HTTP failure: ", loginReturn$code, " ", loginReturn$message, "\nOops, something went wrong.",
         "\n401 Invalid redentials: Please check the entered username and password.",
         "\n404 No route found: Please check the entered endpoint URL.")
  }
}
