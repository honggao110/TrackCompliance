#' Keep Date Format in ifelse
#'
#' This function keeps the correct date format in a ifelse condition function
#'
#' @param cond a true or false condition statement which can be coerced to logical mode.
#' @param yes return values for true elements of \code{cond}.
#' @param no return values for false elements of \code{cond}.
#'
#' @return A vector of the same length and attributes (including dimensions and "class") as cond
#' and data values from the values of \code{yes} or \code{no}.
#'
#' @examples
#'
#' @import
#' @export

SafeIfelse <- function(cond, yes, no)
  structure(ifelse(cond, yes, no), class = class(yes))
