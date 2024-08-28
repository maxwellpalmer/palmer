

#' date_name
#'
#' @param date A date object
#' @param formal Boolean. Default is FALSE.
#' @param year Boolean. Default is FALSE.
#' @param month.full Boolean. Default is FALSE.
#' @param weekday Boolean. Default is FALSE.
#' @param weekday.parens Boolean. Default is FALSE.
#'
#' @return A string of the date name
#' @export
#'
#' @importFrom scales ordinal
#' @importFrom stringr str_replace
#' @import glue
#'
date_name <- function(date, formal=F, year=F, month.full=F, weekday=F, weekday.parens=F) {

  pattern <- "^(.*?\\s)0(\\d.*)$"

  if(formal==TRUE) {
    return(paste0(scales::ordinal(as.numeric(format(date, "%d"))),
           ' day of ',
           format(date, "%B"),
           ", ",
           format(date, "%Y"))
    )
  }

  d <- format(date, glue::glue('%{ifelse(month.full==T, "B", "b.")} %d{ifelse(year==T, ", %Y", "")}'))
  d <- stringr::str_replace(d, pattern, "\\1\\2")
  if(weekday.parens==T) {
    d <- paste0(d, " (", format(date, "%a"), ".)")
  } else if(weekday==T) {
    d <- paste0(format(date, "%A"), ", ", d)
  }

  return(d)
}
