#' Convert a vector of numbers to percentages.
#'
#' Convert numbers to percentags. If all numbers are less than 1, numbers are
#' multiplied by 100. If any number is greater than 1, numbers are not multiplied.
#'
#' @param v A vector of numbers.
#' @param digits The number of digits after the decimal. Default is zero.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' pct(.5)
#' pct(runif(10), digits=2)
#' pct(100*runif(10))
#'
pct <- function(v, digits=0) {
  if(max(v, na.rm=T)<=1) v <- 100*v
  sprintf(paste0("%1.", digits, "f%%"), v)
}

#' Format a large number with commas and a set number of digits.
#'
#' @param x A number or vector of numbers.
#' @param digits The number of digits after the decimal place.
#' @param nsmall The number of digits after the decimal place.
#' @param prefix A character string to prepend to the formatted number, such as "$".
#'
#' @return A string or vector of strings.
#' @export
#'
comma <- function(x, digits=0, nsmall=digits, prefix=NULL) {
  x <- format(round(as.numeric(x), digits), nsmall=nsmall, big.mark=",")
  if(!is.null(prefix)) x <- paste0(prefix, x)
  x
}


#' Summarize a data frame and calculate percent for each group.
#'
#' @param .data A data frame or tibble.
#' @param ... Grouping variables.
#' @param .keep_n Boolean to keep the counts in addition to the percentage. Default is FALSE.
#'
#' @return A data frame.
#' @export
#'
#' @importFrom dplyr count mutate select
#'
percent <- function(.data, ..., .keep_n = F) {

  df <- dplyr::count(.data, ...)
  df <- dplyr::mutate(df, pct = n/sum(n))

  if (.keep_n == F) {
    df <- dplyr::select(df, -n)
  }
  return(df)
}

#' Print as part of pipe. Print and return original value.
#'
#' @param x Any printable object.
#'
#' @return x
#' @export
#'
pr <- function(x) {
  print(x)
  return(x)
}


#' Drop columns of a data frame where all values are NA
#'
#' @param x A data frame
#' @param verbose Optional. Display list of dropped columns.
#'
#' @return A data frame, without any columns where all values are missing.
#' @export
#'
#' @examples
#' x <- data.frame(a=1:5, b=1:5, c=NA, d=NA, e=letters[1:5])
#' drop_na_cols(x, verbose=TRUE)
drop_na_cols <- function(x, verbose=FALSE) {
  to_drop <- colSums(is.na(x))==nrow(x)
  if(verbose==TRUE & sum(to_drop)>0) {
    message(paste("Dropping columns:", paste(names(x)[to_drop==TRUE], collapse=", ")))
  }
  if(verbose==TRUE & sum(to_drop)==0) {
    message("No columns to drop.")
  }
  x[ , to_drop==FALSE]
}
