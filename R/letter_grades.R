#' Calculate letter grades from percentages
#'
#' @param scores A vector of percentages
#' @param vals Optional. A vector of the lower bound of each grade level, sorted from best to worst.
#' @param labs Optional. A vector of the names of each grade, sorted from best to worse. Must be the same length as `vals`.
#'
#' @return A vector of grades, with factor levels as specified by `labs`.
#' @export
#'
#' @examples
#' letter_grades(c(.6, .75, .85, .95, 1))
#' letter_grades(scores=c(.5, .75, .9, .3),
#'               vals=c(.8, .6, .4, 0),
#'               labs=c("Excellent", "Good", "Poor", "Fail"))
#'
letter_grades <- function(scores, vals=c(0.93, 0.90, 0.87, 0.83, 0.80, 0.77, 0.73, 0.70, 0.60, 0),
                          labs=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F")) {
  if(length(vals)!=length(labs)) stop("vals and labs must have the same length.")

  factor(unlist(lapply(scores, function(i) labs[which.max(i>=vals)])), levels=labs)
}
