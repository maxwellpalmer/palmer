% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/letter_grades.R
\name{letter_grades_to_pct}
\alias{letter_grades_to_pct}
\title{Calculate percentages from letter grades}
\usage{
letter_grades_to_pct(
  grades,
  vals = c(1, 0.92, 0.89, 0.86, 0.82, 0.79, 0.76, 0.72, 0.69, 0.59),
  labs = c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F")
)
}
\arguments{
\item{grades}{A vector of letter grades}

\item{vals}{Optional. A vector of the percentage value for each letter grade.}

\item{labs}{Optional. A vector of the names of each grade, sorted from best to worse. Must be the same length as \code{vals}.}
}
\value{
A vector of percentages.
}
\description{
Calculate percentages from letter grades
}
