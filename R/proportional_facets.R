
#' Proportional facets
#'
#' The default behavior for ggplot facets is that all facets have the same dimensions. When plotting
#' facets with discrete axes and `scales=free_y` or `scales=free_x`, the axis spacing will be inconsistent.
#' This function adjusts the relative heights of the facets so that the facet heights are proportional to the number
#' of items on the discrete scale for each facet.
#'
#' ## Notes:
#' These functions only work for plots where the facets are a single row or column. The functions return a `grob` object,
#' and can be displayed interactively with `plot`, and saved with `ggsave` and other functions to save images, or used in a
#' Quarto or Rmarkdown document. The output of this function does not update `ggplot2::last_plot()`.
#'
#' @param p A ggplot with wrapped facets in a single column using `facet_wrap(~ var, ncol=1, scales="free_y")` or `facet_wrap(~ var, nrow=1, scales="free_x")`
#'
#' @import ggplot2 grid dplyr
#' @return A grob
#' @export
#'
#' @examples
#' library(ggplot2)
#' dat <- data.frame(x=c(1,2,3,1,2,3,1), y=LETTERS[1:7], z=c(rep("Set 1", 2), rep("Set 2", 5)))
#' p <- ggplot(dat, aes(x=x, y=y)) +
#' geom_point() +
#' facet_wrap(~z, ncol=1, scales="free_y")
#' p
#' plot(proportional_facet_heights(p))
#'
#' p <- ggplot(dat, aes(x=y, y=x)) +
#' geom_point() +
#' facet_wrap(~z, nrow=1, scales="free_x")
#' p
#' plot(proportional_facet_widths(p))
#'
proportional_facet_heights <- function(p) {
  if(! "ggplot" %in% class(p)) stop("p must be a ggplot object.")

  gt = ggplot2::ggplotGrob(p)

  N <- p$data |>
    dplyr::count(!!rlang::quo_get_expr(p$facet$params$facets[[1]])) |>
    dplyr::pull(n)
  panelI <- gt$layout$t[grepl("panel", gt$layout$name)]
  gt$heights[panelI] <- grid::unit(N, "null")

  return(gt)
}

#' @rdname proportional_facet_heights
#' @export
proportional_facet_widths <- function(p) {
  if(! "ggplot" %in% class(p)) stop("p must be a ggplot object.")

  gt = ggplot2::ggplotGrob(p)

  N <- p$data |>
    dplyr::count(!!rlang::quo_get_expr(p$facet$params$facets[[1]])) |>
    dplyr::pull(n)
  panelI <- gt$layout$l[grepl("panel", gt$layout$name)]
  gt$widths[panelI] <- grid::unit(N, "null")

  return(gt)
}

