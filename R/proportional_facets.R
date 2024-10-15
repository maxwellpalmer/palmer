
#' Proportional facet heights
#'
#' The default behavior for ggplot facets is that all facets have the same dimension. When plotting
#' facets with discrete axes and `scales=free_y`, the axis spacing will be inconsistent. This function
#' adjusts the relative heights of the facets so that the facet heights are proportional to the number
#' of items on the discrete scale for each facet.
#'
#' @param p A ggplot with wrapped facets in a single column using `facet_wrap(~ var, ncol=1, scales="free_y")`
#'
#'#' @import ggplot2, grid, dplyr
#' @return A grob
#' @export
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

