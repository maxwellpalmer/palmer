#' Custom ggplot2 theme
#'
#' @param base_family Base font family. Default is "Arial Narrow".
#' @param base_size Default is `11`.
#' @param grid Options to control grid. Options are "minor" (all lines), "major" (major lines only), "major.x", "minor.x", "major.y", "minor.y".
#'
#' @return ggplot2 theme
#'
#' @export
#'
#' @import ggplot2
#'
theme_mp <- function(base_family="Arial Narrow", base_size=12, grid="minor") {
  g <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)
  g <- g + ggplot2::theme(panel.background = element_rect(fill = "transparent", colour = NA),
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          legend.background = element_rect(fill = "transparent", colour = NA),
                          legend.box.background = element_rect(fill = "transparent", colour = NA),
                          legend.position = "bottom")
  g <- g + ggplot2::theme(axis.text.y = element_text(size=10),
                          axis.title.y = element_text(size=10),
                          axis.text.x = element_text(size=10),
                          axis.title.x = element_text(size=10),
                          plot.caption = element_text(size=10),
                          plot.title.position = "plot")
  g <- g + ggplot2::theme(panel.spacing.x=unit(.5, "lines"),
                          panel.spacing.y=unit(.5, "lines"),
                          strip.text=element_text(size=10),
                          panel.border = element_rect(color="gray40", fill=NA, linewidth=.25))
  if(grid =="none") {
    g <- g + ggplot2::theme(panel.grid.major.x = element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank())
  } else if(grid =="major") {
    g <- g + ggplot2::theme(panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank())
  } else if(grid =="major.x") {
    g <- g + ggplot2::theme(panel.grid.major.y = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank())
  } else if(grid =="major.y") {
    g <- g + ggplot2::theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank())
  } else if(grid =="minor.x" | grid=="x") {
    g <- g + ggplot2::theme(panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank())
  } else if(grid=="minor.y" | grid=="y") {
    g <- g + ggplot2::theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank())
  }

  g
}


#' ggplot2 theme for presentations
#'
#' @param base_size Base font size. Default is 32.
#' @param base_family Base font family. Default is Montserrat (must be installed)
#'
#' @return ggplot2 theme
#' @export
#'
#' @import ggplot2
#'
theme_pres <- function(base_size=32, base_family="Montserrat") {
  g <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)
  g <- g + ggplot2::theme(panel.background = element_rect(fill = "transparent", colour = NA),
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          legend.background = element_rect(fill = "transparent", colour = NA),
                          legend.box.background = element_rect(fill = "transparent", colour = NA),
                          legend.position = "bottom")
  g <- g + ggplot2::theme(plot.title.position = "plot")
  g <- g + ggplot2::theme(panel.spacing.x=unit(.5, "lines"),
                          panel.spacing.y=unit(.5, "lines"),
                          panel.border = element_rect(color="#58585B", fill=NA, size=1),
                          panel.grid = element_line(color="#58585B", linewidth=.2),
                          axis.text = element_text(color="#58585B"),
                          axis.title = element_text(color="#58585B"))
  g
}

#' Reasonable Colors palette generator
#'
#' @param color Starting color for palette. One of "rose", "raspberry", "red", "orange", "cinnamon", "amber", "yellow", "lime", "chartreuse", "green", "emerald", "aquamarine", "teal", "cyan", "powder", "sky", "cerulean", "azure", "blue", "indigo", "violet", "purple", "magenta", "pink".
#' @param shade An integer from 1 (lightest) to 6 (darkest). Default is 3.
#' @param pairing One of "complementary", "split_complementary", "triadic", "tetradic", "analogous", or "set6".
#'
#' @return A named vector of html color codes.
#' @export
#'
#' @examples
#' rcols.pallette("green", shade=2, pairing="triadic")
#'
rcols.pallette <- function(color, shade=3, pairing="complementary") {
  if(is.null(color)) stop("`color' must be specified.")
  if(! color %in% rcols.pairings$lab) stop("Invalid `color`.")
  if(!all(is.numeric(shade)) | ! all(shade %in% seq(1:6)) | length(shade)>1) stop("`shade` must be an integer from 1 to 6.")
  if(! pairing %in% names(rcols.pairings)[2:7]) stop(paste0("`pairing` must be one of: ", glue::glue_collapse(names(rcols.pairings)[2:7], sep=", ", last=", or ")))

  palmer::rcols[paste0(rcols.pairings[rcols.pairings$lab==color, pairing][[1]] |> unlist(), shade)]
}

rcols.pallette.print <- function(color=NULL, shade=NULL, pairing="complementary") {
  if(is.null(shade)) shade <- 1:6
  if(!all(is.numeric(shade))) stop("`shade` must be one or more integers from 1 to 6.")
  shade <- sort(unique(shade))
  if(is.null(color)) color <- rcols.pairings$lab
  if(!all(color %in% rcols.pairings$lab)) stop("One or more invalid values in `color`.")
  if(! pairing %in% names(rcols.pairings)[2:7]) stop(paste0("`pairing` must be one of: ", glue::glue_collapse(names(rcols.pairings)[2:7], sep=", ", last=", or ")))

  x <- tidyr::expand_grid(shade=shade, color=color) |>
    mutate(pal=lapply(1:n(), function(i) rcols.pallette(color[i], shade[i], pairing))) |>
    tidyr::unnest(cols=pal) |>
    dplyr::mutate(x=dplyr::row_number(), .by=c(shade, color))

  p <- ggplot2::ggplot(x, ggplot2::aes(x=x, y=color)) +
        ggplot2::geom_tile(fill=x$pal, color="black") +
        ggplot2::facet_wrap(~shade, nrow = 1) +
        ggplot2::coord_fixed(ratio=.5) +
        ggplot2::labs(x=NULL, y=NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank())

  if(!interactive()) return(p)
  plot(p)
}

