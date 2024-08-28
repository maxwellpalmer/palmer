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
                          panel.border = element_rect(color="gray40", fill=NA, size=.25))
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


