#' Custom ggplot2 theme
#'
#' @return ggplot2 theme
#'
#' @export
#'
#' @importFrom hrbrthemes theme_ipsum_rc
#' @import ggplot2
#'
theme_mp <- function() {
  hrbrthemes::theme_ipsum_rc(base_family = "Arial Narrow",
                             subtitle_family = "Arial Narrow",
                             caption_family = "Arial Narrow",
                             plot_title_margin = 10,
                             plot_title_size=14,
                             axis_title_size = 12,
                             plot_margin = margin(10, 10, 10, 10)) +
    ggplot2::theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA)) +
    ggplot2::theme(axis.text.y = element_text(size=8),
          axis.title.y = element_text(size=8),
          axis.text.x = element_text(size=8),
          axis.title.x = element_text(size=8),
          plot.caption = element_text(size=8),
          plot.title.position = "plot")
}
