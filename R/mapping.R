

#' Make a simple ggplot map
#'
#' @param .data A sf object
#' @param ... Additional arguments passed to `geom_sf`
#'
#' @return A ggplot2 object
#' @export
#'
#' @import ggplot2
#'
qmap <- function(.data, ...) {
  ggplot(.data) +
    geom_sf(color="black", size=.25, ...) +
    theme_void()
}

#' Get envelope of a shape file
#'
#' @param shp A sf object
#'
#' @return A sf object.
#' @export
#'
#' @importFrom sf st_as_sfc st_bbox
#'
#' @examples
#' st_envelope(ma[1,])
#'
st_envelope = function(shp){
  sf::st_as_sfc(sf::st_bbox(shp))
}

#' Set ggplot coordinates using bounding box of sf object
#'
#' @param .shp A sf object.
#' @param expand A boolean if the coordinates should be expanded to include padding around the shape.
#' @param ... Additional arguments passed to `coord_sf`.
#'
#' @export
#'
coord_sf_from_sf <- function(.shp, expand=TRUE, ...) {
  box <- sf::st_bbox(.shp)
  ggplot2::coord_sf(xlim = c(box$xmin,
                        box$xmax),
               ylim = c(box$ymin,
                        box$ymax),
               expand = expand, ...)
}

#' @rdname coord_sf_from_sf
#' @export
coord_sf_from_sf_square <- function(.shp, expand=TRUE, ...) {
  e <- st_envelope(.shp)
  b <- st_bbox(e)

  d <- max(b$xmax-b$xmin, b$ymax-b$ymin)/2
  x <- st_centroid(e)[[1]]

  coord_sf(xlim = c(x[1]-d, x[1]+d),
           ylim = c(x[2]-d, x[2]+d),
           expand = expand, ...)
}

#' Add geom_sf_label with labels located in inscribed circles instead of points on surface.
#'
#' @param ... Additional arguments passed to `geom_sf_label`.
#'
#' @importFrom sf st_centroid st_inscribed_circle st_geometry st_is_empty
#' @importFrom ggplot2 geom_sf_label geom_sf_text
#'
#' @export
#'
geom_sf_label2 <- function(...) {
  ggplot2::geom_sf_label(...,
                         fun.geometry = function(x) {
                           s <- sf_use_s2()
                           sf_use_s2(FALSE)
                           y <- suppressMessages(suppressWarnings(sf::st_centroid(sf::st_inscribed_circle(sf::st_geometry(x)))))
                           sf_use_s2(s)
                           y[!sf::st_is_empty(y)]
                           }
  )
}

#' @rdname geom_sf_label2
#' @export
geom_sf_text2 <- function(...) {
  ggplot2::geom_sf_text(...,
                        fun.geometry = function(x) {
                          s <- sf_use_s2()
                          sf_use_s2(FALSE)
                          y <- suppressMessages(suppressWarnings(sf::st_centroid(sf::st_inscribed_circle(sf::st_geometry(x)))))
                          sf_use_s2(s)
                          y[!sf::st_is_empty(y)]}
  )
}


#' Calculate rook adjacency
#'
#' @param a A sf object.
#' @param b A sf object. Optional. Default is object `a`.
#'
#' @return Sparse geometry binary predicate list
#' @export
#'
#' @examples
#' st_rook(ma)
#'
st_rook = function(a, b = a){
  suppressMessages(sf::st_relate(a, b, pattern = "F***1****"))
}

#' Calculate adjacency list for a shape file.
#'
#' @param shp A sf object
#' @param add_adj_for_islands Boolean. Default is `TRUE`. If there are unconnected islands, add adjancecies to nearest polygon(s).
#' @param nearest_for_islands Integer. Default is `1`. How many adjacencies to add for islands.
#'
#' @return A dataframe of the indices of adjacent polygons.
#' @export
#'
#' @importFrom sf st_distance
#' @importFrom purrr map_dfr map_int
#' @importFrom dplyr tibble row_number arrange
#'
#' @examples
#' st_envelope(ma[1,])
#'
adjacent_polys <- function(shp, add_adj_for_islands=T, nearest_for_islands=1) {
  if(is.data.frame(shp)) {
    if(nrow(shp)==1) return(data.frame(poly1=1, poly2=1))
  } else if(is(shp, "sfc")) {
    if(length(shp)==1) return(data.frame(poly1=1, poly2=1))
  }
  adj <- st_rook(shp)
  # Check for islands. Add adjacencies for nearest neighbors
  if(add_adj_for_islands==T) {
    if(sum(map_int(adj, ~ length(.)==0))>0) {
      for(i in 1:nrow(shp)) {
        if(length(adj[[i]])==0) {
          x <- sf::st_distance(shp[i, ], shp) %>%
            dplyr::tibble(dist=as.numeric(.)) %>% mutate(st=dplyr::row_number()) %>%
            dplyr::arrange(dist) %>%
            filter(dplyr::row_number()>1 & dplyr::row_number()<=nearest_for_islands+1)
          adj[[i]] <- x$st
        }
      }
    }
  }

  adj <- purrr::map_dfr(1:length(adj), ~ data.frame(poly1=., poly2=adj[[.]]))
  return(adj)
}

#' Renumber polygons so neighboring polygons are more likely to have sequential numbers.
#'
#' @param shp A polygon shape file to renumber.
#' @param adj Optional. An adjacency list, as produced by `palmer::adjacent_polys`.
#'
#' @return A vector of new district numbers
#' @export
#' @importFrom dplyr add_count arrange mutate select distinct pull
#'
renumber_polygons <- function(shp, adj=NULL) {
  if(is.null(adj)) adj <- palmer::adjacent_polys(shp)
  adj.renum <- adj %>% dplyr::add_count(poly1) %>%
    dplyr::add_count(poly2, name="n2") %>%
    dplyr::arrange(n, n2, poly1) %>%
    dplyr::mutate(block.new=NA, block2.new=NA)
  block.next=0
  for(i in 1:nrow(adj.renum)) {
    if(is.na(adj.renum$block.new[i])) {
      block.next <- block.next+1
      a = block.next
      adj.renum$block.new[adj.renum$poly1==adj.renum$poly1[i]] = a
      adj.renum$block2.new[adj.renum$poly2==adj.renum$poly1[i]] = a
    }
    if(is.na(adj.renum$block2.new[i])) {
      block.next <- block.next+1
      a = block.next
      adj.renum$block.new[adj.renum$poly1==adj.renum$poly2[i]] = a
      adj.renum$block2.new[adj.renum$poly2==adj.renum$poly2[i]] = a
    }
    adj.renum <- dplyr::arrange(adj.renum, block.new, poly1, n, n2)
  }
  res <- adj.renum %>% dplyr::select(d0=poly1, d.new=block.new) %>%
    dplyr::distinct() %>%
    dplyr::arrange(d0) %>%
    dplyr::pull(d.new)
  res
}
