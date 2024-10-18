globalVariables(c("geometry", "y", "a", "ycent"))

#' Make a simple ggplot map
#'
#' @param .data A sf object
#' @param size Optional point size
#' @param ... Additional arguments passed to `geom_sf`
#'
#' @return A ggplot2 object
#' @export
#'
#' @import ggplot2
#'
qmap <- function(.data, size=.25, ...) {
  ggplot2::ggplot(.data) +
    ggplot2::geom_sf(color="black", size=size, ...) +
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

#' Find centroid of the inscribed circle of a polygon
#' @param shp A polygon shape file
#' @return List of points
#'
centroid_of_inscribed_circle <- function(shp) {
  s <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  y <- suppressMessages(suppressWarnings(sf::st_centroid(sf::st_inscribed_circle(sf::st_geometry(shp)))))
  suppressMessages(sf::sf_use_s2(s))
  y[!sf::st_is_empty(y)]
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
                         fun.geometry = centroid_of_inscribed_circle
  )
}

#' @rdname geom_sf_label2
#' @export
geom_sf_text2 <- function(...) {
  ggplot2::geom_sf_text(...,
                        fun.geometry = centroid_of_inscribed_circle()
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
#' @importFrom dplyr tibble row_number arrange filter
#' @importFrom methods is
#' @importFrom rlang .data
#'
adjacent_polys <- function(shp, add_adj_for_islands=T, nearest_for_islands=1) {
  if(is.data.frame(shp)) {
    if(nrow(shp)==1) return(data.frame(poly1=1, poly2=1))
  } else if(methods::is(shp, "sfc")) {
    if(length(shp)==1) return(data.frame(poly1=1, poly2=1))
  }
  adj <- st_rook(shp)
  # Check for islands. Add adjacencies for nearest neighbors
  if(add_adj_for_islands==T) {
    if(sum(map_int(adj, function(i) length(i)==0))>0) {
      for(i in 1:nrow(shp)) {
        if(length(adj[[i]])==0) {
          x <- sf::st_distance(shp[i, ], shp)
          x <- dplyr::tibble(dist=as.numeric(x)) %>% mutate(st=dplyr::row_number()) %>%
            dplyr::arrange(.data$dist) %>%
            dplyr::filter(dplyr::row_number()>1 & dplyr::row_number()<=nearest_for_islands+1)
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

#' Find the weighted centroid of a set of points
#'
#' @param x A sf or sfc object. Must be "POINTS".
#' @param weight A vector of weights
#'
#' @return A sfc object with the weighted centroid of the points
#' @export
#' @import sf
#'
st_weighted_centroid <- function(x, weight) {
  if(!inherits(x, "sf") & !inherits(x, "sfc")) stop(deparse(substitute(x)), " must be a 'sf' or 'sfc' object")
  if(all(st_geometry_type(x)!="POINT")) stop(deparse(substitute(x)), " must be type \"POINT\"")
  if(!all(is.numeric(weight))) stop(deparse(substitute(weight)), " must be type numeric")

  st_point(c(sum(st_coordinates(x)[,1]*weight)/sum(weight), sum(st_coordinates(x)[,2]*weight)/sum(weight))) %>%
    st_sfc(crs=st_crs(x))
}


#' Color polygons of a map
#'
#' This function assigns colors to polygons in a map to (1) color each polygon differently from its neighbors, and (2) use each color roughly the same number of times.
#'
#' @param shp A polygon shape file. Must be of class `sf` or `sfc`.
#' @param n_cols The number of colors to use. Defaul is 5.
#' @param retry If the algorithm fails, retry with `n_cols+1` colors. Default is TRUE. Function fails if polygons unable to be colors with 10 colors.
#' @param ... Additional arguments, only used internally.
#'
#' @return A vector of numbers identifying the colors for each shape.
#'
#' @import dplyr glue
#' @importFrom sf st_as_sf
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#'
#' @export
#'
color_shp <- function(shp, n_cols = 5, retry = TRUE, ...) {
  v <- list(...)
  if(is(shp, "sfc")) shp <- sf::st_as_sf(shp)
  if(!is(shp, "sf")) stop("shp must be class sf or sfc")

  if("adj" %in% names(v)) {
    adj <- v$adj
  } else {
    message("Calculating adjacencies")
    adj <- palmer::adjacent_polys(shp, nearest_for_islands = 4)
  }
  adj.length <- adj %>% count(poly1) %>% pull(n)

  n=nrow(shp)
  cols.list <- data.frame(col=seq(1,n_cols), n=rep(0, n_cols))

  cli::cli_progress_bar("Assigning colors", total=n, clear=F)
  dat <- tibble(id=1:nrow(shp), col=NA, adj.length, choices=rep(list(cols.list$col), n), n_choices=n_cols)
  for(x in 1:n) {
    i <- dat %>% filter(is.na(col)) %>%
      filter(n_choices==min(n_choices)) %>%
      filter(adj.length==max(adj.length)) %>% slice(1) %>% pull(id)
    next_col <- filter(cols.list, col %in% dat$choices[[i]]) %>% filter(n==min(n)) %>% slice(1) %>% pull(col)
    dat$col[i] <- next_col
    cols.list$n[next_col] <- cols.list$n[next_col]+1

    dat <- dat %>% mutate(choices=map(1:n, \(j) {
      if(is.na(col[j]) & i %in% adj$poly2[adj$poly1==j]) return(choices[[j]][choices[[j]]!=next_col])
      return(choices[[j]])
    }),
    n_choices=map_dbl(choices, length))
    if(min(dat$n_choices)==0) {
      cli::cli_progress_done(result="failed")
      break
    }
    cli::cli_progress_update()
  }

  if(any(is.na(dat$col)) & retry==TRUE) {
    if(n_cols>=10) stop("Unable to complete algorithm with 10 colors.")
    message(glue::glue("Unable to assign colors with only {n_cols} colors. Trying again with {n_cols+1} colors."))
    return(color_shp(shp, n_cols=n_cols+1, adj=adj))
  }
  return(dat$col)
}

#' Find widest rectangle in state.
#'
#' Used to label placement. Finds a rectangle at least 90% as wide as possible, with preference for rectangles closer to the centroid.
#'
#' @param shp Polygon shape file
#' @param n Two-item list of the number of rectangles to draw in each direction. Higher values are more precise but slower. Default is `c(20,20)`.
#'
#' @return Centroid of wide rectangle.
widest_rectangle <- function(shp, n=c(20, 20)) {

  if("sf" %in% class(shp)) {
    shp <- pull(shp, attr(shp, "sf_column"))
  }
  if(length(shp)>1) {
    x <- lapply(1:length(shp), function(i) widest_rectangle(shp[i], n))
    return(do.call(rbind, x) %>% tibble() %>% st_as_sf(crs=st_crs(shp)))
  }

  g <- st_make_grid(shp, square=T, n=n)
  g <- tibble(i=1:length(g), geometry=g) %>% st_as_sf()
  g <- g %>% mutate(x=map_dbl(geometry, ~ st_bbox(.)$xmin),
                    y=map_dbl(geometry, ~ st_bbox(.)$ymin)) %>%
    arrange(x, y) %>%
    group_by(x) %>%
    mutate(x2=cur_group_id()) %>%
    group_by(y) %>%
    mutate(y2=cur_group_id()) %>%
    ungroup()

  g2 <- suppressMessages(g[st_contains(shp, g) %>% unlist(), ])

  g3 <- g2 %>% group_by(y) %>%
    summarise(geometry=suppressMessages(st_union(geometry))) %>%
    select(y, geometry) %>%
    st_cast("MULTIPOLYGON") %>% suppressWarnings(st_cast("POLYGON")) %>%
    mutate(a=as.numeric(st_area(geometry))) %>%
    filter(a>=.9*max(a)) %>%
    mutate(ycent=st_coordinates(suppressWarnings(st_centroid(shp)))[2]) %>%
    filter(abs(y-ycent)==min(abs(y-ycent))) %>%
    filter(y==min(y), .by=y) %>%
    filter(row_number()==1)

  suppressWarnings(st_centroid(g3$geometry[1]))
}
