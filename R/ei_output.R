#' Title
#'
#' @param ei_md_model A model object estimated from eiPack::ei.MD.bayes()
#' @param cols A vector of column names to include in the output, as characters. If NULL, columns are automatically detected from the model formula.
#' @param formula The formula usef for running EI. Must be provided if cols is null.
#' @param full If TRUE, provide differences between columns across rows and between rows across columns. Default is FALSE.
#' @param drop_last_col If TRUE, drop the last column variable. Only applies if cols is not supplied. Default is FALSE.
#'
#' @return If full==FALSE, a data frame with means and confidence intervals for each row and specified column. If full==TRUE, a named
#' list with (1) a data frame with means and confidence intervals for each row and specified column, (2) a data frame with means and CIs for
#' differences between each pair of rows for every column, (3) a data frame with means and CIs for the differences between each pair of columns
#' for every row, (4) the number of ei.samples, and (5) the number of observations in the data.
#'
#' @export
#'
#' @import dplyr eiPack glue tidyr
#' @importFrom stats quantile formula
#' @importFrom Formula Formula
#'
ei_output <- function(ei_md_model, cols=NULL, formula=NULL, full=FALSE, drop_last_col=FALSE) {
  if(is.null(cols) & is.null(formula)) stop("cols or formula must be provided.")
  if(is.null(cols)) {
    cols <- formula(Formula::Formula(eval(formula)), lhs = TRUE, rhs = FALSE) %>% all.vars()
    if(drop_last_col==TRUE) cols <- cols[-length(cols)]
    message(glue("cols not specified. Using column variables: {glue_collapse(cols, sep=', ')}"))
  }

  d <- eiPack::lambda.MD(ei_md_model, columns=cols) %>%
    as_tibble() %>%
    mutate(id=row_number()) %>%
    pivot_longer(-id) %>%
    separate(name, into=c("v1", "row", "col"), sep="\\.") %>%
    select(-v1) %>%
    arrange(row, col, id)

  ei.res <- d %>% group_by(row, col) %>%
    summarize(est=mean(value),
              ci.lower = stats::quantile(value, probs = .025),
              ci.upper = stats::quantile(value, probs = .975)) %>%
    ungroup()

  if(full==FALSE) return(ei.res)

  x <- d %>% pivot_wider(names_from=col, values_from=value)
  res <- list()
  for(i in unique(d$col)) {
    for(j in unique(d$col)[unique(d$col)!=i]) {
      for(k in unique(d$row)) {
        y <- filter(x, row==k) %>% mutate(x=.[[i]]-.[[j]])
        res[[glue("{i}/{j}/{k}")]] <- tibble(c1=i, c2=j, row=k,
                                             diff=mean(y$x),
                                             ci.lower=stats::quantile(y$x, .025),
                                             ci.upper=stats::quantile(y$x, .975),
                                             pct_greater=mean(y$x>0))
      }
    }
  }
  col.diffs <- bind_rows(res)

  x <- d %>% pivot_wider(names_from=row, values_from=value)

  res <- list()
  for(i in unique(d$row)) {
    for(j in unique(d$row)[unique(d$row)!=i]) {
      for(k in unique(d$col)) {
        y <- filter(x, col==k) %>% mutate(x=.[[i]]-.[[j]])
        res[[glue("{i}/{j}/{k}")]] <- tibble(c1=i, c2=j, row=k,
                                             diff=mean(y$x),
                                             ci.lower=stats::quantile(y$x, .025),
                                             ci.upper=stats::quantile(y$x, .975),
                                             pct_greater=mean(y$x>0))
      }
    }
  }
  row.diffs <- bind_rows(res)

  return(list(ei.res=ei.res, col.diffs=col.diffs, row.diffs=row.diffs,
              ei.samples=ei_md_model$draws$Beta %>% nrow(),
              n_obs = ncol(ei_md_model$draws$Beta)/ncol(ei_md_model$draws$Alpha)))

}
