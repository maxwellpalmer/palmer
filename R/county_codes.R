

#' Standardize states from state names, abbreviations, or fips codes.
#'
#' @param s A state name, two-letter abbreviate, or FIPS code, or a vector
#' @param type Output type. "fips", "abb", or "name". Default is "fips".
#'
#' @export
#'
#' @examples
#' state_std(c("MA", "NY", "California", "025", "33", 34), type="fips")
#' state_std(c("MA", "NY", "California", "025", "33", 34), type="abb")
#' state_std(c("MA", "NY", "California", "025", "33", 34), type="name")
#'
state_std <- function(s=NULL, type="fips") {
  if(is.null(s)) stop("s cannot be null.")
  if(! type %in% c("fips", "abb", "name")) stop(paste0("Invalid type: ", type))
  s <- state_fips(s)

  if(type=="fips") return(s)
  if(type=="abb") return(sapply(s, function(i) {
    ifelse(is.na(i), NA, state_lookup$state[state_lookup$state_code==i])
    }, USE.NAMES = F))
  if(type=="name") return(sapply(s, function(i) {
    ifelse(is.na(i), NA, state_lookup$state_name[state_lookup$state_code==i])
    }, USE.NAMES = F))
}

# Not exported
state_fips <- function(s=NULL)  {
  if(is.null(s)) stop("s cannot be null.")

  if(is.vector(s) & length(s)>1) {
    return(sapply(s, state_fips, USE.NAMES = F))
  }

  if(is.na(s)) return(NA)

  if(is.numeric(s)) {
    s <- sprintf("%02d", s)
  }
  if(nchar(s)==1) s <- paste0("0", s)
  if(nchar(s)==3 & substr(s, 1, 1) =="0") {
    s <- substr(s, 2, nchar(s))
  }
  s <- toupper(s)

  if(s %in% state_lookup$state_code) {
    return(s)
  }
  if(s %in% toupper(state_lookup$state)) {
    return(state_lookup$state_code[toupper(state_lookup$state) == s])
  }
  if(s %in% toupper(state_lookup$state_name)) {
    return(state_lookup$state_code[toupper(state_lookup$state_name) == s])
  }
  return(NA)
}


#
#
# fips %>% mutate(county_name=str_replace(county, " County", ""),
#                 county_name=str_replace(county_name, " Parish", ""),
#                 county_name=str_replace(county_name, " Census Area", ""),
#                 county_name=str_replace(county_name, " Borough", ""))


