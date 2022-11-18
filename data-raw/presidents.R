## code to prepare `presidents` dataset goes here
library(tidyverse)
library(lubridate)
presidents <- googlesheets4::read_sheet("1-Cl4IvWSe9rhWy4h9zHOkaykD1h3MWsEv3XKGjqIkDg")
presidents <- presidents %>% mutate(across(contains("date"), as_date))
usethis::use_data(presidents, overwrite = TRUE)
