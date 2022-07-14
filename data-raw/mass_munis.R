## code to prepare `mass_munis` dataset goes here
library(tidyverse)
library(sf)
sf_use_s2(FALSE)

munis <- tigris::county_subdivisions(state="MA", cb=T) %>%
  select(muni=NAME, muni_full=NAMELSAD, geoid=GEOID, county_fips=COUNTYFP)

counties <- tigris::fips_codes %>% filter(state=="MA") %>% select(county_fips=county_code, county)
munis <- munis %>% left_join(counties) %>%
  mutate(county=str_replace(county, " County", ""))

mass_munis <- munis

usethis::use_data(mass_munis, overwrite = TRUE)
