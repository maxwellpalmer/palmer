library(tidyverse)

raw <- read_file("https://raw.githubusercontent.com/matthewhowell/reasonable-colors/master/reasonable-colors.scss") |>
  str_split("\n") |> unlist()

x <- raw[str_detect(raw, "^\\$")]
x <- tibble(raw=x) |>
  mutate(lab=str_extract(raw, "(?<=\\$color-)[a-z]+(?=-)"),
         hue=str_extract(raw, "\\d"),
         html=str_extract(raw, "#.{6}")) |>
  select(lab, html) |>
  mutate(lab=factor(lab, levels=unique(lab)))

rcols <- split(x$html, x$lab) |> unlist()
usethis::use_data(rcols, overwrite = TRUE)


rcols.pairings <- tibble(lab=unique(x$lab)) |>
  filter(lab!="gray") |>
  mutate(i=row_number()) |>
  mutate(complementary=map(i, ~ c(lab[.], lab[((.-1+12) %% 24)+1])),
         split_complementary=map(i, ~ c(lab[.], lab[(.-1+13) %% 24 + 1], lab[(.-1+11) %% 24+1])),
         triadic=map(i, ~ c(lab[.], lab[(.-1+8) %% 24 + 1], lab[(.-1+16) %% 24+1])),
         tetradic=map(i, ~ c(lab[.], lab[(.-1+6) %% 24 + 1], lab[(.-1+12) %% 24+1], lab[(.-1+18) %% 24 + 1])),
         analogous=map(i, ~ c(lab[.], lab[(.-1+1) %% 24 + 1], lab[(.-1+23) %% 24+1])),
         set6=map(i, ~ c(lab[.], lab[(.-1+4) %% 24 + 1], lab[(.-1+8) %% 24 + 1], lab[(.-1+12) %% 24 + 1], lab[(.-1+16) %% 24 + 1], lab[(.-1+20) %% 24 + 1]))) |>
  select(-i)
usethis::use_data(rcols.pairings, overwrite = TRUE, internal=TRUE)
