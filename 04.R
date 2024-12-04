# Day 4
library(tidyverse)
raw <- read_fwf("data/04.txt", col_positions = fwf_widths(rep(1, 140), col_names = paste0("v", 1:140)))

## Part 1

to_str <- function(x) {
  paste(x, collapse = "")
}

count_xmas <- function(x) {
  x <- to_str(x)
  forward <- str_count(x, "XMAS")
  back <- str_count(stringi::stri_reverse(x), "XMAS")
  forward + back
}

do_cols <- function(df) {
  df |>
    summarize(across(everything(), count_xmas))  |>
    unlist(use.names = FALSE)
}

do_rows <- function(df) {
  df |>
    rowwise() |>
    summarize(res = count_xmas(c_across(everything()))) |>
    pull(res)
}

get_diagonals <- function(df) {
  ind <- matrix(1:(ncol(df) * ncol(df)), nrow = ncol(df), ncol = ncol(df))
  splitter <- as.factor(row(ind) - col(ind))
  
  vec <- df |>
    pivot_longer(everything()) |>
    pull(value)
  
  split(vec, splitter)
}

do_diagonals <- function(df) {
  # count_xmas() already takes care of reversals
  l_and_r <- get_diagonals(df) |>
    map_int(count_xmas)
  
  up_and_dn <- get_diagonals(df[1:ncol(df), nrow(df):1]) |>
    map_int(count_xmas)
  
  l_and_r + up_and_dn
  
}

# Do it
sum(c(do_cols(raw), do_rows(raw), do_diagonals(raw)))

## Part 2
# I hate these stupid elves now
get_submatrix <- function(data = raw, r1, r2, c1, c2) {
  data[r1:r2, c1:c2]
}

is_xmas <- function(x) {
  vec <- unlist(x)
  ((vec[5] == "A") &
      ((vec[1] == "M" &
          vec[9] == "S") | (vec[1] == "S" & vec[9] == "M"))) &
    ((vec[5] == "A") &
       ((vec[3] == "M" &
           vec[7] == "S") | (vec[3] == "S" & vec[7] == "M")))
}

ind <- matrix(1:(ncol(raw) * ncol(raw)), nrow = ncol(raw), ncol = ncol(raw))

rows <- 1:(nrow(ind))
cols <- 1:(ncol(ind))

bound <- ncol(raw) + 1

ind_list <- expand.grid(r1 = rows, c1 = cols) |>
  as_tibble() |>
  mutate(r2 = r1 + 2, c2 = c1 + 2) |>
  filter(r1 < bound, c1 < bound, r2 < bound, c2 < bound) |>
  transpose()

# Do it
map(ind_list, \(x) rlang::exec("get_submatrix", !!!x)) |> # gross
  map(is_xmas) |>
  as.numeric() |>
  sum()
