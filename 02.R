## Advent of Code Day 2

safe_diff <- function(x) {
  all(abs(diff(x)) >= 1) & all(abs(diff(x)) <= 3)
}

safe_seq <- function(x) {
 all(sign(diff(x)) == 1) | all(sign(diff(x)) == -1)
}

is_safe <- function(x) {
  all(safe_diff(x), safe_seq(x))
}

# Data
raw <- readr::read_lines("data/02.txt") |> 
  as.list() |> 
  purrr::map(stringr::str_split_1, " ") |> 
  purrr::map(as.numeric)

# Part 1
purrr::map_lgl(raw, is_safe) |> 
  table()


# Part 2
dampener_safe <- function(x) {
  all_grps <- c(list(x), combn(x, length(x)-1, simplify = FALSE))
  any(purrr::map_lgl(all_grps, is_safe))
}

purrr::map_lgl(raw, dampener_safe) |> 
  table()

