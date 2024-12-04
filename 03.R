## Day 3
library(tidyverse)

raw <- read_lines("data/03.txt") |> paste(collapse = " ")

## Part 1
str_replace_all(raw, "mul", "\nmul") |> 
  str_split_1("\n") |> 
  str_extract_all("mul\\(\\d{1,3},\\d{1,3}\\)") |> 
  flatten_chr() |> 
  as_tibble() |> 
  separate_wider_delim(value, ",", names = c("l", "r"), cols_remove = FALSE) |> 
  mutate(l = as.numeric(str_remove_all(l, "mul\\(")), 
         r = as.numeric(str_remove_all(r, "\\)")), 
         result = l*r) |> 
  summarize(total = sum(result))
  

## Part 2
## If we split lines on do(), everything after a don't() can be deleted on 
## each line. Then we're just back to Part 1.
str_replace_all(raw, "do\\(\\)", "\ndo()") |> 
  str_split_1("\n") |> 
  str_remove_all("don\\'t\\(\\).*") |> 
  str_extract_all("mul\\(\\d{1,3},\\d{1,3}\\)") |> 
  flatten_chr() |> 
  as_tibble() |> 
  separate_wider_delim(value, ",", names = c("l", "r"), cols_remove = FALSE) |> 
  mutate(l = as.numeric(str_remove_all(l, "mul\\(")), 
         r = as.numeric(str_remove_all(r, "\\)")), 
         result = l*r) |> 
  summarize(total = sum(result))

