library(tidyverse) 

# Data
raw <- read_table("data/01.txt",
                  col_names = c("left", "right")) 

# Part 1
tibble(left = sort(raw$left), 
       right = sort(raw$right)) |> 
  mutate(d = abs(left-right)) |> 
  summarize(tot = sum(d))


# Part 2
get_n_cases <- function(x, table = raw$right) {
  length(which(table %in% x))
}

raw |>   
  mutate(times = map_int(left, \(x) get_n_cases(x)), 
         score = left * times) |> 
  summarize(dist = sum(score))

