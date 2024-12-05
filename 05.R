library(tidyverse)

raw <- read_lines("data/05.txt")

# Part 1
all_rules <- raw[str_detect(raw, "\\|")] |> 
  as_tibble() |>
  separate(value, into = c("before", "after"), 
           remove = FALSE)

all_pages <- raw[str_detect(raw, ",")] |> 
  as.list() |> 
  map(str_split_1, ",") 

## The tedious bit
valid_update <- function(page_vec, all_rules = all_rules) {
  rules_to_apply <- all_rules[all_rules$before %in% page_vec & all_rules$after %in% page_vec,] 
  rule_tester <- rules_to_apply |> 
    pull(value) |> str_replace("\\|", ".*")
  all(str_detect(paste(page_vec,collapse="."), rule_tester))
}

get_mid <- function(x) {
  # Assumes they're all odd
  x[(length(x) + 1) / 2]
}

## Do it
is_valid <- all_pages |> 
  map(\(x) valid_update(x, all_rules)) |> 
  unlist()

all_pages[is_valid] |> 
  map(get_mid) |> 
  as.numeric() |> 
  sum()
  


# Part 2
which_rules_violated <- function(page_vec, rule_table = all_rules) {
  rules_to_apply <- all_rules[rule_table$before %in% page_vec & rule_table$after %in% page_vec,] 
  rule_tester <- rules_to_apply |> 
    pull(value) |> str_replace("\\|", ".*")
  red_flag <- !str_detect(paste(page_vec,collapse="."), rule_tester)

  if(nrow(rules_to_apply[red_flag,])==0) return(NULL)
  
  rules_to_apply[red_flag,]
}

repair_sequence <- function(page_vec, rule_table = all_rules, ...) {
  rules_violated <- which_rules_violated(page_vec, rule_table = all_rules)
  
  # Base case
  if(is.null(rules_violated)) return(page_vec)
  
  o <- match(c(rules_violated[1, "before"], rules_violated[1, "after"]), page_vec)
  rev_o <- rev(o)
  page_vec[o] <- page_vec[rev_o]
  repair_sequence(page_vec)
}

all_pages[!is_valid] |> 
  map(repair_sequence) |> 
  map(get_mid) |> 
  as.numeric() |> 
  sum()

