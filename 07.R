library(tidyverse)
library(RcppAlgos)
library(furrr)
plan(multisession)

make_opgrid <- function(n_operations, operators) {
  RcppAlgos::permuteGeneral(operators, n_operations, repetition = TRUE)
}

make_str <- function(x, y, sep = ".") paste(x, y, sep = sep)

# Again, fuck you, elves
`%schmultiply%` <- function (a, b) a * b
`%schmadd%` <- function (a, b) a + b
`%schmollapse%` <- function (a, b) {
  as.numeric(paste0(a,b))
} 

raw <- readr::read_lines("data/07.txt") 

equations <- raw |> 
  str_remove("\\d{1,}: ") |> 
  str_split(" ") |> 
  map(as.numeric)

n_operations <- map_int(equations, length) - 1

## Part 1 and Part 2
# Fuck it we're just going to brute force it; 
# I have 16 cores I paid good money for and recursion 
# is above my recursion pay grade.

pt1_operators <- c("*", "+")
pt2_operators <- c("*", "+", "#")

# Choose pt1 or pt2
operators <- pt2_operators

# Some of them are too big to be coerced to integers. Fuck you, elves
test_values <- str_extract(raw, "(^)(\\d{1,})(:)", group = 2) |> 
  as.numeric()

op_sequences <- n_operations |> 
  map(\(x) make_opgrid(x, operators)) 


# Assemble a single equation and evaluate it
reduce_to_string <- function(op_sequence, equation) {
  
  # # Have to do this here because of furrr
  `%schmultiply%` <- function (a, b) a * b
  `%schmadd%` <- function (a, b) a + b
  `%schmollapse%` <- function (a, b) {
    as.numeric(paste0(a,b))
  }
  
  str <- equation |> 
    reduce2(op_sequence, make_str) 
  str <- str_replace_all(str, "\\*", "%schmultiply%")
  str <- str_replace_all(str, "\\+", "%schmadd%")
  str <- str_replace_all(str, "#", "%schmollapse%")
  str
}

evaluate_string <- function(str) {
  # # Have to do this here because of furrr
  `%schmultiply%` <- function (a, b) a * b
  `%schmadd%` <- function (a, b) a + b
  `%schmollapse%` <- function (a, b) {
    as.numeric(paste0(a,b))
  } 
  
  str |> str2expression() |> eval()   
}

# Take a list of operator permutations and calculate them all for a given equation
check_operator_perms <- function(op_sequences, equation) {
  ops <- op_sequences |> asplit(1)
  ops |> 
    map(\(x) reduce_to_string(x, equation)) |> unlist()
}

# Are any of the values the test value?
is_valid <- function(result_vec, test_value) {
  any(test_value %in% result_vec)
}

# Check all operator sequences and equations
strings <- future_map2(op_sequences, equations, check_operator_perms)  
results <- future_map(strings, \(x) future_map_dbl(x, evaluate_string))
valid <- results |>  
  map2_lgl(test_values, \(x,y) is_valid(x, y)) 

options(digits=20)
sum(test_values[valid])
options(digits=7)



