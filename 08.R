raw <- read_fwf("data/08.txt", 
                col_positions = fwf_widths(rep(1, 50), 
                                           col_names = paste0("v", 1:50))) |> 
  as.matrix()

# Part 1
antennae <- unique(raw[raw != "."])

bind_it <- function(x) {
  do.call(rbind, x)
}

get_antenna_coords <- function(antenna_type, source = raw) {
  which(source == antenna_type, arr.ind = TRUE) |> 
    apply(1, as.vector, simplify = F) |> 
    combn(2, simplify = F) # for when n given antennae > 2
}


get_full_pairlist <- function(antenna_char, source = raw) {
  which(source == antenna_char, arr.ind = TRUE) |> 
    apply(1, as.vector, simplify = F) #|> 
    combn(2, simplify = F) # for when n given antennae > 2
}

get_antinodes <- function(coord_pair) {
  
  dist <- coord_pair[[1]] - coord_pair[[2]]
  list(coord_pair[[1]] + dist,
       coord_pair[[1]] - dist,
       coord_pair[[2]] + dist,
       coord_pair[[2]] - dist) |>
    bind_it()
}


get_all_antinodes <- function(coord_pair_list) {
  coord_pair_list |> 
    map(get_antinodes) |> 
    bind_it()
}

collapse_to_pairs <- function(x) {
  apply(x, 1, \(x) paste(x, collapse = ","))
}

# Only filter when it's the antinode's own antenna type
filter_own_antennae <- function(antinode_vec, own_antennae) {
  antinode_vec[!(antinode_vec %in% own_antennae)]
}

filter_oob <- function(x) {
  n_row <- dim(raw)[1]
  n_col <- dim(raw)[2]
  cond <- x[,1] < 1 | x[,1] > n_row |  x[,2] < 1 | x[,2] > n_col
  x[!cond,]
}

process_antennae <- function(indv_ant) {
  # Only one antenna type is coming down the chute here
  coords <- indv_ant |> 
    map(get_antenna_coords) 
  
  all_antinodes <- coords |> 
    map(get_all_antinodes) |> 
    map(filter_oob) |> 
    map(unique) 
    
  all_node_pairs <- coords |>
    unlist() |>
    matrix(ncol = 2, byrow = T) |>
    unique() |> 
    collapse_to_pairs()
  
  all_antinodes |>  
    map(filter_oob) |> 
    map(collapse_to_pairs) |> 
    unlist() |> 
    filter_own_antennae(own_antennae = all_node_pairs) 
}

map(antennae, process_antennae) |> 
  unlist() |> 
  unique() |> 
  length()


# Part 2

get_t_antinodes <- function(coord_pair) {
  
  dist <- coord_pair[[1]] - coord_pair[[2]]
  list(
    map(seq_along(raw), \(x){coord_pair[[1]] + x*dist}) |> bind_it(),
    map(seq_along(raw), \(x){coord_pair[[1]] - x*dist}) |> bind_it(),
    map(seq_along(raw), \(x){coord_pair[[2]] + x*dist}) |> bind_it(),
    map(seq_along(raw), \(x){coord_pair[[2]] - x*dist}) |> bind_it()
  ) |> 
    bind_it()
}


get_all_t_antinodes <- function(coord_pair_list) {
  coord_pair_list |> 
    map(get_t_antinodes) |> 
    bind_it()
}


process_t_antennae <- function(indv_ant) {
  
  coords <- indv_ant |> 
    map(get_antenna_coords) 
  
  coords |> 
    map(get_all_t_antinodes) |> 
    map(filter_oob) |> 
    map(unique) |>  
    map(collapse_to_pairs) |> 
    unlist() 

}

map(antennae, process_t_antennae) |> 
  unlist() |> 
  unique() |> 
  length()

