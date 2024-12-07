raw <- scan("data/06_test.txt", what = character()) 
maze <- matrix(unlist(strsplit(raw,"")),
       nrow = length(raw), byrow = T)

move <- function(direction){
    switch(direction,
           "u" = c(-1,0),
           "d" = c(1,0),
           "l" = c(0,-1),
           "r" = c(0,1))
  }

# Absolute direction after 90 degree right turn
turn90 <- function(direction){
    switch(direction,
           "u" = "r",
           "d" = "l",
           "l" = "u",
           "r" = "d")
  }

## Part 1

# Starting at position, pointing up
location <- which(maze == "^", arr.ind = T) |>
  as.vector()

direction <- "u"

# Keep track
visited <- list()

# Walk the maze
while(location[1] != 1 & location[1] != nrow(maze) &
      location[2] != 1 & location[2] != ncol(maze)){
  
  next_location <- location + move(direction)
  
  if(maze[next_location[1], next_location[2]] == "#") {
    visited[[length(visited) + 1]] <- paste(location, collapse = ",")
    direction  <- turn90(direction)
  } else {
    visited[[length(visited) + 1]] = paste(location, collapse = ",")
    location <- next_location
  }
}

# Last step
visited[[length(visited) + 1]]  <- paste(next_location, collapse = ",")

length(unique(visited))

## Part 2
tictoc::tic()
# Reset location
location <- which(maze == "^", arr.ind = T) |>
  as.vector()

base_path <- visited # path walked from Part 1
tot <- length(base_path)
result <- rep(FALSE, length = length(base_path) -1)
k <- 0

for(i in seq_along(base_path)[-1]){

  if (i %% 100 == 0) message(paste(i, "/", tot))
  
  k <- i-1
  j <- as.numeric(strsplit(base_path[[i]],",")[[1]][2])
  i <- as.numeric(strsplit(base_path[[i]],",")[[1]][1])
  
  if(maze[i,j] == "#" | maze[i,j] == "^"){next}
  
  location <- which(maze == "^", arr.ind = T) |> as.vector()
  direction <-  "u"
  
  # Like before
  visited <- list()
  
  # Add an obstacle at the next step on our walk
  new_maze <- maze |> (\(.){.[i,j] = "#"; .})()
  
  # And walk that new maze, waiting to either get to an edge
  # or back to where we began
  while(location[1] != 1 & location[1] != nrow(new_maze) &
        location[2] != 1 & location[2] != ncol(new_maze) &
        !paste(paste(location, collapse = ","), direction, sep = ",") %in% visited[-length(visited)]) { 
    next_location <- location + move(direction)
    
    if(new_maze[next_location[1], next_location[2]] == "#"){
      visited[[length(visited) + 1]] <- paste(paste(location, collapse = ","), direction,sep = ",")
      direction <- turn90(direction)
    } else {
      location <- next_location
    }
    
  }
  
  # If we've reached an edge it's an exit
  if(location[1] != 1 & location[1] != nrow(maze) &
     location[2] != 1 & location[2] != ncol(maze)){
    result[k] <- TRUE # It's a loop
  } else {
    result[k] <- FALSE # It's an exit
  }
  
}

# Now filter the base path by what we have learned 
# about which positions produce paths that loop
unique(base_path[-1][result]) |>
  length()
tictoc::toc()

## Having finished this I see it could be parallelized on the list. Turn the for loop into a function and parcel it out.