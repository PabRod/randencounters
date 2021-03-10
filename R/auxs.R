get_one <- function(names_location = 'data/list.txt') {
  # Load the list of people
  people <- readLines(names_location)

  # Choose one
  chosen_one <- sample(people, 1)

  return(chosen_one)
}

make_groups <- function(names_location = 'data/list.txt', group_size = 2, padding = '') {
  # Load the list of people
  people <- readLines(names_location)

  # Use the remainder to get the size of the container
  # If the group size is not an integer divisor of the amount of people, some
  # groups will need the addition of dummy members
  container_size <- group_size + group_size * (length(people) - 1) %/% group_size
  # Pad the ungrouped ones with NA
  people <- c(people, rep(padding, container_size - length(people)))

  # Shuffle and arrange as a matrix of the desired size
  shuffled <- sample(people)
  n_groups <- length(people) / group_size
  arranged <- matrix(shuffled, nrow = n_groups)

  return(arranged)
}
