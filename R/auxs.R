get_one <- function(names_location = 'data/list.txt') {
  # Load the list of people
  people <- readLines(names_location)

  # Choose one
  chosen_one <- sample(people, 1)

  return(chosen_one)
}

make_groups <- function(names_location = 'data/list.txt', group_size = 2) {
  # Load the list of people
  people <- readLines(names_location)

  # Pad the ungrouped ones with NA
  # If the group size is not an integer divisor of the amount of people, some
  # groups will need the addition of dummy members
  while(length(people) %% group_size != 0) people <- c(people, 'NA')

  # Shuffle and arrange as a matrix of the desired size
  shuffled <- sample(people)
  n_groups <- length(people) / group_size
  arranged <- matrix(shuffled, nrow = n_groups)

  return(arranged)
}
