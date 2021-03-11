get_one <- function(names_location = 'data/list.txt') {
  # Load the list of people
  people <- readLines(names_location)
  people <- people[sapply(people, nchar) > 0] # Remove empty lines, if any

  # Choose one
  chosen_one <- sample(people, 1)

  return(chosen_one)
}

make_groups <- function(names_location = 'data/list.txt', group_size = 2, padding = '') {
  # Load the list of people
  people <- readLines(names_location)
  people <- people[sapply(people, nchar) > 0] # Remove empty lines, if any
  n_people <- length(people)

  # Shuffle the list of people
  shuffled <- sample(people)

  # The grouping problem:
  # You cannot accommodate 5 people in 2-person rooms. At least not without
  # having a room with 1 or 3 people.
  # In order to fix this, we'll create groups with one extra spot, and fill them
  # following this rule: better too crowded than too empty.
  #
  # group_size = 2
  # n_people = 1, 1 group
  # x o o
  # n_people = 2, 1 group
  # x x o
  # n_people = 3, 1 group
  # x x x
  # n_people = 4, 2 groups
  # x x o
  # x x o
  # n_people = 5, 2 groups
  # x x x
  # x x o
  # n_people = 6, 3 groups
  # x x o
  # x x o
  # x x o
  # ...
  #
  # We see that the number of groups relates to the number of people by:
  n_groups <- n_people %/% group_size
  n_slots <- n_groups * (group_size + 1) # + 1 because of the extra spot we may need

  # Pad the ungrouped ones with an empty name at the end
  shuffled <- c(shuffled, rep(padding, n_slots - n_people))

  # Arrange as a matrix / table
  arranged <- matrix(shuffled, nrow = n_groups)

  return(arranged)
}
