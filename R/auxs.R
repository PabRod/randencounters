#' Read names list
#'
#' @param names_location Location of the names file
#' @param remove.empty (default=TRUE) for ignoring empty lines in file
#'
#' @return A vector containing all the names
read_people <- function(names_location = 'data/list.txt', remove.empty = TRUE) {
  # Load the list of people
  people <- readLines(names_location)

  # Remove empty lines, if any
  if(remove.empty) people <- people[sapply(people, nchar) > 0]

  return(people)
}

#' Get one
#'
#' @param names_location Location of the names file
#' @param remove.empty (default=TRUE) for ignoring empty lines in file
#'
#' @return One random name from the names file
#' @export
get_one <- function(names_location = 'data/list.txt') {
  # Load the list of people
  people <- read_people(names_location)

  # Choose one
  chosen_one <- sample(people, 1)

  return(chosen_one)
}

#' Make groups
#'
#' @param names_location Location of the names file
#' @param group_size Desired size for each group
#' @param padding Character for empty slots
#'
#' @return The names, grouped
#' @export
make_groups <- function(names_location = 'data/list.txt', group_size = 2, padding = '') {
  # Load the list of people
  people <- read_people(names_location)
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
