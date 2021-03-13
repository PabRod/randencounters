# Location of the test list
loc <- "../../data/list.txt"

# ===============================================
context("Read people")

test_that("Number", {
  people <- read_people(loc)
  n <- length(people)

  expect_equal(n, 11)
})

# ===============================================
context("Get one")

test_that("Number", {
  chosen_one <- get_one(loc)
  n <- length(chosen_one)

  expect_equal(n, 1)
})

# ===============================================
context("Make groups")

test_that("Dimensions", {
  tab <- make_groups(loc, group_size = 3)
  slots_per_group <- ncol(tab)

  expect_equal(slots_per_group, 4)
})
