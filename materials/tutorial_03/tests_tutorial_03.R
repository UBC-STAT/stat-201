library(testthat)
library(digest)

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.1, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.1))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("Think about potential edge cases.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.2, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.2))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("When we sample with replacement, we put the observation we just sampled BACK into the pool before selecting another.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "answer2.0"', {
    expect_true(exists("answer2.0"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.0, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.0))
  if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8" | answer_hash == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Where would the centre of the new sampling distribution be? Would expect either of the edges of the distribution still appear 'cut off'?")
  } else if (answer_hash == "d110f00cfb1b248e835137025804a23b") {
    print("Try to visualize what the new sampling disitribution would look like compared to the sampling distribution above.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "barrier_pop"', {
    expect_true(exists("barrier_pop"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(barrier_pop))
  })

  expected_colnames <- c("root_barrier")
  given_colnames <- colnames(barrier_pop)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame should not contain any NA values", {
    expect_equal(digest(as.integer(nrow(filter(barrier_pop, is.na(root_barrier))))), "1473d70e5646a26de3c52aa1abd85b1f")
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(barrier_pop))), "33cdfaf5b5548592e62ab05a10e99d7d")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(nrow(filter(barrier_pop, root_barrier == "N")))), "7b52488abfeed248a0eecb9d27db8758")
  })

  print("Success!")
}

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "barrier_sampling_dist"', {
    expect_true(exists("barrier_sampling_dist"))
  })
  properties <- c(barrier_sampling_dist$layers[[1]]$mapping, barrier_sampling_dist$mapping)

  test_that("Plot should have p on the x-axis", {
    expect_true("p" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(barrier_sampling_dist$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", barrier_sampling_dist$layers[[1]])[["stat_params"]][["binwidth"]]) * 1000),
    "908d1fd10b357ed0ceaaec823abf81bc"
    )
  })

  test_that("Plot does not use the correct data. Sampling distribution should be drawn by sampling without replacement", {
    expect_equal(digest(nrow(barrier_sampling_dist$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(sd(barrier_sampling_dist$data$p),7)), "e9db0e152223a01249a4c3225e899af9")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(barrier_sampling_dist$labels$x == "p")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(barrier_sampling_dist$labels))
  })

  print("Success!")
}

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "barrier_sample"', {
    expect_true(exists("barrier_sample"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(barrier_sample))
  })

  expected_colnames <- c("root_barrier")
  given_colnames <- colnames(barrier_sample)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(barrier_sample))), "be3c152f6f6bcd5f85f9e4cba49b1e48")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(nrow(filter(barrier_sample, root_barrier == "N")))), "2a099397e2d2dd0f2a2e5a5b4234867d")
  })

  print("Success!")
}

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.4"', {
    expect_true(exists("answer2.4"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", "F", or "G")', {
    expect_match(answer2.4, "a|b|c|d|e|f|g", ignore.case = TRUE)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.4)), "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "barrier_bootstrap_dist"', {
    expect_true(exists("barrier_bootstrap_dist"))
  })
  properties <- c(barrier_bootstrap_dist$layers[[1]]$mapping, barrier_bootstrap_dist$mapping)

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(barrier_bootstrap_dist$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", barrier_bootstrap_dist$layers[[1]])[["stat_params"]][["binwidth"]]) * 1000),
    "908d1fd10b357ed0ceaaec823abf81bc"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(barrier_bootstrap_dist$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(sum(pull(barrier_bootstrap_dist$data, rlang::get_expr(properties$x))))), "39fe840086944a2ff92b5b0413fada63")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(barrier_bootstrap_dist$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(barrier_bootstrap_dist$labels))
  })

  print("Success!")
}

test_2.6 <- function() {
  # part A
  test_that('Did not assign answer to an object called "standard_error"', {
    expect_true(exists("standard_error"))
  })

  answer_as_numeric <- as.numeric(standard_error)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1000000)), "01b27636cb5a0bc29d3245cf9b5d14d7")
  })

  # part B
  test_that('Did not assign answer to an object called "standard_deviation"', {
    expect_true(exists("standard_deviation"))
  })

  answer_as_numeric <- as.numeric(standard_deviation)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1000000)), "b46b14ab0d3b4cdb172dae88b8921fa6")
  })

  print("Success!")
}

test_2.7 <- function() {
  test_that('Did not assign answer to an object called "answer2.7"', {
    expect_true(exists("answer2.7"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.7, "true|false", ignore.case = TRUE)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.7)), "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_3.0 <- function() {
  test_that('Did not assign answer to an object called "plum_pop"', {
    expect_true(exists("plum_pop"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(plum_pop))
  })

  expected_colnames <- c("diameter")
  given_colnames <- colnames(plum_pop)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(plum_pop))), "81069550898d54275db061d49bb7f779")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(plum_pop$diameter))), "c82ea42b76ddfa4115c6472821803b9f")
  })

  print("Success!")
}

