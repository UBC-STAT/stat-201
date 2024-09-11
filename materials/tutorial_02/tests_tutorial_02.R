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
  
  answer <- as.numeric(answer2.0)
  test_that("Solution should be a number", {
    expect_false(is.na(answer))
  })
  
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer*100000000)), "cae2b05d9ab58b419a67405bb6e59257")
  })
  
  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.1"', {
    expect_true(exists("answer2.1"))
  })
  
  answer <- as.numeric(answer2.1)
  test_that("Solution should be a number", {
    expect_false(is.na(answer))
  })
  
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer)), "57cd4574292fec5b2b517c39404a7800")
  })
  
  print("Success!")
}

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.2"', {
    expect_true(exists("answer2.2"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.2, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.2))
  if (answer == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Is the variable a measurement or count of something?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.2)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "geo_pop"', {
    expect_true(exists("geo_pop"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(geo_pop))
  })
  
  expected_colnames <- c("geo_local_area")
  given_colnames <- colnames(geo_pop)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(geo_pop))), "64fbbe98c827cc7528e73a6215ceca18")
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(sum(as.integer(as.factor(geo_pop$geo_local_area)))), "5d9e6350c1dd8a1ea00593dd9e2280bc")
  })
  print("Success!")
}

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "p"', {
    expect_true(exists("p"))
  })
  
  answer <- as.numeric(p)
  test_that("Object p should be a number, not a dataframe", {
    expect_false("data.frame" %in% class(p))
    expect_false(is.na(answer))
  })
  test_that("the proportion p should be between 0 and 1, do not express as %", {
    expect_true(between(p, 0, 1) )
  })
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer*1000000000)), "e5fa23f0d7f52b406dc5c5d551e97dc6")
  })
  
  print("Success!")
}

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "samples_10"', {
    expect_true(exists("samples_10"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(samples_10))
  })
  
  expected_colnames <- c("replicate", "geo_local_area")
  given_colnames <- colnames(samples_10)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(samples_10))), "7ea55401005f54e88bdc2ce0c9a9ceb1")
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(sum(as.integer(as.factor(samples_10$geo_local_area)))), "5a2453251b4a9d7d98fba220df6fc49a")
  })
  print("Success!")
}

test_2.6 <- function() {
  test_that('Did not assign answer to an object called "sample_proportions_10"', {
    expect_true(exists("sample_proportions_10"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_proportions_10))
  })
  
  test_that("Data frame does not have the correct columns", {
    expect_true("sample_proportion" %in% colnames(sample_proportions_10))
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sample_proportions_10))), "6e96c307060fba1b1d3a36d2410fd595")
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(sample_proportions_10$sample_proportion*10))), "6c00c8f47ed699f6655b2de0196ef19b")
  })
  print("Success!")
}

test_2.7 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_10"', {
    expect_true(exists("sampling_dist_10"))
  })
  properties <- c(sampling_dist_10$layers[[1]]$mapping, sampling_dist_10$mapping)
  
  test_that("Plot should have sample_proportion on the x-axis", {
    expect_true("sample_proportion" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_10$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", sampling_dist_10$layers[[1]])[["stat_params"]][["binwidth"]])*10),
      "908d1fd10b357ed0ceaaec823abf81bc"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_10$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(as.integer(sum(sampling_dist_10$data$sample_proportion)*10)), "6c00c8f47ed699f6655b2de0196ef19b")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_10$labels$x == "sample_proportion")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_10$labels))
  })
  
  print("Success!")
}

test_2.8 <- function() {
  test_that('Did not assign answer to an object called "answer2.8"', {
    expect_true(exists("answer2.8"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.8, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.8))
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

test_3.1 <- function() {
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

test_3.2 <- function() {
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

test_3.3 <- function() {
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

test_3.4 <- function() {
  test_that('Did not assign answer to an object called "answer3.4"', {
    expect_true(exists("answer3.4"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", "F", or "G")', {
    expect_match(answer3.4, "a|b|c|d|e|f|g", ignore.case = TRUE)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer3.4)), "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

test_3.5 <- function() {
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

test_3.6 <- function() {
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

test_3.7 <- function() {
  test_that('Did not assign answer to an object called "answer3.7"', {
    expect_true(exists("answer3.7"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.7, "true|false", ignore.case = TRUE)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer3.7)), "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_4.0 <- function() {
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

