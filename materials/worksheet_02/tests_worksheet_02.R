library(digest)
library(tidyverse)
library(testthat)

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "answer1.0"', {
    expect_true(exists("answer1.0"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.0, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer1.0))
  if (answer == "05ca18b596514af73f6880309a21b5dd") {
    print("You may get unlucky and end up with a sample whos distribution is very different than the population")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.0)), "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success!")
}

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.1, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer1.1))
  if (answer != "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Is the variable a result of measuring or counting something?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.1)), "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.2, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer1.2))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Variance COULD be used, but we do not use the term 'standard variance'")
  } else if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Variance COULD be used, but variance is not the same as 'standard error'")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("We do not use the term 'standard variance' to describe this quantity")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.2)), "d110f00cfb1b248e835137025804a23b")
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
  
  answer <- digest(tolower(answer2.0))
  if (answer != "127a2ec00989b9f7faf671ed470be7f8") {
    print("Think about whether we can know the outcome/result/value ahead of time.")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.0)), "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.1"', {
    expect_true(exists("answer2.1"))
  })
  
  test_that('Solution should be a single character ("A", "B", or "C")', {
    expect_match(answer2.1, "a|b|c", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.1))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Note that the age column contains 3000 different values")
  } else if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Note that we have access to data for the entire population")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.1)), "6e7a8c1c098e8817e3df3fd1b21149d1")
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
    print("Would it be possible to know the number of doors on the randomly chosen car ahead of time?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.2)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.3"', {
    expect_true(exists("answer2.3"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.3, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.3))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8" | answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("The observations in a population are constant")
  } else if (answer == "d110f00cfb1b248e835137025804a23b") {
    print("We choose the number of individuals that will be in a random sample")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.3)), "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.4"', {
    expect_true(exists("answer2.4"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.4, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.4))
  if (answer == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Can we predict the exact value of a point estimate ahead of time?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.4)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}

test_3.0 <- function() {
  test_that('Did not assign answer to an object called "samples_10"', {
    expect_true(exists("samples_10"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(samples_10))
  })
  
  expected_colnames <- c("replicate", "current_land_value")
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
    expect_equal(digest(round(sum(samples_10$current_land_value))), "3e099e0db802813c09ca8348c926590c")
  })
  print("Success!")
}

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "sample_means_10"', {
    expect_true(exists("sample_means_10"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_means_10))
  })
  
  expected_colnames <- c("replicate", "sample_mean")
  given_colnames <- colnames(sample_means_10)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sample_means_10))), "6e96c307060fba1b1d3a36d2410fd595")
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(round(sum(sample_means_10$sample_mean))), "02c8a90ff3e9d6e7ae52d3a1a91473d0")
  })
  print("Success!")
}

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_10"', {
    expect_true(exists("sampling_dist_10"))
  })
  properties <- c(sampling_dist_10$layers[[1]]$mapping, sampling_dist_10$mapping)
  
  test_that("Plot should have sample_mean on the x-axis", {
    expect_true("sample_mean" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_10$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", sampling_dist_10$layers[[1]])[["stat_params"]][["binwidth"]])),
      "829aba66b0d64feac09b067c4cce133c"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_10$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(sum(sampling_dist_10$data$sample_mean))), "02c8a90ff3e9d6e7ae52d3a1a91473d0")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_10$labels$x == "sample_mean")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_10$labels))
  })
  
  print("Success!")
}

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_30"', {
    expect_true(exists("sampling_dist_30"))
  })
  properties <- c(sampling_dist_30$layers[[1]]$mapping, sampling_dist_30$mapping)
  
  test_that("Plot should have sample_mean on the x-axis", {
    expect_true("sample_mean" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_30$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", sampling_dist_30$layers[[1]])[["stat_params"]][["binwidth"]])),
      "829aba66b0d64feac09b067c4cce133c"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_30$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(sum(sampling_dist_30$data$sample_mean))), "dcb992ef78d0037df70f40d9bcc38502")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_30$labels$x == "sample_mean")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_30$labels))
  })
  
  print("Success!")
}

test_3.4 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_100"', {
    expect_true(exists("sampling_dist_100"))
  })
  properties <- c(sampling_dist_100$layers[[1]]$mapping, sampling_dist_100$mapping)
  
  test_that("Plot should have sample_mean on the x-axis", {
    expect_true("sample_mean" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_100$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", sampling_dist_100$layers[[1]])[["stat_params"]][["binwidth"]])),
      "829aba66b0d64feac09b067c4cce133c"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_100$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(sum(sampling_dist_100$data$sample_mean))), "1b4540113f4b9b7acbbf8457887e2708")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_100$labels$x == "sample_mean")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_100$labels))
  })
  
  print("Success!")
}

test_3.5 <- function() {
  test_that('Did not assign answer to an object called "answer3.5"', {
    expect_true(exists("answer3.5"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer3.5, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer3.5))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8" | answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Compare the widths of the distributions more closely")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Compare the shape and symmetry of the distributions more closely")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer3.5)), "d110f00cfb1b248e835137025804a23b")
  })
  
  print("Success!")
}

test_3.6 <- function() {
  test_that('Did not assign answer to an object called "answer3.6"', {
    expect_true(exists("answer3.6"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.6, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer3.6))
  if (answer == "05ca18b596514af73f6880309a21b5dd") {
    print("As sample size DECREASES, how does the width of the sampling distribution change?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer3.6)), "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success!")
}

test_4.0 <- function() {
  test_that('Did not assign answer to an object called "answer4.0"', {
    expect_true(exists("answer4.0"))
  })
  
  answer <- as.numeric(answer4.0)
  test_that("Solution should be a number", {
    expect_false(is.na(answer))
  })
  
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer*100000000)), "cae2b05d9ab58b419a67405bb6e59257")
  })
  
  print("Success!")
}

test_4.1 <- function() {
  test_that('Did not assign answer to an object called "answer4.1"', {
    expect_true(exists("answer4.1"))
  })
  
  answer <- as.numeric(answer4.1)
  test_that("Solution should be a number", {
    expect_false(is.na(answer))
  })
  
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer)), "57cd4574292fec5b2b517c39404a7800")
  })
  
  print("Success!")
}

test_4.2 <- function() {
  test_that('Did not assign answer to an object called "answer4.2"', {
    expect_true(exists("answer4.2"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer4.2, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer4.2))
  if (answer == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Is the variable a measurement or count of something?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer4.2)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}

test_4.3 <- function() {
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

test_4.4 <- function() {
  test_that('Did not assign answer to an object called "p"', {
    expect_true(exists("p"))
  })
  
  answer <- as.numeric(p)
  test_that("Solution should be a number", {
    expect_false(is.na(answer))
  })
  
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer*1000000000)), "e5fa23f0d7f52b406dc5c5d551e97dc6")
  })
  
  print("Success!")
}

test_4.5 <- function() {
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

test_4.6 <- function() {
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

test_4.7 <- function() {
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

test_4.8 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_30"', {
    expect_true(exists("sampling_dist_30"))
  })
  properties <- c(sampling_dist_30$layers[[1]]$mapping, sampling_dist_30$mapping)
  
  test_that("Plot should have sample_proportion on the x-axis", {
    expect_true("sample_proportion" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_30$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", sampling_dist_30$layers[[1]])[["stat_params"]][["binwidth"]])*30),
      "908d1fd10b357ed0ceaaec823abf81bc"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_30$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(as.integer(sum(sampling_dist_30$data$sample_proportion)*30)), "22ccbb1d89a68e2c01565574d0bdd3f6")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_30$labels$x == "sample_proportion")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_30$labels))
  })
  
  print("Success!")
}

test_4.9 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_100"', {
    expect_true(exists("sampling_dist_100"))
  })
  properties <- c(sampling_dist_100$layers[[1]]$mapping, sampling_dist_100$mapping)
  
  test_that("Plot should have sample_proportion on the x-axis", {
    expect_true("sample_proportion" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_100$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", sampling_dist_100$layers[[1]])[["stat_params"]][["binwidth"]])*100),
      "908d1fd10b357ed0ceaaec823abf81bc"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_100$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(as.integer(sum(sampling_dist_100$data$sample_proportion)*100)), "8910cf9b125a2c8164816bbf8593228a")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_100$labels$x == "sample_proportion")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_100$labels))
  })
  
  print("Success!")
}

test_4.10 <- function() {
  test_that('Did not assign answer to an object called "answer4.10"', {
    expect_true(exists("answer4.10"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer4.10, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer4.10))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8" | answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Compare the widths of the distributions more closely")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Compare the shape and symmetry of the distributions more closely")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer4.10)), "d110f00cfb1b248e835137025804a23b")
  })
  
  print("Success!")
}

test_4.11 <- function() {
  test_that('Did not assign answer to an object called "answer4.11"', {
    expect_true(exists("answer4.11"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer4.11, "true|false", ignore.case = TRUE)
  })
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer4.11)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}

test_5.0 <- function() {
  test_that('Did not assign answer to an object called "answer5.0"', {
    expect_true(exists("answer5.0"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer5.0, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer5.0))
  if (answer != "127a2ec00989b9f7faf671ed470be7f8") {
    print("Think of the potential biases that could result from this method")
  } 
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer5.0)), "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("Success!")
}

test_5.1 <- function() {
  test_that('Did not assign answer to an object called "answer5.1"', {
    expect_true(exists("answer5.1"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer5.1, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer5.1))
  if (answer == "05ca18b596514af73f6880309a21b5dd") {
    print("Does everyone follow UBC's social media? Who might be more likely to submit a response to the poll?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer5.1)), "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success!")
}

test_5.2 <- function() {
  test_that('Did not assign answer to an object called "answer5.2"', {
    expect_true(exists("answer5.2"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer5.2, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer5.2))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Is the sample of undergraduate TAs representative of the population of interest?")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("In what way is the sample not representative of the population of interest?")
  } else if (answer == "d110f00cfb1b248e835137025804a23b") {
    print("The sample size is not small enough that we can say that there is a 'high' chance that our estimate would be inaccurate")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer5.2)), "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}

test_5.3 <- function() {
  test_that('Did not assign answer to an object called "answer5.3"', {
    expect_true(exists("answer5.3"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer5.3, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer5.3))
  if (answer == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("ompare the proportions of each type of worker in the sample to their proportion of the population")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer5.3)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}