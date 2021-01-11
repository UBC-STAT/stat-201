library(testthat)
library(digest)

test_1.0 <- function() {
  test_that('Did not assign answer1.0 to an object called "answer1.0"', {
    expect_true(exists("answer1.0"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.0, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer1.0))
  
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("These parameters are typically used to describe the spread of a distribution (how 'wide') it is.")
  } else if (answer == "ddf100612805359cd81fdc5ce3b9fbba" | answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Only one of these parameters is typically used to describe the centre of a distribution.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer, "d110f00cfb1b248e835137025804a23b")
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
  
  if (digest(tolower(answer1.2)) != "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Consider the amount of resources that it takes to acquire data for an entire population.")
  }

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.2)), "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer1.3 to an object called "answer1.3"', {
    expect_true(exists("answer1.3"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D", "E", or "F")', {
    expect_match(answer1.3, "a|b|c|d|e|f", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer1.3))
  
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Usually we do not have the resources to take many random samples from the population.")
    print("We cannot calculate the population parameter without access to data for the entire population.")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Usually we do not have the resources to take many random samples from the population.")
  } else if (answer == "d110f00cfb1b248e835137025804a23b") {
    print("We cannot calculate the population parameter without access to data for the entire population.")
  } else if (answer == "93a9078c6326f37b481d3e99b60ad987") {
    print("Usually we do not have the resources to take many random samples from the population.")
  } else if (answer == "7279bb4184f9c53d42729c6eb22db36a") {
    print("We cannot calculate the population parameter without access to data for the entire population.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "metric_trees"', {
    expect_true(exists("metric_trees"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(metric_trees))
  })

  expected_colnames <- c(
    "tree_id", "civic_number", "std_street", "genus_name", "species_name",
    "cultivar_name", "common_name", "assigned", "root_barrier", "plant_area",
    "on_street_block", "on_street", "neighbourhood_name", "street_side_name",
    "height_range_id", "diameter", "curb", "date_planted", "longitude",
    "latitude"
  )
  given_colnames <- colnames(metric_trees)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(metric_trees), 146611)
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(metric_trees$diameter))), "d03a8bcfcd74d036e0bddd06382e5c6a")
  })
  
  print("Success!")
}

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "acer_pop"', {
    expect_true(exists("acer_pop"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(acer_pop))
  })
  
  expected_colnames <- c("diameter")
  given_colnames <- colnames(acer_pop)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(acer_pop), 36062)
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(acer_pop$diameter))), "8e1550615fd0a1e740ee6a2d73e3b9f1")
  })
  
  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "acer_pop_dist"', {
    expect_true(exists("acer_pop_dist"))
  })
  properties <- c(acer_pop_dist$layers[[1]]$mapping, acer_pop_dist$mapping)
  
  test_that("Plot does not have the correct variable on the x-axis", {
    expect_true("diameter" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(acer_pop_dist$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", acer_pop_dist$layers[[1]])[["stat_params"]][["binwidth"]])),
      "dd4ad37ee474732a009111e3456e7ed7"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(nrow(acer_pop_dist$data), 36062)
    expect_equal(digest(as.integer(sum(acer_pop_dist$data$diameter))), "8e1550615fd0a1e740ee6a2d73e3b9f1")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(acer_pop_dist$labels$x == "diameter")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(acer_pop_dist$labels))
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
  
  if (digest(tolower(answer2.2)) != "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("The distribution is not symmetrical and does appear to be shaped like a bell.")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.2)), "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success!")
}

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "acer_estimates"', {
    expect_true(exists("acer_estimates"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(acer_estimates))
  })
  
  expected_colnames <- c("replicate", "sample_mean")
  given_colnames <- colnames(acer_estimates)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(acer_estimates), 1500)
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(acer_estimates$sample_mean))), "5477edd975c4d2fb63c95d1996742535")
  })
  
  print("Success!")
}

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "acer_sampling_dist"', {
    expect_true(exists("acer_sampling_dist"))
  })
  properties <- c(acer_sampling_dist$layers[[1]]$mapping, acer_sampling_dist$mapping)
  
  test_that("Plot does not have the correct variable on the x-axis", {
    expect_true("sample_mean" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(acer_sampling_dist$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", acer_sampling_dist$layers[[1]])[["stat_params"]][["binwidth"]])),
      "4b5630ee914e848e8d07221556b0a2fb"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(nrow(acer_sampling_dist$data), 1500)
    expect_equal(digest(as.integer(sum(acer_sampling_dist$data$sample_mean))), "5477edd975c4d2fb63c95d1996742535")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(acer_sampling_dist$labels$x == "sample_mean")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(acer_sampling_dist$labels))
  })
  
  print("Success!")
}

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "answer2.5"', {
    expect_true(exists("answer2.5"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.5, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.5))
  
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Usually we do not have the resources to take many random samples from the population.")
  } else if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Recall that this is the sampling distribution of sample -MEANS-.")
  } else  if (answer == "d110f00cfb1b248e835137025804a23b") {
    print("The population does not have a bell-shaped distribution. Note we would usually expect a -SAMPLE- distribution to have a similar shape to the population distribution, regardless of the shape of the population distribution. However, this is a -SAMPLING- distribution.")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(answer, "6e7a8c1c098e8817e3df3fd1b21149d1")
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
    expect_equal(nrow(plum_pop), 8692)
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(plum_pop$diameter))), "cb68d342e29a827cd9c7026b6cbb404d")
  })
  
  print("Success!")
}

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "plum_pop_dist"', {
    expect_true(exists("plum_pop_dist"))
  })
  properties <- c(plum_pop_dist$layers[[1]]$mapping, plum_pop_dist$mapping)
  
  test_that("Plot does not have the correct variable on the x-axis", {
    expect_true("diameter" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(plum_pop_dist$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", plum_pop_dist$layers[[1]])[["stat_params"]][["binwidth"]])),
      "c01f179e4b57ab8bd9de309e6d576c48"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(nrow(plum_pop_dist$data), 8692)
    expect_equal(digest(as.integer(sum(plum_pop_dist$data$diameter))), "cb68d342e29a827cd9c7026b6cbb404d")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(plum_pop_dist$labels$x == "diameter")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(plum_pop_dist$labels))
  })
  
  print("Success!")
}

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "plum_estimates"', {
    expect_true(exists("plum_estimates"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(plum_estimates))
  })
  
  expected_colnames <- c("replicate", "sample_median")
  given_colnames <- colnames(plum_estimates)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(plum_estimates), 1500)
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(plum_estimates$sample_median))), "49ee54bb174c15327bb4882d335a1344")
  })
  
  print("Success!")
}

test_3.4 <- function() {
  test_that('Did not assign answer to an object called "plum_sampling_dist"', {
    expect_true(exists("plum_sampling_dist"))
  })
  properties <- c(plum_sampling_dist$layers[[1]]$mapping, plum_sampling_dist$mapping)
  
  test_that("Plot does not have the correct variable on the x-axis", {
    expect_true("sample_median" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(plum_sampling_dist$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", plum_sampling_dist$layers[[1]])[["stat_params"]][["binwidth"]])),
      "4b5630ee914e848e8d07221556b0a2fb"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(nrow(plum_sampling_dist$data), 1500)
    expect_equal(digest(as.integer(sum(plum_sampling_dist$data$sample_median))), "49ee54bb174c15327bb4882d335a1344")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(plum_sampling_dist$labels$x == "sample_median")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(plum_sampling_dist$labels))
  })
  
  print("Success!")
}

test_4.1 <- function() {
  test_that('Did not assign answer to an object called "plum_pop_dist"', {
    expect_true(exists("plum_pop_dist"))
  })
  properties <- c(plum_pop_dist$layers[[1]]$mapping, plum_pop_dist$mapping)
  
  test_that("Plot does not have the correct variable on the x-axis", {
    expect_true("diameter" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(plum_pop_dist$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", plum_pop_dist$layers[[1]])[["stat_params"]][["binwidth"]])),
      "c01f179e4b57ab8bd9de309e6d576c48"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(nrow(plum_pop_dist$data), 8692)
    expect_equal(digest(as.integer(sum(plum_pop_dist$data$diameter))), "cb68d342e29a827cd9c7026b6cbb404d")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(plum_pop_dist$labels$x == "diameter")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(plum_pop_dist$labels))
  })
  
  print("Success!")
}