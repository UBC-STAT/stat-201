library(digest)
library(testthat)

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "answer1.0"', {
    expect_true(exists("answer1.0"))
  })

  test_that('Solution should be a string of ALL the following characters ("A", "B", "C", "D", and "E")', {
    expect_match(answer1.0, "^[aA|bB|cC|dD|eE]{5}$")
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.0)), "aa817d77bfc37cf923b17fe3465952f0")
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

  answer_hash <- digest(tolower(answer1.1))
  if (answer_hash == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Think about how the shape of the bootstrap distribution is affected by sample size")
  } else if (answer_hash == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("The centre of the bootstrap distribution may not be the same, but the standard deviation is a good estimate of the standard error of an estimator")
  } else if (answer_hash == "d110f00cfb1b248e835137025804a23b") {
    print("Think about the pennies example fromv modern dive")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "pop_mean"', {
    expect_true(exists("pop_mean"))
  })
  test_that("Answer object should be a single number, not a dataframe", {
    expect_false("data.frame" %in% class(pop_mean))
  })
  answer_as_numeric <- as.numeric(pop_mean)
  test_that("Solution should be a single number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1000000)), "47ec131903ad9e44ca74761a81ba7e29")
  })

  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.1"', {
    expect_true(exists("answer2.1"))
  })

  answer_as_numeric <- as.numeric(answer2.1)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution should be an integer", {
    expect_true(answer_as_numeric %% 1 == 0)
  })

  test_that("Solution is incorrect", {
    expect_true(digest(as.integer(answer_as_numeric)) == "7c7124efff5c7039a1b1e7cba65c5379" | digest(as.integer(answer_as_numeric)) == "9d08099943f8627959cfb8ecee0d2f5d" | digest(as.integer(answer_as_numeric)) == "8eaca7c9b35d05ab15c9125bc92372fa")
  })

  print("Success!")
}

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "sample_1"', {
    expect_true(exists("sample_1"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_1))
  })

  expected_colnames <- c("diameter")
  given_colnames <- colnames(sample_1)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sample_1))), "2bbdc9479e5ddf03425adc57599af655")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(sample_1$diameter))), "1d7b23b077358cf4fa53bf3d1e1d81d8")
  })

  print("Success!")
}

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "upper_quantile"', {
    expect_true(exists("upper_quantile"))
  })

  answer_as_numeric <- as.numeric(upper_quantile)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric) * 1000000), "f7be34588ada5ef454b8117a06dd0d33")
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

  answer_hash <- digest(tolower(answer2.4))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Revisit the definition of a quantile")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "lower_quantile"', {
    expect_true(exists("lower_quantile"))
  })

  answer_as_numeric <- as.numeric(lower_quantile)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric) * 1000000), "50f9a6d45084d41172a5f0fefc4178a5")
  })

  print("Success!")
}

test_2.6 <- function() {
  test_that('Did not assign answer to an object called "sample_quantile_plot"', {
    expect_true(exists("sample_quantile_plot"))
  })
  properties <- c(sample_quantile_plot$layers[[1]]$mapping, sample_quantile_plot$mapping)

  test_that("Plot should have diameter on the x-axis", {
    expect_true("diameter" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(sample_quantile_plot$layers[[1]]$geom))
    expect_true("GeomVline" %in% class(sample_quantile_plot$layers[[2]]$geom))
    expect_true("GeomVline" %in% class(sample_quantile_plot$layers[[3]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", sample_quantile_plot$layers[[1]])[["stat_params"]][["binwidth"]]) * 1000),
    "998633da79c2a3f44fe6482751ba47e1"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sample_quantile_plot$data)), "2bbdc9479e5ddf03425adc57599af655")
    expect_equal(digest(as.integer(sum(sample_quantile_plot$data$diameter) * 1000000)), "c7f66da1cae4f223b9bae717f05900f7")
  })

  test_that("Vertical line layers are not in the correct locations", {
    expect_equal(digest(as.numeric(sample_quantile_plot$layers[[2]]$data) * 1000000), "cbca884fded8a271a9d2c0cafee98a90")
    expect_equal(digest(as.numeric(sample_quantile_plot$layers[[3]]$data) * 1000000), "e202f4968c4813a4470552909accad76")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sample_quantile_plot$labels$x == "diameter")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sample_quantile_plot$labels))
  })

  print("Success!")
}

test_2.7 <- function() {
  test_that('Did not assign answer to an object called "p_below"', {
    expect_true(exists("p_below"))
  })

  answer_as_numeric <- as.numeric(p_below)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1000000)), "19fdc64eb993dca43f6f6d5136ae0208")
  })

  print("Success!")
}

test_2.8 <- function() {
  test_that('Did not assign answer to an object called "p_between"', {
    expect_true(exists("p_between"))
  })

  answer_as_numeric <- as.numeric(p_between)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1000000)), "f4ea81e7a356c1aea215c7f4295cc2e2")
  })

  print("Success!")
}

test_2.9 <- function() {
  test_that('Did not assign answer to an object called "answer2.9"', {
    expect_true(exists("answer2.9"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.9, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.9))
  if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8") {
    print("If 25% of the observations fall below lower_quantile and 25% fall above upper_quantile, what proportion lie between the two?")
  } else if (answer_hash == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Revisit the definition of a quantile: if 75% of the observations lie below, what proportion lie above?")
  } else if (answer_hash == "d110f00cfb1b248e835137025804a23b") {
    print("Revisit the definition of a quantile")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

test_2.10 <- function() {
  test_that('Did not assign answer to an object called "answer2.10"', {
    expect_true(exists("answer2.10"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.10, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.10))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Revisit the definition of quantiles. If 10% of the observation lie below the lower quantile and 40% lie above the upper quantile, what proportion lie between?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_3.0 <- function() {
  test_that('Did not assign answer to an object called "sample_2"', {
    expect_true(exists("sample_2"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_2))
  })

  expected_colnames <- c("diameter")
  given_colnames <- colnames(sample_2)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sample_2))), "7d2842cab7725fd8f382293e410d42b2")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(sample_2$diameter) * 1000000)), "e1e301e732cbe4988c21ffec8bc2c7e3")
  })

  print("Success!")
}

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "bootstrap_dist"', {
    expect_true(exists("bootstrap_dist"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(bootstrap_dist))
  })

  expected_colnames <- c("mean_diameter")
  given_colnames <- colnames(bootstrap_dist)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(bootstrap_dist))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(bootstrap_dist$mean_diameter) * 1000000)), "c7f66da1cae4f223b9bae717f05900f7")
  })

  print("Success!")
}

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "ci"', {
    expect_true(exists("ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(ci))
  })

  expected_colnames <- c("ci_lower", "ci_upper")
  given_colnames <- colnames(ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(ci$ci_lower) * 1000000), "d4ef090268118448c4f4b8f7c2cef425")
    expect_equal(digest(as.integer(ci$ci_upper) * 1000000), "9e62aede040a3339d7fe4f57cb3b61da")
  })

  print("Success!")
}

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "ci_plot"', {
    expect_true(exists("ci_plot"))
  })
  properties <- c(ci_plot$layers[[1]]$mapping, ci_plot$mapping)

  test_that("Plot should have mean_diameter on the x-axis", {
    expect_true("mean_diameter" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(ci_plot$layers[[1]]$geom))
    expect_true("GeomVline" %in% class(ci_plot$layers[[3]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", ci_plot$layers[[1]])[["stat_params"]][["binwidth"]])),
    "4b5630ee914e848e8d07221556b0a2fb"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(ci_plot$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(as.integer(sum(ci_plot$data$mean_diameter) * 1000000)), "c7f66da1cae4f223b9bae717f05900f7")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(ci_plot$labels$x == "mean_diameter")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(ci_plot$labels))
  })

  print("Success!")
}

test_3.4 <- function() {
  test_that('Did not assign answer to an object called "answer3.4"', {
    expect_true(exists("answer3.4"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.4, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.4))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Take a look at the mapping for the different layers of the plot")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_3.5 <- function() {
  test_that('Did not assign answer to an object called "answer3.5"', {
    expect_true(exists("answer3.5"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.5, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.5))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("Compare the ranges of the sample ID column")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
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

  answer_hash <- digest(tolower(answer3.6))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("Compare the ranges of the sample ID column")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

test_3.7 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_estimate"', {
    expect_true(exists("sampling_dist_estimate"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sampling_dist_estimate))
  })

  expected_colnames <- c("replicate", "bootstrap_mean")
  given_colnames <- colnames(sampling_dist_estimate)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sampling_dist_estimate))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(sampling_dist_estimate$bootstrap_mean) * 1000000)), "c7f66da1cae4f223b9bae717f05900f7")
  })

  print("Success!")
}

test_3.8 <- function() {
  test_that('Did not assign answer to an object called "intervals"', {
    expect_true(exists("intervals"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(intervals))
  })

  expected_colnames <- c("sample_id", "ci_lower", "ci_upper")
  given_colnames <- colnames(intervals)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(intervals))), "5d6e7fe43b3b73e5fd2961d5162486fa")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(intervals$ci_lower) * 100000)), "475863f7b59a7ac90ba822dc3f1e28f3")
    expect_equal(digest(as.integer(sum(intervals$ci_upper) * 100000)), "f487416aca9e23b69bc7dee15ed812ca")
  })

  print("Success!")
}

test_3.9 <- function() {
  test_that('Did not assign answer to an object called "intervals_captured"', {
    expect_true(exists("intervals_captured"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(intervals_captured))
  })

  expected_colnames <- c("sample_id", "ci_lower", "ci_upper", "captured")
  given_colnames <- colnames(intervals_captured)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(intervals_captured))), "5d6e7fe43b3b73e5fd2961d5162486fa")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(intervals_captured$ci_lower) * 1000000)), "5e2e6071008dd2bd800537489b3a59c5")
    expect_equal(digest(as.integer(sum(intervals_captured$ci_upper) * 1000000)), "c7f66da1cae4f223b9bae717f05900f7")
    expect_equal(digest(as.integer(sum(intervals_captured$captured))), "e444a32cd8c806b12b8baff9696a342f")
  })

  print("Success!")
}

test_3.10 <- function() {
  test_that('Did not assign answer to an object called "many_ci_plot"', {
    expect_true(exists("many_ci_plot"))
  })
  properties <- c(many_ci_plot$layers[[1]]$mapping, many_ci_plot$mapping)

  test_that("Lower boundary of x-axis segments should be ci_lower", {
    expect_true("ci_lower" == rlang::get_expr(properties$x))
  })

  test_that("Upper boundary of x-axis segments should be ci_upper", {
    expect_true("ci_upper" == rlang::get_expr(properties$xend))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(as.integer(sum(many_ci_plot$data$ci_lower) * 1000000)), "5e2e6071008dd2bd800537489b3a59c5")
    expect_equal(digest(as.integer(sum(many_ci_plot$data$ci_upper) * 1000000)), "c7f66da1cae4f223b9bae717f05900f7")
    expect_equal(digest(as.integer(sum(many_ci_plot$data$captured))), "e444a32cd8c806b12b8baff9696a342f")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(ci_plot$labels$x == "ci_lower")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(ci_plot$labels))
  })

  print("Success!")
}

test_3.11 <- function() {
  test_that('Did not assign answer to an object called "answer3.11"', {
    expect_true(exists("answer3.11"))
  })

  answer_as_numeric <- as.numeric(answer3.11)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution should be an integer", {
    expect_true(answer_as_numeric %% 1 == 0)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric)), "e444a32cd8c806b12b8baff9696a342f")
  })

  print("Success!")
}

test_3.12 <- function() {
  test_that('Did not assign answer to an object called "answer3.12"', {
    expect_true(exists("answer3.12"))
  })

  answer_as_numeric <- as.numeric(answer3.12)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution should be an integer", {
    expect_true(answer_as_numeric %% 1 == 0)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric)), "5ea3aa881cac20eac898460bc769efae")
  })

  print("Success!")
}

test_3.13 <- function() {
  test_that('Did not assign answer to an object called "answer3.13"', {
    expect_true(exists("answer3.13"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.13, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.13))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("These are two common interpretations of a confidence interval. HOWEVER, note that we DO NOT say 'The interval has a 90% CHANCE OF CAPTURING THE TRUE PARAMETER'.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_4.0 <- function() {
  test_that('Did not assign answer to an object called "answer4.0"', {
    expect_true(exists("answer4.0"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer4.0, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer4.0))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("This is a common interpretation of confidence intervals. Once an interval is calculated, it either captures or does not capture the true parameter (i.e., the probability of the interval capturing the interval is either 0 or 1)")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

test_4.1 <- function() {
  test_that('Did not assign answer to an object called "answer4.1"', {
    expect_true(exists("answer4.1"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", "F", "G", "H", "I")', {
    expect_match(answer4.1, "a|b|c|d|e|f|g|h|i", ignore.case = TRUE)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer4.1)), "fe98eba4312fd761affde1df9b1b51ea")
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

  answer_hash <- digest(tolower(answer4.2))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("So long as the proportion of the bootstrap distribution that falls between the two quantiles is equal to the confidence level of the interval, the confidence interval is still valid and we can interpret it the same way!")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_4.3 <- function() {
  test_that('Did not assign answer to an object called "answer4.3"', {
    expect_true(exists("answer4.3"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer4.3, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer4.3))
  if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Is this more likely than the other potential confidence level?")
  } else if (answer_hash == "ddf100612805359cd81fdc5ce3b9fbba" | answer_hash == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Think about how the confidence level relates to the proportion of intervals that would capture the true parameter if you calculated many, many confidence intervals.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d110f00cfb1b248e835137025804a23b")
  })

  print("Success!")
}
