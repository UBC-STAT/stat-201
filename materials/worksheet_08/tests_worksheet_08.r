# ---
# jupyter:
#   jupytext:
#     formats: r:light
#     text_representation:
#       extension: .r
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.5.2
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

library(digest)
library(testthat)

# +
## Question 1.1

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.1, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.1))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("There are some assumptions that are made. Don't remember those? Take a look at Worksheet 07.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

# +
## Question 1.2

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.2, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.2))
  if (answer_hash != "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Make sure to use the right quantiles.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# +
## Question 1.3 

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "answer1.3"', {
    expect_true(exists("answer1.3"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.3, "a|b|c|d|e|f", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.3))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# +
## Question 2.1

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "gaussian_pop_dist"', {
    expect_true(exists("gaussian_pop_dist"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(gaussian_pop_dist))
  })

  properties <- c(gaussian_pop_dist$layers[[1]]$mapping, gaussian_pop_dist$mapping)

  test_that("Plot should have height on the x-axis", {
    expect_true("height" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(gaussian_pop_dist$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })


  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(gaussian_pop_dist$data)), "f1a30baa3072c1aad822c059f35c6841")
    expect_equal(digest(round(sum(gaussian_pop_dist$data$height))), "989457ec4f527ff994d6c68af659fc92")

    # If height is not known:
    # expect_equal(digest(round(sum(pull(gaussian_pop_dist$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })


  test_that("Plot should have a title", {
    expect_true("title" %in% names(gaussian_pop_dist$labels))
  })

  print("Success!")
}

# +
# Question 2.2

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "zscore_sample_means"', {
    expect_true(exists("zscore_sample_means"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(zscore_sample_means))
  })

  expected_colnames <- c("replicate", "sample_mean", "z")
  given_colnames <- colnames(zscore_sample_means)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(zscore_sample_means))), "6e96c307060fba1b1d3a36d2410fd595")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(zscore_sample_means$z) * 10e4)), "97e4a71eb2558f7f0bb404408c7baa1f")
  })

  print("Success!")
}

# +
# Question 2.3

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_sample_mean_z"', {
    expect_true(exists("sampling_dist_sample_mean_z"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(sampling_dist_sample_mean_z))
  })

  properties <- c(sampling_dist_sample_mean_z$layers[[1]]$mapping, sampling_dist_sample_mean_z$mapping)

  test_that("Plot should have z on the x-axis", {
    expect_true("z" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(sampling_dist_sample_mean_z$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", sampling_dist_sample_mean_z$layers[[1]])[["stat_params"]][["binwidth"]]*10)),
    "11946e7a3ed5e1776e81c0f0ecd383d0"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_sample_mean_z$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(sum(sampling_dist_sample_mean_z$data$z))), "f1c543e92910a8c0066e2ed53c6f8bd1")

    # If sample_mean_z is not known:
    # expect_equal(digest(round(sum(pull(sampling_dist_sample_mean_z$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_sample_mean_z$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_sample_mean_z$labels))
  })

  print("Success!")
}

# +
# Question 2.4

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_zscore_s"', {
    expect_true(exists("sampling_dist_zscore_s"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(sampling_dist_zscore_s))
  })

  properties <- c(sampling_dist_zscore_s$layers[[1]]$mapping, sampling_dist_zscore_s$mapping)

  test_that("Plot should have z on the x-axis", {
    expect_true("z" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(sampling_dist_zscore_s$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", sampling_dist_zscore_s$layers[[1]])[["stat_params"]][["binwidth"]]*10)),
    "11946e7a3ed5e1776e81c0f0ecd383d0"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_zscore_s$data)), "f1a30baa3072c1aad822c059f35c6841")
    expect_equal(digest(round(sum(sampling_dist_zscore_s$data$z))), "da10619a752a3655f3a5f583be45b726")

    # If z is not known:
    # expect_equal(digest(round(sum(pull(sampling_dist_zscore_s$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_zscore_s$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_zscore_s$labels))
  })

  print("Success!")
}

# +
# Question 3.1.0

test_3.1.0 <- function() {
  test_that('Did not assign answer to an object called "answer3.1.0"', {
    expect_true(exists("answer3.1.0"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", "F", "G", or "H")', {
    expect_match(answer3.1.0, "a|b|c|d|e|f|g|h", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.1.0))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# +
# Question 3.1.1

test_3.1.1 <- function() {
  test_that('Did not assign answer to an object called "answer3.1.1"', {
    expect_true(exists("answer3.1.1"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(answer3.1.1))
  })

  properties <- c(answer3.1.1$layers[[1]]$mapping, answer3.1.1$mapping)

  test_that("Plot should have body_temp on the x-axis", {
    expect_true("body_temp" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(answer3.1.1$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", answer3.1.1$layers[[1]])[["stat_params"]][["binwidth"]]*100)),
    "be3c152f6f6bcd5f85f9e4cba49b1e48"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(answer3.1.1$data)), "bbf77ebc365ea5855142ed8da3c87cf6")
    expect_equal(digest(round(sum(answer3.1.1$data$body_temp))), "5c9b553f50867cee815aa458af1d3b42")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(answer3.1.1$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(answer3.1.1$labels))
  })

  print("Success!")
}

# +
# Question 3.1.2

test_3.1.2 <- function() {
  test_that('Did not assign answer to an object called "answer3.1.2"', {
    expect_true(exists("answer3.1.2"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.1.2, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.1.2))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

# +
# Question 3.1.3

test_3.1.3 <- function() {
  test_that('Did not assign answer to an object called "observed_test_statistic3.1.3"', {
    expect_true(exists("observed_test_statistic3.1.3"))
  })

  answer_as_numeric <- as.numeric(observed_test_statistic3.1.3)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "b530578a0d50e02f4b0a2476d1d05cf6")
  })

  print("Success!")
}

# +
# Question 3.1.4

test_3.1.4 <- function() {
  test_that('Did not assign answer to an object called "null_model3.1.4"', {
    expect_true(exists("null_model3.1.4"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(null_model3.1.4))
  })

  properties <- c(null_model3.1.4$layers[[1]]$mapping, null_model3.1.4$mapping)

  test_that("Plot should have t on the x-axis", {
    expect_true("t" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomLine" %in% class(null_model3.1.4$layers[[1]]$geom))

    # Remove if not needed:
    expect_true("GeomSegment" %in% class(null_model3.1.4$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })


  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(null_model3.1.4$data)), "c806da0181d245ba4f35cf385ab29983")
    expect_equal(digest(round(sum(null_model3.1.4$data$t))), "908d1fd10b357ed0ceaaec823abf81bc")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(null_model3.1.4$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(null_model3.1.4$labels))
  })

  print("Success!")
}

# +
# Question 3.1.5

test_3.1.5 <- function() {
  test_that('Did not assign answer to an object called "answer3.1.5"', {
    expect_true(exists("answer3.1.5"))
  })

  answer_as_numeric <- as.numeric(answer3.1.5)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "529289aebf92ae5e4bf78b3453ba8d8d")
  })

  print("Success!")
}

# +
# Question 3.1.6

test_3.1.6 <- function() {
  test_that('Did not assign answer to an object called "answer3.1.6"', {
    expect_true(exists("answer3.1.6"))
  })

  test_that('Solution should be a single character ("A", or "B")', {
    expect_match(answer3.1.6, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.1.6))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# +
# Question 3.1.7

test_3.1.7 <- function() {
  test_that('Did not assign answer to an object called "answer3.1.7"', {
    expect_true(exists("answer3.1.7"))
  })

  test_that("Solution should be the output of t.test", {
    expect_true("data.frame" %in% class(answer3.1.7))
  })

  test_that("Wrong statistic value", {
    expect_equal(digest(as.integer(answer3.1.7$statistic*10e6)), "b530578a0d50e02f4b0a2476d1d05cf6")
  })
  
  test_that("Wrong p-value", {
    expect_equal(digest(as.integer(answer3.1.7$p.value*10e6)), "529289aebf92ae5e4bf78b3453ba8d8d")
  })
  test_that("Wrong estimate", {
    expect_equal(digest(as.integer(answer3.1.7$estimate*10e6)), "2a3bad28c47da4ed94bd04dd3a8e2402")
  })
  test_that("Wrong parameter", {
    expect_equal(digest(answer3.1.7$parameter), "63996d1e8f7bb83b447df0b0188f621f")
  })
  test_that("Wrong alternative hypothesis", {
    expect_equal(digest(answer3.1.7$alternative), "d095e00cba6f80bf1d5ad5db0400f812")
  })

  print("Success!")
}

# +
# Question 3.2.1

test_3.2.1 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.1"', {
    expect_true(exists("answer3.2.1"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer3.2.1, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.2.1))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# +
# Question 3.2.2

test_3.2.2 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.2_phat"', {
    expect_true(exists("answer3.2.2_phat"))
  })

  answer_as_numeric <- as.numeric(answer3.2.2_phat)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "5cdefc187251dd42f295b91ffd41f667")
  })

  print("Success!")
}

# +
# Question 3.2.3

test_3.2.3 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.3_answer3.2.3_test_statistic"', {
    expect_true(exists("answer3.2.3_test_statistic"))
  })

  answer_as_numeric <- as.numeric(answer3.2.3_test_statistic)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "f7d4a8ba32a4c01ee5b69bc037cb8d1b")
  })

  print("Success!")
}

# +
# Question 3.2.4

test_3.2.4 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.4_answer3.2.4_pvalue"', {
    expect_true(exists("answer3.2.4_pvalue"))
  })

  answer_as_numeric <- as.numeric(answer3.2.4_pvalue)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e10)), "ec330019799dea65b930886ff878ca6d")
  })

  print("Success!")
}

# +
# Question 3.2.5

test_3.2.5 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.5"', {
    expect_true(exists("answer3.2.5"))
  })

  test_that('Solution should be a single character ("A", "B")', {
    expect_match(answer3.2.5, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.2.5))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# +
# Question 3.2.6

test_3.2.6 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.6"', {
    expect_true(exists("answer3.2.6"))
  })

  test_that("Solution should be the output of t.test", {
    expect_true("data.frame" %in% class(answer3.2.6))
  })

  test_that("Wrong statistic value", {
    expect_equal(digest(as.integer(answer3.2.6$statistic*10e6)), "f4b128273540763be08409cb78e6370e")
  })
  
  test_that("Wrong p-value", {
    expect_equal(digest(as.integer(answer3.2.6$p.value*10e10)), "ec330019799dea65b930886ff878ca6d")
  })
  test_that("Wrong estimate", {
    expect_equal(digest(as.integer(answer3.2.6$estimate*10e6)), "5cdefc187251dd42f295b91ffd41f667")
  })
  test_that("Wrong parameter", {
    expect_equal(digest(answer3.2.6$parameter), "cff4821792ec6a04a0622fe7246d298a")
  })
  test_that("Wrong alternative hypothesis", {
    expect_equal(digest(answer3.2.6$alternative), "0ff92e9c2aed54e21f95489292c00267")
  })

  print("Success!")
}

# +
# Question 3.3.1

test_3.3.1 <- function() {
  test_that('Did not assign answer to an object called "flipper_length_boxplots"', {
    expect_true(exists("flipper_length_boxplots"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(flipper_length_boxplots))
  })

  properties <- c(flipper_length_boxplots$layers[[1]]$mapping, flipper_length_boxplots$mapping)

  test_that("Plot should have species on the x-axis", {
    expect_true("species" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot should have flipper_length_mm on the y-axis", {
    expect_true("flipper_length_mm" == rlang::get_expr(properties$y))
  })
  test_that("Plot should have species on fill property", {
    expect_true("species" == rlang::get_expr(properties$fill))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBoxplot" %in% class(flipper_length_boxplots$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(flipper_length_boxplots$data)), "10fe46640da0fc3a66d68ae9fe105e76")
    expect_equal(digest(length(unique(flipper_length_boxplots$data$species))), "c01f179e4b57ab8bd9de309e6d576c48")

    # If species is not known:
    # expect_equal(digest(round(sum(pull(flipper_length_boxplots$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(flipper_length_boxplots$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(flipper_length_boxplots$labels))
  })

  print("Success!")
}

# +
# Question 3.3.2 

test_3.3.2 <- function() {
  test_that('Did not assign answer to an object called "adelie_chin_summary"', {
    expect_true(exists("adelie_chin_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(adelie_chin_summary))
  })

  expected_colnames <- c("species", "sample_mean", "sample_var", "n")
  given_colnames <- colnames(adelie_chin_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(adelie_chin_summary))), "c01f179e4b57ab8bd9de309e6d576c48")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(adelie_chin_summary$sample_mean) * 10000)), "d87713c7a00bc3c4b9cc12fe09075692")
    expect_equal(digest(as.integer(sum(adelie_chin_summary$sample_var) * 10000)), "c75517ee769cbf8810fdc220ff7e2a0c")
    expect_equal(digest(as.integer(sum(adelie_chin_summary$n))), "7a508917c5a0cf5111df42bb714e32a8")
  })

  print("Success!")
}

# +
# Question 3.3.3

test_3.3.3 <- function() {
  test_that('Did not assign answer to an object called "answer3.3.3"', {
    expect_true(exists("answer3.3.3"))
  })

  answer_as_numeric <- as.numeric(answer3.3.3)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10000)), "80f85a912d286d46065649d49a98c8b8")
  })

  print("Success!")
}

# +
# Question 3.3.4

test_3.3.4 <- function() {
  test_that('Did not assign answer to an object called "answer3.3.4"', {
    expect_true(exists("answer3.3.4"))
  })

  answer_as_numeric <- as.numeric(answer3.3.4)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10000)), "1473d70e5646a26de3c52aa1abd85b1f")
  })

  print("Success!")
}


# +
# Question 3.3.5

test_3.3.5 <- function() {
  test_that('Did not assign answer to an object called "answer3.3.5"', {
    expect_true(exists("answer3.3.5"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.3.5, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.3.5))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

# +
# Question 3.3.6

test_3.3.6 <- function() {
    test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer3.3.6$statistic * 1e4)), "80f85a912d286d46065649d49a98c8b8")
    expect_equal(digest(as.integer(answer3.3.6$parameter * 1e5)), "0c60d382756b9734b7ef2b771ecb514b")
    expect_equal(digest(as.integer(answer3.3.6$p.value * 1e9)), "3e98cd61c0f0313ba5b0b2fa1a37a0ec")
  })

  print("Success!")
}

# +
# Question 3.4.1

test_3.4.1 <- function() {
  test_that('Did not assign answer to an object called "answer3.4.1"', {
    expect_true(exists("answer3.4.1"))
  })

  answer_as_numeric <- as.numeric(answer3.4.1)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "eadc6c976e92d423f7d3e4551fc729fe")
  })

  print("Success!")
}

# +
# Question 3.4.2

test_3.4.2 <- function() {
  test_that('Did not assign answer to an object called "answer3.4.2"', {
    expect_true(exists("answer3.4.2"))
  })

  answer_as_numeric <- as.numeric(answer3.4.2)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric)), "1473d70e5646a26de3c52aa1abd85b1f")
  })

  print("Success!")
}

# +
# Question 3.4.3

test_3.4.3 <- function() {
  test_that('Did not assign answer to an object called "answer3.4.3"', {
    expect_true(exists("answer3.4.3"))
  })

  test_that('Solution should be a single character ("A", "B")', {
    expect_match(answer3.4.3, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.4.3))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# +
# Question 3.4.4

test_3.4.4 <- function() {
  test_that('Did not assign answer to an object called "answer3.4.4"', {
    expect_true(exists("answer3.4.4"))
  })

  test_that("Solution should be the output of t.test", {
    expect_true("data.frame" %in% class(answer3.4.4))
  })

  test_that("Wrong statistic value", {
    expect_equal(digest(as.integer(answer3.4.4$statistic*10e6)), "3fa793e6e1421e1687213c53ce135c4f")
  })
  
  test_that("Wrong p-value", {
    expect_equal(digest(as.integer(answer3.4.4$p.value*10e10)), "1473d70e5646a26de3c52aa1abd85b1f")
  })
  test_that("Wrong estimate", {
    expect_equal(digest(as.integer(answer3.4.4$estimate1 * 10e6)), "bcf8cac70cafa62dfccf44f54f598d49")
    expect_equal(digest(as.integer(answer3.4.4$estimate2 * 10e6)), "4c8f1393313e0f2e70d962d986af8fb2")
  })
  test_that("Wrong parameter", {
    expect_equal(digest(answer3.4.4$parameter), "fdc86b4d739d00b8ef1165920b3245e5")
  })
  test_that("Wrong alternative hypothesis", {
    expect_equal(digest(answer3.4.4$alternative), "d095e00cba6f80bf1d5ad5db0400f812")
  })

  print("Success!")
}

# +
# Question 3.5.1

test_3.5.1 <- function() {
  test_that('Did not assign answer to an object called "training"', {
    expect_true(exists("training"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(training))
  })

  expected_colnames <- c("before_training", "after_training", "d")
  given_colnames <- colnames(training)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(training))), "9a6564e67167bff7e7cf99a541a641f1")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(training$d) * 10e5)), "e43a71d21e54a7d68bf601ff98a4a313")
  })

  print("Success!")
}

# +
# Question 3.5.2

test_3.5.2 <- function() {
  test_that('Did not assign answer to an object called "diff_histogram"', {
    expect_true(exists("diff_histogram"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(diff_histogram))
  })

  properties <- c(diff_histogram$layers[[1]]$mapping, diff_histogram$mapping)

  test_that("Plot should have d on the x-axis", {
    expect_true("d" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(diff_histogram$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", diff_histogram$layers[[1]])[["stat_params"]][["binwidth"]])),
    "dd4ad37ee474732a009111e3456e7ed7"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(diff_histogram$data)), "9a6564e67167bff7e7cf99a541a641f1")
    expect_equal(digest(round(sum(diff_histogram$data$d))), "ce18e33793bae7c1b1834413c4a1a7c8")

    # If d is not known:
    # expect_equal(digest(round(sum(pull(diff_histogram$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(diff_histogram$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(diff_histogram$labels))
  })

  print("Success!")
}

# +
# Question 3.5.3

test_3.5.3 <- function() {
  test_that('Did not assign answer to an object called "answer3.5.3"', {
    expect_true(exists("answer3.5.3"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer3.5.3, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.5.3))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# +
# Question 3.5.4

test_3.5.4 <- function() {
  test_that('Did not assign answer to an object called "answer3.5.4"', {
    expect_true(exists("answer3.5.4"))
  })

  answer_as_numeric <- as.numeric(answer3.5.4)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e5)), "f2a1fe995fd2de8ebb95296927bc5ba7")
  })

  print("Success!")
}

# +
# Question 3.5.5

test_3.5.5 <- function() {
  test_that('Did not assign answer to an object called "answer3.5.5"', {
    expect_true(exists("answer3.5.5"))
  })

  answer_as_numeric <- as.numeric(answer3.5.5)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e11)), "fad7585242507af556c64ac730027ed5")
  })

  print("Success!")
}

# +
# Question 3.5.6

test_3.5.6 <- function() {
  test_that('Did not assign answer to an object called "answer3.5.6"', {
    expect_true(exists("answer3.5.6"))
  })

  test_that('Solution should be a single character ("A", "B")', {
    expect_match(answer3.5.6, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.5.6))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# +
# Question 3.5.7

test_3.5.7 <- function() {
  test_that('Did not assign answer to an object called "answer3.5.7"', {
    expect_true(exists("answer3.5.7"))
  })

  test_that("Solution should be the output of t.test", {
    expect_true("data.frame" %in% class(answer3.5.7))
  })

  test_that("Wrong statistic value", {
    expect_equal(digest(as.integer(answer3.5.7$statistic*10e6)), "5f3f9a2b249700c4abdf83a429cc4b8f")
  })
  
  test_that("Wrong p-value", {
    expect_equal(digest(as.integer(answer3.5.7$p.value*10e6)), "1473d70e5646a26de3c52aa1abd85b1f")
  })
  test_that("Wrong estimate", {
    expect_equal(digest(as.integer(answer3.5.7$estimate*10e6)), "a463bf6ff963e84808b4fc7072ce750d")
  })
  test_that("Wrong parameter", {
    expect_equal(digest(answer3.5.7$parameter), "cb296d017f50125cb5c4cd8e507aaa1b")
  })
  test_that("Wrong alternative hypothesis", {
    expect_equal(digest(answer3.5.7$alternative), "0ff92e9c2aed54e21f95489292c00267")
  })

  print("Success!")
}
