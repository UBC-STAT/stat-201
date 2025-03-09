# ---
# jupyter:
#   jupytext:
#     formats: r:light
#     text_representation:
#       extension: .r
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.10.2
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

library(digest)
library(testthat)

# +
# Question 1.1

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", or "F")', {
    expect_match(answer1.1, "a|b|c|d|e|f", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.1))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d110f00cfb1b248e835137025804a23b")
  })

  print("Success!")
}

# +
# Question 1.2

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.2, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.2))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

# +
# Question 1.3

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "answer1.3"', {
    expect_true(exists("answer1.3"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.3, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.3))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d110f00cfb1b248e835137025804a23b")
  })

  print("Success!")
}

# +
# Question 2.1.1

test_2.1.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.1"', {
    expect_true(exists("answer2.1.1"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", or "E")', {
    expect_match(answer2.1.1, "a|b|c|d|e", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.1.1))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# +
# Question 2.1.2

test_2.1.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.2"', {
    expect_true(exists("answer2.1.2"))
  })

  test_that('Solution should be a single character ("A" or "B")', {
    expect_match(answer2.1.2, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.1.2))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# +
# Question 2.1.3

test_2.1.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.3"', {
    expect_true(exists("answer2.1.3"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", or "E")', {
    expect_match(answer2.1.3, "a|b|c|d|e", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.1.3))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# +
# Question 2.1.4

test_2.1.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.4"', {
    expect_true(exists("answer2.1.4"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.1.4, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.1.4))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

# +
# Question 2.1.5

test_2.1.5 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.5"', {
    expect_true(exists("answer2.1.5"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer2.1.5))
  })

  expected_colnames <- c("population", "alpha", "proportion_rejection")
  given_colnames <- colnames(answer2.1.5)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer2.1.5))), "8eaca7c9b35d05ab15c9125bc92372fa")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer2.1.5$proportion_rejection) * 10e8)), "fb9f7d8415be99bbb8f680f978279b7b")
  })

  print("Success!")
}

# +
# Question 2.2.1

test_2.2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.1"', {
    expect_true(exists("answer2.2.1"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer2.2.1))
  })

  expected_colnames <- c("population", "effect_size", "mu0", "pop_mean", "proportion_rejection")
  given_colnames <- colnames(answer2.2.1)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer2.2.1))), "fa5a4df7ac0f9782037da890557fd8b8")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer2.2.1$proportion_rejection) * 10e6)), "0c8cbf9718f02b5b5360eb91d79ab815")
  })

  print("Success!")
}

# +
# Question 2.2.2

test_2.2.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.2"', {
    expect_true(exists("answer2.2.2"))
  })

  test_that('Solution should be a single character ("A", "B", or "C")', {
    expect_match(answer2.2.2, "a|b|c", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.2.2))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# +
# Question 2.2.3

test_2.2.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.3"', {
    expect_true(exists("answer2.2.3"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", or "F")', {
    expect_match(answer2.2.3, "a|b|c|d|e|f", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.2.3))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "7279bb4184f9c53d42729c6eb22db36a")
  })

  print("Success!")
}

# +
# Question 2.2.4

test_2.2.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.4"', {
    expect_true(exists("answer2.2.4"))
  })

  test_that('Solution should be a single character ("A", "B", or "C")', {
    expect_match(answer2.2.4, "a|b|c", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.2.4))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# +
# Question 2.2.5

test_2.2.5 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.5"', {
    expect_true(exists("answer2.2.5"))
  })
    
  answer_hash <- digest(tolower(paste(sort(unlist(strsplit(answer2.2.5, ""))), collapse = "")))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "72d61ac1d448cce9c6933fde60a9dbe8")
  })

  print("Success!")
}

# +
# Question 2.2.6

test_2.2.6 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.6"', {
    expect_true(exists("answer2.2.6"))
  })

  answer_as_numeric <- as.numeric(answer2.2.6)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric*10e6)), "2ef17cb939db5c58e310bd2436385ad9")
  })

  print("Success!")
}

# +
# Question 2.2.7

test_2.2.7 <- function() {
  test_that('Did not assign answer to an object called "power_function_2.2.7"', {
    expect_true(exists("power_function_2.2.7"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(power_function_2.2.7))
  })

  expected_colnames <- c("possible_mu", "prop_rejection")
  given_colnames <- colnames(power_function_2.2.7)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(power_function_2.2.7))), "ade76ff25149e0df3a56010f12ea82fd")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(power_function_2.2.7$prop_rejection) * 10e6)), "856510ee6e0cf521c129f51b5cf2ed6b")
  })

  print("Success!")
}
# -

test_2.2.8 <- function() {
  test_that('Did not assign answer to an object called "power_function_plot_2.2.8"', {
    expect_true(exists("power_function_plot_2.2.8"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(power_function_plot_2.2.8))
  })

  properties <- c(power_function_plot_2.2.8$layers[[1]]$mapping, power_function_plot_2.2.8$mapping)

  test_that("Plot should have possible_mu on the x-axis", {
    expect_true("possible_mu" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomLine" %in% class(power_function_plot_2.2.8$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(power_function_plot_2.2.8$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(power_function_plot_2.2.8$data)), "ade76ff25149e0df3a56010f12ea82fd")
    expect_equal(digest(round(sum(power_function_plot_2.2.8$data$possible_mu))), "165142c54a626f5155eb49083d5e4a64")

    # If possible_mu is not known:
    # expect_equal(digest(round(sum(pull(power_function_plot_2.2.8$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(power_function_plot_2.2.8$labels))
  })

  print("Success!")
}

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "mean_body_mass_chinstrap_ci"', {
    expect_true(exists("mean_body_mass_chinstrap_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(mean_body_mass_chinstrap_ci))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(mean_body_mass_chinstrap_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(mean_body_mass_chinstrap_ci))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(mean_body_mass_chinstrap_ci$lower_ci) * 1000)), "06eccdb3e477ed83195b3770c2de2751")
    expect_equal(digest(as.integer(sum(mean_body_mass_chinstrap_ci$upper_ci) * 1000)), "a8f21a785cb1bdb766c496785f439e6b")
  })

  print("Success!")
}


test_3.2 <- function() {
  test_that('Did not assign answer to an object called "answer3.2"', {
    expect_true(exists("answer3.2"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer3.2))
  })

  expected_colnames <- c("estimate", "statistic", "p.value", "parameter", "conf.low", "conf.high", "method", "alternative")
  given_colnames <- colnames(answer3.2)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer3.2))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer3.2$estimate) * 1000)), "348165d7d89dee688d274d1932b82618")
    expect_equal(digest(as.integer(sum(answer3.2$statistic) * 1000)), "5ae7d27a6e26b43d45a87f0ee18b2efa")
    expect_equal(digest(as.integer(sum(answer3.2$p.value) * 1000)), "1473d70e5646a26de3c52aa1abd85b1f")
  })

  print("Success!")
}

test_4.1 <- function() {
  test_that('Did not assign answer to an object called "ci_4.1"', {
    expect_true(exists("ci_4.1"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(ci_4.1))
  })

  expected_colnames <- c("variable", "mean", "sd", "lower_ci", "upper_ci")
  given_colnames <- colnames(ci_4.1)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(ci_4.1))), "234a2a5581872457b9fe1187d1616b13")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(ci_4.1$mean) * 10e3)), "2bef5bdf254a103e0bc4e4dad7b62bf8")
    expect_equal(digest(as.integer(sum(ci_4.1$sd) * 10e3)), "881a1faeab1c0053d2870b8abc48b8c7")
    expect_equal(digest(as.integer(sum(ci_4.1$lower_ci) * 10e3)), "ba2fc7616a3531d7860960b93519e1a3")
    expect_equal(digest(as.integer(sum(ci_4.1$upper_ci) * 10e3)), "819a8a6c866fc20e6075217540ac32d3")
  })

  print("Success!")
}

test_4.2 <- function() {
  test_that('Did not assign answer to an object called "ci_4.1"', {
    expect_true(exists("ci_4.1"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(ci_4.1))
  })

  expected_colnames <- c("variable", "mean", "sd", "lower_ci", "upper_ci", "true_mean", "captured")
  given_colnames <- colnames(ci_4.1)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(ci_4.1))), "234a2a5581872457b9fe1187d1616b13")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(ci_4.1$mean) * 10e3)), "2bef5bdf254a103e0bc4e4dad7b62bf8")
    expect_equal(digest(as.integer(sum(ci_4.1$sd) * 10e3)), "881a1faeab1c0053d2870b8abc48b8c7")
    expect_equal(digest(as.integer(sum(ci_4.1$lower_ci) * 10e3)), "ba2fc7616a3531d7860960b93519e1a3")
    expect_equal(digest(as.integer(sum(ci_4.1$upper_ci) * 10e3)), "819a8a6c866fc20e6075217540ac32d3")
    expect_equal(digest(as.integer(sum(ci_4.1$captured))), "234a2a5581872457b9fe1187d1616b13")
  })

  print("Success!")
}

test_4.3 <- function() {
  test_that('Did not assign answer to an object called "prob_collectively_capturing"', {
    expect_true(exists("prob_collectively_capturing"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(prob_collectively_capturing))
  })

  expected_colnames <- c("prob")
  given_colnames <- colnames(prob_collectively_capturing)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(prob_collectively_capturing))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(prob_collectively_capturing$prob) * 10e3)), "665c053af43786d69e096caa27bf601b")
  })

  print("Success!")
}

# +
# Question 4.4

test_4.4 <- function() {
  test_that('Did not assign answer to an object called "answer4.4"', {
    expect_true(exists("answer4.4"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", or "E")', {
    expect_match(answer4.4, "a|b|c|d|e", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer4.4))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}
# -

test_4.5 <- function() {
  test_that('Did not assign answer to an object called "prob_multiple_tests"', {
    expect_true(exists("prob_multiple_tests"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(prob_multiple_tests))
  })

  expected_colnames <- c("prop_error_type_1")
  given_colnames <- colnames(prob_multiple_tests)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(prob_multiple_tests))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(prob_multiple_tests$prop_error_type_1) * 10e3)), "98dd0bfc483dba5a93b76a3fb9b37c37")
  })

  print("Success!")
}
