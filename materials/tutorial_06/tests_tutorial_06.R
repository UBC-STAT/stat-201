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
# Question 1.6

test_1.6 <- function() {
  test_that('Did not assign answer to an object called "max_flow_result_plot"', {
    expect_true(exists("max_flow_result_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(max_flow_result_plot))
  })

  properties <- c(max_flow_result_plot$layers[[1]]$mapping, max_flow_result_plot$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(max_flow_result_plot$layers[[1]]$geom))

    # Remove if not needed:
    expect_true("GeomSegment" %in% class(max_flow_result_plot$layers[[3]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", max_flow_result_plot$layers[[1]])[["stat_params"]][["bins"]])),
    "71db8a6cad03244e6e50f0ad8bc95a65"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(max_flow_result_plot$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(max_flow_result_plot$data$stat))), "9576b6777b306f3bf8649d96d80029dc")

    # If stat is not known:
    # expect_equal(digest(round(sum(pull(max_flow_result_plot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(max_flow_result_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(max_flow_result_plot$labels))
  })

  print("Success!")
}


# +
# Question 1.7

test_1.7 <- function() {
  test_that('Did not assign answer to an object called "answer1.7"', {
    expect_true(exists("answer1.7"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer1.7))
  })

  expected_colnames <- c("p_value")
  given_colnames <- colnames(answer1.7)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer1.7))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer1.7$p_value) * 10e6)), "8b9f16c5c4263ff92734e5885038181d")
  })

  print("Success!")
}

# +
# Question 1.9

test_1.9 <- function() {
  test_that('Did not assign answer to an object called "answer1.9"', {
    expect_true(exists("answer1.9"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.9, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.9))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d110f00cfb1b248e835137025804a23b")
  })

  print("Success!")
}

# +
# Question 1.10

test_1.10 <- function() {
  test_that('Did not assign answer to an object called "mean_max_bootstrap_dist"', {
    expect_true(exists("mean_max_bootstrap_dist"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(mean_max_bootstrap_dist))
  })

  expected_colnames <- c("replicate", "stat")
  given_colnames <- colnames(mean_max_bootstrap_dist)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(mean_max_bootstrap_dist))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(mean_max_bootstrap_dist$stat))), "859a89ee67987480a3e83dcbff13ef15")
  })

  print("Success!")
}

# +
# Question 1.11

test_1.11 <- function() {
  test_that('Did not assign answer to an object called "mean_max_flow_ci"', {
    expect_true(exists("mean_max_flow_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(mean_max_flow_ci))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(mean_max_flow_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(mean_max_flow_ci))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(mean_max_flow_ci$lower_ci) * 10e6)), "a8c2610c276518b43d50ff65cd4649f3")
    expect_equal(digest(as.integer(sum(mean_max_flow_ci$upper_ci) * 10e4)), "57c302d7d4ac0e601cf464264df6447b")
  })

  print("Success!")
}

# +
# Question 1.12

test_1.12 <- function() {
  test_that('Did not assign answer to an object called "mean_flow_ci_plot"', {
    expect_true(exists("mean_flow_ci_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(mean_flow_ci_plot))
  })

  properties <- c(mean_flow_ci_plot$layers[[1]]$mapping, mean_flow_ci_plot$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(mean_flow_ci_plot$layers[[1]]$geom))

    # Remove if not needed:
    expect_true("GeomRect" %in% class(mean_flow_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(mean_flow_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", mean_flow_ci_plot$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(mean_flow_ci_plot$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(mean_flow_ci_plot$data$stat))), "5cb36e46ad96537202f5b6df950ca6da")

    # If stat is not known:
    # expect_equal(digest(round(sum(pull(mean_flow_ci_plot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(mean_flow_ci_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(mean_flow_ci_plot$labels))
  })

  print("Success!")
}

# +
# Question 2.1

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "adelie_chinstrap_flipper"', {
    expect_true(exists("adelie_chinstrap_flipper"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(adelie_chinstrap_flipper))
  })

  expected_colnames <- c("species", "flipper_length_mm")
  given_colnames <- colnames(adelie_chinstrap_flipper)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(adelie_chinstrap_flipper))), "7a508917c5a0cf5111df42bb714e32a8")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(adelie_chinstrap_flipper$flipper_length_mm))), "a1d0138b85cde684c02ba8726d80d9e1")
  })

  print("Success!")
}

# +
# Question 2.4

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "penguin_count"', {
    expect_true(exists("penguin_count"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(penguin_count))
  })

  expected_colnames <- c("species", "n")
  given_colnames <- colnames(penguin_count)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(penguin_count))), "c01f179e4b57ab8bd9de309e6d576c48")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(penguin_count$n))), "7a508917c5a0cf5111df42bb714e32a8")
  })

  print("Success!")
}

# +
# Question 2.5

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "obs_diff_in_medians"', {
    expect_true(exists("obs_diff_in_medians"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(obs_diff_in_medians))
  })

  expected_colnames <- c("stat")
  given_colnames <- colnames(obs_diff_in_medians)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(obs_diff_in_medians))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(obs_diff_in_medians$stat))), "a9a8883dac7a645a24f4ed180029e4a9")
  })

  print("Success!")
}
# -

# Question 2.6
test_2.6 <- function() {
  test_that('Did not assign answer to an object called "null_diff_in_medians"', {
    expect_true(exists("null_diff_in_medians"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(null_diff_in_medians))
  })

  expected_colnames <- c("replicate", "stat")
  given_colnames <- colnames(null_diff_in_medians)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(null_diff_in_medians))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(null_diff_in_medians$stat))), "0b9349e1f2980e47f00ec4dfd6b4e17c")
  })

  print("Success!")
}

# +
# Question 2.7

test_2.7 <- function() {
  test_that('Did not assign answer to an object called "diff_in_medians_plot"', {
    expect_true(exists("diff_in_medians_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(diff_in_medians_plot))
  })

  properties <- c(diff_in_medians_plot$layers[[1]]$mapping, diff_in_medians_plot$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(diff_in_medians_plot$layers[[1]]$geom))

    # Remove if not needed:
    expect_true("GeomArea" %in% class(diff_in_medians_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", diff_in_medians_plot$layers[[1]])[["stat_params"]][["bins"]])),
    "71db8a6cad03244e6e50f0ad8bc95a65"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(diff_in_medians_plot$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(diff_in_medians_plot$data$stat))), "b1bac09bc62ea76ee70585e114b57cce")

    # If stat is not known:
    # expect_equal(digest(round(sum(pull(diff_in_medians_plot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(diff_in_medians_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(diff_in_medians_plot$labels))
  })

  print("Success!")
}

# +
# Question 2.8

test_2.8 <- function() {
  test_that('Did not assign answer to an object called "answer2.8"', {
    expect_true(exists("answer2.8"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer2.8))
  })

  expected_colnames <- c("p_value")
  given_colnames <- colnames(answer2.8)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer2.8))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer2.8$p_value) * 10e6)), "1473d70e5646a26de3c52aa1abd85b1f")
  })

  print("Success!")
}

# +
# Question 2.9

test_2.9 <- function() {
  test_that('Did not assign answer to an object called "answer2.9"', {
    expect_true(exists("answer2.9"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.9, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.9))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# +
# Question 2.11

test_2.11 <- function() {
  test_that('Did not assign answer to an object called "diff_in_medians_bootstrap_dist"', {
    expect_true(exists("diff_in_medians_bootstrap_dist"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(diff_in_medians_bootstrap_dist))
  })

  expected_colnames <- c("replicate", "stat")
  given_colnames <- colnames(diff_in_medians_bootstrap_dist)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(diff_in_medians_bootstrap_dist))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(diff_in_medians_bootstrap_dist$stat))), "dd0dbc05e22c857be679d297cfd1d839")
  })

  print("Success!")
}

# +
# Question 2.13

test_2.13 <- function() {
  test_that('Did not assign answer to an object called "diff_in_medians_ci_plot"', {
    expect_true(exists("diff_in_medians_ci_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(diff_in_medians_ci_plot))
  })

  properties <- c(diff_in_medians_ci_plot$layers[[1]]$mapping, diff_in_medians_ci_plot$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(diff_in_medians_ci_plot$layers[[1]]$geom))

    # Remove if not needed:
    expect_true("GeomRect" %in% class(diff_in_medians_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })


  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(diff_in_medians_ci_plot$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(diff_in_medians_ci_plot$data$stat))), "11a1e979f75e9a284b6dfaf559a4ef06")

    # If stat is not known:
    # expect_equal(digest(round(sum(pull(diff_in_medians_ci_plot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(diff_in_medians_ci_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(diff_in_medians_ci_plot$labels))
  })

  print("Success!")
}

# +
# Question 3.1

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "obs_diff_prop"', {
    expect_true(exists("obs_diff_prop"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(obs_diff_prop))
  })

  expected_colnames <- c("stat")
  given_colnames <- colnames(obs_diff_prop)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(obs_diff_prop))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(obs_diff_prop$stat))), "1473d70e5646a26de3c52aa1abd85b1f")
  })

  print("Success!")
}

# +
# Question 3.2

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "irradiat_null_distribution"', {
    expect_true(exists("irradiat_null_distribution"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(irradiat_null_distribution))
  })

  expected_colnames <- c("replicate", "stat")
  given_colnames <- colnames(irradiat_null_distribution)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(irradiat_null_distribution))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(irradiat_null_distribution$stat) * 10e4)), "29dd644dbe7c349dfa5987eab61afa99")
  })

  print("Success!")
}

# +
# Question 3.3

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "irradiate_result_plot"', {
    expect_true(exists("irradiate_result_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(irradiate_result_plot))
  })

  properties <- c(irradiate_result_plot$layers[[1]]$mapping, irradiate_result_plot$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(irradiate_result_plot$layers[[1]]$geom))

    # Remove if not needed:
    expect_true("GeomSegment" %in% class(irradiate_result_plot$layers[[3]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", irradiate_result_plot$layers[[1]])[["stat_params"]][["bins"]])),
    "71db8a6cad03244e6e50f0ad8bc95a65"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(irradiate_result_plot$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(irradiate_result_plot$data$stat))), "cb6bf34f4fa5893cf9fe8286e81bd32d")

    # If stat is not known:
    # expect_equal(digest(round(sum(pull(irradiate_result_plot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(irradiate_result_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(irradiate_result_plot$labels))
  })

  print("Success!")
}

