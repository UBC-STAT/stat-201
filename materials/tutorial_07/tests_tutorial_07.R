library(digest)
library(testthat)

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "salmon_sample_dist"', {
    expect_true(exists("salmon_sample_dist"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(salmon_sample_dist))
  })

  properties <- c(salmon_sample_dist$layers[[1]]$mapping, salmon_sample_dist$mapping)

  test_that("Plot should have mercury_concentration on the x-axis", {
    expect_true("mercury_concentration" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(salmon_sample_dist$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", salmon_sample_dist$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(salmon_sample_dist$data)), "ea0bb59a602a20e8b227d3dfb8bdc698")
    expect_equal(digest(round(sum(salmon_sample_dist$data$mercury_concentration))), "db8e490a925a60e62212cefc7674ca02")

    # If mercury_concentration is not known:
    # expect_equal(digest(round(sum(pull(salmon_sample_dist$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(salmon_sample_dist$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(salmon_sample_dist$labels))
  })

  print("Success!")
}

test_1.3_salmon_x_bar <- function() {
  test_that('Did not assign answer to an object called "salmon_x_bar"', {
    expect_true(exists("salmon_x_bar"))
  })

  answer_as_numeric <- as.numeric(salmon_x_bar)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "65f1bc740101c6f7f503857eef279e42")
  })

  print("Success!")
}

test_1.3_salmon_std_error <- function() {
  test_that('Did not assign answer to an object called "salmon_std_error"', {
    expect_true(exists("salmon_std_error"))
  })

  answer_as_numeric <- as.numeric(salmon_std_error)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "9c49fcfa854f01e66979daa8e7876c2c")
  })

  print("Success!")
}


test_1.3_salmon_btsp_samp_dist <- function() {
  test_that('Did not assign answer to an object called "salmon_btsp_samp_dist"', {
    expect_true(exists("salmon_btsp_samp_dist"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(salmon_btsp_samp_dist))
  })

  expected_colnames <- c("replicate", "stat")
  given_colnames <- colnames(salmon_btsp_samp_dist)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(salmon_btsp_samp_dist))), "6e96c307060fba1b1d3a36d2410fd595")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(salmon_btsp_samp_dist$stat))), "4d4c1ad2286f1a7670a024467dd10808")
  })

  print("Success!")
}

test_1.3_salmon_btsp_vs_clt_samp_dist <- function() {
  test_that('Did not assign answer to an object called "salmon_btsp_vs_clt_samp_dist_plot"', {
    expect_true(exists("salmon_btsp_vs_clt_samp_dist_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(salmon_btsp_vs_clt_samp_dist_plot))
  })

  properties <- c(salmon_btsp_vs_clt_samp_dist_plot$layers[[1]]$mapping, salmon_btsp_vs_clt_samp_dist_plot$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(salmon_btsp_vs_clt_samp_dist_plot$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", salmon_btsp_vs_clt_samp_dist_plot$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(salmon_btsp_vs_clt_samp_dist_plot$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(sum(salmon_btsp_vs_clt_samp_dist_plot$data$stat))), "bb5df6b3c6caf4ae04e67e264bbe4113")

    # If stat is not known:
    # expect_equal(digest(round(sum(pull(salmon_btsp_vs_clt_samp_dist_plot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(salmon_btsp_vs_clt_samp_dist_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(salmon_btsp_vs_clt_samp_dist_plot$labels))
  })

  print("Success!")
}

test_1.3 <- function(){
    test_1.3_salmon_x_bar()
    test_1.3_salmon_std_error()
    test_1.3_salmon_btsp_samp_dist()
    test_1.3_salmon_btsp_vs_clt_samp_dist()
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "salmon_btsp_ci"', {
    expect_true(exists("salmon_btsp_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(salmon_btsp_ci))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(salmon_btsp_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(salmon_btsp_ci))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
      expect_equal(digest(as.integer(salmon_btsp_ci$lower_ci+salmon_btsp_ci$upper_ci * 10e6)), "8dd7a0a82149d246e645b6a61172e4b2")
  })

  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "parking_samp_dist_plot"', {
    expect_true(exists("parking_samp_dist_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(parking_samp_dist_plot))
  })

  properties <- c(parking_samp_dist_plot$layers[[1]]$mapping, parking_samp_dist_plot$mapping)

  test_that("Plot should have r_mf_9a_6p on the x-axis", {
    expect_true("r_mf_9a_6p" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(parking_samp_dist_plot$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })


  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(parking_samp_dist_plot$data)), "e1c87205446e1c8cc10879d9851cd008")
    expect_equal(digest(round(sum(parking_samp_dist_plot$data$r_mf_9a_6p))), "02e5eeeda016fcbe0bf6c4c7c7d94bed")

    # If r_mf_9a_6p is not known:
    # expect_equal(digest(round(sum(pull(parking_samp_dist_plot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(parking_samp_dist_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(parking_samp_dist_plot$labels))
  })

  print("Success!")
}

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "parking_summary"', {
    expect_true(exists("parking_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(parking_summary))
  })

  expected_colnames <- c("geo_local_area", "sample_mean", "sample_std_error")
  given_colnames <- colnames(parking_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(parking_summary))), "c01f179e4b57ab8bd9de309e6d576c48")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(parking_summary$sample_mean) * 10e4)), "12db114819edc18d7aff84b33c6f5305")
    expect_equal(digest(as.integer(sum(parking_summary$sample_std_error) * 10e4)), "fb0f0843118f10611154691e118a14c7")
  })

  print("Success!")
}

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "parking_multiple_ci"', {
    expect_true(exists("parking_multiple_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(parking_multiple_ci))
  })

  expected_colnames <- c("replicate", "lower_ci", "upper_ci", "captured")
  given_colnames <- colnames(parking_multiple_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(parking_multiple_ci))), "5d6e7fe43b3b73e5fd2961d5162486fa")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(parking_multiple_ci$lower_ci))), "263b9db4d09244c0e0ff3f826caa8d2e")
    expect_equal(digest(as.integer(sum(parking_multiple_ci$upper_ci))), "10ed7b15ca812930d29ea06c2edde7ef")
    expect_equal(digest(as.integer(sum(parking_multiple_ci$captured))), "8636eeb2cc941c94ef05c8b79cde15fa")
  })

  print("Success!")
}

test_4.1 <- function() {
  test_that('Did not assign answer to an object called "p_summary"', {
    expect_true(exists("p_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(p_summary))
  })

  expected_colnames <- c("n_no", "n_yes", "p_no", "p_yes")
  given_colnames <- colnames(p_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(p_summary))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(p_summary$n_no))), "69291c7a64149c882605dffd1b74141a")
    expect_equal(digest(as.integer(sum(p_summary$n_yes))), "1b387a44b518030b040d524a5fa7e672")
    expect_equal(digest(as.integer(sum(p_summary$p_no) * 10e6)), "36501df683b8973ff0c3976c5eea9f1c")
    expect_equal(digest(as.integer(sum(p_summary$p_yes) * 10e6)), "759e71d4a4767b90c14e3c8d110b7b2a")      
  })

  print("Success!")
}

test_4.2 <- function() {
  test_that('Did not assign answer to an object called "p_summary"', {
    expect_true(exists("p_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(p_summary))
  })

  expected_colnames <- c("n_no", "n_yes", "p_no", "p_yes", "p_diff", "p_diff_std_error")
  given_colnames <- colnames(p_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(p_summary))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(p_summary$n_no))), "69291c7a64149c882605dffd1b74141a")
    expect_equal(digest(as.integer(sum(p_summary$n_yes))), "1b387a44b518030b040d524a5fa7e672")
    expect_equal(digest(as.integer(sum(p_summary$p_no) * 10e6)), "36501df683b8973ff0c3976c5eea9f1c")
    expect_equal(digest(as.integer(sum(p_summary$p_yes) * 10e6)), "759e71d4a4767b90c14e3c8d110b7b2a")      
    expect_equal(digest(as.integer(sum(p_summary$p_diff) * 10e6)), "87ed83a98443f988e25cb2f2d75bfc4b")
    expect_equal(digest(as.integer(sum(p_summary$p_diff_std_error) * 10e6)), "5e18c6c71d9faf021165f2e491b4c019")
  })

  print("Success!")
}

test_4.4 <- function() {
  test_that('Did not assign answer to an object called "diff_in_props_btsp_ci"', {
    expect_true(exists("diff_in_props_btsp_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(diff_in_props_btsp_ci))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(diff_in_props_btsp_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(diff_in_props_btsp_ci))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
      expect_equal(digest(as.integer(diff_in_props_btsp_ci$lower_ci+diff_in_props_btsp_ci$upper_ci * 10e6)), "7f583fda9b120b1dadc48c5a1d04c408")
  })

  print("Success!")
}
