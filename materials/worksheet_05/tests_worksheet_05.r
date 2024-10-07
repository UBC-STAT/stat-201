# ---
# jupyter:
#   jupytext:
#     formats: r:hydrogen
#     text_representation:
#       extension: .r
#       format_name: hydrogen
#       format_version: '1.3'
#       jupytext_version: 1.5.2
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# %%
library(digest)
library(testthat)


# %%
# Question 1.0

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "answer1.0"', {
    expect_true(exists("answer1.0"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.0, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.0))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}


# %%
# Question 2.0

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "answer2.0_mean"', {
    expect_true(exists("answer2.0_mean"))
  })
  test_that('Did not assign answer to an object called "answer2.0_std_dev"', {
    expect_true(exists("answer2.0_std_error"))
  })

  answer_mean_as_numeric <- as.numeric(answer2.0_mean)
  answer_sd_as_numeric <- as.numeric(answer2.0_std_error)
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_mean_as_numeric * 100)), "1b0ed73227e2e7826da63b2b356975e0")
    expect_equal(digest(as.integer(answer_sd_as_numeric * 100)), "11946e7a3ed5e1776e81c0f0ecd383d0")
  })

  print("Success!")
}

# %%
# Question 2.1
test_2.1 <- function() {
  test_that('Did not assign answer to an object called "prop_adelie_ci"', {
    expect_true(exists("prop_adelie_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(prop_adelie_ci))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(prop_adelie_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(prop_adelie_ci))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(prop_adelie_ci$lower_ci) * 1000)), "ed195206b77fdf687961e3fa4d671e19")
    expect_equal(digest(as.integer(sum(prop_adelie_ci$upper_ci) * 1000)), "fdf6fa3c76dbe4f1284ce4f08e00924c")
  })

  print("Success!")
}



# %%
# Question 2.2

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.2_mean"', {
    expect_true(exists("answer2.2_mean"))
  })
  test_that('Did not assign answer to an object called "answer2.2_std_dev"', {
    expect_true(exists("answer2.2_std_error"))
  })

  answer_mean_as_numeric <- as.numeric(answer2.2_mean)
  answer_sd_as_numeric <- as.numeric(answer2.2_std_error)
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_mean_as_numeric * 100)), "4992f3d85c6e0f9027e558d795183aff")
    expect_equal(digest(as.integer(answer_sd_as_numeric * 100)), "023b324ed55d4ab7e58de0161e1f37eb")
  })

  print("Success!")
}

# %%
# Question 2.3
test_2.3 <- function() {
  test_that('Did not assign answer to an object called "mean_body_mass_adelie_ci"', {
    expect_true(exists("mean_body_mass_adelie_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(mean_body_mass_adelie_ci))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(mean_body_mass_adelie_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(mean_body_mass_adelie_ci))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(mean_body_mass_adelie_ci$lower_ci) * 1000)), "8fb46facd51895f35b8367ac48f80848")
    expect_equal(digest(as.integer(sum(mean_body_mass_adelie_ci$upper_ci) * 1000)), "19013d07f4732f9ca508ed790afe482f")
  })

  print("Success!")
}

# %%
# Question 2.4

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "bootstrap_ci"', {
    expect_true(exists("bootstrap_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(bootstrap_ci))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(bootstrap_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(bootstrap_ci))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(bootstrap_ci$lower_ci) * 100)), "c64af25a5a615949ced5213330e7c121")
    expect_equal(digest(as.integer(sum(bootstrap_ci$upper_ci) * 100)), "e574ea489ec73540e070f63012718c9c")
  })

  print("Success!")
}


# +
## Question 3.1

test_3.1 <- function() {
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
# Question 3.2

test_3.2 <- function() {
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
# Question 3.3

test_3.3 <- function() {
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
# Question 3.4

test_3.4 <- function() {
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
    expect_equal(digest(round(sum(sampling_dist_zscore_s$data$z))), "96fa2197e700c5281688bea6a5e2f0d6")

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


# %%
# Question 4.0
test_4.0 <- function() {
  test_that('Did not assign answer to an object called "penguins_diff_means_ci"', {
    expect_true(exists("penguins_diff_means_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(penguins_diff_means_ci))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(penguins_diff_means_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(penguins_diff_means_ci))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(penguins_diff_means_ci$lower_ci) * 1000)), "00e0e0ab7809e753ec531e9f5aa421ca")
    expect_equal(digest(as.integer(sum(penguins_diff_means_ci$upper_ci) * 1000)), "804dd8ac107afbe2de4293f25feec684")
  })

  print("Success!")
}



# %%
# Question 4.1
test_4.1 <- function() {
  test_that('Did not assign answer to an object called "penguins_diff_means_ci_t"', {
    expect_true(exists("penguins_diff_means_ci_t"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(penguins_diff_means_ci_t))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(penguins_diff_means_ci_t)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(penguins_diff_means_ci_t))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(penguins_diff_means_ci_t$lower_ci) * 1000)), "1d0ada32172e2fffca429a98a7ff6939")
    expect_equal(digest(as.integer(sum(penguins_diff_means_ci_t$upper_ci) * 1000)), "57dc22847f6b5a192e3c0874d3915c6e")
  })

  print("Success!")
}