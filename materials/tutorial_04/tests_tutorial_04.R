library(digest)
library(testthat)

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "steam_pop"', {
    expect_true(exists("steam_pop"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(steam_pop))
  })

  expected_colnames <- c("original_price")
  given_colnames <- colnames(steam_pop)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(steam_pop))), "bc015e4c2e1698184f3f79d5432f54f1")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(steam_pop$original_price) * 1000)), "4d38676bfa8adb0e95087935662737c9")
  })

  print("Success!")
}

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "steam_sample"', {
    expect_true(exists("steam_sample"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(steam_sample))
  })

  expected_colnames <- c("original_price")
  given_colnames <- colnames(steam_sample)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(steam_sample))), "16071ab8270571c6c83d682892e00ea5")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(steam_sample$original_price) * 1000)), "9e4221b66064892c1311cfb904d05af9")
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "steam_bootstrapped"', {
    expect_true(exists("steam_bootstrapped"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(steam_bootstrapped))
  })

  expected_colnames <- c("replicate", "bootstrap_median")
  given_colnames <- colnames(steam_bootstrapped)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(steam_bootstrapped))), "a6d2eaaf1485f7b5c2c656e014e1835c")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(steam_bootstrapped$bootstrap_median) * 1000)), "81f8e8b8151f4ce03110e473991992a8")
  })

  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "steam_ci"', {
    expect_true(exists("steam_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(steam_ci))
  })

  expected_colnames <- c("ci_lower", "ci_upper")
  given_colnames <- colnames(steam_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(steam_ci))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(steam_ci$ci_lower * 1000)), "8da5500b2599f3c65fdea9ebde1e892e")
    expect_equal(digest(as.integer(steam_ci$ci_upper * 1000)), "5078d99ad73dda092eca9e13110ea1ce")
  })

  print("Success!")
}

test_1.2_infer <- function() {
  test_that('Did not assign answer to an object called "steam_bootstrapped2"', {
    expect_true(exists("steam_bootstrapped2"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(steam_bootstrapped2))
  })

  expected_colnames <- c("replicate", "stat")
  given_colnames <- colnames(steam_bootstrapped2)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(steam_bootstrapped2))), "a6d2eaaf1485f7b5c2c656e014e1835c")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(steam_bootstrapped2$stat) * 1000)), "81f8e8b8151f4ce03110e473991992a8")
  })

  print("Success!")
}

test_1.3_infer <- function() {
  test_that('Did not assign answer to an object called "steam_ci2"', {
    expect_true(exists("steam_ci2"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(steam_ci2))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(steam_ci2)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(steam_ci2))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(steam_ci2$lower_ci * 1000)), "8da5500b2599f3c65fdea9ebde1e892e")
    expect_equal(digest(as.integer(steam_ci2$upper_ci * 1000)), "5078d99ad73dda092eca9e13110ea1ce")
  })

  print("Success!")
}

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "steam_ci_plot"', {
    expect_true(exists("steam_ci_plot"))
  })
  properties <- c(steam_ci_plot$layers[[1]]$mapping, steam_ci_plot$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(steam_ci_plot$layers[[1]]$geom))
    expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    expect_true("GeomVline" %in% class(steam_ci_plot$layers[[3]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", steam_ci_plot$layers[[1]])[["stat_params"]][["binwidth"]] * 1000)),
    "189e2f1b2fbb3743811990e9708c226a"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(steam_ci_plot$data)), "a6d2eaaf1485f7b5c2c656e014e1835c")
    expect_equal(digest(as.integer(sum(steam_ci_plot$data$stat) * 1000)), "81f8e8b8151f4ce03110e473991992a8")
  })

  test_that("geom_vline layers are not in the correct locations", {
    expect_equal(digest(as.integer(steam_ci_plot$layers[[2]]$data * 1000)), "8da5500b2599f3c65fdea9ebde1e892e")
    expect_equal(digest(as.integer(steam_ci_plot$layers[[3]]$data * 1000)), "5078d99ad73dda092eca9e13110ea1ce")
  })

  print("Success!")
}

test_1.6 <- function() {
  test_that('Did not assign answer to an object called "steam_median"', {
    expect_true(exists("steam_median"))
  })

  answer_as_numeric <- as.numeric(steam_median)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1000)), "8da5500b2599f3c65fdea9ebde1e892e")
  })

  print("Success!")
}

test_1.7 <- function() {
  test_that('Did not assign answer to an object called "answer1.7"', {
    expect_true(exists("answer1.7"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.7, "true|false", ignore.case = TRUE)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.7)), "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "cancer_ci"', {
    expect_true(exists("cancer_ci"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(cancer_ci))
  })

  expected_colnames <- c("lower_ci", "upper_ci")
  given_colnames <- colnames(cancer_ci)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(cancer_ci))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(cancer_ci$lower_ci * 1000000)), "81d11d2d9c5edc8dde5943e7e8ccf428")
    expect_equal(digest(as.integer(cancer_ci$upper_ci * 1000000)), "f79b45b4d0457d5c267b08e74659f678")
  })

  print("Success!")
}
