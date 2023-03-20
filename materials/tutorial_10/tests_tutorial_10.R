library(digest)
library(testthat)


## Question 4.1
test_4.1 <- function() {
  test_that('Did not assign answer to an object called "true_pop_hist"', {
    expect_true(exists("true_pop_hist"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(true_pop_hist))
  })

  properties <- c(true_pop_hist$layers[[1]]$mapping, true_pop_hist$mapping)

  test_that("Plot should have albumin on the x-axis", {
    expect_true("albumin" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(true_pop_hist$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(true_pop_hist$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(true_pop_hist$data)), "595ab0ff7ab1832abc986c3e876c9267")
    expect_equal(digest(round(sum(true_pop_hist$data$albumin))), "d58a4101e05e6de3e9945c1c3300f5da")

    # If albumin is not known:
    # expect_equal(digest(round(sum(pull(true_pop_hist$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })


  print("Success!")
}

# +
## Question 4.2

test_4.2 <- function() {
  test_that('Did not assign answer to an object called "sample"', {
    expect_true(exists("sample"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample))
  })

  if ('replicate' %in% colnames(sample)){
      sample <-
          sample %>% 
          ungroup() %>%
          select(-replicate)
  }
    
  expected_colnames <- c("albumin")
  given_colnames <- colnames(sample)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sample))), "7d2842cab7725fd8f382293e410d42b2")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(sample$albumin) * 100)), "dca3b1b1194565eb3a4c722989d9797d")
  })

  print("Success!")
}

# +
## Question 4.3

test_4.3A <- function() {
  test_that('Did not assign answer to an object called "boot_sampling_dist"', {
    expect_true(exists("boot_sampling_dist"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(boot_sampling_dist))
  })

  expected_colnames <- c("replicate", "stat", "samp_dist")
  given_colnames <- colnames(boot_sampling_dist)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(boot_sampling_dist))), "f1a30baa3072c1aad822c059f35c6841")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(boot_sampling_dist$stat) * 1000)), "e4b4d3fc067f99779fd1c3b27e21471f")
  })

  print("Success!")
}

test_4.3B <- function() {
  test_that('Did not assign answer to an object called "boot_sampl_dist_hist"', {
    expect_true(exists("boot_sampl_dist_hist"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(boot_sampl_dist_hist))
  })

  properties <- c(boot_sampl_dist_hist$layers[[1]]$mapping, boot_sampl_dist_hist$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(boot_sampl_dist_hist$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(boot_sampl_dist_hist$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(boot_sampl_dist_hist$data)), "f1a30baa3072c1aad822c059f35c6841")
    expect_equal(digest(round(sum(boot_sampl_dist_hist$data$stat))), "40e4813ebbc0699bf8be1d6d2eaf7ff0")

    # If stat is not known:
    # expect_equal(digest(round(sum(pull(boot_sampl_dist_hist$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  print("Success!")
}

# +
## Question 4.4

test_4.4A <- function() {
  test_that('Did not assign answer to an object called "obs_test_stat_4.4"', {
    expect_true(exists("obs_test_stat_4.4"))
  })

  answer_as_numeric <- as.numeric(obs_test_stat_4.4)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1000)), "01e5727061a13ece7167ef49308a95ad")
  })

  print("Success!")
}


test_4.4B <- function() {
  test_that('Did not assign answer to an object called "null_model_4.4"', {
    expect_true(exists("null_model_4.4"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(null_model_4.4))
  })

  expected_colnames <- c("replicate", "stat", "samp_dist")
  given_colnames <- colnames(null_model_4.4)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(null_model_4.4))), "f1a30baa3072c1aad822c059f35c6841")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(null_model_4.4$stat) * 1000)), "65afbd089d7bad0bd0b248a663117b67")
  })

  print("Success!")
}
# -

## Question 4.5
test_4.5 <- function() {
  test_that('Did not assign answer to an object called "answer4.5"', {
    expect_true(exists("answer4.5"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer4.5, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer4.5))
  if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Since we already know that H0 is false, the error we could commit is not rejecting H0.")
  }
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# +
## Question 4.6

## Question 4.6

test_4.6 <- function() {
  test_that('Did not assign answer to an object called "answer4.6"', {
    expect_true(exists("answer4.6"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer4.6, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer4.6))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}
