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

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "answer1.3"', {
    expect_true(exists("answer1.3"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.3, "true|false", ignore.case = TRUE)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.3)), "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_2.0 <- function() {
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

test_2.1 <- function() {
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

test_2.2 <- function() {
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
      "49348d765354bf43c41c3050b5ea9380"
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

test_2.3 <- function() {
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
      "f5b46d85d364dcd91922018720a2c475"
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

test_2.4 <- function() {
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
      "17c71ae5b918c32891d1b43864c23025"
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

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "answer3.5"', {
    expect_true(exists("answer2.5"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.5, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.5))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8" | answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Compare the widths of the distributions more closely")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Compare the shape and symmetry of the distributions more closely")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.5)), "d110f00cfb1b248e835137025804a23b")
  })
  
  print("Success!")
}

test_2.6 <- function() {
  test_that('Did not assign answer to an object called "answer2.6"', {
    expect_true(exists("answer2.6"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.6, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.6))
  if (answer == "05ca18b596514af73f6880309a21b5dd") {
    print("As sample size DECREASES, how does the width of the sampling distribution change?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.6)), "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success!")
}

# %%
# Question 3.1.1

test_3.1.1 <- function() {
  test_that('Did not assign answer to an object called "law_large_numbers"', {
    expect_true(exists("law_large_numbers"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(law_large_numbers_plot))
  })

  properties <- c(law_large_numbers_plot$layers[[1]]$mapping, law_large_numbers_plot$mapping)

  test_that("Plot should have sample_size on the x-axis", {
    expect_true("sample_size" == rlang::get_expr(properties$x))
  })
    
  test_that("Plot should have mean on the y-axis", {
    expect_true("mean" == rlang::get_expr(properties$y))
  })  

  test_that("Plot does not have the correct layers", {
    expect_true("GeomLine" %in% class(law_large_numbers_plot$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(law_large_numbers_plot$data)), "4384ceb41972194d4861d408150c8252")
    expect_equal(digest(round(sum(law_large_numbers_plot$data$sample_size))), "7551b2ca77e9f6f2ec9e4c9d7c69de90")

    # If sample_size is not known:
    # expect_equal(digest(round(sum(pull(law_large_numbers$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(law_large_numbers_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(law_large_numbers_plot$labels))
  })

  print("Success!")
}

# %%
# Question 3.1.2

test_3.1.2 <- function() {
  test_that('Did not assign answer to an object called "answer3.1.2"', {
    expect_true(exists("answer3.1.2"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer3.1.2, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.1.2))
 
    test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# %%
# Question 3.2.1

test_3.2.1 <- function() {
  test_that('Did not assign answer to an object called "pop_dist_flies"', {
    expect_true(exists("pop_dist_flies"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(pop_dist_flies))
  })

  properties <- c(pop_dist_flies$layers[[1]]$mapping, pop_dist_flies$mapping)

  test_that("Plot should have wings_length on the x-axis", {
    expect_true("wings_length" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(pop_dist_flies$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", pop_dist_flies$layers[[1]])[["stat_params"]][["binwidth"]]*1000)),
    "3a8dae6905fd3d815ca0c3b8a20c107f"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(pop_dist_flies$data)), "5d6e7fe43b3b73e5fd2961d5162486fa")
    expect_equal(digest(round(sum(pop_dist_flies$data$wings_length))), "4c61eb168db19438af5758c2c0a4cdeb")

    # If wings_length is not known:
    # expect_equal(digest(round(sum(pull(pop_dist_flies$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(pop_dist_flies$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(pop_dist_flies$labels))
  })

  print("Success!")
}

# %%
# Question 3.2.2

test_3.2.2 <- function() {
  test_that('Did not assign answer to an object called "pop_dist_normal"', {
    expect_true(exists("pop_dist_normal"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(pop_dist_normal))
  })

  properties <- c(pop_dist_normal$layers[[1]]$mapping, pop_dist_normal$mapping)

  test_that("Plot should have wings_length on the x-axis", {
    expect_true("wings_length" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(pop_dist_normal$layers[[1]]$geom))

    # Remove if not needed:
    expect_true("GeomLine" %in% class(pop_dist_normal$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", pop_dist_flies$layers[[1]])[["stat_params"]][["binwidth"]]*1000)),
    "3a8dae6905fd3d815ca0c3b8a20c107f"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(pop_dist_normal$data)), "5d6e7fe43b3b73e5fd2961d5162486fa")
    expect_equal(digest(round(sum(pop_dist_normal$data$wings_length))), "4c61eb168db19438af5758c2c0a4cdeb")

    # If wings_length is not known:
    # expect_equal(digest(round(sum(pull(pop_dist_normal$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(pop_dist_normal$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(pop_dist_normal$labels))
  })

  print("Success!")
}

# %%
# Question 3.2.3
test_3.2.3 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.3"', {
    expect_true(exists("answer3.2.3"))
  })

  answer_hash <- digest(tolower(paste(sort(unlist(strsplit(answer3.2.3, ""))), collapse = "")))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "95767987b2037a2f09c4e5c0997ec206")
  })

  print("Success!")
}

# %%
# Question 3.2.4
test_3.2.4 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.4_partA"', {
    expect_true(exists("answer3.2.4_partA"))
  })
  test_that('Did not assign answer to an object called "answer3.2.4_partB"', {
    expect_true(exists("answer3.2.4_partB"))
  })
  test_that('Did not assign answer to an object called "answer3.2.4_partC"', {
    expect_true(exists("answer3.2.4_partC"))
  })

  answer_partA_as_numeric <- as.numeric(answer3.2.4_partA)
  answer_partB_as_numeric <- as.numeric(answer3.2.4_partB)
  answer_partC_as_numeric <- as.numeric(answer3.2.4_partC)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_partA_as_numeric))
    expect_false(is.na(answer_partB_as_numeric))
    expect_false(is.na(answer_partC_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_partA_as_numeric * 100)), "1b387a44b518030b040d524a5fa7e672")
    expect_equal(digest(as.integer(answer_partB_as_numeric * 100)), "c573cfd6ff802932578f5a94dbb38a79")
    expect_equal(digest(as.integer(answer_partC_as_numeric * 100)), "5d6e7fe43b3b73e5fd2961d5162486fa")
  })

  print("Success!")
}

# %%
# Question 3.2.5

test_3.2.5 <- function() {
  test_that('Did not assign answer to an object called "samples_houseflies"', {
    expect_true(exists("samples_houseflies"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(samples_houseflies))
  })

  expected_colnames <- c("replicate", "wings_length")
  given_colnames <- colnames(samples_houseflies)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(samples_houseflies))), "53153aa8f66b0f33c556559781ad9bdb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(samples_houseflies$wings_length) * 10)), "83f712ff5e2b3b020f2c03a0d11281d3")
  })

  print("Success!")
}

# %%
# Question 3.2.6

test_3.2.6 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_mean_houseflies"', {
    expect_true(exists("sampling_dist_mean_houseflies"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(sampling_dist_mean_houseflies))
  })

  properties <- c(sampling_dist_mean_houseflies$layers[[1]]$mapping, sampling_dist_mean_houseflies$mapping)

  test_that("Plot should have sample_mean on the x-axis", {
    expect_true("sample_mean" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(sampling_dist_mean_houseflies$layers[[1]]$geom))
    expect_true("GeomLine" %in% class(sampling_dist_mean_houseflies$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", sampling_dist_mean_houseflies$layers[[1]])[["stat_params"]][["binwidth"]])),
    "1473d70e5646a26de3c52aa1abd85b1f"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_mean_houseflies$data)), "f1a30baa3072c1aad822c059f35c6841")
    expect_equal(digest(round(sum(sampling_dist_mean_houseflies$data$sample_mean))), "deee98510e8ed0ba91b31f0c62e54ea7")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_mean_houseflies$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_mean_houseflies$labels))
  })

  print("Success!")
}

# %%
# Question 3.3.1

test_3.3.1 <- function() {
  test_that('Did not assign answer to an object called "samples_size10"', {
    expect_true(exists("samples_size10"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(samples_size10))
  })

  expected_colnames <- c("replicate", "value")
  given_colnames <- colnames(samples_size10)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(samples_size10))), "49348d765354bf43c41c3050b5ea9380")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(samples_size10$value)*1000)), "f31d5cc2965c8a6b867393269bfaf433")
  })

  print("Success!")
}

# %%
# Question 3.3.2
test_3.3.2 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_size10"', {
    expect_true(exists("sampling_dist_size10"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(sampling_dist_size10))
  })

  properties <- c(sampling_dist_size10$layers[[1]]$mapping, sampling_dist_size10$mapping)

  test_that("Plot should have sample_mean on the x-axis", {
    expect_true("sample_mean" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(sampling_dist_size10$layers[[1]]$geom))
    expect_true("GeomLine" %in% class(sampling_dist_size10$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_size10$data)), "b5458e3b37771063dd9f7160eb1e653f")
    expect_equal(digest(round(sum(sampling_dist_size10$data$sample_mean))), "9db34424a2dd2cf59be53da961139363")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_size10$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_size10$labels))
  })

  print("Success!")
}

# %%
# Question 3.3.3

test_3.3.3 <- function() {
  test_that('Did not assign answer to an object called "samples_size500"', {
    expect_true(exists("samples_size500"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(samples_size500))
  })

  expected_colnames <- c("replicate", "value")
  given_colnames <- colnames(samples_size500)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(samples_size500))), "d8859a5de726179fa6d31d73747e6e6e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(samples_size500$value)*100)), "ff0602b28d8d217fe24da35e79cfe5fd")
  })

  print("Success!")
}

# %%
# Question 3.3.4
test_3.3.4 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_size500"', {
    expect_true(exists("sampling_dist_size500"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(sampling_dist_size500))
  })

  properties <- c(sampling_dist_size500$layers[[1]]$mapping, sampling_dist_size500$mapping)

  test_that("Plot should have sample_mean on the x-axis", {
    expect_true("sample_mean" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(sampling_dist_size500$layers[[1]]$geom))
    expect_true("GeomLine" %in% class(sampling_dist_size500$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_size500$data)), "b5458e3b37771063dd9f7160eb1e653f")
    expect_equal(digest(round(sum(sampling_dist_size500$data$sample_mean))), "9f3d9d585cef923972bc6bccdb6ad44e")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_size500$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_size500$labels))
  })

  print("Success!")
}

# %%
# Question 3.3.5

test_3.3.5 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_min"', {
    expect_true(exists("sampling_dist_min"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(sampling_dist_min))
  })

  properties <- c(sampling_dist_min$layers[[1]]$mapping, sampling_dist_min$mapping)

  test_that("Plot should have sample_min on the x-axis", {
    expect_true("sample_min" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(sampling_dist_min$layers[[1]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_min$data)), "317d1cec3b1ee7a9ee5b84a636bcebd9")
    expect_equal(digest(round(sum(sampling_dist_min$data$sample_min))), "5c67ad186fa10d224429f3b4520697be")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_min$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_min$labels))
  })

  print("Success!")
}
