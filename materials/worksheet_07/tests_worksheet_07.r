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

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", or "F")', {
    expect_match(answer1.1, "a|b|c|d|e|f", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.1))
  if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8") {
    print("In hypothesis tests there are only two hypotheses and they must cover all the possibilities.")
  } else if (answer_hash == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("The null hypothesis is either true or not. The probability associated is either 0 or 1, no matter the decision you make.")
  } else if (answer_hash == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("To reject the null hypothesis all we need is that the observed test statistic is too unlikely to happen under the null model. 
However, this doesn't say anything about the likelihood of the observed test statistic if the alternative hypothesis were true.")
  } 

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "93a9078c6326f37b481d3e99b60ad987")
  })

  print("Success!")
}

# %%
# Question 1.2

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })
    
  answer_hash <- digest(tolower(paste(sort(unlist(strsplit(answer1.2, ""))), collapse = "")))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "1e16b9a452ef9cc6197e732db3ec6c59")
  })

  print("Success!")
}

# %%
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
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# %%
# Question 2.1.1

test_2.1.1 <- function() {
  test_that('Did not assign answer to an object called "law_large_numbers"', {
    expect_true(exists("law_large_numbers"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(law_large_numbers))
  })

  properties <- c(law_large_numbers$layers[[1]]$mapping, law_large_numbers$mapping)

  test_that("Plot should have sample_size on the x-axis", {
    expect_true("sample_size" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomLine" %in% class(law_large_numbers$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(law_large_numbers$data)), "bed06daa618c0df551f67a983db5fa24")
    expect_equal(digest(round(sum(law_large_numbers$data$sample_size))), "2088de1c8a3d86fc7f34def7d3e1f726")

    # If sample_size is not known:
    # expect_equal(digest(round(sum(pull(law_large_numbers$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(law_large_numbers$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(law_large_numbers$labels))
  })

  print("Success!")
}

# %%
# Question 2.1.2

test_2.1.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.2"', {
    expect_true(exists("answer2.1.2"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.1.2, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.1.2))
 
    test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# %%
# Question 2.2.1

test_2.2.1 <- function() {
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
# Question 2.2.2

test_2.2.2 <- function() {
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
# Question 2.2.3
test_2.2.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.3"', {
    expect_true(exists("answer2.2.3"))
  })

  answer_hash <- digest(tolower(paste(sort(unlist(strsplit(answer2.2.3, ""))), collapse = "")))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "95767987b2037a2f09c4e5c0997ec206")
  })

  print("Success!")
}

# %%
# Question 2.2.4
test_2.2.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.4_partA"', {
    expect_true(exists("answer2.2.4_partA"))
  })
  test_that('Did not assign answer to an object called "answer2.2.4_partB"', {
    expect_true(exists("answer2.2.4_partB"))
  })
  test_that('Did not assign answer to an object called "answer2.2.4_partC"', {
    expect_true(exists("answer2.2.4_partC"))
  })

  answer_partA_as_numeric <- as.numeric(answer2.2.4_partA)
  answer_partB_as_numeric <- as.numeric(answer2.2.4_partB)
  answer_partC_as_numeric <- as.numeric(answer2.2.4_partC)
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
# Question 2.2.5

test_2.2.5 <- function() {
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
# Question 2.2.6

test_2.2.6 <- function() {
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
# Question 2.3.1

test_2.3.1 <- function() {
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
# Question 2.3.2
test_2.3.2 <- function() {
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
# Question 2.3.3

test_2.3.3 <- function() {
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
# Question 2.3.4
test_2.3.4 <- function() {
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
# Question 2.3.5

test_2.3.5 <- function() {
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

# %%
# Question 3.1

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "answer3.1_mean"', {
    expect_true(exists("answer3.1_mean"))
  })
  test_that('Did not assign answer to an object called "answer3.1_std_dev"', {
    expect_true(exists("answer3.1_std_dev"))
  })

  answer_mean_as_numeric <- as.numeric(answer3.1_mean)
  answer_sd_as_numeric <- as.numeric(answer3.1_std_dev)
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_mean_as_numeric * 100)), "4992f3d85c6e0f9027e558d795183aff")
    expect_equal(digest(as.integer(answer_sd_as_numeric * 100)), "023b324ed55d4ab7e58de0161e1f37eb")
  })

  print("Success!")
}

# %%
# Question 3.2
test_3.2 <- function() {
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
# Question 3.3

test_3.3 <- function() {
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


# %%
# Question 3.4
test_3.4 <- function() {
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
    expect_equal(digest(as.integer(sum(mean_body_mass_chinstrap_ci$lower_ci) * 1000)), "37a95756491d9550efe495513bd7d08c")
    expect_equal(digest(as.integer(sum(mean_body_mass_chinstrap_ci$upper_ci) * 1000)), "de6b383c7848f67dbc58ccdb7da4a40f")
  })

  print("Success!")
}

# %%
# Question 3.5

test_3.5 <- function() {
  test_that('Did not assign answer to an object called "answer3.5_mean"', {
    expect_true(exists("answer3.5_mean"))
  })
  test_that('Did not assign answer to an object called "answer3.5_std_dev"', {
    expect_true(exists("answer3.5_std_dev"))
  })

  answer_mean_as_numeric <- as.numeric(answer3.5_mean)
  answer_sd_as_numeric <- as.numeric(answer3.5_std_dev)
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_mean_as_numeric * 100)), "1b0ed73227e2e7826da63b2b356975e0")
    expect_equal(digest(as.integer(answer_sd_as_numeric * 100)), "11946e7a3ed5e1776e81c0f0ecd383d0")
  })

  print("Success!")
}

# %%
# Question 3.6
test_3.6 <- function() {
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
# Question 3.7
test_3.7 <- function() {
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
