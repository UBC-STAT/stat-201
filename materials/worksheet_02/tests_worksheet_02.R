library(digest)
library(testthat)

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "answer1.0.0"', {
    expect_true(exists("answer1.0.0"))
  })
  test_that('Did not assign answer to an object called "answer1.0.1"', {
    expect_true(exists("answer1.0.1"))
  })
  test_that('Did not assign answer to an object called "answer1.0.2"', {
    expect_true(exists("answer1.0.2"))
  })
  test_that('Did not assign answer to an object called "answer1.0.3"', {
    expect_true(exists("answer1.0.3"))
  })

  test_that('Solutions should be a single character ("A", "B", or "C")', {
    expect_match(answer1.0.0, "a|b|c", ignore.case = TRUE)
    expect_match(answer1.0.1, "a|b|c", ignore.case = TRUE)
    expect_match(answer1.0.2, "a|b|c", ignore.case = TRUE)
    expect_match(answer1.0.3, "a|b|c", ignore.case = TRUE)
  })

  test_that("One or more solutions are incorrect", {
    expect_equal(digest(tolower(answer1.0.0)), "127a2ec00989b9f7faf671ed470be7f8")
    expect_equal(digest(tolower(answer1.0.1)), "ddf100612805359cd81fdc5ce3b9fbba")
    expect_equal(digest(tolower(answer1.0.2)), "ddf100612805359cd81fdc5ce3b9fbba")
    expect_equal(digest(tolower(answer1.0.3)), "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.1, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.1))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("What is required to generate a bootstrap distribution?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.2, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.2))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("What do we need to compute a population parameter? Is it realistic?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "answer2.0"', {
    expect_true(exists("answer2.0"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", or "F")', {
    expect_match(answer2.0, "a|b|c|d|e|f", ignore.case = TRUE)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.0)), "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.1"', {
    expect_true(exists("answer2.1"))
  })

  test_that('Solution should be a single character ("A", "B", or "C")', {
    expect_match(answer2.1, "a|b|c", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.1))
  if (answer_hash == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Is there any characteristic of the sample that guarantees it was taken without replacement?")
  } else if (answer_hash == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Is there any scenario where you wouldn't know if the sample was taken with or without replacement?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.2"', {
    expect_true(exists("answer2.2"))
  })

  test_that('Solution should be a single character ("A", "B", or "C")', {
    expect_match(answer2.2, "a|b|c", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.2))
  if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Is there any characteristic of the sample that guarantees it was taken with replacement?")
  } else if (answer_hash == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Is there any characteristic of the sample that guarantees it was taken without replacement?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.3"', {
    expect_true(exists("answer2.3"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.3, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.3))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("Read carefully!")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

test_3.0 <- function() {
  test_that('Did not assign answer to an object called "bootstrap_sample"', {
    expect_true(exists("bootstrap_sample"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(bootstrap_sample))
  })

  expected_colnames <- c("replicate", "full_years")
  given_colnames <- colnames(bootstrap_sample)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(bootstrap_sample))), "20c9a920779e3feca5b4ed6948450f8a")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(bootstrap_sample$full_years))), "dc1d757c56fd15656b23102dc5f6727c")
  })
  print("Success!")
}

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "ubc_sample_mean"', {
    expect_true(exists("ubc_sample_mean"))
  })

  answer_as_numeric <- as.numeric(ubc_sample_mean)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(round(ubc_sample_mean,3)), "baf0148fd27fa92338a8c33829af5128")
  })

  print("Success!")
}

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "bootstrap_sample_mean"', {
    expect_true(exists("bootstrap_sample_mean"))
  })

  answer_as_numeric <- as.numeric(bootstrap_sample_mean)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric*1000000000)), "328b8c66e7f4b6511242246eab9bd08b")
  })

  print("Success!")
}

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "answer3.3"', {
    expect_true(exists("answer3.3"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.3, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.3))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Would the mean change if we sampled without replacement?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_3.4 <- function() {
  test_that('Did not assign answer to an object called "bootstrap_sample_dist"', {
    expect_true(exists("bootstrap_sample_dist"))
  })
  properties <- c(bootstrap_sample_dist$layers[[1]]$mapping, bootstrap_sample_dist$mapping)

  test_that("Plot should have full_years on the x-axis", {
    expect_true("full_years" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(bootstrap_sample_dist$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", bootstrap_sample_dist$layers[[1]])[["stat_params"]][["binwidth"]])),
    "4b5630ee914e848e8d07221556b0a2fb"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(bootstrap_sample_dist$data)), "20c9a920779e3feca5b4ed6948450f8a")
    expect_equal(digest(as.integer(sum(bootstrap_sample_dist$data$full_years))), "dc1d757c56fd15656b23102dc5f6727c")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(bootstrap_sample_dist$labels$x == "full_years")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(bootstrap_sample_dist$labels))
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
    print("Think through the process of sampling with replacement carefully")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

test_4.0 <- function() {
  test_that('Did not assign answer to an object called "sample_10"', {
    expect_true(exists("sample_10"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_10))
  })

  expected_colnames <- c("current_land_value")
  given_colnames <- colnames(sample_10)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sample_10))), "71db8a6cad03244e6e50f0ad8bc95a65")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(sample_10$current_land_value))), "3f92c7fab3c6a839e5ab5aacda979f3a")
  })

    
  print("Success!")
}

test_4.1 <- function() {
  test_that('Did not assign answer to an object called "resampled_means_10"', {
    expect_true(exists("resampled_means_10"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(resampled_means_10))
  })

  expected_colnames <- c("replicate", "mean_land_value")
  given_colnames <- colnames(resampled_means_10)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(resampled_means_10))), "6e96c307060fba1b1d3a36d2410fd595")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(resampled_means_10$mean_land_value))), "1de19f20fb7008fb8a562077cbcc2cf0")
  })

  print("Success!")
}

test_4.2 <- function() {
  test_that('Did not assign answer to an object called "bootstrap_dist_10"', {
    expect_true(exists("bootstrap_dist_10"))
  })
  properties <- c(bootstrap_dist_10$layers[[1]]$mapping, bootstrap_dist_10$mapping)

  test_that("Plot should have mean_land_value on the x-axis", {
    expect_true("mean_land_value" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(bootstrap_dist_10$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", bootstrap_dist_10$layers[[1]])[["stat_params"]][["binwidth"]])),
    "829aba66b0d64feac09b067c4cce133c"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(bootstrap_dist_10$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(as.integer(bootstrap_dist_10$data$mean_land_value))), "60b3274d21a3e83b615c7731adfd0d79")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(bootstrap_dist_10$labels$x == "mean_land_value")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(bootstrap_dist_10$labels))
  })

  print("Success!")
}

test_4.3 <- function() {
  test_that('Did not assign answer to an object called "answer4.3"', {
    expect_true(exists("answer4.3"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer4.3, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer4.3))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("Note: the distribution above is a BOOTSTRAP distribution. Think about what you sample to create a bootstrap distribution.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

test_4.4 <- function() {
  test_that('Did not assign answer to an object called "sample_30"', {
    expect_true(exists("sample_30"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_30))
  })

  expected_colnames <- c("current_land_value")
  given_colnames <- colnames(sample_30)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sample_30))), "7d2842cab7725fd8f382293e410d42b2")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(sample_30$current_land_value))), "20a5caed5f997ae762307c6a6e98c276")
  })
  print("Success!")
}

test_4.5 <- function() {
  test_that('Did not assign answer to an object called "bootstrap_dist_30"', {
    expect_true(exists("bootstrap_dist_30"))
  })
  properties <- c(bootstrap_dist_30$layers[[1]]$mapping, bootstrap_dist_30$mapping)

  test_that("Plot should have mean_land_value on the x-axis", {
    expect_true("mean_land_value" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(bootstrap_dist_30$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", bootstrap_dist_30$layers[[1]])[["stat_params"]][["binwidth"]])),
    "829aba66b0d64feac09b067c4cce133c"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(bootstrap_dist_30$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(as.integer(sum(bootstrap_dist_30$data$mean_land_value))), "a6d96db9e40e16a8b310b518ba018d08")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(bootstrap_dist_30$labels$x == "mean_land_value")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(bootstrap_dist_30$labels))
  })

  print("Success!")
}

test_4.6 <- function() {
  test_that('Did not assign answer to an object called "sample_100"', {
    expect_true(exists("sample_100"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_100))
  })

  expected_colnames <- c("current_land_value")
  given_colnames <- colnames(sample_100)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sample_100))), "5d6e7fe43b3b73e5fd2961d5162486fa")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(sample_100$current_land_value))), "225421bda4b1e262315936431acd58aa")
  })

  print("Success!")
}

test_4.7 <- function() {
  test_that('Did not assign answer to an object called "bootstrap_dist_100"', {
    expect_true(exists("bootstrap_dist_100"))
  })
  properties <- c(bootstrap_dist_100$layers[[1]]$mapping, bootstrap_dist_100$mapping)

  test_that("Plot should have mean_land_value on the x-axis", {
    expect_true("mean_land_value" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(bootstrap_dist_100$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", bootstrap_dist_100$layers[[1]])[["stat_params"]][["binwidth"]])),
    "829aba66b0d64feac09b067c4cce133c"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(bootstrap_dist_100$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(as.integer(sum(bootstrap_dist_100$data$mean_land_value))), "518a8fc51da6a689fee9e954795fb1d2")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(bootstrap_dist_100$labels$x == "mean_land_value")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(bootstrap_dist_100$labels))
  })

  print("Success!")
}

test_4.8 <- function() {
  test_that('Did not assign answer to an object called "answer4.8"', {
    expect_true(exists("answer4.8"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", or "E")', {
    expect_match(answer4.8, "a|b|c|d|e", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer4.8))
  if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Compare the red vertical lines for each bootstrap distribution and its corresponding sampling distribution")
  } else if (answer_hash == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Are the widths truly identical?")
  } else if (answer_hash == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("In other words, do the plots become more narrow as the sample size increases?")
  } else if (answer_hash == "d110f00cfb1b248e835137025804a23b") {
    print("The sampling & bootstrap distributions for larger sample sizes may not look EXACTLY symmetrical and bell-shaped, but consider the OVERALL trend as sample size increases")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "93a9078c6326f37b481d3e99b60ad987")
  })

  print("Success!")
}

test_4.9 <- function() {
  test_that('Did not assign answer to an object called "answer4.9"', {
    expect_true(exists("answer4.9"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer4.9, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer4.9))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Do the bootstrap distributions above look at least SOMEWHAT similar to their corresponding sampling distributions?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}
