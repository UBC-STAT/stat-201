library(testthat)
library(digest)

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "answer1.0"', {
    expect_true(exists("answer1.0"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.0, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.0))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Can you predict the exact value of the point estimate?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })

  answer_as_numeric <- as.numeric(answer1.1)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 100)), "20c9a920779e3feca5b4ed6948450f8a")
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", or "E")', {
    expect_match(answer1.2, "a|b|c|d|e", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.2))
  if (answer_hash != "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Is the variable quantitative or categorical?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "apt_ages"', {
    expect_true(exists("apt_ages"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(apt_ages))
  })

  expected_colnames <- c("age_yrs")
  given_colnames <- colnames(apt_ages)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(apt_ages))), "2dcefbce96b11c4b76e1916faeb43a0a")
  })
  
  sol <- apt_buildings %>% 
    mutate(age_yrs = year(Sys.Date()) - year_built) %>%    
    select(age_yrs) %>% 
    filter(!is.na(age_yrs))
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(as.integer(sum(apt_ages$age_yrs)), as.integer(sum(sol$age_yrs)))
  })
  
  print("Success!")
}


test_2.1 <- function() {
  test_that('Did not assign answer to an object called "apt_age_dist"', {
    expect_true(exists("apt_age_dist"))
  })
  properties <- c(apt_age_dist$layers[[1]]$mapping, apt_age_dist$mapping)

  test_that("Plot should have age_yrs on the x-axis", {
    expect_true("age_yrs" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(apt_age_dist$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", apt_age_dist$layers[[1]])[["stat_params"]][["binwidth"]])),
      "71db8a6cad03244e6e50f0ad8bc95a65"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(apt_age_dist$data)), "2dcefbce96b11c4b76e1916faeb43a0a")
  })


  test_that("x-axis label should be descriptive and human readable", {
    expect_false(apt_age_dist$labels$x == "age_yrs")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(apt_age_dist$labels))
  })

  print("Success!")
}

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "sample_vars_10x2000"', {
    expect_true(exists("sample_vars_10x2000"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_vars_10x2000))
  })

  expected_colnames <- c("replicate", "sample_var")
  given_colnames <- colnames(sample_vars_10x2000)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sample_vars_10x2000))), "6e96c307060fba1b1d3a36d2410fd595")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(round(sum(sample_vars_10x2000$sample_var) * 100000)), "e0db39a68cc808cdf08892629c1ec854")
  })
  print("Success!")
}

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_10x2000"', {
    expect_true(exists("sampling_dist_10x2000"))
  })
  properties <- c(sampling_dist_10x2000$layers[[1]]$mapping, sampling_dist_10x2000$mapping)

  test_that("Plot should have sample_var on the x-axis", {
    expect_true("sample_var" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_10x2000$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", sampling_dist_10x2000$layers[[1]])[["stat_params"]][["binwidth"]])),
      "e4ced46f028c228b02d106d874876fe8"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_10x2000$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(sum(sampling_dist_10x2000$data$sample_var) * 100000)), "e0db39a68cc808cdf08892629c1ec854")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_10x2000$labels$x == "sample_var")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_10x2000$labels))
  })

  print("Success!")
}

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_50x2000"', {
    expect_true(exists("sampling_dist_50x2000"))
  })
  properties <- c(sampling_dist_50x2000$layers[[1]]$mapping, sampling_dist_50x2000$mapping)

  test_that("Plot should have sample_var on the x-axis", {
    expect_true("sample_var" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_50x2000$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", sampling_dist_50x2000$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e98cd61c0f0313ba5b0b2fa1a37a0ec"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_50x2000$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(sum(sampling_dist_50x2000$data$sample_var) * 100000)), "f81216a3f2ab20d8ff93657ff135daf9")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_50x2000$labels$x == "sample_var")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_50x2000$labels))
  })

  print("Success!")
}

test_2.6 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_150x2000"', {
    expect_true(exists("sampling_dist_250x2000"))
  })
  properties <- c(sampling_dist_250x2000$layers[[1]]$mapping, sampling_dist_250x2000$mapping)

  test_that("Plot should have sample_var on the x-axis", {
    expect_true("sample_var" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_250x2000$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", sampling_dist_250x2000$layers[[1]])[["stat_params"]][["binwidth"]])),
    "be3c152f6f6bcd5f85f9e4cba49b1e48"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_250x2000$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(round(sum(sampling_dist_250x2000$data$sample_var) * 100000)), "1173be54dcb7f7ff6d4e4367d0bcb559")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_250x2000$labels$x == "sample_var")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_250x2000$labels))
  })

  print("Success!")
}

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "sample_vars_20x1000"', {
    expect_true(exists("sample_vars_20x1000"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_vars_20x1000))
  })

  expected_colnames <- c("replicate", "sample_var")
  given_colnames <- colnames(sample_vars_20x1000)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(sample_vars_20x1000))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(sample_vars_20x1000$sample_var))), "169bdec9cc8caaa1e50d00c36e66856c")
  })

  print("Success!")
}

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_20x1000"', {
    expect_true(exists("sampling_dist_20x1000"))
  })
  properties <- c(sampling_dist_20x1000$layers[[1]]$mapping, sampling_dist_20x1000$mapping)

  test_that("Plot should have sample_var on the x-axis", {
    expect_true("sample_var" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_20x1000$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", sampling_dist_20x1000$layers[[1]])[["stat_params"]][["binwidth"]])),
    "dd4ad37ee474732a009111e3456e7ed7"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_20x1000$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(sampling_dist_20x1000$data$sample_var))), "f89d4662943b2d3c3f6646e75fb3f024")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_20x1000$labels$x == "sample_var")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_20x1000$labels))
  })

  print("Success!")
}

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_20x5000"', {
    expect_true(exists("sampling_dist_20x5000"))
  })
  properties <- c(sampling_dist_20x5000$layers[[1]]$mapping, sampling_dist_20x5000$mapping)

  test_that("Plot should have sample_var on the x-axis", {
    expect_true("sample_var" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_20x5000$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", sampling_dist_20x5000$layers[[1]])[["stat_params"]][["binwidth"]])),
    "dd4ad37ee474732a009111e3456e7ed7"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_20x5000$data)), "189e2f1b2fbb3743811990e9708c226a")
    expect_equal(digest(round(sum(sampling_dist_20x5000$data$sample_var))), "068b325d753925b78b784101023658ba")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_20x5000$labels$x == "sample_var")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_20x5000$labels))
  })

  print("Success!")
}

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_20x20000"', {
    expect_true(exists("sampling_dist_20x20000"))
  })
  properties <- c(sampling_dist_20x20000$layers[[1]]$mapping, sampling_dist_20x20000$mapping)

  test_that("Plot should have sample_var on the x-axis", {
    expect_true("sample_var" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_20x20000$layers[[1]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", sampling_dist_20x20000$layers[[1]])[["stat_params"]][["binwidth"]])),
    "dd4ad37ee474732a009111e3456e7ed7"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_20x20000$data)), "7ea55401005f54e88bdc2ce0c9a9ceb1")
    expect_equal(digest(round(sum(sampling_dist_20x20000$data$sample_var))), "abbc5e27ee210fa0afa8552002aef0d9")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_20x20000$labels$x == "sample_var")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_20x20000$labels))
  })

  print("Success!")
}

test_3.5 <- function() {
  test_that('Did not assign answer to an object called "answer3.5"', {
    expect_true(exists("answer3.5"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", "F", "G", or "H")', {
    expect_match(answer3.5, "a|b|c|d|e|f|g|h", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.5))
  if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8" | answer_hash == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Read the other options carefully")
  } else if (answer_hash == "ddf100612805359cd81fdc5ce3b9fbba" | answer_hash == "93a9078c6326f37b481d3e99b60ad987" | answer_hash == "854b0aa33e9f6e2745dd724877ac9f9b") {
    print("Think carefully about the process that we use to generate a sampling distribution")
  } else if (answer_hash == "d110f00cfb1b248e835137025804a23b") {
    print("Why might we ask you to NOT attempt to take this many samples inside this notebook? Also, think carefully about the process that we use to generate a sampling distribution")
  } else if (answer_hash == "4ec8e2ba97f68c60f1c25f2718273a23") {
    print("At least one of the options listed above is correct.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "7279bb4184f9c53d42729c6eb22db36a")
  })

  print("Success!")
}

test_3.6 <- function() {
  test_that('Did not assign answer to an object called "answer3.6"', {
    expect_true(exists("answer3.6"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.6, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.6))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Try to squint your eyes and then look at the overall shape of the distribution!")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_4.2 <- function() {


test_that('Answer should be "true" or "false"', {
  expect_match(answer4.2, "true|false", ignore.case = TRUE)
})

test_that("Solution is incorrect", {
  expect_equal(digest(tolower(answer4.2)), "d2a90307aac5ae8d0ef58e2fe730d38b")
})

print("Success!")
}


test_5.0 <- function() {
test_that('Did not assign answer to an object called "sample_mean"', {
  expect_true(exists("sample_mean"))
})

  answer_as_numeric <- as.numeric(sample_mean)
test_that("Solution should be a number", {
  expect_false(is.na(answer_as_numeric))
})

test_that("Solution is incorrect", {
  expect_equal(digest(as.integer(answer_as_numeric * 100000)), "7f8201dd33e50ceb4cd9315c9055c64f")
})

  print("Success!")
}

test_5.0 <- function() {
  test_that('Did not assign answer to an object called "answer5.0"', {
    expect_true(exists("answer5.0"))
  })

  test_that('Solution should be a single character ("A" or "B")', {
    expect_match(answer5.0, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer5.0))
  if (answer_hash == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("How does sampling size affect the sampling distribution of sample medians?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

test_5.1 <- function() {
  test_that('Did not assign answer to an object called "answer5.1"', {
    expect_true(exists("answer5.1"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer5.1, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer5.1))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("How does sampling size affect the sampling distribution of sample medians?")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

# Question 6
test_6.0 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_30"', {
    expect_true(exists("sampling_dist_30"))
  })
  properties <- c(sampling_dist_30$layers[[1]]$mapping, sampling_dist_30$mapping)
  
  test_that("Plot should have sample_proportion on the x-axis", {
    expect_true("sample_proportion" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_30$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", sampling_dist_30$layers[[1]])[["stat_params"]][["binwidth"]])*30),
      "908d1fd10b357ed0ceaaec823abf81bc"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_30$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(as.integer(sum(sampling_dist_30$data$sample_proportion)*30)), "22ccbb1d89a68e2c01565574d0bdd3f6")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_30$labels$x == "sample_proportion")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_30$labels))
  })
  
  print("Success!")
}

test_6.1 <- function() {
  test_that('Did not assign answer to an object called "sampling_dist_100"', {
    expect_true(exists("sampling_dist_100"))
  })
  properties <- c(sampling_dist_100$layers[[1]]$mapping, sampling_dist_100$mapping)
  
  test_that("Plot should have sample_proportion on the x-axis", {
    expect_true("sample_proportion" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot is not the correct type", {
    expect_true("GeomBar" %in% class(sampling_dist_100$layers[[1]]$geom))
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", sampling_dist_100$layers[[1]])[["stat_params"]][["binwidth"]])*100),
      "908d1fd10b357ed0ceaaec823abf81bc"
    )
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(sampling_dist_100$data)), "6e96c307060fba1b1d3a36d2410fd595")
    expect_equal(digest(as.integer(sum(sampling_dist_100$data$sample_proportion)*100)), "8910cf9b125a2c8164816bbf8593228a")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sampling_dist_100$labels$x == "sample_proportion")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_dist_100$labels))
  })
  
  print("Success!")
}

test_6.2 <- function() {
  test_that('Did not assign answer to an object called "answer6.2"', {
    expect_true(exists("answer6.2"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer6.2, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer6.2))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8" | answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Compare the widths of the distributions more closely")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Compare the shape and symmetry of the distributions more closely")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer6.2)), "d110f00cfb1b248e835137025804a23b")
  })
  
  print("Success!")
}

test_6.3 <- function() {
  test_that('Did not assign answer to an object called "answer6.3"', {
    expect_true(exists("answer6.3"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer6.3, "true|false", ignore.case = TRUE)
  })
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer6.3)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}


test_6.4 <- function() {
  test_that('Did not assign answer to an object called "sample_10"', {
    expect_true(exists("sample_10"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_10))
  })

  expected_colnames <- c("geo_local_area")
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
    expect_equal(digest(sample_10$geo_local_area), "4966aab7b1af187acc407331b5780785")
  })
  print("Success!")
}

test_6.5 <- function() {
  test_that('Did not assign answer to an object called "resampled_proportions_10"', {
    expect_true(exists("resampled_proportions_10"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(resampled_proportions_10))
  })

  expected_colnames <- c("replicate", "sample_proportion")
  given_colnames <- colnames(resampled_proportions_10)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(resampled_proportions_10))), "6e96c307060fba1b1d3a36d2410fd595")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(resampled_proportions_10$sample_proportion))), "49b68793d28badea3f835333367d697f")
  })

  print("Success!")
}

test_6.6 <- function() {
  test_that('Did not assign answer to an object called "answer6.6_mean"', {
    expect_true(exists("answer6.6_mean"))
  })
  test_that('Did not assign answer to an object called "answer6.6_std_dev"', {
    expect_true(exists("answer6.6_std_error"))
  })

  answer_mean_as_numeric <- as.numeric(answer6.6_mean)
  answer_sd_as_numeric <- as.numeric(answer6.6_std_error)
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_mean_as_numeric * 100)), "be3c152f6f6bcd5f85f9e4cba49b1e48")
    expect_equal(digest(as.integer(answer_sd_as_numeric * 100)), "fa5a4df7ac0f9782037da890557fd8b8")
  })

  print("Success!")
}


test_6.7 <- function() {
  test_that('Did not assign answer to an object called "answer6.7"', {
    expect_true(exists("answer6.7"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer6.7, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer6.7))


  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}




