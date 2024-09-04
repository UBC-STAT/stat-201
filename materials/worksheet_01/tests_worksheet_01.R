library(testthat)
library(digest)

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "answer1.0"', {
    expect_true(exists("answer1.0"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.0, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer1.0))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("This is an example of a descriptive question.")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("This is an example of a predictive question.")
  } else if (answer == "d110f00cfb1b248e835137025804a23b") {
    print("This is an example of a causal question.")
  }

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.0)), "ddf100612805359cd81fdc5ce3b9fbba")
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
  
  answer <- digest(tolower(answer1.1))
  if (answer == "05ca18b596514af73f6880309a21b5dd") {
    print("An inferential question does not ask about the underlying mechanism of a pattern, trend, or relationship; only whether one exists.")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.1)), "d2a90307aac5ae8d0ef58e2fe730d38b")
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
    print("This package is used for wrangling remote database tables.")
  } else if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("This is a function used for sampling simulation, not a package.")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("This package is used to make it easier to work with some R objects.")
  }

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.2)), "d110f00cfb1b248e835137025804a23b")
  })
  
  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "mean"', {
    expect_true(exists("mean"))
  })
  test_that('Did not assign answer to an object called "proportion"', {
    expect_true(exists("proportion"))
  })
  test_that('Did not assign answer to an object called "median"', {
    expect_true(exists("median"))
  })

  answers <- c(mean, proportion, median) %>%
    map(as.numeric)

  test_that("Solutions should be numbers", {
    expect_false(anyNA(answers))
  })

  test_that("Solutions should be integers", {
    for (multi_family_strata in answers) {
      expect_true(multi_family_strata %% 1 == 0)
    }
  })

  test_that("At least one term-definition match is incorrect", {
    expect_equal(digest(as.numeric(mean)), "e5b57f323c7b3719bbaaf9f96b260d39")
    expect_equal(digest(as.numeric(proportion)), "6717f2823d3202449301145073ab8719")
    expect_equal(digest(as.numeric(median)), "db8e490a925a60e62212cefc7674ca02")
  })
  
  print("Success!")
}

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "point_estimate"', {
    expect_true(exists("point_estimate"))
  })
  test_that('Did not assign answer to an object called "population"', {
    expect_true(exists("population"))
  })
  test_that('Did not assign answer to an object called "population_parameter"', {
    expect_true(exists("population_parameter"))
  })
  test_that('Did not assign answer to an object called "sample"', {
    expect_true(exists("sample"))
  })
  test_that('Did not assign answer to an object called "observation"', {
    expect_true(exists("observation"))
  })
  test_that('Did not assign answer to an object called "sampling_distribution"', {
    expect_true(exists("sampling_distribution"))
  })

  answers <- c(
    point_estimate,
    population,
    population_parameter,
    sample,
    observation,
    sampling_distribution
  ) %>%
    map(as.numeric)

  test_that("Solutions should be numbers", {
    expect_false(anyNA(answers))
  })

  test_that("Solutions should be integers", {
    for (answer in answers) {
      expect_true(answer %% 1 == 0)
    }
  })


  test_that("At least one term-definition match is incorrect", {
    expect_equal(digest(as.numeric(point_estimate)), "dbc09cba9fe2583fb01d63c70e1555a8")
    expect_equal(digest(as.numeric(population)), "6717f2823d3202449301145073ab8719")
    expect_equal(digest(as.numeric(population_parameter)), "db8e490a925a60e62212cefc7674ca02")
    expect_equal(digest(as.numeric(sample)), "e5b57f323c7b3719bbaaf9f96b260d39")
    expect_equal(digest(as.numeric(observation)), "0aee9b78301d7ec8998971363be87c03")
    expect_equal(digest(as.numeric(sampling_distribution)), "5e338704a8e069ebd8b38ca71991cf94")
  })
  
  print("Success!")
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "answer1.5"', {
    expect_true(exists("answer1.5"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.5, "true|false", ignore.case = TRUE)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.5)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}

test_1.6 <- function() {
  test_that('Did not assign answer to an object called "answer1.6"', {
    expect_true(exists("answer1.6"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.6, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer1.6))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Read the values in the table closely.")
  } else if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Be sure to look at more than just the shape of the histogram.")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Be sure to look at more than just the shape of the histogram.")
  }

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.6)), "d110f00cfb1b248e835137025804a23b")
  })
  
  print("Success!")
}

test_1.7 <- function() {
  test_that('Did not assign answer to an object called "answer1.8"', {
    expect_true(exists("answer1.7"))
  })

  test_that("Solution should be a number", {
    expect_false(is.na(as.numeric(answer1.7)))
  })

  test_that("Solution should be an integer", {
    expect_true(answer1.7 %% 1 == 0)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(answer1.7), "db8e490a925a60e62212cefc7674ca02")
  })
  
  print("Success!")
}

test_1.8 <- function() {
  test_that('Did not assign answer to an object called "answer1.8"', {
    expect_true(exists("answer1.8"))
  })

  test_that("Solution should be a number", {
    expect_false(is.na(as.numeric(answer1.8)))
  })

  test_that("Solution should be an integer", {
    expect_true(answer1.8 %% 1 == 0)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(answer1.8), "4a5d7d50676e6d0ea065f445d8a5539d")
  })
  
  print("Success!")
}

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "multi_family_strata"', {
    expect_true(exists("multi_family_strata"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(multi_family_strata))
  })

  test_that("Data frame does not have the correct columns", {
    expect_equal(ncol(multi_family_strata), 1)
    expect_true("current_land_value" %in% colnames(multi_family_strata))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(multi_family_strata), 23173)
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(round(sum(as.numeric(multi_family_strata$current_land_value)))), "80e1a37cc1c8e9d7b09de25702051bb8")
  })
  
  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "pop_dist"', {
    expect_true(exists("pop_dist"))
  })
  properties <- c(pop_dist$layers[[1]]$mapping, pop_dist$mapping)

  test_that("Plot should have current_land_value on the x-axis", {
    expect_true("current_land_value" == rlang::get_expr(properties$x))
  })

  test_that("Plot should be a histogram", {
    expect_true("GeomBar" %in% class(pop_dist$layers[[1]]$geom))
  })

  test_that("Plot should use 50 bins", {
    expect_equal(mget("stat_params", pop_dist$layers[[1]])[["stat_params"]][["bins"]], 50)
  })

  test_that("Plot does not use the correct data", {
    expect_equal(nrow(pop_dist$data), 23173)
    expect_true("current_land_value" %in% colnames(pop_dist$data))
    expect_equal(digest(round(sum(pop_dist$data$current_land_value))), "80e1a37cc1c8e9d7b09de25702051bb8")
  })

  test_that("You should add label on the x-axis to make it descriptive and human readable", {
    expect_false((pop_dist$labels$x) == "current_land_value")
  })

  print("Success!")
}

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "pop_mean"', {
    expect_true(exists("pop_mean"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(pop_mean))
  })

  test_that("Data frame does not have the correct columns", {
    expect_equal(ncol(pop_mean), 1)
    expect_true("pop_mean" %in% colnames(pop_mean))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(pop_mean), 1)
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(round(colSums(Filter(is.numeric, pop_mean)))), '8f085c7c5a38597332e68a2164e495fd')
  })

  print("Success!")
}

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "sample_1"', {
    expect_true(exists("sample_1"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_1))
  })

  test_that("Data frame does not have the correct columns", {
    expect_equal(ncol(sample_1), 2)
    expect_true("replicate" %in% colnames(sample_1))
    expect_true("current_land_value" %in% colnames(sample_1))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(sample_1), 40)
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(round(sum(sample_1$current_land_value))), "da570df3e36a3391e870d593a6fd1a65")
  })

  print("Success!")
}

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "sample_1_dist"', {
    expect_true(exists("sample_1_dist"))
  })
  properties <- c(sample_1_dist$layers[[1]]$mapping, sample_1_dist$mapping)

  test_that("Plot should have current_land_value on the x-axis", {
    expect_true("current_land_value" == rlang::get_expr(properties$x))
  })

  test_that("Plot should be a histogram", {
    expect_true("GeomBar" %in% class(sample_1_dist$layers[[1]]$geom))
  })

  test_that("Plot should use 50 bins", {
    expect_equal(mget("stat_params", sample_1_dist$layers[[1]])[["stat_params"]][["bins"]], 50)
  })

  test_that("Plot does not use the correct data", {
    expect_equal(nrow(sample_1_dist$data), 40)
    expect_true("current_land_value" %in% colnames(sample_1_dist$data))
    expect_equal(digest(round(sum(sample_1_dist$data$current_land_value))), "da570df3e36a3391e870d593a6fd1a65")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false((sample_1_dist$labels$x) == "current_land_value")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sample_1_dist$labels))
  })

  print("Success!")
}

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "sample_1_mean"', {
    expect_true(exists("sample_1_mean"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_1_mean))
  })

  test_that("Data frame does not have the correct columns", {
    expect_equal(ncol(sample_1_mean), 1)
    expect_true("sample_1_mean" %in% colnames(sample_1_mean))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(sample_1_mean), 1)
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(round(colSums(Filter(is.numeric, sample_1_mean)))), "1258c254cd1a27b1a151c1f75678594e")
  })

  print("Success!")
}

test_2.6 <- function() {
  test_that('Did not assign answer to an object called "answer2.6"', {
    expect_true(exists("answer2.6"))
  })

  test_that('Solution should be a single character ("A", "B", or "C")', {
    expect_match(answer2.6, "a|b|c", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.6))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Compare the values of pop_mean and sample_1_mean. Is sample_1_mean close enough to be considered a good guess of pop_mean?")
  } else if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Are the location of the peaks of the distributions approximately the same? What about the relative frequencies of the values surrounding the peaks?")
  }

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.6)), "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

test_2.7 <- function() {
  test_that('Did not assign answer to an object called "sample_2_dist"', {
    expect_true(exists("sample_2_dist"))
  })
  
  properties <- c(sample_2_dist$layers[[1]]$mapping, sample_2_dist$mapping)
  
  test_that("Plot should have current_land_value on the x-axis", {
    expect_true("current_land_value" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot should be a histogram", {
    expect_true("GeomBar" %in% class(sample_2_dist$layers[[1]]$geom))
  })
  
  test_that("Plot should use 50 bins", {
    expect_equal(mget("stat_params", sample_2_dist$layers[[1]])[["stat_params"]][["bins"]], 50)
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(nrow(sample_2_dist$data), 40)
    expect_true("current_land_value" %in% colnames(sample_2_dist$data))
    expect_equal(digest(round(sum(sample_2_dist$data$current_land_value))), "6dbc7b8728a6a6c799d4a14826649945")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false((sample_2_dist$labels$x) == "current_land_value")
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(sample_2_dist$labels))
  })
  
  test_that('Did not assign answer to an object called "sample_2_mean"', {
    expect_true(exists("sample_2_mean"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_2_mean))
  })
  
  test_that("Data frame does not have the correct columns", {
    expect_equal(ncol(sample_2_mean), 2)
    expect_true("replicate" %in% colnames(sample_2_mean))
    expect_true("sample_2_mean" %in% colnames(sample_2_mean))
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(sample_2_mean), 1)
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(round(sum(sample_2_mean$sample_2_mean))), "5aa2ab3dd5136cd158c4deada0385103")
  })
  
  print("Success!")
}


test_2.8 <- function() {
  test_that('Did not assign answer to an object called "answer2.8"', {
    expect_true(exists("answer2.8"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.8, "a|b|c|d", ignore.case = TRUE)
  })

  answer <- digest(tolower(answer2.8))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("It is very unlikely that we choose the same set of observations from the population across multiple samples.")
  } else if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("It is very unlikely that we choose the same set of observations from the population across multiple samples, but on average, the sample distributions should still look like the population distribution.")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("It is very unlikely that we choose the same set of observations from the population across multiple samples.")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.8)), "d110f00cfb1b248e835137025804a23b")
  })

  print("Success!")
}


test_3.0 <- function() {
  test_that('Did not assign answer to an object called "samples"', {
    expect_true(exists("samples"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(samples))
  })

  test_that("Data frame does not have the correct columns", {
    expect_equal(ncol(samples), 2)
    expect_true("replicate" %in% colnames(samples))
    expect_true("current_land_value" %in% colnames(samples))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(samples), 60000)
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(round(sum(samples$current_land_value))), "65b111959bc6481da7a38815fcabfbb4")
  })

  print("Success!")
}

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "sample_estimates"', {
    expect_true(exists("sample_estimates"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(sample_estimates))
  })

  test_that("Data frame does not have the correct columns", {
    expect_equal(ncol(sample_estimates), 2)
    expect_true("replicate" %in% colnames(sample_estimates))
    expect_true("sample_mean" %in% colnames(sample_estimates))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(nrow(sample_estimates), 1500)
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(round(sum(sample_estimates$sample_mean))), "c800763017fda5bb0b4dca99a8c6f3ea")
  })

  print("Success!")
}

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "sampling_distribution"', {
    expect_true(exists("sampling_distribution"))
  })

  properties <- c(sampling_distribution$layers[[1]]$mapping, sampling_distribution$mapping)

  test_that("Plot should have sample_mean on the x-axis", {
    expect_true("sample_mean" == rlang::get_expr(properties$x))
  })

  test_that("Plot should be a histogram", {
    expect_true("GeomBar" %in% class(sampling_distribution$layers[[1]]$geom))
  })

  test_that("Plot should use 30 bins", {
    expect_equal(mget("stat_params", sampling_distribution$layers[[1]])[["stat_params"]][["bins"]], 30)
  })

  test_that("Plot does not use the correct data", {
    expect_equal(nrow(sampling_distribution$data), 1500)
    expect_true("sample_mean" %in% colnames(sampling_distribution$data))
    expect_equal(digest(round(sum(sampling_distribution$data$sample_mean))), "c800763017fda5bb0b4dca99a8c6f3ea")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false((sampling_distribution$labels$x) == "sample_mean")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sampling_distribution$labels))
  })

  print("Success!")
}

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "answer3.3"', {
    expect_true(exists("answer3.3"))
  })

  test_that("Solution should be a number", {
    expect_false(is.na(as.numeric(answer3.3)))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(floor(answer3.3)), "53849aef7ad3a06c28f3031fab4ef6fc")
  })

  print("Success!")
}

test_3.4 <- function() {
  test_that('Did not assign answer to an object called "answer3.4"', {
    expect_true(exists("answer3.4"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer3.4, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer3.4))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("The true population mean is around $620,000, and this is near where both the 'peak' & centre of the sampling distribution appear to be.")
  } else if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("It appears that we capture a large majority of the sample means within a range of $50-75 thousand from the centre of the sampling distribution.")
  } else if (answer == "d110f00cfb1b248e835137025804a23b") {
    print("It appears that we capture a large majority of the sample means within a range of $50-75 thousand from the centre of the sampling distribution.")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer3.4)), "6e7a8c1c098e8817e3df3fd1b21149d1")
  })
  
  print("Success!")
}

test_3.5 <- function() {
  test_that('Did not assign answer to an object called "answer3.5"', {
    expect_true(exists("answer3.5"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer3.5, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer3.5))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("This range does not capture the centre of the sampling distribution (many of the sample means lie near there).")
  } else if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("This range does not capture the centre of the sampling distribution (many of the sample means lie near there).")
  } else if (answer == "d110f00cfb1b248e835137025804a23b") {
    print("This range captures many of the sample means, but where in the interval would you want the centre of the distribution to lie to maximize the number of sample means you capture?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer3.5)), "6e7a8c1c098e8817e3df3fd1b21149d1")
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
  
  answer <- digest(tolower(answer3.6))
  if (answer == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("In the sampling distribution above, where do most of the sample means lie?")
  }

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer3.6)), "05ca18b596514af73f6880309a21b5dd")
  })
  print("Success!")
}

test_3.7 <- function() {
  test_that('Did not assign answer to an object called "answer3.7"', {
    expect_true(exists("answer3.7"))
  })
  
  test_that('Solution should be a single character ("A", "B", or "C")', {
    expect_match(answer3.7, "a|b|c", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer3.7))
  if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("What about the case where you get unlucky and your point estimate is inaccurate? Would you be able to tell if this was the case? Would be more informative to your boss if you were also able to infer about the accuracy of your point estimate?")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Consider the sampling distribution we explored above. Would you consider all of the sample means to be 'accurate'?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer3.7)), "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("Success!")
}

test_4.0 <- function() {
  test_that('Did not assign answer to an object called "answer4.0"', {
    expect_true(exists("answer4.0"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer4.0, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer4.0))
  if (answer != "127a2ec00989b9f7faf671ed470be7f8") {
    print("Think of the potential biases that could result from this method")
  } 
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer4.0)), "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("Success!")
}

test_4.1 <- function() {
  test_that('Did not assign answer to an object called "answer4.1"', {
    expect_true(exists("answer4.1"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer4.1, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer4.1))
  if (answer == "05ca18b596514af73f6880309a21b5dd") {
    print("Does everyone follow UBC's social media? Who might be more likely to submit a response to the poll?")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer4.1)), "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success!")
}

test_4.2 <- function() {
  test_that('Did not assign answer to an object called "answer4.2"', {
    expect_true(exists("answer4.2"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer4.2, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer4.2))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Is the sample of undergraduate TAs representative of the population of interest?")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("In what way is the sample not representative of the population of interest?")
  } else if (answer == "d110f00cfb1b248e835137025804a23b") {
    print("The sample size is not small enough that we can say that there is a 'high' chance that our estimate would be inaccurate")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer4.2)), "ddf100612805359cd81fdc5ce3b9fbba")
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
  
  answer <- digest(tolower(answer4.3))
  if (answer == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("ompare the proportions of each type of worker in the sample to their proportion of the population")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer4.3)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}

