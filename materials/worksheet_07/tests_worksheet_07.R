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


## Question 1.3

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "answer1.3"', {
    expect_true(exists("answer1.3"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.3, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.3))
  if (answer_hash == "05ca18b596514af73f6880309a21b5dd") {
    print("There are some assumptions that are made. Don't remember those? Look back at Module 03.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

# +
## Question 1.4

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "answer1.4"', {
    expect_true(exists("answer1.4"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.4, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.4))
  if (answer_hash != "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Make sure to use the right quantiles.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# +
## Question 1.5 

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "answer1.5"', {
    expect_true(exists("answer1.5"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.5, "a|b|c|d|e|f", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.5))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# +
# Question 2.1.0

test_2.1.0 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.0"', {
    expect_true(exists("answer2.1.0"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", "F", "G", or "H")', {
    expect_match(answer2.1.0, "a|b|c|d|e|f|g|h", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.1.0))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}


# +
# Question 2.1.2

test_2.1.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.2"', {
    expect_true(exists("answer2.1.2"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.1.2, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.1.2))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

# +
# Question 2.1.3

test_2.1.3 <- function() {
  test_that('Did not assign answer to an object called "observed_test_statistic2.1.3"', {
    expect_true(exists("observed_test_statistic2.1.3"))
  })

  answer_as_numeric <- as.numeric(observed_test_statistic2.1.3)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "b530578a0d50e02f4b0a2476d1d05cf6")
  })

  print("Success!")
}


# +
# Question 2.1.5

test_2.1.5 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.5"', {
    expect_true(exists("answer2.1.5"))
  })

  answer_as_numeric <- as.numeric(answer2.1.5)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "529289aebf92ae5e4bf78b3453ba8d8d")
  })

  print("Success!")
}

# +
# Question 2.1.6

test_2.1.6 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.6"', {
    expect_true(exists("answer2.1.6"))
  })

  test_that('Solution should be a single character ("A", or "B")', {
    expect_match(answer2.1.6, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.1.6))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# +
# Question 2.1.7

test_2.1.7 <- function() {
  test_that('Did not assign answer to an object called "answer2.1.7"', {
    expect_true(exists("answer2.1.7"))
  })

  test_that("Solution should be the output of t.test", {
    expect_true("data.frame" %in% class(answer2.1.7))
  })

  test_that("Wrong statistic value", {
    expect_equal(digest(as.integer(answer2.1.7$statistic*10e6)), "b530578a0d50e02f4b0a2476d1d05cf6")
  })
  
  test_that("Wrong p-value", {
    expect_equal(digest(as.integer(answer2.1.7$p.value*10e6)), "529289aebf92ae5e4bf78b3453ba8d8d")
  })
  test_that("Wrong estimate", {
    expect_equal(digest(as.integer(answer2.1.7$estimate*10e6)), "2a3bad28c47da4ed94bd04dd3a8e2402")
  })
  test_that("Wrong parameter", {
    expect_equal(digest(answer2.1.7$parameter), "63996d1e8f7bb83b447df0b0188f621f")
  })
  test_that("Wrong alternative hypothesis", {
    expect_equal(digest(answer2.1.7$alternative), "d095e00cba6f80bf1d5ad5db0400f812")
  })

  print("Success!")
}

# +
# Question 2.2.1

test_2.2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.1"', {
    expect_true(exists("answer2.2.1"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.2.1, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.2.1))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# +
# Question 2.2.2

test_2.2.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.2_phat"', {
    expect_true(exists("answer2.2.2_phat"))
  })

  answer_as_numeric <- as.numeric(answer2.2.2_phat)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "5cdefc187251dd42f295b91ffd41f667")
  })

  print("Success!")
}

# +
# Question 2.2.3

test_2.2.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.3_test_statistic"', {
    expect_true(exists("answer2.2.3_test_statistic"))
  })

  answer_as_numeric <- as.numeric(answer2.2.3_test_statistic)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "f7d4a8ba32a4c01ee5b69bc037cb8d1b")
  })

  print("Success!")
}

# +
# Question 2.2.4

test_2.2.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.4_pvalue"', {
    expect_true(exists("answer2.2.4_pvalue"))
  })

  answer_as_numeric <- as.numeric(answer2.2.4_pvalue)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e10)), "ec330019799dea65b930886ff878ca6d")
  })

  print("Success!")
}

# +
# Question 2.2.5

test_2.2.5 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.5"', {
    expect_true(exists("answer2.2.5"))
  })

  test_that('Solution should be a single character ("A", "B")', {
    expect_match(answer2.2.5, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.2.5))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# +
# Question 2.2.6

test_2.2.6 <- function() {
  test_that('Did not assign answer to an object called "answer2.2.6"', {
    expect_true(exists("answer2.2.6"))
  })

  test_that("Solution should be the output of t.test", {
    expect_true("data.frame" %in% class(answer2.2.6))
  })

  test_that("Wrong statistic value", {
    expect_equal(digest(as.integer(answer2.2.6$statistic*10e6)), "f4b128273540763be08409cb78e6370e")
  })
  
  test_that("Wrong p-value", {
    expect_equal(digest(as.integer(answer2.2.6$p.value*10e10)), "ec330019799dea65b930886ff878ca6d")
  })
  test_that("Wrong estimate", {
    expect_equal(digest(as.integer(answer2.2.6$estimate*10e6)), "5cdefc187251dd42f295b91ffd41f667")
  })
  test_that("Wrong parameter", {
    expect_equal(digest(answer2.2.6$parameter), "cff4821792ec6a04a0622fe7246d298a")
  })
  test_that("Wrong alternative hypothesis", {
    expect_equal(digest(answer2.2.6$alternative), "0ff92e9c2aed54e21f95489292c00267")
  })

  print("Success!")
}

# +
# Question 2.3.1

test_2.3.1 <- function() {
  test_that('Did not assign answer to an object called "flipper_length_boxplots"', {
    expect_true(exists("flipper_length_boxplots"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(flipper_length_boxplots))
  })

  properties <- c(flipper_length_boxplots$layers[[1]]$mapping, flipper_length_boxplots$mapping)

  test_that("Plot should have species on the x-axis", {
    expect_true("species" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot should have flipper_length_mm on the y-axis", {
    expect_true("flipper_length_mm" == rlang::get_expr(properties$y))
  })
  test_that("Plot should have species on fill property", {
    expect_true("species" == rlang::get_expr(properties$fill))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBoxplot" %in% class(flipper_length_boxplots$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
    # expect_true("GeomVline" %in% class(steam_ci_plot$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(flipper_length_boxplots$data)), "10fe46640da0fc3a66d68ae9fe105e76")
    expect_equal(digest(length(unique(flipper_length_boxplots$data$species))), "c01f179e4b57ab8bd9de309e6d576c48")

    # If species is not known:
    # expect_equal(digest(round(sum(pull(flipper_length_boxplots$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(flipper_length_boxplots$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(flipper_length_boxplots$labels))
  })

  print("Success!")
}

# +
# Question 2.3.2 

test_2.3.2 <- function() {
  test_that('Did not assign answer to an object called "adelie_chin_summary"', {
    expect_true(exists("adelie_chin_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(adelie_chin_summary))
  })

  expected_colnames <- c("species", "sample_mean", "sample_var", "n")
  given_colnames <- colnames(adelie_chin_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(adelie_chin_summary))), "c01f179e4b57ab8bd9de309e6d576c48")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(adelie_chin_summary$sample_mean) * 10000)), "d87713c7a00bc3c4b9cc12fe09075692")
    expect_equal(digest(as.integer(sum(adelie_chin_summary$sample_var) * 10000)), "c75517ee769cbf8810fdc220ff7e2a0c")
    expect_equal(digest(as.integer(sum(adelie_chin_summary$n))), "7a508917c5a0cf5111df42bb714e32a8")
  })

  print("Success!")
}

# +
# Question 2.3.3

test_2.3.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.3.3"', {
    expect_true(exists("answer2.3.3"))
  })

  answer_as_numeric <- as.numeric(answer2.3.3)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10000)), "80f85a912d286d46065649d49a98c8b8")
  })

  print("Success!")
}

# +
# Question 2.3.4

test_2.3.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.3.4"', {
    expect_true(exists("answer2.3.4"))
  })

  answer_as_numeric <- as.numeric(answer2.3.4)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10000)), "1473d70e5646a26de3c52aa1abd85b1f")
  })

  print("Success!")
}


# +
# Question 2.3.5

test_2.3.5 <- function() {
  test_that('Did not assign answer to an object called "answer2.3.5"', {
    expect_true(exists("answer2.3.5"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.3.5, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.3.5))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

# +
# Question 3.3.6

test_2.3.6 <- function() {
    test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer2.3.6$statistic * 1e4)), "80f85a912d286d46065649d49a98c8b8")
    expect_equal(digest(as.integer(answer2.3.6$parameter * 1e5)), "0c60d382756b9734b7ef2b771ecb514b")
    expect_equal(digest(as.integer(answer2.3.6$p.value * 1e9)), "3e98cd61c0f0313ba5b0b2fa1a37a0ec")
  })

  print("Success!")
}

# +
# Question 2.4.1

test_2.4.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.4.1"', {
    expect_true(exists("answer2.4.1"))
  })

  answer_as_numeric <- as.numeric(answer2.4.1)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "eadc6c976e92d423f7d3e4551fc729fe")
  })

  print("Success!")
}

# +
# Question 2.4.2

test_2.4.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.4.2"', {
    expect_true(exists("answer2.4.2"))
  })

  answer_as_numeric <- as.numeric(answer2.4.2)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric)), "1473d70e5646a26de3c52aa1abd85b1f")
  })

  print("Success!")
}

# +
# Question 2.4.3

test_2.4.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.4.3"', {
    expect_true(exists("answer2.4.3"))
  })

  test_that('Solution should be a single character ("A", "B")', {
    expect_match(answer2.4.3, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.4.3))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# +
# Question 2.4.4

test_2.4.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.4.4"', {
    expect_true(exists("answer2.4.4"))
  })

  test_that("Solution should be the output of t.test", {
    expect_true("data.frame" %in% class(answer2.4.4))
  })

  test_that("Wrong statistic value", {
    expect_equal(digest(as.integer(answer2.4.4$statistic*10e6)), "3fa793e6e1421e1687213c53ce135c4f")
  })
  
  test_that("Wrong p-value", {
    expect_equal(digest(as.integer(answer2.4.4$p.value*10e10)), "1473d70e5646a26de3c52aa1abd85b1f")
  })
  test_that("Wrong estimate", {
    expect_equal(digest(as.integer(answer2.4.4$estimate1 * 10e6)), "bcf8cac70cafa62dfccf44f54f598d49")
    expect_equal(digest(as.integer(answer2.4.4$estimate2 * 10e6)), "4c8f1393313e0f2e70d962d986af8fb2")
  })
  test_that("Wrong parameter", {
    expect_equal(digest(answer2.4.4$parameter), "fdc86b4d739d00b8ef1165920b3245e5")
  })
  test_that("Wrong alternative hypothesis", {
    expect_equal(digest(answer2.4.4$alternative), "d095e00cba6f80bf1d5ad5db0400f812")
  })

  print("Success!")
}

# +
# Question 2.5.1

test_2.5.1 <- function() {
  test_that('Did not assign answer to an object called "training"', {
    expect_true(exists("training"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(training))
  })

  expected_colnames <- c("before_training", "after_training", "d")
  given_colnames <- colnames(training)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(training))), "9a6564e67167bff7e7cf99a541a641f1")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(training$d) * 10e5)), "e43a71d21e54a7d68bf601ff98a4a313")
  })

  print("Success!")
}

# +
# Question 2.5.3

test_2.5.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.5.3"', {
    expect_true(exists("answer2.5.3"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.5.3, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.5.3))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# +
# Question 2.5.4

test_2.5.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.5.4"', {
    expect_true(exists("answer2.5.4"))
  })

  answer_as_numeric <- as.numeric(answer2.5.4)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e5)), "f2a1fe995fd2de8ebb95296927bc5ba7")
  })

  print("Success!")
}

# +
# Question 2.5.5

test_2.5.5 <- function() {
  test_that('Did not assign answer to an object called "answer2.5.5"', {
    expect_true(exists("answer2.5.5"))
  })

  answer_as_numeric <- as.numeric(answer2.5.5)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e11)), "fad7585242507af556c64ac730027ed5")
  })

  print("Success!")
}

# +
# Question 2.5.6

test_2.5.6 <- function() {
  test_that('Did not assign answer to an object called "answer2.5.6"', {
    expect_true(exists("answer2.5.6"))
  })

  test_that('Solution should be a single character ("A", "B")', {
    expect_match(answer2.5.6, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.5.6))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# +
# Question 2.5.7

test_2.5.7 <- function() {
  test_that('Did not assign answer to an object called "answer2.5.7"', {
    expect_true(exists("answer2.5.7"))
  })

  test_that("Solution should be the output of t.test", {
    expect_true("data.frame" %in% class(answer2.5.7))
  })

  test_that("Wrong statistic value", {
    expect_equal(digest(as.integer(answer2.5.7$statistic*10e6)), "5f3f9a2b249700c4abdf83a429cc4b8f")
  })
  
  test_that("Wrong p-value", {
    expect_equal(digest(as.integer(answer2.5.7$p.value*10e6)), "1473d70e5646a26de3c52aa1abd85b1f")
  })
  test_that("Wrong estimate", {
    expect_equal(digest(as.integer(answer2.5.7$estimate*10e6)), "a463bf6ff963e84808b4fc7072ce750d")
  })
  test_that("Wrong parameter", {
    expect_equal(digest(answer2.5.7$parameter), "cb296d017f50125cb5c4cd8e507aaa1b")
  })
  test_that("Wrong alternative hypothesis", {
    expect_equal(digest(answer2.5.7$alternative), "0ff92e9c2aed54e21f95489292c00267")
  })

  print("Success!")
}
