library(digest)
library(testthat)

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
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })
  print("Success!")
}

# Question 1.1

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })
  
  test_that('Solution should be a single character ("A", "B", or "C")', {
    expect_match(answer1.1, "a|b|c", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.1))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })
  print("Success!")
}

# Question 1.2

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.2, "true|false", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.2))
  #if (answer_hash == "") {
  #  print("HINT_HERE")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}

# Question 2.0

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "answer2.0"', {
    expect_true(exists("answer2.0"))
  })

  test_that('Solution should be a single character ("A", "B", or, "C")', {
    expect_match(answer2.0, "a|b|c", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.0))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# Question 2.1

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.1"', {
    expect_true(exists("answer2.1"))
  })
  
  test_that("answer2.1 should be a data frame", {
    expect_true("data.frame" %in% class(answer2.1))
  })
  
  test_that("answer2.1 does not contain the correct data", {
    #expect_equal(digest(as.integer(sum(answer2.1$p_value) * 1e5)), "634bb5b4ea8305f2b5a4fb11f6c23555")
    expect_equal(digest(as.integer(sum(answer2.1$p_value) * 1e5)), "4c9b31fb1bead98ab279175cc58f6237")
    
     })
  
  print("Success!")
}

# Question 2.2

test_2.2 <- function() {

  test_that('Did not assign answer to an object called "sequential_pvalue"', {
    expect_true(exists("sequential_pvalue"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(sequential_pvalue))
  })

  properties <- c(sequential_pvalue$layers[[1]]$mapping, sequential_pvalue$mapping)

  test_that("Plot should have inc_sample_size on the x-axis", {
    expect_true("inc_sample_size" == rlang::get_expr(properties$x))
  })

  test_that("Plot should have p_value on the y-axis", {
    expect_true("p_value" == rlang::get_expr(properties$y))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomLine" %in% class(sequential_pvalue$layers[[1]]$geom))
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sequential_pvalue$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("y-axis label should be descriptive and human readable", {
    expect_false(sequential_pvalue$labels$y == toString(rlang::get_expr(properties$y)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(sequential_pvalue$labels))
  })

  print("Success!")
}

# Question 2.3

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.3"', {
    expect_true(exists("answer2.3"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C")', {
    expect_match(answer2.3, "a|b|c", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer2.3))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}

# Question 2.4

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.4"', {
    expect_true(exists("answer2.4"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C")', {
    expect_match(answer2.4, "a|b|c", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer2.4))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}

# Question 2.5

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "answer2.5"', {
    expect_true(exists("answer2.5"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer2.5))
  })

  expected_colnames <- c("n_rejections", "expected_n_rejections")
  given_colnames <- colnames(answer2.5)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer2.5))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer2.5$n_rejections) * 1e3)), "5564fa9c839a49eac3525e6f8c15faf3")
  })

  print("Success!")
}

# Question 2.6

test_2.6 <- function() {
  test_that('Did not assign answer to an object called "answer2.6"', {
    expect_true(exists("answer2.6"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E")', {
    expect_match(answer2.6, "a|b|c|d|e", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.6))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "93a9078c6326f37b481d3e99b60ad987")
  })

  print("Success!")
}

# Question 3.1.0

test_3.1.0 <- function() {
  test_that('Did not assign answer to an object called "answer3.1.0"', {
    expect_true(exists("answer3.1.0"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer3.1.0, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.1.0))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}

# Question 3.1.1

test_3.1.1 <- function() {
  test_that('Did not assign answer to an object called "answer3.1.1"', {
    expect_true(exists("answer3.1.1"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer3.1.1, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.1.1))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })
  
  print("Success!")
}

# Question 3.1.2

test_3.1.2 <- function() {
  test_that('Did not assign answer to an object called "answer3.1.2"', {
    expect_true(exists("answer3.1.2"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer3.1.2))
  })
  
  expected_colnames <- c("n_rejections_Bonf", "expected_n_rejections")
  given_colnames <- colnames(answer3.1.2)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer3.1.2))), "4b5630ee914e848e8d07221556b0a2fb")
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer3.1.2$n_rejections_Bonf) * 1e3)), "6e96c307060fba1b1d3a36d2410fd595")
  })
  
  print("Success!")
}

# Question 3.2.0

test_3.2.0 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.0"', {
    expect_true(exists("answer3.2.0"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.2.0, "true|false", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.2.0))
  #if (answer_hash == "") {
  #  print("HINT_HERE")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success!")
}

# Question 3.2.1

test_3.2.1 <- function() {
  
  test_that('Did not assign answer to an object called "sequential_stat"', {
    expect_true(exists("sequential_stat"))
  })
  
  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(sequential_stat))
  })
  
  properties <- c(sequential_stat$layers[[1]]$mapping, sequential_stat$mapping)
  
  test_that("Plot should have inc_sample_size on the x-axis", {
    expect_true("inc_sample_size" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot should have values of the t-statistic on the y-axis", {
    expect_true("statistic" == rlang::get_expr(properties$y))
  })
  
  test_that("Plot does not have the correct layers", {
    expect_true("GeomLine" %in% class(sequential_stat$layers[[1]]$geom))
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(sequential_stat$labels$x == toString(rlang::get_expr(properties$x)))
  })
  
  test_that("y-axis label should be descriptive and human readable", {
    expect_false(sequential_stat$labels$y == toString(rlang::get_expr(properties$y)))
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(sequential_stat$labels))
  })
  
  print("Success!")
}

# Question 3.2.2

test_3.2.2 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.2"', {
    expect_true(exists("answer3.2.2"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer3.2.2, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.2.2))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d110f00cfb1b248e835137025804a23b")
  })
  
  print("Success!")
}

# Question 3.2.3

test_3.2.3 <- function() {
  test_that('Did not assign answer to an object called "answer3.2.3"', {
    expect_true(exists("answer3.2.3"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer3.2.3))
  })
  
  expected_colnames <- c("n_rejections_Pocock", "expected_n_rejections")
  given_colnames <- colnames(answer3.2.3)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer3.2.3))), "4b5630ee914e848e8d07221556b0a2fb")
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer3.2.3$n_rejections_Pocock) * 1e3)), "e1aa7a373b6b420518ad3006b0942f41")
  })
  
  print("Success!")
}

