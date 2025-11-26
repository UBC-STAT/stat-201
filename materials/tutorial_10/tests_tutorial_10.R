library(digest)
library(testthat)

# Question 1.0

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "answer1.0"', {
    expect_true(exists("answer1.0"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.0, "true|false", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.0))
  #if (answer_hash == "") {
  #  print("HINT_HERE")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success! Good start!!")
}

# Question 1.1

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.1, "true|false", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.1))
  #if (answer_hash == "") {
  #  print("HINT_HERE")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success!")
}

# Question 2.0
test_2.0 <- function() {
  test_that('Did not assign answer to an object called "crit_pocock_20"', {
    expect_true(exists("crit_pocock_20"))
  })

  test_that("Solution should be a vector of length 20", {
    expect_equal(digest(length(crit_pocock_20)), "be3c152f6f6bcd5f85f9e4cba49b1e48")
  })
  
  test_that("Solution is incorrect", {
    expect_equal(digest(sum(crit_pocock_20)), "7bb25ec05af390eb51ebe476fa9ef96d")
  })

  print("Success!")
}

# Question 2.1
test_2.1 <- function() {
  test_that('Did not assign answer to an object called "crit_pocock_10"', {
    expect_true(exists("crit_pocock_10"))
  })
  
  test_that("Solution should be a vector of length 10", {
    expect_equal(digest(length(crit_pocock_10)), "71db8a6cad03244e6e50f0ad8bc95a65")
  })
  
  test_that("Solution is incorrect", {
    expect_equal(digest(sum(crit_pocock_10)), "87970dc4131c4aa6927d4ec68b3565c7")
  })
  
  print("Success!")
}

# Question 2.3

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.3"', {
    expect_true(exists("answer2.3"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.3, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.3))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# Question 3.0

test_3.0 <- function() {
  test_that('Did not assign answer to an object called "answer3.0"', {
    expect_true(exists("answer3.0"))
  })
  
  test_that("answer3.0 should be a data frame", {
    expect_true("data.frame" %in% class(answer3.0))
  })
  
  test_that("answer3.0 does not contain the correct number of columns", {
    expect_equal(digest(ncol(answer3.0)), "11946e7a3ed5e1776e81c0f0ecd383d0")
  })
  
  test_that("answer3.0 does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer3.0$p_value) * 1e5)), "2d6c2c2e53d7ed419407077a10907ca7")
    
  })

  test_that("answer3.0 does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer3.0$statistic) * 1e5)), "ba34c261b4d647d3c6cf4f543de7e814")
    
  })

  test_that("answer3.0 does not contain the correct data", {
    expect_equal(digest(answer3.0$inc_sample_size), "46d2901cf06c7e64a49f05797c929628")
    
  })
  
  print("Success!")
}


# Question 3.2
  
test_3.2 <- function() {
    
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
    
    test_that("Plot should have Statistic on the y-axis", {
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
    
    test_that("The plot does not contain the correct data", {
        expect_equal(digest(ggplot_build(sequential_stat)$data[[1]]), "cf71620d6bb34d32ae75063ebc465978")
    })
    
    test_that("The plot does not contain the correct data", {
        expect_equal(digest(ggplot_build(sequential_stat)$data[[2]]), "2b93ec98ecb341d1e822e029fa5c7809")
    })

    test_that("The plot does not contain the correct data", {
        expect_equal(digest(ggplot_build(sequential_stat)$data[[3]]), "c728df484dad6a35336327b469c82455")
    })

    test_that("The plot does not contain the correct data", {
        expect_equal(digest(ggplot_build(sequential_stat)$data[[4]]), "6aa83becc14bd5150d77a7af6c7bbbac")
    })
    
    print("Success!")
}  
  


# Question 3.4

test_3.4 <- function() {
  test_that('Did not assign answer to an object called "answer3.4"', {
    expect_true(exists("answer3.4"))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer3.4))
  })
  
  expected_colnames <- c("n_rejections_OF", "n_rejections_unadj", "expected_n_rejections")
  given_colnames <- colnames(answer3.4)
  test_that("Data frame does not have the correct number of columns or column names", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer3.4))), "4b5630ee914e848e8d07221556b0a2fb")
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer3.4$n_rejections_OF) * 1e3)), "189e2f1b2fbb3743811990e9708c226a")
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer3.4$n_rejections_unadj) * 1e3)), "7ea55401005f54e88bdc2ce0c9a9ceb1")
  })
  
  print("Success! One more and you are done!")
}
