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
  if (answer == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("This is an example of a descriptive question.")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("This is an example of a predictive question.")
  } else if (answer == "d110f00cfb1b248e835137025804a23b") {
    print("This is an example of an exploratory question.")
  }

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.0)), "127a2ec00989b9f7faf671ed470be7f8")
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

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "answer2.0"', {
    expect_true(exists("answer2.0"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.0, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.0))
  if (answer == "127a2ec00989b9f7faf671ed470be7f8") {
    print("This command tells R to represent the data as a histogram.")
  } else if (answer == "df100612805359cd81fdc5ce3b9fbba") {
    print("This command tells R to rename the x-axis Body mass (g).")
  } else if (answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("This is the data frame that contains the data.")
  }

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.0)), "d110f00cfb1b248e835137025804a23b")
  })
  
  print("Success!")
}


test_2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.1"', {
    expect_true(exists("answer2.1"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.1, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.1))
  if (answer == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Look at the range of values on the x-axis.")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.1)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.1"', {
    expect_true(exists("answer2.2"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.1, "true|false", ignore.case = TRUE)
  })
  
  answer <- digest(tolower(answer2.2))
  if (answer == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Remember that while the median is not influenced by extreme values, the mean will be.")
  }
  
  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.2)), "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}
