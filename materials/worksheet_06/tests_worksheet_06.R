library(digest)
library(testthat)


## Question 1.1
test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.1, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.1))
  if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Think about the reason you took a sample from the population?")
  } else if (answer_hash == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("Think about a dichotomous random variable, and a confidence interval for proportion.")
  } else if (answer_hash == "d110f00cfb1b248e835137025804a23b") {
    print("Think about a dichotomous random variable, and a confidence interval for proportion.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

## Question 1.2
test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })

  test_that('Solution should be a string of size 11 containing only "A" and "B")', {
    expect_match(answer1.2, "^[aA|bB]{11}$")
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer1.2)), "4d9d5967d6c5d04f8176e4b028de1e7f")
  })

  print("Success!")
}

## Question 2.1
test_2.1 <- function() {
  test_that('Did not assign answer to an object called "boxplots"', {
    expect_true(exists("boxplots"))
  })
  properties <- c(boxplots$layers[[1]]$mapping, boxplots$mapping)

  test_that("Plot should have 'category' on the x-axis", {
    expect_true("category" == rlang::get_expr(properties$x))
  })

  test_that("Plot is not the correct type", {
    expect_true("GeomBoxplot" %in% class(boxplots$layers[[1]]$geom))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(boxplots$labels))
  })

  print("Success!")
}

## Question 2.2
test_2.2 <- function() {
  test_that('Solution should be a single character ("a", "b", "c", or "d")', {
    expect_match(toString(answer2.2["No-Fibrosis"]), "a|b|c|d", ignore.case = TRUE)
  })
    
  test_that('Solution should be a single character ("a", "b", "c", or "d")', {
    expect_match(toString(answer2.2["Fibrosis"]), "a|b|c|d", ignore.case = TRUE)
  })
    
  test_that('Solution should be a single character ("a", "b", "c", or "d")', {
    expect_match(toString(answer2.2["Cirrhosis"]), "a|b|c|d", ignore.case = TRUE)
  })  

  answer_hash <- digest(tolower(toString(answer2.2["No-Fibrosis"])))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, 'ddf100612805359cd81fdc5ce3b9fbba')
  })
    
  answer_hash <- digest(tolower(toString(answer2.2["Fibrosis"])))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, '6e7a8c1c098e8817e3df3fd1b21149d1')
  })
    
  answer_hash <- digest(tolower(toString(answer2.2["Cirrhosis"])))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, '127a2ec00989b9f7faf671ed470be7f8')
  })

  print("Success!")
}

## Question 2.3
test_2.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.3"', {
    expect_true(exists("answer2.3"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.3, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.3))
  if (answer_hash != "ddf100612805359cd81fdc5ce3b9fbba") {
    print("We don't know yet if there is a reduction in albumin level or not. So, what would be the a 'no-change' hypothesis?")
  } 

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

## Question 2.4
test_2.4 <- function() {
  test_that('Did not assign answer to an object called "answer2.4"', {
    expect_true(exists("answer2.4"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.4, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.4))
  if (answer_hash != "127a2ec00989b9f7faf671ed470be7f8") {
    print("We are interested to see if there is a **reduction** in the albumin level.")
  } 

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# +
## Question 2.5

test_2.5_A <- function() {
  test_that('Did not assign answer to an object called "answer2.5_A"', {
    expect_true(exists("answer2.5_A"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer2.5_A))
  })

  expected_colnames <- c("mean_albumin")
  given_colnames <- colnames(answer2.5_A)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer2.5_A))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer2.5_A$mean_albumin))), "ade76ff25149e0df3a56010f12ea82fd")
  })

  print("Success!")
}

test_2.5_B <- function(){
  test_that('Did not assign answer to an object called "answer2.5_B"', {
    expect_true(exists("answer2.5_B"))
  })

  test_that('Solution should be "lower", "same", or "higher".', {
    expect_match(answer2.5_B, "lower|same|higher", ignore.case = TRUE)
  })  
 
 answer_hash <- digest(tolower(answer2.5_B)) 
 test_that("Solution is incorrect", {
    expect_equal(answer_hash, "226b36da0f6c0424c28f826a4d22d8ac")
  })
 
 print("Success!")
}

test_2.5_C <- function() {
  test_that('Did not assign answer to an object called "answer2.5_C"', {
    expect_true(exists("answer2.5_C"))
  })

  test_that('Solution should be "TRUE" or "FALSE"', {
    expect_match(answer2.5_C, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.5_C))
  if (answer_hash == "d2a90307aac5ae8d0ef58e2fe730d38b") {
    print("Remember that we are assuming that we have the entire population.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

# +
## Question 2.6

test_2.6 <- function() {
  test_that('Did not assign answer to an object called "answer2.6"', {
    expect_true(exists("answer2.6"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.6, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.6))
  if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8") {
    print("Remember that the median and the mean can be substantially different in asymmetric distributions.")
  } else if (answer_hash == "6e7a8c1c098e8817e3df3fd1b21149d1") {
    print("In general, the sample standard deviation will not be a good estimator for the populational mean.")
  } else if (answer_hash == "d110f00cfb1b248e835137025804a23b") {
    print("This is the value you want to compare your populational mean to. But first, you need to find a good quantity to estimate
your populational mean.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# +
## Question 2.7

test_2.7 <- function() {
  test_that('Did not assign answer to an object called "samp_dist_mean_albumin"', {
    expect_true(exists("samp_dist_mean_albumin"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(samp_dist_mean_albumin))
  })

  expected_colnames <- c("replicate", "stat")
  given_colnames <- colnames(samp_dist_mean_albumin)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(samp_dist_mean_albumin))), "f1a30baa3072c1aad822c059f35c6841")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(samp_dist_mean_albumin$stat))), "0ddfbd766f51d105f1ee88218aaafdb5")
  })

  print("Success!")
}

# +
## Question 2.8

test_2.8 <- function() {
  test_that('Did not assign answer to an object called "obs_test_stat"', {
    expect_true(exists("obs_test_stat"))
  })

  answer_as_numeric <- as.numeric(obs_test_stat)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(answer_as_numeric), "839554b82f517488c1c16c8e32454e31")
  })

  print("Success!")
}

# +
## Question 2.9

test_2.9 <- function() {
  test_that('Did not assign answer to an object called "samp_dist_mean_albumin_plot"', {
    expect_true(exists("samp_dist_mean_albumin_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(samp_dist_mean_albumin_plot))
  })

  properties <- c(samp_dist_mean_albumin_plot$layers[[1]]$mapping, samp_dist_mean_albumin_plot$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(samp_dist_mean_albumin_plot$layers[[1]]$geom))

    # Remove if not needed:
    expect_true("GeomVline" %in% class(samp_dist_mean_albumin_plot$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(samp_dist_mean_albumin_plot$data)), "f1a30baa3072c1aad822c059f35c6841")
    expect_equal(digest(round(sum(samp_dist_mean_albumin_plot$data$stat))), "df228a42984939c068b853b5dbbf3cc7")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(samp_dist_mean_albumin_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  print("Success!")
}

# +
## Question 2.10

test_2.10 <- function() {
  test_that('Did not assign answer to an object called "null_model"', {
    expect_true(exists("null_model"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(null_model))
  })

  expected_colnames <- c("replicate", "stat")
  given_colnames <- colnames(null_model)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(null_model))), "f1a30baa3072c1aad822c059f35c6841")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(null_model$stat)*1000)), "df711bac668d817e415d347b90b67019")
  })

  print("Success!")
}

# +
## Question 2.11

test_2.11 <- function() {
  test_that('Did not assign answer to an object called "null_model_plot"', {
    expect_true(exists("null_model_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(null_model_plot))
  })

  properties <- c(null_model_plot$layers[[1]]$mapping, null_model_plot$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(null_model_plot$layers[[1]]$geom))

    # Remove if not needed:
    expect_true("GeomVline" %in% class(null_model_plot$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(null_model_plot$data)), "f1a30baa3072c1aad822c059f35c6841")
    expect_equal(digest(round(sum(null_model_plot$data$stat))), "b803e9666348b8cc7fb63699366f3865")

    # If stat is not known:
    # expect_equal(digest(round(sum(pull(null_model_plot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(null_model_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  print("Success!")
}

# +
## Question 2.12

test_2.12 <- function() {
  test_that('Did not assign answer to an object called "p_value"', {
    expect_true(exists("p_value"))
  })

  answer_as_numeric <- as.numeric(p_value)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10000)), "20e70f4a08bdc6a54e53ad0a7d1498b6")
  })

  print("Success!")
}

# +
## Question 2.13

test_2.13 <- function() {
  test_that('Did not assign answer to an object called "answer2.13"', {
    expect_true(exists("answer2.13"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.13, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.13))
  if (answer_hash == "ddf100612805359cd81fdc5ce3b9fbba") {
    print("Although there is a low probability. There's still a probability that under H0 you obtain a less or equal value
    than the observed test statistic")
  } else if (answer_hash == "d110f00cfb1b248e835137025804a23b") {
    print("If you are confused about why this is not the correct answer, please ask the TAs or the instructor.")
  }

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# +
## Question 2.14

test_2.14 <- function() {
  test_that('Did not assign answer to an object called "answer2.14"', {
    expect_true(exists("answer2.14"))
  })

  test_that('Solution should be a single character ("A", "B")', {
    expect_match(answer2.14, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.14))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# +
## Question 2.15

test_2.15 <- function() {
  test_that('Did not assign answer to an object called "answer2.15"', {
    expect_true(exists("answer2.15"))
  })

  test_that('Solution should be a string of size between 1 and 4 containing only "A", "B", "C", "D", and "E")', {
    expect_match(answer2.15, "^[aA|bB|cC|dD|eE]{1+}$")
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(tolower(answer2.15)), "0aa9c59ea893e51a8cc55e8ea353e592")
  })

  print("Success!")
}

# +
## Question 2.16

test_2.16 <- function() {
  test_that('Did not assign answer to an object called "null_model_infer"', {
    expect_true(exists("null_model_infer"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(null_model_infer))
  })

  expected_colnames <- c("replicate", "stat")
  given_colnames <- colnames(null_model_infer)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(null_model_infer))), "f1a30baa3072c1aad822c059f35c6841")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(null_model_infer$stat))), "129297e2da029ee99482f60461ecbd31")
  })

  print("Success!")
}
# -

test_2.17 <- function() {
  test_that('Did not assign answer to an object called "null_model_vis_infer"', {
    expect_true(exists("null_model_vis_infer"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(null_model_vis_infer))
  })

  properties <- c(null_model_vis_infer$layers[[1]]$mapping, null_model_vis_infer$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(null_model_vis_infer$layers[[1]]$geom))

    # Remove if not needed:
    #expect_true("GeomVline" %in% class(null_model_vis_infer$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(null_model_vis_infer$data)), "f1a30baa3072c1aad822c059f35c6841")
    expect_equal(digest(round(sum(null_model_vis_infer$data$stat))), "b803e9666348b8cc7fb63699366f3865")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(null_model_vis_infer$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(null_model_vis_infer$labels))
  })

    # Check that it's the good p-value shaded areas
    correct_build <- ggplot_build(null_model_vis_infer)

    test_that("The shaded area is incorrect", {
        expect_equal(digest(correct_build$data[[2]]), "f0b6564781297adbd59860e525ad65b5")
    })

    
  print("Success!")
}

# +
## Question 2.18

test_2.18 <- function() {
  test_that('Did not assign answer to an object called "p_value_infer"', {
    expect_true(exists("p_value_infer"))
  })

  answer_as_numeric <- as.numeric(p_value_infer)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "319413e7bdc61bf728528fe9eb02c0c8")
  })

  print("Success!")
}

# +
## Question 3.1

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "answer3.1"', {
    expect_true(exists("answer3.1"))
  })

  test_that('Solution should be a sequence of one up to four characters ("A", "B", "C", and/or "D")', {
    expect_match(answer3.1, "^a?b?c?d?$", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.1))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "7ecaefcc6ebc9848e7cb04b5c783ae0a")
  })

  print("Success!")
}

# +
## Question 3.2

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "answer3.2"', {
    expect_true(exists("answer3.2"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer3.2, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.2))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# +
## Question 3.3

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "answer3.3"', {
    expect_true(exists("answer3.3"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answer3.3))
  })

  expected_colnames <- c("neighbourhood_name", "median")
  given_colnames <- colnames(answer3.3)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answer3.3))), "c01f179e4b57ab8bd9de309e6d576c48")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(answer3.3$median))), "b225fb1495dbb5e5dfb3e327dceb7ab2")
  })

  print("Success!")
}

# +
## Question 3.4

test_3.4 <- function() {
  test_that('Did not assign answer to an object called "obs_med_diam_diff"', {
    expect_true(exists("obs_med_diam_diff"))
  })

  answer_as_numeric <- as.numeric(obs_med_diam_diff)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric)), "515231903becc1906b20d1cade17fd44")
  })

  print("Success!")
}

# +
## Question 3.5

test_3.5 <- function() {
  test_that('Did not assign answer to an object called "null_model_trees"', {
    expect_true(exists("null_model_trees"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(null_model_trees))
  })

  expected_colnames <- c("replicate", "stat")
  given_colnames <- colnames(null_model_trees)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(null_model_trees))), "189e2f1b2fbb3743811990e9708c226a")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(null_model_trees$stat))), "55f763ae917b48ca04293acf55b1cfde")
  })

  print("Success!")
}

# +
## Question 3.6

test_3.6 <- function() {
  test_that('Did not assign answer to an object called "trees_result_plot"', {
    expect_true(exists("trees_result_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(trees_result_plot))
  })

  properties <- c(trees_result_plot$layers[[1]]$mapping, trees_result_plot$mapping)

  test_that("Plot should have stat on the x-axis", {
    expect_true("stat" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(trees_result_plot$layers[[1]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(trees_result_plot$data)), "189e2f1b2fbb3743811990e9708c226a")
    expect_equal(digest(round(sum(trees_result_plot$data$stat))), "0dca766ef4554c1915c0cbf3d7d78fcd")

    # If stat is not known:
    # expect_equal(digest(round(sum(pull(trees_result_plot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(trees_result_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(trees_result_plot$labels))
  })

  print("Success!")
}

# +
## Question 3.7

test_3.7 <- function() {
  test_that('Did not assign answer to an object called "answer3.7"', {
    expect_true(exists("answer3.7"))
  })

  answer_as_numeric <- as.numeric(answer3.7)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1000)), "e0d4379f8d0680cb869f2438980de3e8")
  })

  print("Success!")
}

# +
## Question 3.8

test_3.8 <- function() {
  test_that('Did not assign answer to an object called "answer3.8"', {
    expect_true(exists("answer3.8"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", or "F")', {
    expect_match(answer3.8, "a|b|c|d|e|f", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.8))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "93a9078c6326f37b481d3e99b60ad987")
  })

  print("Success!")
}
# -

## Question 4.1
test_4.1 <- function() {
  test_that('Please skip this question, we will do them in tutorial_10', {
    expect_true(1==1)
  })

  print("Please skip this question, we will do them in tutorial_10!")
}

# +
## Question 4.2

test_4.2 <- function() {
  test_that('Please skip this question, we will do them in tutorial_10', {
    expect_true(1==1)
  })

  print("Please skip this question, we will do them in tutorial_10!")
}

# +
## Question 4.3

test_4.3A <- function() {
  test_that('Please skip this question, we will do them in tutorial_10', {
    expect_true(1==1)
  })

  print("Please skip this question, we will do them in tutorial_10!")
}
test_4.3B <- function() {
  test_that('Please skip this question, we will do them in tutorial_10', {
    expect_true(1==1)
  })

  print("Please skip this question, we will do them in tutorial_10!")
}

# +
## Question 4.4

test_4.4A <- function() {
  test_that('Please skip this question, we will do them in tutorial_10', {
    expect_true(1==1)
  })

  print("Please skip this question, we will do them in tutorial_10!")
}

test_4.4B <- function() {
  test_that('Please skip this question, we will do them in tutorial_10', {
    expect_true(1==1)
  })

  print("Please skip this question, we will do them in tutorial_10!")
}
# -

## Question 4.5
test_4.5 <- function() {
  test_that('Please skip this question, we will do them in tutorial_10', {
    expect_true(1==1)
  })

  print("Please skip this question, we will do them in tutorial_10!")
}

# +
## Question 4.6

## Question 4.6

test_4.6 <- function() {
  test_that('Please skip this question, we will do them in tutorial_10', {
    expect_true(1==1)
  })

  print("Please skip this question, we will do them in tutorial_10!")
}
