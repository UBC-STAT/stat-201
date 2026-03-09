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

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "lotto_p_hat"', {
    expect_true(exists("lotto_p_hat"))
  })

  answer_as_numeric <- as.numeric(lotto_p_hat)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e4)), "fc7b456bd90221a76da7ff85abe4cf85")
  })

  print("Success!")
}

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "lotto_std_error"', {
    expect_true(exists("lotto_std_error"))
  })

  answer_as_numeric <- as.numeric(lotto_std_error)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "9988e30cdb1537829bea087b3650218b")
  })

  print("Success!")
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "lotto_p_value"', {
    expect_true(exists("lotto_p_value"))
  })

  answer_as_numeric <- as.numeric(lotto_p_value)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "83e9ccfb5fc36266bd1f437fe5529eb0")
  })

  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "dangar_mu0"', {
    expect_true(exists("dangar_mu0"))
  })

  answer_as_numeric <- as.numeric(dangar_mu0)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "f5baea5e39e1c9872c9667c0fd79571d")
  })

    
  test_that('Did not assign answer to an object called "dangar"', {
    expect_true(exists("dangar"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(dangar))
  })

  expected_colnames <- c("Length")
  given_colnames <- colnames(dangar)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(dangar))), "b225fb1495dbb5e5dfb3e327dceb7ab2")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(dangar$Length))), "07f9d312018c1799bc2e2b5b4fdc4c1b")
  })

  print("Success!")
}

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "dangar_x_bar"', {
    expect_true(exists("dangar_x_bar"))
  })

  answer_as_numeric <- as.numeric(dangar_x_bar)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "85b4fb3fc7bcebf153c433fac3244fdc")
  })

  print("Success!")
}

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "dangar_std_error"', {
    expect_true(exists("dangar_std_error"))
  })

  answer_as_numeric <- as.numeric(dangar_std_error)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "c64b6659d9679cb99dc2aff6b98db9bd")
  })

  print("Success!")
}

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "dust_summary"', {
    expect_true(exists("dust_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(dust_summary))
  })

  expected_colnames <- c("n_non_smoker", "n_smoker", "p_hat_non_smoker", "p_hat_smoker", "prop_diff")
  given_colnames <- colnames(dust_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(dust_summary))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(dust_summary$n_non_smoker))), "87f3407882014ba8f18016b8e408ad35")
    expect_equal(digest(as.integer(sum(dust_summary$n_smoker))), "76bb48cccd68153482cda2b8ebfecd93")
    expect_equal(digest(as.integer(sum(dust_summary$p_hat_non_smoker) * 10e6)), "e772aa3c54a729784d27e153931979c7")
    expect_equal(digest(as.integer(sum(dust_summary$p_hat_smoker) * 10e6)), "65105150a89cf0f1e8f0b93ae773058d")
    expect_equal(digest(as.integer(sum(dust_summary$prop_diff) * 10e6)), "2b59c114711ded35f1991bdb2cfe5562")
  })

  print("Success!")
}

test_3.2 <- function(){
   test_that('Did not assign answer to an object called "dust_summary"', {
    expect_true(exists("dust_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(dust_summary))
  })

  expected_colnames <- c("n_non_smoker", "n_smoker", "p_hat_non_smoker", "p_hat_smoker", "prop_diff", "null_std_error")
  given_colnames <- colnames(dust_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(dust_summary))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(dust_summary$n_non_smoker))), "87f3407882014ba8f18016b8e408ad35")
    expect_equal(digest(as.integer(sum(dust_summary$n_smoker))), "76bb48cccd68153482cda2b8ebfecd93")
    expect_equal(digest(as.integer(sum(dust_summary$p_hat_non_smoker) * 10e6)), "e772aa3c54a729784d27e153931979c7")
    expect_equal(digest(as.integer(sum(dust_summary$p_hat_smoker) * 10e6)), "65105150a89cf0f1e8f0b93ae773058d")
    expect_equal(digest(as.integer(sum(dust_summary$prop_diff) * 10e6)), "2b59c114711ded35f1991bdb2cfe5562")
  })

    test_that("Solution is incorrect", {
      expect_equal(digest(as.integer(sum(dust_summary$null_std_error) * 10e6)), "cc782cbe4d3b401bd627f6992b356310")
    })
    print("Success!")
 
}

test_3.4 <- function() {
    test_that('Did not assign answer to an object called "dust_prop_test"', {
        expect_true(exists("dust_prop_test"))
    })
    test_that("Solution should be the output of t.test", {
        expect_true("data.frame" %in% class(dust_prop_test))
    })
    test_that("Wrong statistic value", {
        expect_equal(digest(as.integer(dust_prop_test$statistic*10e6)), "250889ca4bea4d137b10164b5a1bb897")
    })

    test_that("Wrong p-value", {
        expect_equal(digest(as.integer(dust_prop_test$p.value*10e6)), "40a71fcefb374785bd9c9e10386e24e5")
    })
    test_that("Wrong estimate", {
        expect_equal(digest(as.integer(dust_prop_test$estimate1*10e6)), "65105150a89cf0f1e8f0b93ae773058d")
        expect_equal(digest(as.integer(dust_prop_test$estimate2*10e6)), "e772aa3c54a729784d27e153931979c7")
    })
    test_that("Wrong parameter", {
        expect_equal(digest(dust_prop_test$parameter), "fdc86b4d739d00b8ef1165920b3245e5")
    })
    test_that("Wrong alternative hypothesis", {
        expect_equal(digest(dust_prop_test$alternative), "d095e00cba6f80bf1d5ad5db0400f812")
    })

    print("Success!")
}

test_4.2 <- function() {
  test_that('Did not assign answer to an object called "bream_roach"', {
    expect_true(exists("bream_roach"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(bream_roach))
  })

  expected_colnames <- c("Species", "Length3")
  given_colnames <- colnames(bream_roach)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(bream_roach))), "8dcc9ab9194a962167360a4ff6bb6827")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(bream_roach$Length3))), "e318c05d590e603b5b57dd433d0c7c66")
  })

  print("Success!")
}

test_4.3 <- function() {
  test_that('Did not assign answer to an object called "bream_roach_summary"', {
    expect_true(exists("bream_roach_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(bream_roach_summary))
  })

  expected_colnames <- c("n_Bream", "n_Roach", "x_bar_Bream", "x_bar_Roach", "sd_Bream", "sd_Roach", "mean_diff")
  given_colnames <- colnames(bream_roach_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(bream_roach_summary))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(bream_roach_summary$n_Bream))), "20c9a920779e3feca5b4ed6948450f8a")
    expect_equal(digest(as.integer(sum(bream_roach_summary$n_Roach))), "be3c152f6f6bcd5f85f9e4cba49b1e48")
    expect_equal(digest(as.integer(sum(bream_roach_summary$x_bar_Bream) * 10e6)), "49dae440c406bba1b6d3365af624c22b")
    expect_equal(digest(as.integer(sum(bream_roach_summary$x_bar_Roach) * 10e6)), "c1b4a9c15eb1b94ef6812ed21b927135")
    expect_equal(digest(as.integer(sum(bream_roach_summary$sd_Bream) * 10e6)), "8dc7711d23aa7802eda3daff77a8df74")
    expect_equal(digest(as.integer(sum(bream_roach_summary$sd_Roach) * 10e6)), "4198de71b2de745e6c1a09b69cc12552")
    expect_equal(digest(as.integer(sum(bream_roach_summary$mean_diff) * 10e6)), "161a0344e19fbe9c576f83511f8558cf")
  })

  print("Success!")
}


test_4.4 <- function() {
  test_that('Did not assign answer to an object called "bream_roach_summary"', {
    expect_true(exists("bream_roach_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(bream_roach_summary))
  })

  expected_colnames <- c("n_Bream", "n_Roach", "x_bar_Bream", "x_bar_Roach", "sd_Bream", "sd_Roach", "mean_diff", "null_std_error")
  given_colnames <- colnames(bream_roach_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(bream_roach_summary))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(bream_roach_summary$n_Bream))), "20c9a920779e3feca5b4ed6948450f8a")
    expect_equal(digest(as.integer(sum(bream_roach_summary$n_Roach))), "be3c152f6f6bcd5f85f9e4cba49b1e48")
    expect_equal(digest(as.integer(sum(bream_roach_summary$x_bar_Bream) * 10e6)), "49dae440c406bba1b6d3365af624c22b")
    expect_equal(digest(as.integer(sum(bream_roach_summary$x_bar_Roach) * 10e6)), "c1b4a9c15eb1b94ef6812ed21b927135")
    expect_equal(digest(as.integer(sum(bream_roach_summary$sd_Bream) * 10e6)), "8dc7711d23aa7802eda3daff77a8df74")
    expect_equal(digest(as.integer(sum(bream_roach_summary$sd_Roach) * 10e6)), "4198de71b2de745e6c1a09b69cc12552")
    expect_equal(digest(as.integer(sum(bream_roach_summary$mean_diff) * 10e6)), "161a0344e19fbe9c576f83511f8558cf")
  })

  print("Success!")
}

test_4.5 <- function() {
  test_that('Did not assign answer to an object called "bream_roach_summary"', {
    expect_true(exists("bream_roach_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(bream_roach_summary))
  })

  expected_colnames <- c("n_Bream", "n_Roach", "x_bar_Bream", "x_bar_Roach", "sd_Bream", "sd_Roach", "mean_diff", "null_std_error", "p_value")
  given_colnames <- colnames(bream_roach_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(bream_roach_summary))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(bream_roach_summary$n_Bream))), "20c9a920779e3feca5b4ed6948450f8a")
    expect_equal(digest(as.integer(sum(bream_roach_summary$n_Roach))), "be3c152f6f6bcd5f85f9e4cba49b1e48")
    expect_equal(digest(as.integer(sum(bream_roach_summary$x_bar_Bream) * 10e6)), "49dae440c406bba1b6d3365af624c22b")
    expect_equal(digest(as.integer(sum(bream_roach_summary$x_bar_Roach) * 10e6)), "c1b4a9c15eb1b94ef6812ed21b927135")
    expect_equal(digest(as.integer(sum(bream_roach_summary$sd_Bream) * 10e6)), "8dc7711d23aa7802eda3daff77a8df74")
    expect_equal(digest(as.integer(sum(bream_roach_summary$sd_Roach) * 10e6)), "4198de71b2de745e6c1a09b69cc12552")
    expect_equal(digest(as.integer(sum(bream_roach_summary$mean_diff) * 10e6)), "161a0344e19fbe9c576f83511f8558cf")
    expect_equal(digest(as.integer(sum(bream_roach_summary$p_value) * 10e6)), "1473d70e5646a26de3c52aa1abd85b1f")
  })

  print("Success!")
}

test_4.6 <- function() {
    test_that('Did not assign answer to an object called "bream_roach_t_test"', {
        expect_true(exists("bream_roach_t_test"))
    })
    test_that("Solution should be the output of t.test", {
        expect_true("data.frame" %in% class(bream_roach_t_test))
    })
    test_that("Wrong statistic value", {
        expect_equal(digest(as.integer(bream_roach_t_test$statistic*10e6)), "20c12978dfb6845dce123b45ec38e0a4")
    })

    test_that("Wrong p-value", {
        expect_equal(digest(as.integer(bream_roach_t_test$p.value*10e6)), "1473d70e5646a26de3c52aa1abd85b1f")
    })
    test_that("Wrong estimate", {
        expect_equal(digest(as.integer(bream_roach_t_test$estimate*10e6)), "161a0344e19fbe9c576f83511f8558cf")
        expect_equal(digest(as.integer(bream_roach_t_test$estimate1*10e6)), "49dae440c406bba1b6d3365af624c22b")
        expect_equal(digest(as.integer(bream_roach_t_test$estimate2*10e6)), "c1b4a9c15eb1b94ef6812ed21b927135")
    })
    test_that("Wrong parameter", {
        expect_equal(digest(as.integer(bream_roach_t_test$parameter*10e6)), "f465154f6fe80717d253f2d581c25654")
    })
    test_that("Wrong alternative hypothesis", {
        expect_equal(digest(bream_roach_t_test$alternative), "d095e00cba6f80bf1d5ad5db0400f812")
    })

    print("Success!")
}

test_5.1 <- function() {
  test_that('Did not assign answer to an object called "soybean_summary"', {
    expect_true(exists("soybean_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(soybean_summary))
  })

  expected_colnames <- c("n", "d_bar", "sd", "std_error")
  given_colnames <- colnames(soybean_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(soybean_summary))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(soybean_summary$n))), "8ae1ac7bdf62dca7c19b427a9153445c")
    expect_equal(digest(as.integer(sum(soybean_summary$d_bar) * 10e6)), "68917e2e9f740c4888443459ab16c9cb")
    expect_equal(digest(as.integer(sum(soybean_summary$sd) * 10e6)), "58ed9f7bf746d4d1c59414a2c1acb0fd")
    expect_equal(digest(as.integer(sum(soybean_summary$std_error) * 10e6)), "f31ee44ef1c9bfc2daba62973a64e4e5")

  })

  print("Success!")
}

test_5.3 <- function() {
    test_that('Did not assign answer to an object called "soybean_t_test"', {
        expect_true(exists("soybean_t_test"))
    })
    test_that("Solution should be the output of t.test", {
        expect_true("data.frame" %in% class(soybean_t_test))
    })
    test_that("Wrong statistic value", {
        expect_equal(digest(as.integer(soybean_t_test$statistic*10e6)), "b7c57bf4673b8239d82326fa658ff11d")
    })

    test_that("Wrong p-value", {
        expect_equal(digest(as.integer(soybean_t_test$p.value*10e6)), "c01f179e4b57ab8bd9de309e6d576c48")
    })
    test_that("Wrong estimate", {
        expect_equal(digest(as.integer(soybean_t_test$estimate*10e6)), "68917e2e9f740c4888443459ab16c9cb")
    })
    test_that("Wrong parameter", {
        expect_equal(digest(as.integer(soybean_t_test$parameter)), "fa5a4df7ac0f9782037da890557fd8b8")
    })
    test_that("Wrong alternative hypothesis", {
        expect_equal(digest(soybean_t_test$alternative), "d095e00cba6f80bf1d5ad5db0400f812")
    })

    print("Success!")
}
