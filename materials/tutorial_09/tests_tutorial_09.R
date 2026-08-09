library(digest)
library(testthat)


## Question 1.3
test_1.3 <- function(){
    test_that("Amount significant", {
        expect_identical(
            digest(signif(sort(pval_bh), digits = 4)),
            "40cdfc2e1720e67f3aae56823ec9ed9e"
        )
        expect_identical(
            digest(as.numeric(count_bh)),
            "5e338704a8e069ebd8b38ca71991cf94"
        )
    })
cat("success!")
}


test_2.1 <- function(){
    test_that("Mean prediction of Raises", {
        expect_identical(
            digest(round(prediction_raises, digits = 4)),
            "dd346a8cabe03648836f07864372d5f5"
        )
    })
    cat("success!")
}

test_2.3 <- function(){
    test_that("Prediction Critical is right", {
        answer_hash <- digest(tolower(prediction_critical))
        expect_identical(answer_hash, "d110f00cfb1b248e835137025804a23b")
    })
    cat("success!")
}

test_2.4 <- function(){
    test_that("Prediction decisions is/are right", {
        answer_hash <- digest(tolower(prediction_decisions))
        expect_identical(answer_hash, "7ecaefcc6ebc9848e7cb04b5c783ae0a")
    })
    cat("success!")
}

test_3.1 <- function(){
    test_that('Did not assign answer to an object called "two_t_equal_tidy"', {
        expect_true(exists("two_t_equal_tidy"))
    })
    
    test_that("Solution should be a tbl", {
        expect_true("tbl" %in% class(two_t_equal_tidy))
    })
    
    test_that("Wrong statistic value", {
        expect_equal(digest(as.integer(two_t_equal_tidy$statistic*10e6)), "d11a34a3f79ac44d2005af0e8223eff8")
    })
    
    test_that("Wrong p-value", {
        expect_equal(digest(as.integer(two_t_equal_tidy$p.value*10e6)), "f3c042ceddbfd206b5b9cd5bc4d29abd")
    })
    
    test_that("Wrong estimate", {
        expect_equal(digest(as.integer(two_t_equal_tidy$estimate*10e6)), "bbbb689dbc76366408d10ec00d258660")
    })
    
    test_that("Wrong parameter", {
        expect_equal(digest(two_t_equal_tidy$parameter), "d44294f7a737fc50e075e5a28ef01cbd")
    })
    
    test_that("Wrong alternative hypothesis", {
        expect_equal(digest(two_t_equal_tidy$alternative), "d095e00cba6f80bf1d5ad5db0400f812")
    })
    
    print("Success!")
}
    
test_3.2 <- function(){
    test_that('Did not assign answer to an object called "two_t_unequal_tidy"', {
        expect_true(exists("two_t_unequal_tidy"))
    })
    
    test_that("Solution should be a tbl", {
        expect_true("tbl" %in% class(two_t_unequal_tidy))
    })
    
    test_that("Wrong statistic value", {
        expect_equal(digest(as.integer(two_t_unequal_tidy$statistic*10e6)), "d11a34a3f79ac44d2005af0e8223eff8")
    })
    
    test_that("Wrong p-value", {
        expect_equal(digest(as.integer(two_t_unequal_tidy$p.value*10e6)), "ddea8d1dfdec1052092ff20d4a6d5e16")
    })
    
    test_that("Wrong estimate", {
        expect_equal(digest(as.integer(two_t_unequal_tidy$estimate*10e6)), "bbbb689dbc76366408d10ec00d258660")
    })
    
    test_that("Wrong parameter", {
        expect_equal(digest(two_t_unequal_tidy$parameter), "3821fcaf9325ef3bdfc3d01e79905de4")
    })
    
    test_that("Wrong alternative hypothesis", {
        expect_equal(digest(two_t_unequal_tidy$alternative), "d095e00cba6f80bf1d5ad5db0400f812")
    })
    
    print("Success!")
}

test_3.4 <- function(){
    test_that("The value for the two-sample t-test with equal variance is incorrect.", {
        expect_identical(
            digest(round(two_t_equal, digits = 6)), 
            "77710c78ca705287d335be9ac2ca8d86"
        )
    })
    test_that("The value for the two-sample t-test with unequal variance is incorrect.", {
        expect_identical(
            digest(round(two_t_unequal, digits = 6)), 
            "5e8f21b565d74ce485c5852857bf2786"
        )
    })
    test_that("The value for the ANOVA is incorrect.", {
        expect_identical(
            digest(round(two_anova, digits = 6)), 
            "77710c78ca705287d335be9ac2ca8d86"
        )
    })
    cat("success!")
}


