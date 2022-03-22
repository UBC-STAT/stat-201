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

test_1.1 <- function(){
    test_that("recap 1", {
      expect_identical(digest(as.numeric(recap1)), "c72d0198505540505acfb4b2b49911e6")
    })
    cat("success!")
}

test_1.2 <- function(){
    test_that("recap 2", {
        expect_identical(digest(as.numeric(recap2)), "2522027d230e3dfe02d8b6eba1fd73e1")
    })
    cat("success!")
}

test_2.1 <- function(){
    test_that("null hypothesis", {
        hashed_answer <- digest(tolower(null_hypothesis))
        if (hashed_answer == "ddf100612805359cd81fdc5ce3b9fbba") {
            cat("This would result in 15 separate tests -- we're looking for a single test.")
        } else if (hashed_answer == "6e7a8c1c098e8817e3df3fd1b21149d1") {
            cat("The grouping variable has no inherent order, so there's no such thing as a trend.")
        }
        expect_identical(hashed_answer, "127a2ec00989b9f7faf671ed470be7f8")
    })
    cat("success!")
}

test_2.2 <- function(){
    test_that("attitude group variances are correct", {
      standardized_answer <- attitude_variances %>% 
        mutate(variance = round(variance, digits = 4)) %>% 
        arrange(question) %>% 
        unclass()
      expect_identical(
        digest(standardized_answer), 
        "f236ecf1d8852cf445dc3e5085d1414f"
      )
    })
    cat("success!")
}

test_2.3 <- function(){
    test_that("Variance call is correct", {
      answer_hash <- digest(tolower(variance_call))
      if (answer_hash == "127a2ec00989b9f7faf671ed470be7f8") {
        cat("It's not good language to 'assume' equal variances, ",
            "since the variances are almost surely at least a little different.")
      }
      expect_identical(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
    })
    cat("success!")
}

test_2.4 <- function(){
    test_that("variance calculations", {
        overall <- digest(round(variance_overall, digits = 2))
        group_avg <- digest(round(variance_group_avg, digits = 2))
        ratio <- digest(round(variance_ratio, digits = 2))
        expect_identical(overall, "6363a1d1005026d0626f68f768ad1ecc")
        expect_identical(group_avg, "f4e70e61a8f13b7409b2b296a1d1d386")
        expect_identical(ratio, "1088ff6cbfbfe0ca38e6d5b79804c749")
    })
    cat("success!")
}

test_2.5 <- function(){
    test_that("Question 2.6", {
      expect_identical(
        digest(round(f_stat, digits = 4)),
        "201c106c767545f7ae0f82bf91390fe4"
      )
      expect_identical(
        digest(signif(anova_pval, digits = 4)),
        "85048b8d9987b60a8ff053eccf2128b1"
      )
    })
    cat("success!")    
}

test_2.6 <- function(){
    test_that("ANOVA conclusion", {
      answer_hash <- digest(tolower(anova_conclusion))
      if (answer_hash == "93a9078c6326f37b481d3e99b60ad987") {
        cat("The 'spacing' and ordering of questions is arbitrary,",
            "so there's no way we can talk about direction and shape",
            "of a relationship.")
      }
      expect_identical(answer_hash, "d110f00cfb1b248e835137025804a23b")
    })
    cat("success!")    
}

test_3.1 <- function(){
    test_that("One-Family Dwelling PI", {
      expect_identical(
        digest(round(unname(dwelling_pi), digits = 2)),
        "ebbcb0aa1dd53b57e2da4ca96cfd46d3"
      )
    })
    cat("success!")    
}

test_3.2 <- function(){
    test_that("Realities of smaller coverage probability", {
      answer_hash <- digest(tolower(interval_trends))
      expect_identical(answer_hash, "95767987b2037a2f09c4e5c0997ec206")
    })
    cat("success!")    
}
test_3.3 <- function(){
    test_that("Proportion in prediction interval", {
      expect_identical(
        digest(round(dwellings_in_pi, digits = 4)),
        "e2ad489efa88de3347f33df53dca0e44"
      )
    })
    cat("success!")    
}

test_3.4 <- function(){
    test_that("Sample coverages", {
      expect_identical(
        digest(round(unname(sort(coverage)), digits = 4)),
        "a87fa2047aca2dd118f118b0b49dd3e3"
      )
    })
    cat("success!")    
}


test_3.5 <- function(){
    test_that("Probability of smaller coverage", {
      expect_identical(
        digest(round(prob_smaller, digits = 3)),
        "4b2646cfd98fd99bc210dbf47d832ffd"
      )
    })
    cat("success!")   
}

test_3.6 <- function(){
    test_that("Realities of smaller coverage probability", {
      answer_hash <- digest(tolower(realities))
      expect_identical(answer_hash, "7ecaefcc6ebc9848e7cb04b5c783ae0a")
    })
    cat("success!")    
}
