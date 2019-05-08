library(hips)
library(testthat)
context("cohort stratified partitioning runs with built-in pre- and post-conditions")

test_that("mtcars can be partitioned by transmission type", {
    expect_is(StratifiedPartition(datasets::mtcars, grp_col="am", target_col="mpg"), "data.frame")
})
