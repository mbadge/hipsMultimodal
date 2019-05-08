library(hips)
library(testthat)
context("hipsOpt() fetches or displays pkg options")

test_that("hipsOpt() fetches requested options or displays available options", {
    expect_output(hipsOpt(), "hips pkg options:")
    expect_is(hipsOpt(targets), "character")
})
