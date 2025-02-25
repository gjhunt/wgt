test_that("basic-logn-calcp", {
    N <- 500
    x <- c(rnorm(N, -3/2, 1/3), rnorm(N, 3/2, 1/3))
    x <- exp(x)
    x <- c(rep(0, length(x)), x)

    expect_snapshot(cran = FALSE, wgt::calc_p(x))
})
