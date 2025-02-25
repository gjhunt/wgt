test_that("basic-logn-estimate", {
    N <- 500
    x <- c(rnorm(N, -3/2, 1/3), rnorm(N, 3/2, 1/3))
    x <- exp(x)

    expect_snapshot(cran = FALSE, wgt::estimate(x, eta = 3/4))
})
