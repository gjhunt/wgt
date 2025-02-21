#' Efficient calcuation of regularized empirical inverse quantiles for each data point.
#' @param x sorted values
#' @param MX -- regularization limit
#' @export
calc_p <- function(x, MX = 1) {

    ord <- order(x)
    oord <- order(ord)

    xs <- x[ord]
    xsu <- unique(xs)

    val <- 0
    p_out <- rep(NA, length(xs))

    for (i in 1:length(xsu)) {
        idx <- which(xs == xsu[i])
        add_val <- min(length(idx), MX)
        val <- val + add_val
        p_out[idx] <- val
    }

    p_out <- p_out/max(p_out)
    p_ret <- p_out[oord]

    return(p_ret)
}
