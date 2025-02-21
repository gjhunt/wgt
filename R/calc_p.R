#' Efficient calcuation of regularized empirical inverse quantiles for each data point.
#' @param x sorted values
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




old_calc_p <- function(x_sort, count = FALSE) {
    j <- 1
    i <- 1
    p_count <- rep(NA, length(x_sort))
    for (i in 1:length(x_sort)) {
        if (j <= length(x_sort))
            while (x_sort[j] <= x_sort[i]) {
                j <- j + 1
                if (j > length(x_sort))
                  break
            }

        p_count[i] <- j - 1
    }

    if (count)
        return(p_count)


    p <- p_count/length(p_count)
    return(p)
}
