#' @export
estimate <- function(x, theta = NULL, eta = NULL) {

    # calculate F_hat(x_n) F_vals <- sapply(x, function(xx) mean(unique(x) <=
    # xx))
    F_vals <- calc_p(x)

    # set value for theta
    s <- stats::sd(F_vals)/stats::sd(x)
    r <- stats::cor(x, F_vals)

    choose_theta <- function(eta) {
        if (r == 1)
            return(0)
        nu <- 1 - (1 - r) * (1 - eta)^2

        numer <- nu * s * sqrt((nu^2 - 1) * (r^2 - 1)) + nu^2 * (r * s - 1) + r *
            (r - s)
        denomer <- nu^2 * (2 * r * s - s^2 - 1) + (r - s)^2
        theta_star <- numer/denomer

        theta_star <- max(theta_star, 0)
        theta_star <- min(theta_star, 1)

        return(theta_star)
    }

    if (is.null(theta))
        theta <- choose_theta(eta)

    y <- (1 - theta) * x + theta * F_vals

    return(list(y = y, theta = theta, F = F_vals, eta = eta, theta_fn = choose_theta))
}

