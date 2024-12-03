library(boot)


#' Bootstrap Function
#'
#' @description
#' `boot_r` computes the bootstrap estimate from a function
#'
#' @details
#' `boot_r` is a student-written bootstrap function which computes the bootstrap
#' estimate of a given statistic
#'
#' @param x Numeric Vector of observations
#'
#' @param func Statistic to compute the bootstrap estimate of
#'
#' @param n The number of samples in each bootstrap sample
#'
#' @param B The number of replicatates to take
#'
#' @returns List of the numeric estimate of the statistic and the 95%
#'  confidence intervals
#'
#' @examples{
#' library(datar)
#' x <- rnorm(100, 10) # random normal distribution with mean = 10
#' boot_r(x, mean, 10) # use a bootstrap approximation of the mean
#' }
#'
#' @references
#' DATA 495 Lectures at Northern Michigan University by John Kloke
#'
#' @importFrom stats quantile
#' @export
boot_r <- function(x, func, n, B = 1001) {
    func <- match.fun(func)

    boot_mat <- matrix(replicate(B, sample(x, size = n, replace = TRUE)), nrow = B, ncol = n)
    boot_mat_func <- apply(boot_mat, 1, func)
    q <- quantile(boot_mat_func, c(0.025, .975))
    estimate <- mean(boot_mat_func)

    return(list(q, estimate))
}


#' Mean function
#'
#' @description
#' `means` computes the mean of a numeric vector
#'
#' @details
#' `means` exists only to simplify the passing of the mean statistic into the
#' `boot_boot` function.
#'
#' @param x Numeric Vector of observations
#'
#' @returns The numeric mean of the given vector
#'
#' @examples{
#' library(datar)
#' x <- rnorm(100, 10) # random normal distribution with mean = 10
#' means(x) # compute the mean
#' }
#'
#' @references
#' DATA 495 Lectures at Northern Michigan University by John Kloke
#' @export
means <- function(x) {
    return(mean(x, trim = 0))
}

#' Boot package Bootstrap Function
#'
#' @description
#' `boot_boot` computes the bootstrap estimate from a function
#'
#' @details
#' `boot_r` is a student-written bootstrap function based on the boot package
#' which computes the bootstrap estimate of a given statistic
#'
#' @param x Numeric Vector of observations
#'
#' @param func Statistic to compute the bootstrap estimate of
#'
#' @param B The number of replicatates to take
#'
#' @returns List of the numeric estimate of the statistic and the 95%
#'  confidence intervals
#'
#' @examples{
#' library(datar)
#' x <- rnorm(100, 10) # random normal distribution with mean = 10
#' boot_boot(x) # use a bootstrap approximation of the mean
#' }
#'
#' @references
#' DATA 495 Lectures at Northern Michigan University by John Kloke
#' @importFrom boot boot
#' @export
boot_boot <- function(x, func = means, B = 1001) {
    return(boot(x, func, R = B, sim = "parametric"))
}
