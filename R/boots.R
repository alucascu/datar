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


bs_samp <- function(x, n) sample(x, n, replace = TRUE)

bs_samps <- function(x, n, B) map2(x, n, \(x, n) matrix(replicate(B, bs_samp(x, n)), ncol = n, nrow = B))


#' Dual Boot
#'
#' @description
#' 'dual_boot' is a function which performs a bootstrap on a function which takes two inputs
#'
#' @details
#' While I believe it to be feasible to simply pass the second arg as as `...` arg, I find this solution
#' unsatisfying. To remedy this, this bootstrap function introduces a new package dependency of purrr.
#' Aside from maybe being a touch faster, the map functions provide exceedingly good readability in my mind.
#'
#' @param x List of observations
#'
#' @param y Second List of observations
#'
#' @param fun Character vector of function to compute the desired statistic
#'
#' @param n Size of a single bootstrap sample
#'
#' @param B Number of boostrap samples to take
#'
#' @returns Dataframe containing the estimate and the confidence intervals
#'
#' @examples{
#' library(datar)
#' library(purrr)
#' # Create a list of randomly generated normal samples of different sizes at mean_x = 5, mean_y = 10
#' d_x_norm <- map(1:3, \(x) 10^x) |> map(\(y) rnorm(y, 5))
#' d_y_norm <- map(1:3, \(x) 10^x) |> map(\(y) rnorm(y, 10))
#'
#' dual_boot(d_x_norm, d_y_norm, "cpp_hle2")
#' }
#'
#' @references
#' DATA 495 lectures with John Kloke
#'
#'
#' @export
dual_boot <- function(x, y, fun, n = map2(x, y, \(xs, ys) min(length(xs), length(ys))), B = 1001) {
  func <- match.fun(fun)

  bs_samps_x <- bs_samps(x, n, B)
  bs_samps_y <- bs_samps(y, n, B)

  if (fun != "diff_means") {
    out <- map2(bs_samps_x, bs_samps_y, \(xs, ys) map2(array_branch(xs, 1), array_branch(ys, 1), func))
    est <- map(out, \(x1) unlist(x1, recursive = FALSE) %>% mean())
    qqs <- map(out, \(x1) quantile(unlist(x1, recursive = FALSE), c(0.025, .975)))
    names(qqs) <- NULL

    res <- map2(est, qqs, \(x, y) c(upper = y[2], estimate = x, lower = y[1]))
  } else {
    res <- map2(bs_samps_x, bs_samps_y, func)
  }

  return(res)
}


#' gen_dist
#'
#' @description Generate a list of disributions of varying sizes
#'
#' @details
#' This helper function is primarily for testing. However, I found this family of helper functions rather useful
#' so I chose to export them. 'gen_dist' is especially powerful when running bootstraps using the 'dual_boot' function
#' as it outputs the samples in the format needed by that function.
#'
#' @param ns A numeric vector of the sizes of each sample **on a log 10 scale**
#'
#' @param mean A numeric representing the desired mean location
#'
#' @param dist String with name of the requested distribution. Available values are "logistic", "normal" and "exp"
#'
#' @param scale Numeric representing scale parameter. Only used for the logistic distibution
#'
#' @returns A list of samples
#'
#' @examples{
#' # We want samples of size 10, 100 and 1000
#' ns <- c(1, 2, 3)
#' gen_dist(ns, mean = 1, dist = "normal")
#' }
#'
#' @export
gen_dist <- function(ns, mean, dist = c("normal", "logistic", "exp"), scale = 1) {
  dist <- match.arg(dist)
  ns <- map(ns, \(x) 10^x)
  switch(dist,
    normal = {
      map(ns, \(x) rnorm(x, mean))
    },
    logistic = {
      map(ns, \(x) rlogis(x, mean, scale))
    },
    exp = {
      map(ns, \(x) rexp(x, 1 / mean))
    }
  )
}

#' diff_means
#'
#' @description Calculate an estimate of the shift of location parameter using difference of means
#'
#' @param x Numeric vector of samples
#'
#' @param y Numeric vector of samples
#'
#' @returns List of containing estimate and confidence intervals
#'
#' @examples{
#' x <- rnorm(100, 5)
#' y <- rnorm(100, 10)
#' diff_means(x, y)
#' }
#'
#' @export
diff_means <- function(x, y) {
  estimates <- mean(y) - mean(x)
  ses <- (c(-1, 1) * 2.037 * (2 * (sd(y)^2 + sd(x)^2) / 2) / sqrt(length(x) + length(y)))
  return(c(upper = estimates + ses[2], estimate = estimates, lower = estimates + ses[1]))
}
