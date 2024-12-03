#' Hodges-Lehmann Estimate of Location
#'
#' @description
#' `outer_hle1` computes the Hodges-Lehmann Location Estimator of a Sample
#'
#' @details
#' This particular implementation of hle1 is computed using the R Core `outer`
#' function. Outer is less performant when compared to the C++ method.
#' The estimate should be roughly equivalent, though.
#'
#' @param x Numeric Vector containing the sample
#'
#'
#' @returns Numeric Estimate of the location parameter
#'
#' @examples
#' library(datar)
#' x <- rnorm(100, 10) # Random Normal with Location Parameter of 10
#' outer_hle1(x)
#'
#' @seealso [cpp_hle1()] The faster, C++, implementation of this function
#'
#' @references
#' DATA 495 Lectures at Northern Michigan University by John Kloke
#' @importFrom stats median
#' @export
outer_hle1 <- function(x) {
    outer_vec <- outer(x, x, "+") / 2
    tri <- outer_vec[upper.tri(outer_vec)]
    return(median(tri))
}
