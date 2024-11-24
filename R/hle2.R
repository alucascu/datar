#' Hodges-Lehmann Estimate of Shift 
#' 
#' @description
#' `outer_hle2` computes the Hodges-Lehmann Shift Estimator of two vectors
#' 
#' @details 
#' This particular implementation of hle2 is computed using the R Core `outer`
#' function. Outer is less performant when compared to the C++ method. 
#' The estimate should be roughly equivalent, though.
#'  
#' @param x Numeric Vector containing the first set of data
#' 
#' @param y Numeric Vector containing the second set of data
#' 
#' @returns Numeric Estimate of the shift of location parameter
#' 
#' @examples
#' x <- rnorm(100, 10) # Random Normal with Location Parameter of 10
#' y <- rnorm(100, 20) # Random Normal with Location Parameter of 20
#' outer_hle2(x, y)
#' 
#' @seealso [cpp_hle2()] The faster, C++, implementation of this function
#' 
#' @references 
#' DATA 495 Lectures at Northern Michigan University by John Kloke  

outer_hle2 <- function(x, y) {
    outer_vec <- outer(y, x, "-")
    return(median(outer_vec))
}
