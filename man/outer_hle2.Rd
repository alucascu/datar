% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hle2.R
\name{outer_hle2}
\alias{outer_hle2}
\title{Hodges-Lehmann Estimate of Shift}
\usage{
outer_hle2(x, y)
}
\arguments{
\item{x}{Numeric Vector containing the first set of data}

\item{y}{Numeric Vector containing the second set of data}
}
\value{
Numeric Estimate of the shift of location parameter
}
\description{
\code{outer_hle2} computes the Hodges-Lehmann Shift Estimator of two vectors
}
\details{
This particular implementation of hle2 is computed using the R Core \code{outer}
function. Outer is less performant when compared to the C++ method.
The estimate should be roughly equivalent, though.
}
\examples{
library(datar)
x <- rnorm(100, 10) # Random Normal with Location Parameter of 10
y <- rnorm(100, 20) # Random Normal with Location Parameter of 20
outer_hle2(x, y)

}
\references{
DATA 495 Lectures at Northern Michigan University by John Kloke
}
\seealso{
\code{\link[=cpp_hle2]{cpp_hle2()}} The faster, C++, implementation of this function
}
