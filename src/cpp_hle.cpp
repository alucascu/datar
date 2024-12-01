#include <iostream>
#include <vector>
#include <algorithm>
#include <Rcpp.h>

//' cpp_median
//' 
//' @description
//' Find the median of a numeric vector
//' 
//' @details
//' Using the c++ function `nth_element`, this function places the element which,
//' if the vector were sorted, would be in the middle position and then returns that same element
//' 
//' @param vec The numeric vector of which to take the median
//' 
//' @returns Numeric of median
//'
//' @examples{
//' x <- rnorm(100, 10) 
//' cpp_median(x)
//' }
//' @references
//' DATA495 lectures with Prof. Kloke. 
//' 
//' @aliases Median
// [[Rcpp::export]]
double cpp_median(std::vector<double> vec){
    if(vec.size() % 2 == 1){
        size_t n = vec.size() / 2;
        nth_element(vec.begin(), vec.begin()+n, vec.end());
        return vec[n];
        }
    else{
        size_t n = vec.size() / 2;
        nth_element(vec.begin(), vec.begin()+n, vec.end());
        nth_element(vec.begin(), vec.begin()+(n + 1), vec.end());
        return (vec[n + 1] + vec[n]) / 2;
    }
}


//' cpp_hle1
//' 
//' @description
//' Compute the Hodges-Lehmann estimate of location
//' 
//' @details
//' Using the function `cpp_median` this function computes the Hodges-Lehmann estimator of location.
//' This function cannot allocate a vector of the correct size if the number of measurements exceeds 
//' 40,000 and so, throws an error. 
//' 
//' @param vec The numeric vector of which find the Hodges-Lehmann location estimator
//' 
//' @returns Numeric representing the Hodges-Lehmann location estimator
//'
//' @examples{
//' x <- rnorm(100, 10) 
//' cpp_hle1(x)
//'}
//' @references
//' DATA495 lectures with Prof. Kloke. 
//' 
//' @seealso [outer_hle1()] The base-R implementation of HLE1
//'
//' @aliases HLE1
// [[Rcpp::export]]
double cpp_hle1(std::vector<double> vec){
    if(vec.size() > 40000){
        throw std::invalid_argument( "Cannot compute HLE1 on vector of size > 40000" );
    }
	int len = vec.size();

	std::vector<double> avgs(len * (len - 1) / 2);
	for(auto i = 0; i < len; i++){
		for(auto j = 0; j < len; j++){
			avgs[j + (i * len * (len - 1)) / 2] = (vec[j] + vec[i]) / 2;		
		}	
	}
	return cpp_median(avgs);
}

//' cpp_hle2
//' 
//' @description
//' Compute the Hodges-Lehmann estimate of shift
//' 
//' @details
//' Using the function `cpp_median` this function computes the Hodges-Lehmann estimator of shift.
//' This function cannot allocate a vector of the correct size if the number of measurements exceeds 
//' 40,000 and so, throws an error. 
//' 
//' @param x Numeric vector of measurements from the first population
//' 
//' @param y Numeric vector of measurements from the second population
//' 
//' @returns Numeric representing the Hodges-Lehmann estimator of shift
//'
//' @examples{
//' x <- rnorm(100, 10) 
//' y <- rnorm(100, 5) 
//' cpp_hle2(x)
//'}
//' @references
//' DATA495 lectures with Prof. Kloke. 
//' 
//' @seealso [outer_hle2()] The base-R implementation of HLE2
//'
//' @aliases HLE2
// [[Rcpp::export]]
double cpp_hle2(std::vector<double> x, std::vector<double> y){
	int xLen = x.size();
	int yLen = y.size();

    if(xLen > 40000 || yLen > 40000){
	    throw std::invalid_argument( "Cannot compute HLE2 on vector of size > 40000" );
    }


    std::vector<double> diffs(xLen * yLen);

    for(auto i = 0; i < xLen; i++){
        for(auto j = 0; j < yLen; j++){
            diffs[j + i * xLen] = y[j] - x[i];		
        }	
    }
    return cpp_median(diffs);
}
