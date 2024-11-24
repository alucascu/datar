#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

class empty_vector:public std::exception{
    virtual const char* what() const throw() {
        return "The median of an empty list is undefined.";
    }
};

// [[Rcpp::export]]
double median(NumericVector vec){
    int len = vec.length();

    if(len == 0){
        throw empty_vector();
    }

    int middle = len / 2;
    std::nth_element(vec.begin(), vec.begin() + middle, vec.end());
    return *(vec.begin() + middle);
}

// [[Rcpp::export]]
double cpp_hle2(NumericVector x, NumericVector y){
    int ylen = y.length();
    int xlen = x.length();

    // Create Matrix from averages
    // Consider computing a vector with 1 element then entering a for loop with
    // push_back()
    NumericMatrix diff_mat (ylen, xlen);
    for(int i = 0; i < ylen; ++i){
        for(int j = 0; j < xlen; ++j){
            diff_mat(i, j) = y[i] - x[j];
        }
    }   
    // Convert the matrix to a vector
    NumericVector diff_vec = diff_mat.import(diff_mat.begin(), diff_mat.end());
    return median(diff_vec);
}


// [[Rcpp::export]]
double cpp_hle1(NumericVector x){
    int len = x.length();
    double lend = x.length();
    // Create Matrix from differences
    // Consider computing a vector with 1 element then entering a for loop with
    // push_back()
    NumericVector avg_vec(lend * (lend - 1) / 2);
    for(int j = 1; j < len; j++){
        for(int i = 0; i < (j); i ++){
            avg_vec[i + (j * (j - 1)) / 2] = (x[i] + x[j]) / 2;
        }
    }
    return median(avg_vec);
}
