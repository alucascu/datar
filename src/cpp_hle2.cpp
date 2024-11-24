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
double matMedian(NumericMatrix mat){
    NumericVector vec(mat.cols() * mat.rows());
    for(int i = 0; i < mat.cols(); ++i){
        for(int j = 0; i < mat.rows(); ++j){
            vec[i * mat.cols() + j] = mat(j, i);
        }
    }
    return median(vec);
}

// [[Rcpp::export]]
double cpp_hle2(NumericVector x, NumericVector y){
    int ylen = y.length();
    int xlen = x.length();


    NumericMatrix diff_mat (ylen, xlen);
    for(int i = 0; i < ylen; ++i){
        for(int j = 0; j < xlen; ++j){
            diff_mat(i, j) = y[i] - x[j];
        }
    }
    NumericVector diff_vec = diff_mat.import(diff_mat.begin(), diff_mat.end());

    return median(diff_vec);
}
