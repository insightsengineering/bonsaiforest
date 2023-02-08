#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
List surv_prob(arma::mat K, arma::mat H){
  NumericMatrix surv(H.n_rows, H.n_cols);
  for (int i = 0; i < H.n_cols; i++){
    arma::mat x1 = K.col(i)*(H.col(i).t());
    arma::mat x2 = exp(x1);
    NumericMatrix x3 = wrap(x2);
    surv(_,i) = colMeans(x3);
  }
  return List::create(surv);
}

