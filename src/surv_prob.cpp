#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// [[Rcpp::export]]
List surv_prob(arma::mat k, arma::mat h){
  NumericMatrix surv(h.n_rows, h.n_cols);
  for (int i = 0; i < h.n_cols; i++){
    arma::mat x1 = k.col(i)*(h.col(i).t());
    arma::mat x2 = exp(x1);
    NumericMatrix x3 = wrap(x2);
    surv(_,i) = colMeans(x3);
  }
  return List::create(surv);
}
