#include <RcppArmadillo.h>
using namespace Rcpp;

//' A fast implementation of weighted linear regression based on the similar RcppArmadillo implementation.
//'
//' @param X A matrix of independent variables
//' @param y A vector representing the resulting dependent variable
//' @param w A vector of weights of equal length as the number of observations of x and y given to the model.
//' @export
// [[Rcpp::export]]
List fastLm_impl(const arma::mat& X, const arma::colvec& y, const arma::colvec& w) {
  int n = X.n_rows, k = X.n_cols;

  arma::mat w_matrix = arma::diagmat(w);
  arma::colvec coef = arma::inv(arma::trans(X) * w_matrix * X)*arma::trans(X)*w_matrix*y;   // fit model y ~ X
  arma::colvec res  = y - X*coef;           // residuals

  // std.errors of coefficients
  double s2 = std::inner_product(res.begin(), res.end(), res.begin(), 0.0)/(n - k);

  arma::colvec std_err = arma::sqrt(s2 * arma::diagvec(arma::pinv(arma::trans(X)*X)));

  return List::create(Named("coefficients") = coef,
                      Named("stderr")       = std_err,
                      Named("df.residual")  = n - k);
}