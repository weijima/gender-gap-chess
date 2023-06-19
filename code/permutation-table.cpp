#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double top10(NumericVector v) {
  NumericVector y = clone(v);
  std::sort(y.begin(), y.end());
  return mean(tail(y, 10));
}


// [[Rcpp::export]]
NumericVector permut_tab(NumericVector x, NumericVector y, Function f, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny;
  NumericVector pool(n), randx(nx), randy(ny), vals(perms);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    vals[i] = as<double>(f(randx)) - as<double>(f(randy));
  }
  return vals;
}
