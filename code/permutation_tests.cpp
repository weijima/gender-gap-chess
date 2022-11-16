#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
double permtest(NumericVector x, NumericVector y, Function fun, int perms = 10000) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny, tally = 0;
  double diffs, observed_diff = abs(as<double>(fun(x)) - as<double>(fun(y)));
  NumericVector pool(n), permut(n), randx(nx), randy(ny);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = nx; i < n; i++) pool[i] = y[i];
  for (i = 0; i < perms; i++) {
    permut = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = permut[j];
    for (j = nx; j < n; j++) randy[j] = permut[j];
    diffs = abs(as<double>(fun(randx)) - as<double>(fun(randy)));
    if (diffs >= observed_diff) tally++;
  }
  return (double)tally / (double)perms;
}
