#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
List permtest(NumericVector x, NumericVector y, int perms = 10000) {
  int i, nx = x.size(), ny = y.size(), pcumul = 0;
  double diffs, observed_diff = abs(mean(x) - mean(y));
  NumericVector pool(nx + ny);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = nx; i < nx + ny; i++) pool[i] = y[i];
  for (i = 0; i < perms; i++) {
    diffs = abs(mean(sample(pool, nx)) - mean(sample(pool, ny)));
    if (diffs >= observed_diff) pcumul++;
  }
  return List::create(Named("p.value") = (double)pcumul / (double)perms);
}
