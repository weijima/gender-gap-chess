#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double top10(NumericVector v) {
  NumericVector y = clone(v);
  std::sort(y.begin(), y.end());
  return mean(tail(y, 10));
}


// [[Rcpp::export]]
double permtest_mean(NumericVector x, NumericVector y, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny, tally = 0;
  double obs = mean(x) - mean(y);
  NumericVector pool(n), randx(nx), randy(ny);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    if (obs <= mean(randx) - mean(randy)) tally++;
  }
  return (double)tally / (double)perms;
}


// [[Rcpp::export]]
double permtest_median(NumericVector x, NumericVector y, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny, tally = 0;
  double obs = median(x) - median(y);
  NumericVector pool(n), randx(nx), randy(ny);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    if (obs <= median(randx) - median(randy)) tally++;
  }
  return (double)tally / (double)perms;
}


// [[Rcpp::export]]
double permtest_sd(NumericVector x, NumericVector y, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny, tally = 0;
  double obs = sd(x) - sd(y);
  NumericVector pool(n), randx(nx), randy(ny);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    if (obs <= sd(randx) - sd(randy)) tally++;
  }
  return (double)tally / (double)perms;
}


// [[Rcpp::export]]
double permtest_top1(NumericVector x, NumericVector y, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny, tally = 0;
  double obs = max(x) - max(y);
  NumericVector pool(n), randx(nx), randy(ny);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    if (obs <= max(randx) - max(randy)) tally++;
  }
  return (double)tally / (double)perms;
}


// [[Rcpp::export]]
double permtest_top10(NumericVector x, NumericVector y, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny, tally = 0;
  double obs = top10(x) - top10(y);
  NumericVector pool(n), randx(nx), randy(ny);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    if (obs <= top10(randx) - top10(randy)) tally++;
  }
  return (double)tally / (double)perms;
}
