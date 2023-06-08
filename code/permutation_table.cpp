#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double top10(NumericVector v) {
  NumericVector y = clone(v);
  std::sort(y.begin(), y.end());
  return mean(tail(y, 10));
}


// [[Rcpp::export]]
NumericVector permtab_mean(NumericVector x, NumericVector y, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny;
  NumericVector pool(n), randx(nx), randy(ny), vals(perms);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    vals[i] = mean(randx) - mean(randy);
  }
  return vals;
}


// [[Rcpp::export]]
NumericVector permtab_median(NumericVector x, NumericVector y, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny;
  NumericVector pool(n), randx(nx), randy(ny), vals(perms);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    vals[i] = median(randx) - median(randy);
  }
  return vals;
}


// [[Rcpp::export]]
NumericVector permtab_sd(NumericVector x, NumericVector y, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny;
  NumericVector pool(n), randx(nx), randy(ny), vals(perms);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    vals[i] = sd(randx) - sd(randy);
  }
  return vals;
}


// [[Rcpp::export]]
NumericVector permtab_top1(NumericVector x, NumericVector y, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny;
  NumericVector pool(n), randx(nx), randy(ny), vals(perms);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    vals[i] = max(randx) - max(randy);
  }
  return vals;
}


// [[Rcpp::export]]
NumericVector permtab_top10(NumericVector x, NumericVector y, int perms) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny;
  NumericVector pool(n), randx(nx), randy(ny), vals(perms);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];
  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    vals[i] = top10(randx) - top10(randy);
  }
  return vals;
}
