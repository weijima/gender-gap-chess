#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double top10(NumericVector v) {
  NumericVector y = clone(v);
  std::sort(y.begin(), y.end());
  return mean(tail(noNA(y), 10));
}


// [[Rcpp::export]]
DataFrame permtest(NumericVector x, NumericVector y, int perms = 10000) {
  int i, j, nx = x.size(), ny = y.size(), n = nx + ny;
  int tally_mean = 0,
    tally_median = 0,
    tally_sd = 0,
    tally_max = 0,
    tally_top10 = 0;
  double obs_mean = mean(noNA(x)) - mean(noNA(y)),
    obs_median = median(x) - median(y), // noNA does not work; possibly Rcpp bug?
    obs_sd = sd(noNA(x)) - sd(noNA(y)),
    obs_max = max(noNA(x)) - max(noNA(y)),
    obs_top10 = top10(x) - top10(y); // noNA called in top10
  NumericVector pool(n), randx(nx), randy(ny);
  for (i = 0; i < nx; i++) pool[i] = x[i];
  for (i = 0; i < ny; i++) pool[i + nx] = y[i];

  for (i = 0; i < perms; i++) {
    pool = sample(pool, n);
    for (j = 0; j < nx; j++) randx[j] = pool[j];
    for (j = 0; j < ny; j++) randy[j] = pool[j + nx];
    if (obs_mean <= mean(noNA(randx)) - mean(noNA(randy))) tally_mean++;
    if (obs_median <= median(randx) - median(randy)) tally_median++;
    if (obs_sd <= sd(noNA(randx)) - sd(noNA(randy))) tally_sd++;
    if (obs_max <= max(noNA(randx)) - max(noNA(randy))) tally_max++;
    if (obs_top10 <= top10(randx) - top10(randy)) tally_top10++;
  }

  DataFrame df = DataFrame::create(
    _["stat"] = StringVector::create("mean", "median", "sd", "top1", "top10"),
    _["pvalue"] = NumericVector::create(
      (double)tally_mean / (double)perms,
      (double)tally_median / (double)perms,
      (double)tally_sd / (double)perms,
      (double)tally_max / (double)perms,
      (double)tally_top10 / (double)perms
    ),
    _["diff"] = NumericVector::create(obs_mean, obs_median, obs_sd, obs_max, obs_top10)
  );
  return df;
}
