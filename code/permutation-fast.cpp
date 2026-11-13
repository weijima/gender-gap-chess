// Fast permutation test for the difference in a summary statistic between two groups.
//
// permut_tab_fast(x, y, metric, perms) returns `perms` values of f(X) - f(Y), where
// (X, Y) is a uniformly random split of c(x, y) into groups of sizes length(x) and
// length(y), and f is the statistic named by `metric` ("mean", "median", "sd",
// "top1", or "top10"). It replaces permut_tab() in permutation-table.cpp.
//
// Speed-ups relative to permut_tab():
//   - No R function calls inside the loop (this also enables multithreading).
//   - Only the smaller group is drawn (partial Fisher-Yates shuffle); statistics of
//     the larger group are obtained from pool totals (mean, sd) or from the
//     complement of the drawn indices (median).
//   - top1/top10: the pool is sorted once, and labels are assigned by sequential
//     sampling from the top down, stopping once both groups have their top values.
//   - Fast PRNG (xoshiro256**) with Lemire's unbiased bounded integers.
//   - Permutations are computed in blocks in parallel via RcppParallel (set the
//     number of threads with RcppParallel::setThreadOptions()). Each block's PRNG
//     is seeded from R's RNG, so set.seed() makes results reproducible regardless
//     of the number of threads.

// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cstdint>
#include <numeric>
#include <string>
#include <vector>
using namespace Rcpp;


// Small, fast, thread-safe PRNG (xoshiro256**), one instance per block of permutations
struct Rng {
  uint64_t s[4];
  static uint64_t splitmix(uint64_t& x) {
    uint64_t z = (x += 0x9e3779b97f4a7c15ULL);
    z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
    z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
    return z ^ (z >> 31);
  }
  explicit Rng(uint64_t seed) { for (auto& v : s) v = splitmix(seed); }
  static uint64_t rotl(uint64_t x, int k) { return (x << k) | (x >> (64 - k)); }
  uint64_t next() {
    const uint64_t r = rotl(s[1] * 5, 7) * 9, t = s[1] << 17;
    s[2] ^= s[0]; s[3] ^= s[1]; s[1] ^= s[2]; s[0] ^= s[3]; s[2] ^= t; s[3] = rotl(s[3], 45);
    return r;
  }
  // Unbiased uniform integer in [0, range) -- Lemire's multiply-shift method
  uint32_t below(uint32_t range) {
    uint64_t m = (next() >> 32) * (uint64_t)range;
    uint32_t l = (uint32_t)m;
    if (l < range) {
      uint32_t t = (uint32_t)(-range) % range;
      while (l < t) { m = (next() >> 32) * (uint64_t)range; l = (uint32_t)m; }
    }
    return (uint32_t)(m >> 32);
  }
};


enum Metric { MEAN, SD, MEDIAN, TOP };


struct PermWorker : public RcppParallel::Worker {
  const std::vector<double>& pool;  // sorted ascending
  const int n, k;                   // k = size of the smaller group
  const bool small_is_x;
  const Metric metric;
  const int top_m;                  // 1 for top1, 10 for top10
  const double c, S1, S2;           // centring constant, centred sum, centred sum of squares
  const int perms, block;
  const uint64_t seed;
  RcppParallel::RVector<double> out;

  PermWorker(const std::vector<double>& pool, int k, bool small_is_x, Metric metric, int top_m,
             double c, double S1, double S2, int perms, int block, uint64_t seed, NumericVector out)
    : pool(pool), n((int)pool.size()), k(k), small_is_x(small_is_x), metric(metric), top_m(top_m),
      c(c), S1(S1), S2(S2), perms(perms), block(block), seed(seed), out(out) {}

  // j-th (0-based) element of {0,...,n-1} \ sel, where sel is sorted ascending
  static int complement_at(const int* sel, int k, int j) {
    int p = j;
    for (int i = 0; i < k && sel[i] <= p; i++) p++;
    return p;
  }

  double median_sorted_idx(const int* sel, int m) const {
    return (m % 2) ? pool[sel[m / 2]] : 0.5 * (pool[sel[m / 2 - 1]] + pool[sel[m / 2]]);
  }

  double median_complement(const int* sel) const {
    int m = n - k;
    if (m % 2) return pool[complement_at(sel, k, m / 2)];
    return 0.5 * (pool[complement_at(sel, k, m / 2 - 1)] + pool[complement_at(sel, k, m / 2)]);
  }

  void operator()(std::size_t begin, std::size_t end) {
    std::vector<int> idx(n);
    for (std::size_t b = begin; b < end; b++) {
      Rng rng(seed + 0x632be59bd9b4e019ULL * (b + 1));   // one stream per block => reproducible
      std::iota(idx.begin(), idx.end(), 0);
      int lo = (int)b * block, hi = std::min(perms, lo + block);
      for (int p = lo; p < hi; p++) {
        double fs, fl;  // metric for smaller / larger group
        if (metric == TOP) {
          // Walk down the sorted pool, assigning labels by sequential sampling;
          // stop as soon as both groups have their top_m values
          int rs = k, rl = n - k, ms = std::min(top_m, k), ml = std::min(top_m, n - k);
          int cs = 0, cl = 0; double ss = 0.0, sl = 0.0;
          for (int i = n - 1; cs < ms || cl < ml; i--) {
            if (rng.below(rs + rl) < (uint32_t)rs) { rs--; if (cs < ms) { ss += pool[i]; cs++; } }
            else                                   { rl--; if (cl < ml) { sl += pool[i]; cl++; } }
          }
          fs = ss / ms; fl = sl / ml;
        } else {
          // Partial Fisher-Yates: only the k slots of the smaller group need shuffling
          for (int i = 0; i < k; i++) std::swap(idx[i], idx[i + rng.below(n - i)]);
          if (metric == MEDIAN) {
            std::sort(idx.begin(), idx.begin() + k);
            fs = median_sorted_idx(idx.data(), k);
            fl = median_complement(idx.data());
          } else {
            double s1 = 0.0, s2 = 0.0;
            for (int i = 0; i < k; i++) { double v = pool[idx[i]] - c; s1 += v; s2 += v * v; }
            int nl = n - k;
            if (metric == MEAN) {
              fs = s1 / k; fl = (S1 - s1) / nl;
            } else {
              fs = std::sqrt((s2 - s1 * s1 / k) / (k - 1));
              fl = std::sqrt(((S2 - s2) - (S1 - s1) * (S1 - s1) / nl) / (nl - 1));
            }
          }
        }
        out[p] = small_is_x ? fs - fl : fl - fs;
      }
    }
  }
};


// [[Rcpp::export]]
NumericVector permut_tab_fast(NumericVector x, NumericVector y, std::string metric, int perms,
                              int block = 1024) {
  int nx = x.size(), ny = y.size(), n = nx + ny;
  Metric m; int top_m = 0;
  if (metric == "mean") m = MEAN;
  else if (metric == "sd") m = SD;
  else if (metric == "median") m = MEDIAN;
  else if (metric == "top1") { m = TOP; top_m = 1; }
  else if (metric == "top10") { m = TOP; top_m = 10; }
  else stop("unknown metric: " + metric);
  std::vector<double> pool(n);
  std::copy(x.begin(), x.end(), pool.begin());
  std::copy(y.begin(), y.end(), pool.begin() + nx);
  std::sort(pool.begin(), pool.end());
  double c = std::accumulate(pool.begin(), pool.end(), 0.0) / n, S1 = 0.0, S2 = 0.0;
  for (double v : pool) { S1 += v - c; S2 += (v - c) * (v - c); }
  // Seed derived from R's RNG, so set.seed() makes results reproducible
  // (independently of the number of threads)
  uint64_t seed;
  { RNGScope scope;
    seed = ((uint64_t)(R::unif_rand() * 4294967296.0) << 32) ^ (uint64_t)(R::unif_rand() * 4294967296.0); }
  NumericVector out(perms);
  PermWorker w(pool, std::min(nx, ny), nx <= ny, m, top_m, c, S1, S2, perms, block, seed, out);
  RcppParallel::parallelFor(0, (perms + block - 1) / block, w, 1);
  return out;
}
