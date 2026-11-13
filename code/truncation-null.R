# Is the shrinkage of the gender gap with increasing rating floors greater than what
# truncation alone would produce?
#
# Imposing a rating floor c mechanically shrinks the observed gap even if the
# underlying difference between women and men never changes: if women's ratings are
# men's shifted down by a constant, the floor removes more women, and the remaining
# women are bunched up just above c. This script tests the observed gap trajectory
# against that null hypothesis:
#
#   H0 (constant shift): men's ratings have the same distribution as women's + delta.
#
# Under H0, men rated at least c are distributed like women rated at least c - delta,
# shifted up by delta. The predicted (null) gap at floor c is therefore
#
#   pred(c) = [stat(W | W >= c - delta) + delta] - stat(W | W >= c),
#
# where stat is the mean or the median. The observed gap is
# obs(c) = stat(M | M >= c) - stat(W | W >= c), and the test statistic is the
# difference D(c) = obs(c) - pred(c), which is zero for all c under H0. A negative D(c)
# means the gap above floor c is smaller than a constant shift would imply; i.e., the
# gap shrinks faster than expected from truncation.
#
# Estimating delta: FIDE ratings are themselves truncated at 1000 (no lower ratings
# exist in the data), and this truncation affects women more than men. To avoid
# biasing delta, it is estimated within the range where H0 can be checked on
# observed data: delta solves
#
#   median(M | M >= 1000 + delta) = median(W | W >= 1000) + delta    (delta >= 0)
#
# (and the mirror-image equation when delta < 0). For the same reason, pred(c) is only
# defined for floors c >= 1000 + max(delta, 0).
#
# Uncertainty: nonparametric bootstrap, resampling women and men separately (within
# federation for the per-federation analysis), re-estimating delta in each bootstrap
# sample, and recomputing obs, pred, and D at all floors. Since ratings are integers,
# each group's bootstrap sample is drawn as multinomial counts over its observed
# rating values, which is equivalent to resampling players but much faster. We report
# pointwise 95% percentile intervals, as well as a 95% simultaneous band for D(c)
# across all floors (max-t method), together with the corresponding p-value for the
# joint null hypothesis D(c) = 0 for all floors c. Per-federation p-values are
# adjusted for multiple comparisons across federations (Benjamini-Hochberg), within
# each data filter and metric.
#
# Output:
#   data/truncation-null.csv            all results (global and per federation)
#   figures/truncation-null-global.pdf  global results for the mean gap
#   figures/truncation-null-fed.pdf     per-federation results for the mean gap

library(tidyverse)
library(patchwork)
library(parallel)


B <- 2000L                                 # Number of bootstrap samples
seed <- 8105L                              # Base random seed
rating_min <- 1000                         # FIDE rating floor: no lower ratings exist
floors_global <- seq(1000, 2200, by = 50)  # Rating floors for the global analysis
floors_fed <- seq(1000, 1800, by = 50)     # Rating floors for the per-federation analysis
min_players <- 30                          # Min. players of each gender above a floor
cores <- max(1L, detectCores() - 1L)       # Cores for running units in parallel



### Data filters (same as in the permutation analyses, minus the rating floor, which is
### swept over here instead)

restrict_data <- function(rating_data, include_junior, include_inactive) {
  max_byear <- if (include_junior) 2019 else 1999
  rating_data %>%
    filter(if (include_inactive) TRUE else active) %>%
    filter(!is.na(born), born <= max_byear, rating >= rating_min)
}



### Summaries of a group of ratings, represented as counts over sorted unique values

# Precompute cumulative sums for fast queries above arbitrary thresholds
summarize_counts <- function(v, k) {
  list(v = v, C = cumsum(k), RC = rev(cumsum(rev(k))), RS = rev(cumsum(rev(v * k))))
}

# Index of the first value >= t (length(v) + 1 if there is none)
first_at_least <- function(s, t) findInterval(t, s$v, left.open = TRUE) + 1L

mean_above <- function(s, t) {
  i <- first_at_least(s, t)
  m <- s$RC[i]
  ifelse(is.na(m) | m == 0, NA_real_, s$RS[i] / m)
}

median_above <- function(s, t) {
  i <- first_at_least(s, t)
  m <- s$RC[i]                                  # Number of ratings >= t
  m[is.na(m)] <- 0
  base <- s$C[length(s$C)] - m                  # Number of ratings < t
  value_at <- function(p) s$v[findInterval(p - 0.5, s$C) + 1L]  # p-th smallest rating
  lo <- value_at(base + floor((m + 1) / 2))
  hi <- value_at(base + floor(m / 2) + 1)
  ifelse(m == 0, NA_real_, (lo + hi) / 2)
}

stat_above <- function(s, t, metric) {
  if (metric == "mean") mean_above(s, t) else median_above(s, t)
}



### Estimating the shift delta

# Mismatch between men's and shifted women's medians, over the range where H0 is
# checkable on observed data; nonincreasing in d
delta_mismatch <- function(d, sm, sw) {
  a <- rating_min + max(d, 0)
  median_above(sm, a) - median_above(sw, a - d) - d
}

estimate_delta <- function(sm, sw, lo = -1000, hi = 1000, tol = 0.01) {
  # Pull in the ends of the search bracket where they run out of data
  while (hi > 0 && is.na(delta_mismatch(hi, sm, sw))) hi <- hi - 50
  while (lo < 0 && is.na(delta_mismatch(lo, sm, sw))) lo <- lo + 50
  h_lo <- delta_mismatch(lo, sm, sw)
  h_hi <- delta_mismatch(hi, sm, sw)
  if (is.na(h_lo) || is.na(h_hi) || h_lo < 0 || h_hi > 0) return(NA_real_)
  while (hi - lo > tol) {
    mid <- (lo + hi) / 2
    if (delta_mismatch(mid, sm, sw) > 0) lo <- mid else hi <- mid
  }
  (lo + hi) / 2
}



### Observed and predicted gap curves for one (possibly bootstrapped) sample

gap_curves <- function(sm, sw, delta, floors, metric) {
  obs <- stat_above(sm, floors, metric) - stat_above(sw, floors, metric)
  pred <- stat_above(sw, floors - delta, metric) + delta - stat_above(sw, floors, metric)
  pred[is.na(delta) | floors < rating_min + max(delta, 0)] <- NA_real_
  list(obs = obs, pred = pred)
}



### Full analysis for one unit (global data, or one federation, under one data filter)

analyze_unit <- function(m, w, floors, unit_seed, metrics = c("mean", "median")) {
  set.seed(unit_seed)  # Per-unit seed: reproducible regardless of parallelization
  tm <- table(m)
  tw <- table(w)
  vm <- as.numeric(names(tm))
  vw <- as.numeric(names(tw))
  km <- as.numeric(tm)
  kw <- as.numeric(tw)
  # Floors with enough players of each gender above them (same rule as elsewhere)
  eligible <- map_lgl(floors, \(f) sum(m >= f) >= min_players && sum(w >= f) >= min_players)
  floors <- floors[eligible]
  if (length(floors) == 0) return(NULL)
  sm <- summarize_counts(vm, km)
  sw <- summarize_counts(vw, kw)
  delta <- estimate_delta(sm, sw)
  if (is.na(delta)) return(NULL)
  # Bootstrap: multinomial counts over observed rating values = resampling players
  Km <- rmultinom(B, length(m), km)
  Kw <- rmultinom(B, length(w), kw)
  nf <- length(floors)
  boot <- map(set_names(metrics), \(x) list(obs = matrix(NA_real_, B, nf),
                                            pred = matrix(NA_real_, B, nf)))
  delta_boot <- numeric(B)
  for (b in seq_len(B)) {
    smb <- summarize_counts(vm, Km[, b])
    swb <- summarize_counts(vw, Kw[, b])
    delta_boot[b] <- estimate_delta(smb, swb)
    for (metric in metrics) {
      g <- gap_curves(smb, swb, delta_boot[b], floors, metric)
      boot[[metric]]$obs[b, ] <- g$obs
      boot[[metric]]$pred[b, ] <- g$pred
    }
  }
  q <- function(M, p) apply(M, 2, quantile, probs = p, na.rm = TRUE, names = FALSE)
  map_dfr(metrics, function(metric) {
    est <- gap_curves(sm, sw, delta, floors, metric)
    D_hat <- est$obs - est$pred
    obs_boot <- boot[[metric]]$obs
    pred_boot <- boot[[metric]]$pred
    D_boot <- obs_boot - pred_boot
    # Floors entering the test: D defined in the data and in every bootstrap sample
    testable <- !is.na(D_hat) & colSums(is.na(D_boot)) == 0
    se <- apply(D_boot, 2, sd)
    lo_sim <- hi_sim <- rep(NA_real_, nf)
    p_sup <- NA_real_
    if (sum(testable) > 0 && all(se[testable] > 0)) {
      Dt <- D_boot[, testable, drop = FALSE]
      dev <- abs(sweep(Dt, 2, D_hat[testable])) / rep(se[testable], each = B)
      t_boot <- apply(dev, 1, max)                          # Bootstrap max-t distribution
      t_obs <- max(abs(D_hat[testable]) / se[testable])     # Observed max-t statistic
      p_sup <- (1 + sum(t_boot >= t_obs)) / (B + 1)
      crit <- quantile(t_boot, 0.95, names = FALSE)
      lo_sim[testable] <- D_hat[testable] - crit * se[testable]
      hi_sim[testable] <- D_hat[testable] + crit * se[testable]
    }
    D_hat[!testable] <- NA_real_
    tibble(metric = metric,
           floor = floors,
           n_F = map_int(floors, \(f) sum(w >= f)),
           n_M = map_int(floors, \(f) sum(m >= f)),
           delta = delta,
           delta_lo = quantile(delta_boot, 0.025, na.rm = TRUE, names = FALSE),
           delta_hi = quantile(delta_boot, 0.975, na.rm = TRUE, names = FALSE),
           obs = est$obs,
           obs_lo = q(obs_boot, 0.025),
           obs_hi = q(obs_boot, 0.975),
           pred = ifelse(testable, est$pred, NA_real_),
           pred_lo = ifelse(testable, q(pred_boot, 0.025), NA_real_),
           pred_hi = ifelse(testable, q(pred_boot, 0.975), NA_real_),
           diff = D_hat,
           diff_lo = ifelse(testable, q(D_boot, 0.025), NA_real_),
           diff_hi = ifelse(testable, q(D_boot, 0.975), NA_real_),
           diff_lo_sim = lo_sim,
           diff_hi_sim = hi_sim,
           p_sup = p_sup)
  })
}



### Run all units

rating_data <- read_csv("data/rating-data.csv", col_types = "cccdiil")

units <- crossing(juniors = c(FALSE, TRUE), inactives = c(FALSE, TRUE)) %>%
  mutate(data = map2(juniors, inactives, \(j, i) restrict_data(rating_data, j, i))) %>%
  mutate(units = map(data, function(d) {
    bind_rows(tibble(scope = "global", fed = "ALL", data = list(d)),
              d %>% nest(data = -fed) %>% mutate(scope = "federation"))
  })) %>%
  select(-data) %>%
  unnest(units) %>%
  mutate(unit_id = row_number())

cat("Running", nrow(units), "units with", B, "bootstrap samples each, on", cores, "cores...\n")
results <- mclapply(seq_len(nrow(units)), function(i) {
  d <- units$data[[i]]
  floors <- if (units$scope[i] == "global") floors_global else floors_fed
  analyze_unit(d$rating[d$sex == "M"], d$rating[d$sex == "F"], floors,
               unit_seed = seed + units$unit_id[i])
}, mc.cores = cores)

truncation_null <- units %>%
  select(scope, juniors, inactives, fed) %>%
  mutate(res = results) %>%
  filter(!map_lgl(res, is.null)) %>%
  unnest(res) %>%
  group_by(scope, juniors, inactives, metric) %>%
  mutate(p_sup_adj = { # BH adjustment across federations (one p-value per federation)
    pf <- tibble(fed, p_sup) %>% distinct()
    pf$p_adj <- p.adjust(pf$p_sup, method = "BH")
    pf$p_adj[match(fed, pf$fed)]
  }) %>%
  ungroup() %>%
  arrange(desc(scope), juniors, inactives, metric, fed, floor)

write_csv(truncation_null, "data/truncation-null.csv")



### Console summary

cat("\nGlobal results (joint test of H0 across floors):\n")
truncation_null %>%
  filter(scope == "global") %>%
  distinct(juniors, inactives, metric, delta, delta_lo, delta_hi, p_sup) %>%
  print()

cat("\nPer federation: number of federations rejecting H0 (BH-adjusted p < 0.05):\n")
truncation_null %>%
  filter(scope == "federation") %>%
  distinct(juniors, inactives, metric, fed, p_sup_adj) %>%
  group_by(juniors, inactives, metric) %>%
  summarize(federations_tested = sum(!is.na(p_sup_adj)),
            rejected = sum(p_sup_adj < 0.05, na.rm = TRUE),
            .groups = "drop") %>%
  print()



### Figures (mean gap)

filter_label <- function(juniors, inactives) {
  str_c(if_else(juniors, "Juniors included", "Juniors excluded"), ", ",
        if_else(inactives, "inactives included", "inactives excluded"))
}

plot_data <- truncation_null %>%
  filter(metric == "mean") %>%
  mutate(filter = filter_label(juniors, inactives))

global_gaps <- plot_data %>%
  filter(scope == "global") %>%
  select(filter, floor, obs_est = obs, obs_lo, obs_hi, pred_est = pred, pred_lo, pred_hi) %>%
  pivot_longer(-c(filter, floor), names_to = c("curve", ".value"), names_sep = "_") %>%
  mutate(curve = recode(curve, obs = "Observed", pred = "Constant shift (truncation only)")) %>%
  ggplot(aes(x = floor, y = est, color = curve, fill = curve)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25, color = NA, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 0.8, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(1000, 2200, by = 400)) +
  facet_wrap(~ filter, nrow = 1) +
  scale_color_manual(values = c("#E69F00", "#0072B2"), aesthetics = c("color", "fill")) +
  labs(x = "Rating floor", y = "Mean rating gap (men - women)", color = NULL, fill = NULL) +
  theme_bw() +
  theme(legend.position = "top")

global_diff <- plot_data %>%
  filter(scope == "global") %>%
  ggplot(aes(x = floor, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_ribbon(aes(ymin = diff_lo_sim, ymax = diff_hi_sim), fill = "grey60", alpha = 0.4,
              na.rm = TRUE) +
  geom_ribbon(aes(ymin = diff_lo, ymax = diff_hi), fill = "grey30", alpha = 0.4, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 0.8, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(1000, 2200, by = 400)) +
  facet_wrap(~ filter, nrow = 1) +
  labs(x = "Rating floor", y = "Observed - predicted gap") +
  theme_bw()

ggsave("figures/truncation-null-global.pdf", global_gaps / global_diff + plot_annotation(tag_levels = "A"),
       width = 12, height = 6)

fed_plot <- plot_data %>%
  filter(scope == "federation", !is.na(diff)) %>%
  mutate(sig = if_else(p_sup_adj < 0.05 & (diff_hi_sim < 0 | diff_lo_sim > 0),
                       "Excludes 0", "Includes 0")) %>%
  ggplot(aes(x = floor, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_boxplot(aes(group = floor), outlier.shape = NA, color = "grey50", fill = NA) +
  geom_point(aes(alpha = sig), position = position_jitter(width = 10, height = 0, seed = 1),
             size = 0.8, color = "#0072B2") +
  facet_wrap(~ filter, nrow = 1) +
  scale_alpha_manual(values = c("Excludes 0" = 0.9, "Includes 0" = 0.2)) +
  labs(x = "Rating floor", y = "Observed - predicted mean gap",
       alpha = "Simultaneous 95% band (BH-significant federations)") +
  theme_bw() +
  theme(legend.position = "top")

ggsave("figures/truncation-null-fed.pdf", fed_plot, width = 12, height = 4)
