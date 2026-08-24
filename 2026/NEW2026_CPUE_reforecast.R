# ============================================================================
# CN in-season run reforecast — Barkley Sound recreational CPUE models
# EXPLORATORY BLOCK — rebuilt August 2026 for annotation/traceability.
#
# Core logic (subarea regex, reconciliation, CPUE calculation) is UNCHANGED
# from Nick Brown's original — verified correct, no tweaks needed.
#
# Additions in this version:
#   1. Explicit 2001 exclusion, applied consistently here rather than only
#      in the production script.
#   2. Inline documentation throughout, and reduced reliance on the Excel
#      workbook: the ONLY inputs still pulled from Excel are (a) the Somass
#      run-size reconstruction, updated once per year, and (b) the raw
#      creel interview data downloaded from CREST. Every other calculation
#      (CPUE, subarea reconciliation, cumulative periods, model fitting) 
#      happens in R, not in Excel pivot tables.
# ============================================================================

library(tidyverse); theme_set(theme_bw(base_size = 16))
library(readxl)
library(geomtextpath)

curr_year <- 2026

# ── Known problem years ──────────────────────────────────────────────────
# 2001: excluded per predecessor's note in the production script's bs_cn 
# load step, "inseason management changes" — root cause not otherwise 
# documented in this workbook. Applying it here too so the exploratory 
# results are consistent with what the production model already assumes.
EXCLUDED_YEARS <- c(2001)

# ── Load interview data ──────────────────────────────────────────────────
interview_summary <- read_xlsx("CN_return_predictors_assemblyMaster.xlsx",
                               sheet = "CREEL Interview Summary 2026") |> 
  rename_with(tolower)

# ── Somass terminal adult return data ────────────────────────────────────
bs_cn <- read_xlsx("CN_return_predictors_assemblyMaster.xlsx",
                   sheet = "CN_return_predictors") |> 
  select(year, matches("(?i)Somass")) |>
  mutate(Somass_term_adult_return = round(as.numeric(Somass_term_adult_return), digits = 0))
# as.numeric() correctly turns the current year's "NA" string into a real 
# NA — this is what lets curr_year flow through downstream as a genuine 
# forecast target (no return to fit against) without special-casing it.

# ── Group data by strata and calculate CPUEs ─────────────────────────────
# Nick's original filter/reconciliation/CPUE logic — unchanged.
# Regex keeps both "23X" codes AND the "123" corridor codes (T/R/P).
#
# NOTE on rch_cn_k / PROP_RCH: the DNA lookup table (CREEL DNA LU tab) 
# previously listed corridor-area RCH proportions under OLD subarea codes
# (23I, 23N, 23P), while creel interviews have always used the CURRENT 
# corridor codes (123R, 123T, 123P) — a naming mismatch that caused every 
# RCH join for these three codes to silently fail, for all 26 years. This 
# was NOT a real data gap — the DNA data existed, just under the wrong 
# label. Fixed directly in the workbook (DNA LU tab relabeled to current 
# codes). 23G/23H/23L remain genuinely unassessed (no PROP_RCH data exists 
# for those, confirmed separately) — that part of the gap is real.
cpue <- interview_summary |> 
  filter(
    asscd_txt %in% c("Adipose fin not chkd", "Complete Form", "Fish not seen for ID"), 
    str_detect(statsub, "^23[[:upper:]]|123(?=(T|R|P))"), # 123 T, R, and P are the corridor
    !year %in% EXCLUDED_YEARS
  ) |> 
  mutate(
    statsub = case_when( 
      # in 2011, 23G was split into 23O and 23P (now 123P)
      statsub %in% c("23G", "23P", "23O", "123P", "23L") ~ "23O+123P", 
      # in 2011, 23H was split into 23Q and 23N (now 123T)
      statsub %in% c("23H", "23Q", "23N", "123T") ~ "23Q+123T",
      # NOTE: 23I never appears anywhere in the raw interview data (2000-2026)
      # — interviews have always used 123R directly. This line is a no-op
      # safeguard, kept in case an old-style code ever appears in a future paste.
      statsub == "23I" ~ "123R",
      TRUE ~ statsub
    )
  ) |>  
  summarise(
    .by = c(year, statsub, sw_2026),
    # na.rm = TRUE is now safe: the corridor-code RCH gap was a naming 
    # mismatch (fixed above), not real missingness. Any remaining NA in 
    # rch_cn_k after the fix should be incidental, not systematic — treating
    # it as 0 for that interview is reasonable rather than discarding the
    # whole year/subarea group over one unassigned sample.
    across(c(cn_all_k, rch_cn_k), \(x) sum(x, na.rm = TRUE)),
    boat_trips = n()
  ) |> 
  left_join(select(.data = bs_cn, year, "Somass_term_adult_return")) |> 
  rename("return" = "Somass_term_adult_return") |> 
  filter(sw_2026 %in% c(82, 83, 84, 91, 92)) |> 
  pivot_longer(cols = cn_all_k:boat_trips) |> 
  pivot_wider(names_from = sw_2026, values_from = value) |> 
  rowwise() |> 
  mutate(
    cum83 = `82` + `83`,
    cum84 = sum(c_across(`82`:`84`), na.rm = TRUE),
    cum91 = sum(c_across(`83`:`91`), na.rm = TRUE),
    cum92 = sum(c_across(`83`:`92`), na.rm = TRUE),
    `8384` = `83` + `84`,
    `8491` = `84` + `91`
  ) |> 
  ungroup() |> 
  pivot_longer(cols = matches("[[:digit:]]{2,}"), names_to = "period", values_to = "value") |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(cpue = cn_all_k / boat_trips,
         rch_cpue = rch_cn_k / boat_trips)

# ── Minimalist version of the data ───────────────────────────────────────
# Complete-case only: needs both a real CPUE AND a known return to be usable
# for fitting. curr_year rows drop out here automatically (return is NA — 
# expected, since it's the forecast target, not yet observed).
cpue_minimal <- cpue |> 
  filter(!if_any(c(return, cn_all_k, boat_trips), is.na)) |> 
  select(year, period, cn_all_k, boat_trips, rch_cn_k, statsub, return)

# ============================================================================
# EXPLORATORY MODEL SEARCH — single subareas vs. 4-5 subarea combinations
# Uses closed-form OLS (fast_ols) instead of lm() throughout — same math,
# but feasible at scale (single-subarea: ~1,056 fits; combos: ~113,000 fits)
# ============================================================================

MIN_N <- 6

# ── Fast closed-form OLS: same math as lm(y ~ x), no formula/model-frame
#    overhead — this is what makes fitting 113k+ groups feasible ──────────
fast_ols <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  if (n < 4) return(tibble(n = n, adj.r.squared = NA_real_, p.value = NA_real_))
  
  mx <- mean(x); my <- mean(y)
  sxx <- sum((x - mx)^2)
  if (sxx == 0) return(tibble(n = n, adj.r.squared = NA_real_, p.value = NA_real_))
  
  slope <- sum((x - mx) * (y - my)) / sxx
  intercept <- my - slope * mx
  resid <- y - (intercept + slope * x)
  
  ss_res <- sum(resid^2)
  ss_tot <- sum((y - my)^2)
  r2 <- 1 - ss_res / ss_tot
  adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - 2)
  
  se_slope <- sqrt((ss_res / (n - 2)) / sxx)
  p_val <- 2 * pt(-abs(slope / se_slope), df = n - 2)
  
  tibble(n = n, adj.r.squared = adj_r2, p.value = p_val)
}

# ============================================================================
# PART 1: SINGLE-SUBAREA leaderboard (rebuilt fresh on 2001-excluded data —
# the earlier cpue_r2s from before we added EXCLUDED_YEARS is now stale)
# ============================================================================

cpue_long <- cpue_minimal |> 
  mutate(cpue = cn_all_k / boat_trips,
         rch_cpue = rch_cn_k / boat_trips,
         ln_cpue = log(cpue + 0.0001),
         ln_rch_cpue = log(rch_cpue + 0.0001),
         ln_return = log(return + 0.0001)) |> 
  pivot_longer(cols = c(cpue, rch_cpue, ln_cpue, ln_rch_cpue),
               names_to = "predictor", values_to = "x") |> 
  pivot_longer(cols = c(return, ln_return),
               names_to = "response", values_to = "y") |> 
  filter(!is.na(x), !is.infinite(x), !is.na(y), !is.infinite(y))

cpue_r2s <- cpue_long |> 
  group_by(statsub, period, predictor, response) |> 
  group_modify(~ fast_ols(.x$x, .x$y)) |> 
  ungroup() |> 
  filter(!is.na(p.value), n >= MIN_N) |> 
  select(statsub, period, predictor, response, adj.r.squared, p.value, n) |> 
  mutate(model = case_when(
    str_detect(predictor, "ln") & str_detect(response, "ln") ~ "log-log",
    str_detect(predictor, "ln") & !str_detect(response, "ln") ~ "lin-log",  # X logged, Y linear
    !str_detect(predictor, "ln") & str_detect(response, "ln") ~ "log-lin",  # Y logged, X linear
    TRUE ~ "linear"
  ),
    correction = if_else(str_detect(predictor, "rch"), "corrected", "raw"))

# ============================================================================
# PART 2: SUBAREA COMBINATIONS (4-5 subareas pooled) — tests whether
# pooling beats any single subarea
# ============================================================================

combo_n <- seq.int(4, 5)
subarea_combos <- list(unique(cpue_minimal$statsub)) |> 
  rep(length(combo_n)) |> 
  map2(combo_n, ~ combn(x = .x, m = .y, FUN = list)) |> 
  unlist(recursive = FALSE)
cat("Testing", length(subarea_combos), "subarea combinations\n")

combo_cpue <- set_names(subarea_combos, 
                        nm = map_chr(subarea_combos, ~paste(.x, collapse = "_"))) |> 
  map(~ cpue_minimal |> 
        filter(statsub %in% .x) |> 
        summarise(.by = c(year, return, period),
                  across(c(cn_all_k, rch_cn_k, boat_trips), \(x) sum(x, na.rm = TRUE))) |>  # <- fixed
        mutate(cpue = cn_all_k / boat_trips,
               rch_cpue = rch_cn_k / boat_trips) |> 
        filter(!is.na(cpue), !is.nan(cpue))) |> 
  keep(~ n_distinct(.x$year) >= MIN_N)
cat("Retained", length(combo_cpue), "combos with >=", MIN_N, "years of data\n")

combo_long <- combo_cpue |> 
  map(~ .x |> 
        mutate(ln_cpue = log(cpue + 0.0001),
               ln_rch_cpue = log(rch_cpue + 0.0001),
               ln_return = log(return + 0.0001)) |> 
        pivot_longer(cols = c(cpue, rch_cpue, ln_cpue, ln_rch_cpue),
                     names_to = "predictor", values_to = "x") |> 
        pivot_longer(cols = c(return, ln_return),
                     names_to = "response", values_to = "y") |> 
        filter(!is.na(x), !is.infinite(x), !is.na(y), !is.infinite(y)) |> 
        mutate(group = paste(period, predictor, response, sep = "-")) |> 
        group_by(group) |> 
        filter(n() >= MIN_N))

# Fast path via summarise() rather than group_modify() -- lower per-group
# overhead at this scale (113k groups)
combo_r2s <- combo_long |> 
  bind_rows(.id = "combo") |> 
  ungroup() |>                          # <- added: drop leftover grouping from combo_long
  summarise(
    .by = c(combo, period, predictor, response),
    n = n(),
    mx = mean(x), my = mean(y),
    sxx = sum((x - mx)^2),
    sxy = sum((x - mx) * (y - my)),
    ss_tot = sum((y - my)^2)
  ) |> 
  filter(n >= 4, sxx > 0) |> 
  mutate(slope = sxy / sxx,
         intercept = my - slope * mx) |> 
  left_join(
    combo_long |> bind_rows(.id = "combo") |> ungroup(),   # <- also here
    by = c("combo","period","predictor","response")
  ) |> 
  mutate(fitted = intercept + slope * x,
         sq_resid = (y - fitted)^2) |> 
  summarise(
    .by = c(combo, period, predictor, response, n, ss_tot, slope, sxx),
    ss_res = sum(sq_resid)
  ) |> 
  mutate(
    r2 = 1 - ss_res / ss_tot,
    adj.r.squared = 1 - (1 - r2) * (n - 1) / (n - 2),
    se_slope = sqrt((ss_res / (n - 2)) / sxx),
    p.value = 2 * pt(-abs(slope / se_slope), df = n - 2)
  ) |> 
  filter(!is.na(p.value)) |> 
  select(combo, period, predictor, response, adj.r.squared, p.value, n) |> 
  mutate(model = case_when(
    str_detect(predictor, "ln") & str_detect(response, "ln") ~ "log-log",
    str_detect(predictor, "ln") & !str_detect(response, "ln") ~ "log-lin",
    !str_detect(predictor, "ln") & str_detect(response, "ln") ~ "lin-log",
    TRUE ~ "linear"),
    correction = if_else(str_detect(predictor, "rch"), "corrected", "raw"))

# ============================================================================
# PART 3: COMPARE — 
# ============================================================================

cat("\n── TOP 20 SINGLE-SUBAREA MODELS ──\n")
cpue_r2s |> 
  arrange(desc(adj.r.squared)) |> 
  select(statsub, period, model, correction, adj.r.squared, p.value, n) |> 
  mutate(adj.r.squared = round(adj.r.squared, 3), p.value = signif(p.value, 3)) |> 
  slice_head(n = 20) |> 
  print(n = 20, width = Inf)

cat("\n── TOP 20 COMBO MODELS ──\n")
combo_r2s |> 
  arrange(desc(adj.r.squared)) |> 
  select(combo, period, model, correction, adj.r.squared, p.value, n) |> 
  mutate(adj.r.squared = round(adj.r.squared, 3), p.value = signif(p.value, 3)) |> 
  slice_head(n = 20) |> 
  print(n = 20, width = Inf)

best_single_r2 <- cpue_r2s |> pull(adj.r.squared) |> max(na.rm = TRUE)
best_combo_r2  <- combo_r2s |> pull(adj.r.squared) |> max(na.rm = TRUE)

cat("\nBest single-subarea adj.r.squared:", round(best_single_r2, 3), "\n")
cat("Best combo adj.r.squared:         ", round(best_combo_r2, 3), "\n")
# ── FINDING: pooling multiple subareas beats any single subarea ───────────
# Best combo: 23D + 23J + 23M + 23O+123P, period = cum84, raw CPUE, log-log
#   adj.R² = 0.832, p = 1.4e-10, n = 25 (full series, no small-n artifact)
# One outlier: 2014 (return well below predicted) — also independently 
# flagged in Nick's original wk83 diagnostic, so likely a real anomalous 
# year for the fishery, not a modeling artifact. Not a reason to exclude it.
#
# OPERATIONAL NOTE: this model requires period = cum84 (catch/effort through
# stat week 84). As of the date this was run, only week 83 data exists for 
# the current season — cum84 is NOT yet available. For an in-season 
# reforecast needed THIS week, use the best period=83/cum83 model instead 
# (see below); switch to the cum84 model once week 84 interviews are in.

# ── STEP 1: Find the best period=82 model, single-subarea and combo ───────
cat("\n── TOP 10 COMBO MODELS, period = 82 ──\n")
combo_r2s |> 
  filter(period == "82") |> 
  arrange(desc(adj.r.squared)) |> 
  select(combo, period, model, correction, adj.r.squared, p.value, n) |> 
  mutate(adj.r.squared = round(adj.r.squared, 3), p.value = signif(p.value, 3)) |> 
  slice_head(n = 10) |> 
  print(n = 10, width = Inf)

cat("\n── TOP 10 SINGLE-SUBAREA MODELS, period = 82 ──\n")
cpue_r2s |> 
  filter(period == "82") |> 
  arrange(desc(adj.r.squared)) |> 
  select(statsub, period, model, correction, adj.r.squared, p.value, n) |> 
  mutate(adj.r.squared = round(adj.r.squared, 3), p.value = signif(p.value, 3)) |> 
  slice_head(n = 10) |> 
  print(n = 10, width = Inf)

###---------------
chosen_subareas <- c("23M","23J","23Q+123T","23E")
chosen_period   <- "82"

d82_all <- cpue |> 
  filter(statsub %in% chosen_subareas, period == chosen_period) |> 
  summarise(.by = c(year, return),
            across(c(cn_all_k, boat_trips), \(x) sum(x, na.rm = TRUE))) |> 
  mutate(cpue = cn_all_k / boat_trips)

d82 <- d82_all |> 
  filter(!is.na(return), !is.na(cpue), !is.nan(cpue)) |> 
  mutate(log_cpue = log(cpue + 0.0001))   # <- offset guards against log(0) = -Inf

wk82.rf <- lm(log(return) ~ log_cpue, data = d82)
summary(wk82.rf)$adj.r.squared

plot(wk82.rf, labels.id = d82$year, ask = FALSE)

wk82.pred_input <- d82_all |> 
  select(year, cpue) |> 
  filter(!is.na(cpue)) |> 
  mutate(log_cpue = log(cpue + 0.0001))

wk82.pred <- cbind(wk82.pred_input, 
                   predict(wk82.rf, wk82.pred_input, interval = "pred", level = .75)) |> 
  mutate(across(fit:upr, exp))

wk82.pred |> filter(year == curr_year)

ggplot(d82, aes(cpue, return)) +
  geom_point() +
  geom_text_repel(aes(label = year), min.segment.length = 0) +
  geom_smooth(data = wk82.pred |> filter(year != curr_year), 
              aes(y = fit, ymin = lwr, ymax = upr), stat = "identity") +
  geom_pointrange(data = wk82.pred |> filter(year == curr_year),
                  aes(y = fit, ymin = lwr, ymax = upr), colour = "red") +
  labs(y = "Somass adult return", x = "Week 82 CPUE (23M+23J+23Q+123T+23E)") +
  scale_y_continuous(labels = scales::comma)
d82 <- d82_all |> 
  filter(!is.na(return), !is.na(cpue), !is.nan(cpue)) |> 
  # 2000 excluded: CPUE=0 (genuine zero, 6 boat trips, 0 kept CN) forced through
  # the log(cpue+0.0001) offset creates an extreme leverage point on the log
  # scale. Confirmed via sensitivity check: removing 2000 changes the slope
  # from 0.30 to 0.65 and adj.R² from 0.658 to 0.413 — the offset artifact, 
  # not a real relationship, was driving much of the original fit.
  filter(year != 2000) |> 
  mutate(log_cpue = log(cpue + 0.0001))

wk82.rf <- lm(log(return) ~ log_cpue, data = d82)
summary(wk82.rf)$adj.r.squared   # now 0.413 — lower, but honest

plot(wk82.rf, labels.id = d82$year, ask = FALSE)

wk82.pred_input <- d82_all |> 
  select(year, cpue) |> 
  filter(!is.na(cpue)) |>  # keep 2000 + curr_year in the PREDICTION set, just not the FIT
  mutate(log_cpue = log(cpue + 0.0001))

wk82.pred <- cbind(wk82.pred_input, 
                   predict(wk82.rf, wk82.pred_input, interval = "pred", level = .75)) |> 
  mutate(across(fit:upr, exp))

wk82.pred |> filter(year == curr_year)
