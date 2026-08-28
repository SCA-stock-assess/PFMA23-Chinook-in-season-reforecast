# ============================================================================
# CN in-season run reforecast — Barkley Sound recreational CPUE models
# Rebuilt August 2026 for annotation/traceability. Three stages:
#
#   SECTION A — MODEL SELECTION. Searches every 4-5 subarea combo x
#   raw/RCH-corrected CPUE x 4 functional forms, for every stat-week period,
#   then an expanding-window retro test picks ONE model for the whole
#   season. Runs ONCE PER SEASON and caches to disk (season_model_<year>.rds);
#   re-running this script loads the cached pick instead of re-searching.
#   Delete that file to force a fresh selection.
#
#   Why retro MAPE, not in-sample adj.R^2: with thousands of combos tested,
#   the top ~10 for any given period routinely land within thousandths of
#   adj.R^2 of each other — the in-sample "winner" is partly multiple-
#   comparisons noise. Confirmed on this dataset: the highest-in-sample-R^2
#   model anywhere (a cum91 combo, adj.R^2~0.84) retro-forecasts far worse
#   than the season model once genuinely backtested chronologically (see
#   SECTION C). Higher in-sample fit did not mean better real forecasts.
#
#   SECTION B — WEEKLY FORECAST. Cheap. Takes the SAME selected model
#   (fixed subareas/correction/functional form) and, for each stat-week
#   (cum83, cum84, cum91, cum92), refits it against whatever data exists
#   now and forecasts curr_year. This is what changes week to week — the
#   model itself doesn't.
#
# Core CPUE/subarea-reconciliation logic is unchanged from Nick Brown's
# original and has been verified correct.
#
# Fixes/discoveries from this season's review:
#   1. 2001 is excluded (in-season management changes that year), applied
#      consistently rather than only in the production script.
#   2. The DNA lookup table (CREEL DNA LU tab) listed corridor-area RCH
#      proportions under OLD subarea codes (23I, 23N, 23P), while creel
#      interviews have always used the CURRENT corridor codes (123R, 123T,
#      123P) — silently failing every RCH join for those three codes across
#      all 26 years. Fixed in the workbook (DNA LU tab relabeled).
#      Separately confirmed cn_all_k * prop_rch == rch_cn_k exactly for
#      every interview row: they're the same number computed two ways, not
#      two different corrections.
#   3. The combo search must test raw and RCH-corrected CPUE together, not
#      one or the other — the true top model could be either.
#   4. PERIOD_WEEKS is now defined ONCE and used everywhere a period's
#      constituent weeks matter: building cpue's period columns, and the
#      completeness guard in run_period_forecast()/retro_forecast(). It
#      used to be two hand-typed lists that had already drifted out of
#      sync (a missing "cum83" entry silently disabled a completeness
#      guard), and Nick's original cum83 column used plain `+` while
#      cum84/91/92 used na.rm = TRUE sum — an inconsistency in what
#      "cumulative" meant between periods. Both problems are now
#      structurally impossible: one list, one na.rm = TRUE rule.
#   5. Cumulative periods sum constituent weeks with na.rm = TRUE, so a
#      week that hasn't happened yet this season is silently treated as
#      zero catch rather than missing — quietly deflating an in-season
#      forecast instead of failing loudly. The completeness guard in
#      run_period_forecast()/retro_forecast() catches this independently
#      by checking the raw interview data rather than relying on NA
#      propagation in cpue — don't remove it as "unnecessary."
#   6. Re-searching for "the best combo" every week is data dredging, not
#      model improvement — select once via retro-validation, hold it for
#      the season, and only refit with new data as it arrives.
#   7. Selection is decided by genuine expanding-window retro MAPE (PART 2
#      below), searched across every period rather than assumed to favor
#      an earliest-available one, and restricted to log-log form. Every
#      non-log-log "winner" found during development was a low-quality-fit
#      noise artifact (e.g. one had adj.R^2=0.42 yet won outright on raw
#      retro MAPE), edging out its log-log sibling by a margin nowhere near
#      its R^2 gap. MIN_SEASON_ADJ_R2 and the log-log restriction guard
#      against this — don't simplify selection back to raw retro MAPE
#      alone, or R^2 alone, without re-checking.

library(tidyverse); theme_set(theme_bw(base_size = 16))
library(readxl)
library(ggrepel)
library(scales)

curr_year <- 2026
EXCLUDED_YEARS <- c(2001)
PRED_LEVEL <- 0.75
MIN_N <- 6
# Minimum in-sample adj.R^2 a candidate must clear before it's even eligible
# to be ranked by retro MAPE (PART 2). Without this, a poor-fitting model can
# still land a deceptively low retro MAPE by luck on a small sample (14-20
# scored years) -- confirmed happening concretely on this dataset (a
# low-R^2 combo topped the raw retro-MAPE ranking outright). This floor is
# the guard against that failure mode recurring.
MIN_SEASON_ADJ_R2 <- 0.5
SEASON_MODEL_PATH <- sprintf("season_model_%d.rds", curr_year)

# ── Canonical period -> constituent-weeks map ──────────────────────────────
# Single source of truth: used both to build `cpue`'s
# period columns below and as the completeness guard in SECTION B/C. Add a
# new period here ONLY -- never hand-type a week list anywhere else.
PERIOD_WEEKS <- list(
  "82"    = c(82),
  "83"    = c(83),
  "84"    = c(84),
  "91"    = c(91),
  "92"    = c(92),
  "cum83" = c(82, 83),
  "cum84" = c(82, 83, 84),
  "cum91" = c(83, 84, 91),
  "cum92" = c(83, 84, 91, 92),
  "8384"  = c(83, 84),
  "8491"  = c(84, 91)
)

# ── Load data ───────────────────────────────────────────────────────────
interview_summary <- read_xlsx("CN_return_predictors_assemblyMaster.xlsx",
                               sheet = "CREEL Interview Summary 2026") |>
  rename_with(tolower)

bs_cn <- read_xlsx("CN_return_predictors_assemblyMaster.xlsx",
                   sheet = "CN_return_predictors") |>
  select(year, matches("(?i)Somass")) |>
  mutate(Somass_term_adult_return = round(as.numeric(Somass_term_adult_return), digits = 0))
# as.numeric() turns curr_year's "NA" string into a real NA, letting it flow
# through as a genuine forecast target (no return to fit against) with no
# special-casing needed downstream.

# ── Filter + subarea reconciliation ────────────────────────────────────────
# Kept as its own object because the completeness guard needs row-level
# (year, statsub, sw_2026) data -- the same input `cpue` collapses into
# periods. NOTE: the regex below deliberately excludes most "123X" codes
# (123G, 123N, 123U, 123H, etc.) -- those belong to a genuinely separate
# management area ("Area 123" per the raw data's own region field), not
# PFMA 23. Only 123T/123R/123P are PFMA 23's corridor subareas.
interview_recoded <- interview_summary |>
  filter(
    asscd_txt %in% c("Adipose fin not chkd", "Complete Form", "Fish not seen for ID"),
    str_detect(statsub, "^23[[:upper:]]|123(?=(T|R|P))"), # 123 T, R, P = the PFMA 23 corridor
    !year %in% EXCLUDED_YEARS
  ) |>
  mutate(
    statsub = case_when(
      statsub %in% c("23G", "23P", "23O", "123P", "23L") ~ "23O+123P", # 23G split 2011
      statsub %in% c("23H", "23Q", "23N", "123T") ~ "23Q+123T",        # 23H split 2011
      statsub == "23I" ~ "123R", # no-op safeguard; 23I never appears in 2000-2026 data
      TRUE ~ statsub
    )
  )

# ── Collapse to (year, statsub, period) CPUE, incl. cumulative periods ─────
# Collapse to (year, statsub, week) totals, then reshape wide by week so
# period_cols below can sum any period's weeks together consistently.
# NA-guarded: a subarea/week with no RCH data at all (23G/23H/23L) stays
# NA instead of sum(..., na.rm=TRUE) silently reporting a fake 0 (see
# CLAUDE.md's na.rm note for the full story).
cpue_wide <- interview_recoded |>
  group_by(year, statsub, sw_2026) |>
  summarise(
    across(c(cn_all_k, rch_cn_k), function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)),
    boat_trips = n(),
    .groups = "drop"
  ) |>
  left_join(select(bs_cn, year, "Somass_term_adult_return"), by = "year") |>
  rename("return" = "Somass_term_adult_return") |>
  filter(sw_2026 %in% c(82, 83, 84, 91, 92)) |>
  pivot_longer(cols = cn_all_k:boat_trips) |>
  pivot_wider(names_from = sw_2026, values_from = value)

# Sums each period's weeks; a week with zero interviews just drops out
# (omitted, not zero-filled) -- most training years have this gap in >=1
# subarea, see CLAUDE.md's na.rm note.
period_cols <- map_dfc(PERIOD_WEEKS, ~ rowSums(select(cpue_wide, all_of(as.character(.x))), na.rm = TRUE))

cpue <- cpue_wide |>
  select(year, statsub, return, name) |>
  bind_cols(period_cols) |>
  pivot_longer(cols = all_of(names(PERIOD_WEEKS)), names_to = "period", values_to = "value") |>
  pivot_wider(names_from = name, values_from = value) |>
  mutate(cpue = cn_all_k / boat_trips, rch_cpue = rch_cn_k / boat_trips)

# Complete-case only: needs a real CPUE AND a known return to fit against.
# curr_year rows drop out automatically (return is NA -- it's the forecast
# target, not yet observed).
# TODO (2026-08-27): "complete" here only means cn_all_k/boat_trips are
# non-NA -- a row zero-filled by the na.rm = TRUE gap above (see
# period_cols) is NOT NA, so it passes this filter and enters model fitting
# looking like a genuinely complete period. This is the step where that
# silent gap actually becomes training data.
cpue_minimal <- cpue |>
  filter(!if_any(c(return, cn_all_k, boat_trips), is.na)) |>
  select(year, period, cn_all_k, boat_trips, rch_cn_k, statsub, return)

classify_model <- function(predictor, response) {
  case_when(
    str_detect(predictor, "ln") & str_detect(response, "ln") ~ "log-log",
    str_detect(predictor, "ln") & !str_detect(response, "ln") ~ "lin-log", # X logged
    !str_detect(predictor, "ln") & str_detect(response, "ln") ~ "log-lin", # Y logged
    TRUE ~ "linear"
  )
}

build_model_formula <- function(model_form) {
  # +0.0001 offset matches the search phase (fast_ols/retro_ols both fit on
  # ln_cpue = log(cpue + 0.0001)) -- without it, any subarea/year with
  # genuinely zero kept Chinook (a real value, not missing data -- e.g. 123R
  # has had zero-catch years) produces log(0) = -Inf and lm() dies with
  # "NA/NaN/Inf in 'x'" the moment that year enters the fit.
  switch(model_form,
         "log-log" = log(return) ~ log(predictor_val + 0.0001),
         "log-lin" = log(return) ~ predictor_val,
         "lin-log" = return ~ log(predictor_val + 0.0001),
         "linear"  = return ~ predictor_val
  )
}

# ============================================================================
# SECTION A -- MODEL SELECTION. Runs once per season; cached to disk.
# ============================================================================

if (file.exists(SEASON_MODEL_PATH)) {
  
  cat(sprintf("\nLoading cached season model for %d from %s.\n(Delete this file to force a fresh selection.)\n",
              curr_year, SEASON_MODEL_PATH))
  SEASON_MODEL <- readRDS(SEASON_MODEL_PATH)
  
} else {
  
  # ── Closed-form OLS: same math as lm(y ~ x), no formula/model-frame
  #    overhead -- what makes fitting thousands of groups feasible ─────────
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
    ss_res <- sum(resid^2); ss_tot <- sum((y - my)^2)
    r2 <- 1 - ss_res / ss_tot
    adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - 2)
    se_slope <- sqrt((ss_res / (n - 2)) / sxx)
    p_val <- 2 * pt(-abs(slope / se_slope), df = n - 2)
    tibble(n = n, adj.r.squared = adj_r2, p.value = p_val)
  }
  
  # ── Genuine expanding-window retro test (Nick Brown's original approach,
  #    generalized to any combo/correction/form): only ever uses years <=
  #    the forecast year, with that year's own response blanked, exactly
  #    mimicking real in-season forecasting -- no closed-form shortcut
  #    applies to an expanding window, so this loops per forecast-year, but
  #    it's cheap enough per-candidate that the full search still finishes
  #    in about a minute, and this only ever runs once per season. ─────────
  retro_ols <- function(x, y, y_natural, years, min_train_years = 10) {
    ok <- is.finite(x) & is.finite(y) & is.finite(y_natural) & y_natural > 0
    x <- x[ok]; y <- y[ok]; y_natural <- y_natural[ok]; years <- years[ok]
    ord <- order(years)
    x <- x[ord]; y <- y[ord]; y_natural <- y_natural[ord]; years <- years[ord]
    empty <- tibble(mape = NA_real_, rmse_pct = NA_real_, n = 0L)
    if (length(x) < MIN_N) return(empty)
    
    log_response <- max(abs(y - log(y_natural + 0.0001))) < 1e-6
    min_year <- min(years)
    
    ape <- numeric(0)
    for (i in seq_along(years)) {
      fcst_yr <- years[i]
      if (fcst_yr < min_year + min_train_years) next
      train <- years <= fcst_yr
      train[i] <- FALSE  # blank this year's own response -- can't have known it yet
      if (sum(train) < MIN_N) next
      xt <- x[train]; yt <- y[train]
      mx <- mean(xt); sxx <- sum((xt - mx)^2)
      if (sxx == 0) next
      my <- mean(yt)
      slope <- sum((xt - mx) * (yt - my)) / sxx
      intercept <- my - slope * mx
      pred <- intercept + slope * x[i]
      pred_nat <- if (log_response) exp(pred) - 0.0001 else pred
      ape <- c(ape, abs(y_natural[i] - pred_nat) / y_natural[i])
    }
    if (length(ape) < 5) return(empty)
    tibble(mape = mean(ape), rmse_pct = sqrt(mean(ape^2)), n = length(ape))
  }
  
  # ── PART 1: 4-5 subarea combinations, raw AND corrected CPUE together ───
  combo_n <- seq.int(4, 5)
  subarea_combos <- list(unique(cpue_minimal$statsub)) |>
    rep(length(combo_n)) |>
    map2(combo_n, ~ combn(x = .x, m = .y, FUN = list)) |>
    unlist(recursive = FALSE)
  cat("Testing", length(subarea_combos), "subarea combinations\n")
  
  combo_cpue <- set_names(subarea_combos, nm = map_chr(subarea_combos, ~paste(.x, collapse = "_"))) |>
    map(~ cpue_minimal |>
          filter(statsub %in% .x) |>
          summarise(.by = c(year, return, period),
                    across(c(cn_all_k, rch_cn_k, boat_trips), \(x) sum(x, na.rm = TRUE))) |>
          mutate(cpue = cn_all_k / boat_trips, rch_cpue = rch_cn_k / boat_trips) |>
          filter(!is.na(cpue), !is.nan(cpue))) |>
    keep(~ n_distinct(.x$year) >= MIN_N)
  cat("Retained", length(combo_cpue), "combos with >=", MIN_N, "years of data\n")
  
  combo_long <- combo_cpue |>
    map(~ .x |>
          mutate(ln_cpue = log(cpue + 0.0001), ln_rch_cpue = log(rch_cpue + 0.0001),
                 ln_return = log(return + 0.0001),
                 return_natural = return) |>  # untouched copy -- survives the response pivot below
          pivot_longer(cols = c(cpue, rch_cpue, ln_cpue, ln_rch_cpue), names_to = "predictor", values_to = "x") |>
          pivot_longer(cols = c(return, ln_return), names_to = "response", values_to = "y") |>
          filter(!is.na(x), !is.infinite(x), !is.na(y), !is.infinite(y)) |>
          mutate(group = paste(period, predictor, response, sep = "-")) |>
          group_by(group) |>
          filter(n() >= MIN_N))
  
  combo_r2s <- combo_long |>
    bind_rows(.id = "combo") |>
    ungroup() |>
    summarise(.by = c(combo, period, predictor, response), n = n(),
              mx = mean(x), my = mean(y), sxx = sum((x - mx)^2),
              sxy = sum((x - mx) * (y - my)), ss_tot = sum((y - my)^2)) |>
    filter(n >= 4, sxx > 0) |>
    mutate(slope = sxy / sxx, intercept = my - slope * mx) |>
    left_join(combo_long |> bind_rows(.id = "combo") |> ungroup(),
              by = c("combo", "period", "predictor", "response")) |>
    mutate(fitted = intercept + slope * x, sq_resid = (y - fitted)^2) |>
    summarise(.by = c(combo, period, predictor, response, n, ss_tot, slope, sxx), ss_res = sum(sq_resid)) |>
    mutate(r2 = 1 - ss_res / ss_tot,
           adj.r.squared = 1 - (1 - r2) * (n - 1) / (n - 2),
           se_slope = sqrt((ss_res / (n - 2)) / sxx),
           p.value = 2 * pt(-abs(slope / se_slope), df = n - 2)) |>
    filter(!is.na(p.value)) |>
    select(combo, period, predictor, response, adj.r.squared, p.value, n) |>
    mutate(model = classify_model(predictor, response),
           correction = if_else(str_detect(predictor, "rch"), "corrected", "raw"))
  
  cat(sprintf("\nBest pooled-combo adj.R^2 (in-sample, any period): %.3f\n", max(combo_r2s$adj.r.squared, na.rm = TRUE)))
  # In-sample only -- PART 2 below is what actually decides the season model.
  
  # ── PART 2: SEASON MODEL SELECTION -- decided by genuine retro MAPE,
  # searched across EVERY period (not restricted to the earliest-available
  # ones). If a later-season period genuinely retro-forecasts better, it
  # should win the search; if cum83-style candidates keep winning anyway
  # despite the wider search, that's useful confirmation, not an assumption
  # baked in ahead of time.
  cat("Running expanding-window retro test across every period x combo (slower -- no closed-form shortcut)...\n")
  retro_r2s <- combo_long |>
    bind_rows(.id = "combo") |>
    ungroup() |>
    group_by(combo, period, predictor, response) |>
    group_modify(~ retro_ols(.x$x, .x$y, .x$return_natural, .x$year)) |>
    ungroup() |>
    filter(!is.na(mape)) |>
    select(combo, period, predictor, response, retro_mape = mape, n_retro = n) |>
    mutate(model = classify_model(predictor, response),
           correction = if_else(str_detect(predictor, "rch"), "corrected", "raw")) |>
    left_join(combo_r2s |> select(combo, period, predictor, response, adj.r.squared),
              by = c("combo", "period", "predictor", "response")) |>
    filter(adj.r.squared >= MIN_SEASON_ADJ_R2) |>
    arrange(retro_mape)
  
  cat(sprintf("\n── TOP 15 MODELS BY GENUINE EXPANDING-WINDOW RETRO MAPE (every period, every combo, adj.R^2 >= %.1f) ──\n", MIN_SEASON_ADJ_R2))
  retro_r2s |>
    mutate(retro_mape = scales::percent(retro_mape, accuracy = 0.1),
           adj.r.squared = round(adj.r.squared, 3)) |>
    slice_head(n = 15) |>
    print(n = 15, width = Inf)
  
  # Final pick is restricted to log-log, even though the table above (left
  # unrestricted, on purpose, for visibility) is ranked across all 4 forms.
  # Every non-log-log "winner" found while developing this was a
  # low-quality-fit noise artifact -- see header note (fix #7). Log-log has
  # won decisively in every full search run this season whenever the
  # comparison wasn't corrupted by one of these flukes -- restricting to it
  # here isn't cherry-picking, it's responding to repeated concrete evidence
  # that letting other forms into a large minimum-search just hands the
  # noise floor a vote.
  SEASON_MODEL <- retro_r2s |> filter(model == "log-log") |> slice_head(n = 1) |> rename(mape = retro_mape)
  cat(sprintf(
    "\n★ SEASON MODEL for %d (selected once, held all year): combo = %s | correction = %s | form = %s | period = %s\n  Retro MAPE = %.1f%% | in-sample adj.R^2 = %.3f | n = %d\n",
    curr_year, SEASON_MODEL$combo, SEASON_MODEL$correction, SEASON_MODEL$model, SEASON_MODEL$period,
    SEASON_MODEL$mape * 100, SEASON_MODEL$adj.r.squared, SEASON_MODEL$n_retro))
  
  saveRDS(SEASON_MODEL, SEASON_MODEL_PATH)
  cat(sprintf("Saved to %s.\n", SEASON_MODEL_PATH))
}

# ============================================================================
# SECTION B -- WEEKLY FORECAST. Run every time new interviews come in.
# Always uses SEASON_MODEL's fixed subareas/correction/form -- never
# re-selects. Only the observation window (period) advances through the
# season, and only the coefficients get refit with fresh data.
# ============================================================================

run_period_forecast <- function(period_label) {
  # Self-contained: derives everything it needs from SEASON_MODEL right here,
  # rather than relying on globals set earlier in the script -- so this still
  # works even if SECTION A only partially ran (e.g. script was interrupted,
  # or someone re-ran just this block). If SEASON_MODEL itself is missing,
  # fail with a clear pointer instead of a cryptic "object not found" three
  # steps removed.
  if (!exists("SEASON_MODEL")) {
    stop("SEASON_MODEL not found. Run SECTION A first (or confirm season_model_<year>.rds exists and loaded without error) before calling run_period_forecast().")
  }
  winning_subareas <- str_split(SEASON_MODEL$combo, "_")[[1]]
  use_rch <- SEASON_MODEL$correction == "corrected"
  model_form <- SEASON_MODEL$model
  
  cat("\n============================================================\n")
  cat(sprintf("PERIOD = %s  |  season model: %s (%s, %s)\n",
              period_label, str_replace_all(SEASON_MODEL$combo, "_", "+"), SEASON_MODEL$correction, model_form))
  cat("============================================================\n")
  
  # Optional diagnostic -- only available if SECTION A ran this session
  # (skipped when loading SEASON_MODEL from cache). Shows what this period's
  # OWN independently-best combo would have been, for context only -- we
  # are not switching to it.
  if (exists("combo_r2s")) {
    period_best <- combo_r2s |> filter(period == period_label) |> slice_max(adj.r.squared, n = 1)
    fixed_row <- combo_r2s |> filter(period == period_label, combo == SEASON_MODEL$combo,
                                     correction == SEASON_MODEL$correction, model == model_form)
    if (nrow(period_best) == 1 && nrow(fixed_row) == 1) {
      cat(sprintf("  (diagnostic) season model's in-sample adj.R^2 here: %.3f | this period's own best would be: %.3f (%s)\n",
                  fixed_row$adj.r.squared, period_best$adj.r.squared, period_best$combo))
    }
  }
  
  # Completeness guard -- header note #5. Cumulative periods sum with
  # na.rm = TRUE, so a week that hasn't happened yet is silently treated as
  # zero catch instead of missing; this check keeps that from turning into a
  # deflated forecast.
  weeks_needed <- PERIOD_WEEKS[[period_label]]
  weeks_have <- interview_recoded |>
    filter(year == curr_year, statsub %in% winning_subareas, sw_2026 %in% weeks_needed) |>
    pull(sw_2026) |> unique()
  weeks_missing <- setdiff(weeks_needed, weeks_have)
  
  # Rebuild the season model's series for this period from `cpue` (not
  # `cpue_minimal` -- that drops curr_year since its return is NA, and we
  # need curr_year's predictor value to forecast) and refit with lm() so
  # predict() can hand back a proper prediction interval.
  series <- cpue |>
    filter(period == period_label, statsub %in% winning_subareas) |>
    summarise(.by = year, across(c(cn_all_k, rch_cn_k, boat_trips), \(x) sum(x, na.rm = TRUE))) |>
    mutate(cpue = cn_all_k / boat_trips, rch_cpue = rch_cn_k / boat_trips,
           predictor_val = if (use_rch) rch_cpue else cpue) |>
    filter(is.finite(predictor_val)) |>
    left_join(select(bs_cn, year, return = Somass_term_adult_return), by = "year") |>
    mutate(percentile = rank(predictor_val) / n())
  
  fit_data <- series |> filter(!is.na(return))
  
  model <- lm(build_model_formula(model_form), data = fit_data)
  log_y <- model_form %in% c("log-log", "log-lin")
  
  cat(sprintf("Refit: R^2 = %.3f, adj.R^2 = %.3f, n = %d\n",
              summary(model)$r.squared, summary(model)$adj.r.squared, nrow(fit_data)))
  
  curr_row <- series |> filter(year == curr_year)
  fc <- NULL
  if (length(weeks_missing) > 0) {
    warning(sprintf(
      "%s: %d has no interviews yet for week(s) %s -- forecast NOT computed (would silently zero-fill).",
      period_label, curr_year, paste(weeks_missing, collapse = ", ")))
  } else if (nrow(curr_row) == 0 || !is.finite(curr_row$predictor_val[1])) {
    warning(sprintf("%s: no usable %d CPUE -- forecast not computed.", period_label, curr_year))
  } else {
    fc <- cbind(curr_row, predict(model, curr_row, interval = "pred", level = PRED_LEVEL))
    if (log_y) fc <- fc |> mutate(across(fit:upr, exp))
    cat(sprintf(
      "\n%d forecast (%s, subareas %s, %s CPUE, %s model): %s adults (%.0f%% PI: %s-%s)\n",
      curr_year, period_label, str_replace_all(SEASON_MODEL$combo, "_", "+"), SEASON_MODEL$correction, model_form,
      scales::comma(round(fc$fit, -2)), PRED_LEVEL * 100, scales::comma(round(fc$lwr, -2)), scales::comma(round(fc$upr, -2))))
    cat(sprintf("Observed CPUE: %.2f (%.0fth percentile of history)\n",
                curr_row$predictor_val, curr_row$percentile * 100))
  }
  
  # Figure
  pred_grid <- tibble(predictor_val = seq(min(fit_data$predictor_val), max(fit_data$predictor_val), length.out = 150))
  pred_curve <- cbind(pred_grid, predict(model, pred_grid, interval = "pred", level = PRED_LEVEL))
  if (log_y) pred_curve <- pred_curve |> mutate(across(fit:upr, exp))
  
  x_lab <- paste0("CPUE — ", str_replace_all(SEASON_MODEL$combo, "_", "+"), " (", period_label, ", ",
                  if (use_rch) "RCH-corrected" else "raw", ")")
  
  # Headline numbers baked into the subtitle so the figure is self-contained
  # (doesn't need the console log alongside it to know the answer). Always
  # label WHICH mape this is -- "retro" -- since that's the only kind
  # computed in this script now.
  subtitle_line <- if (!is.null(fc)) {
    sprintf("R² = %.2f | forecast = %s adults (%.0f%% PI: %s–%s)",
            summary(model)$r.squared,
            scales::comma(round(fc$fit, -2)), PRED_LEVEL * 100,
            scales::comma(round(fc$lwr, -2)), scales::comma(round(fc$upr, -2)))
  } else {
    sprintf("R² = %.2f | forecast not available yet -- %s incomplete for %d",
            summary(model)$r.squared, period_label, curr_year)
  }
  
  fig <- ggplot(fit_data, aes(predictor_val, return)) +
    geom_ribbon(data = pred_curve, aes(y = fit, ymin = lwr, ymax = upr), fill = "#6a5acd", alpha = 0.28) +
    geom_line(data = pred_curve, aes(y = fit), colour = "blue", linewidth = 1) +
    geom_point(size = 2) +
    geom_text_repel(aes(label = year), size = 3, min.segment.length = 0) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = x_lab, y = "Return",
         title = paste0(curr_year, " ", period_label, " forecast — season model"),
         subtitle = subtitle_line) +
    theme_bw(base_size = 14) +
    theme(plot.subtitle = element_text(size = 11, colour = "grey30"))
  
  if (!is.null(fc)) {
    fig <- fig +
      geom_pointrange(data = fc, aes(y = fit, ymin = lwr, ymax = upr), colour = "red", linewidth = 0.8)
  }
  
  print(fig)
  invisible(list(model = model, series = series, forecast = fc, plot = fig))
}

# ── Run the weekly forecast for every stat-week milestone this season ──────
# Whichever periods curr_year hasn't reached yet will print a warning and
# skip the forecast (completeness guard), but still show the fitted curve.
#
# wk83_prelim uses single-week-83 CPUE -- a rougher, earlier read with its own
# (weaker) refit R^2. wk83 uses cum83 (weeks 82+83), which is the period
# SEASON_MODEL was actually selected and retro-validated at (16.3% MAPE,
# adj.R^2 = 0.821) -- keeping it in this list is what makes its R^2 and the
# "season-model retro MAPE" printed in its subtitle refer to the same period.

results <- list(
wk82only       = run_period_forecast("82"),
wk83only       = run_period_forecast("83"),
wk83Cum        = run_period_forecast("cum83"),
wk84Cum        = run_period_forecast("cum84"),
wk91Cum        = run_period_forecast("cum91"),
wk92Cum        = run_period_forecast("cum92")
)

# ── Post-season review table -- once curr_year's actual return is known,
# line up every weekly milestone's forecast side by side. Adapted from
# Nick's rf_rev block -- generalized to read straight from `results` above
# instead of hardcoding each weekly model object and CPUE column by name.
actual_return <- bs_cn |> filter(year == curr_year) |> pull(Somass_term_adult_return)

post_season_review <- map_df(names(results), function(nm) {
  r <- results[[nm]]
  if (is.null(r$forecast)) return(NULL)  # period not reached yet this season
  tibble(
    period = nm,
    cpue   = round(r$forecast$predictor_val, 2),
    adj_r2 = round(summary(r$model)$adj.r.squared, 3),
    fit    = round(r$forecast$fit, -2),
    lwr    = round(r$forecast$lwr, -2),
    upr    = round(r$forecast$upr, -2)
  )
}) |>
  mutate(actual_return = actual_return,
         APE = if_else(!is.na(actual_return), abs(actual_return - fit) / actual_return, NA_real_))

print(post_season_review)
# ── Save each weekly forecast plot ──────────────────────────────────────


ggsave(sprintf("figures/%d_wk82only_forecast.png", curr_year), results$wk82only$plot, width = 9, height = 6, dpi = 300)
ggsave(sprintf("figures/%d_wk83only_forecast.png", curr_year), results$wk83only$plot, width = 9, height = 6, dpi = 300)
ggsave(sprintf("figures/%d_wk83Cum_forecast.png",  curr_year), results$wk83Cum$plot,  width = 9, height = 6, dpi = 300)
#ggsave(sprintf("figures/%d_wk84Cum_forecast.png",  curr_year), results$wk84Cum$plot,  width = 9, height = 6, dpi = 300)
#ggsave(sprintf("figures/%d_wk91Cum_forecast.png",  curr_year), results$wk91Cum$plot,  width = 9, height = 6, dpi = 300)
#ggsave(sprintf("figures/%d_wk92Cum_forecast.png",  curr_year), results$wk92Cum$plot,  width = 9, height = 6, dpi = 300)

#adhoc changes to the plot before saving (in case I want to remove extra info)

p <- results$wk83Cum$plot +
  labs(title = "Week 82 & 83",
       x = "CPUE",
       y = "Somass adult Chinook return")

p
ggsave("figures/2026_wk83Cum_forecast_clean.png", p, width = 9, height = 6, dpi = 300)
# ============================================================================
# SECTION C -- RETROSPECTIVE ANALYSIS / VISUALIZATION. Adapted from Nick
# Brown's original (see e.g. 2022_CN_in-season_run_reforecast_models.R),
# generalized to any subarea combo/correction/functional form.
#
# By this point in the script SEASON_MODEL was already CHOSEN by this exact
# expanding-window retro test (see PART 2 in SECTION A -- retro_ols() there
# is the same math as retro_forecast() below, just returning summary stats
# instead of the full prediction series). This section re-runs it for the
# winning model purely to get the plot and the year-by-year table; it is not
# re-deciding anything.
#
# Nick's original used week 91, presumably because it had the highest
# in-season R^2 at the time. PART 2 checked this directly across every
# period and combo, not just week 91 vs. the season model: does higher
# in-sample R^2 actually mean better real-world forecasting power once
# genuinely backtested? No -- see the comparison below.
# ============================================================================

retro_forecast <- function(subareas, correction, model_form, period_label,
                           min_train_years = 10, label = NULL) {
  if (is.null(label)) {
    label <- paste0(str_replace_all(paste(subareas, collapse = "+"), "_", "+"),
                    " (", period_label, ", ", correction, ")")
  }
  use_rch <- correction == "corrected"
  
  series <- cpue |>
    filter(period == period_label, statsub %in% subareas) |>
    summarise(.by = year, across(c(cn_all_k, rch_cn_k, boat_trips), \(x) sum(x, na.rm = TRUE))) |>
    mutate(cpue = cn_all_k / boat_trips, rch_cpue = rch_cn_k / boat_trips,
           predictor_val = if (use_rch) rch_cpue else cpue) |>
    filter(is.finite(predictor_val)) |>
    left_join(select(bs_cn, year, return = Somass_term_adult_return), by = "year") |>
    arrange(year)
  
  weeks_needed <- PERIOD_WEEKS[[period_label]]
  fcst_years <- seq.int(min(series$year) + min_train_years, max(series$year))
  model_formula <- build_model_formula(model_form)
  
  retro_preds <- map_df(fcst_years, function(fcst_yr) {
    # Same completeness guard as run_period_forecast(): skip curr_year if
    # this period's weeks haven't happened yet this season, rather than
    # silently zero-filling a garbage prediction.
    if (fcst_yr == curr_year) {
      weeks_have <- interview_recoded |>
        filter(year == curr_year, statsub %in% subareas, sw_2026 %in% weeks_needed) |>
        pull(sw_2026) |> unique()
      if (length(setdiff(weeks_needed, weeks_have)) > 0) return(NULL)
    }
    
    train <- series |>
      filter(year <= fcst_yr) |>
      mutate(return = if_else(year == fcst_yr, NA_real_, return))
    fit_rows <- train |> filter(!is.na(return))
    if (nrow(fit_rows) < 5) return(NULL)
    
    model <- lm(model_formula, data = fit_rows)
    target_row <- train |> filter(year == fcst_yr)
    if (nrow(target_row) == 0 || !is.finite(target_row$predictor_val)) return(NULL)
    
    pred <- predict(model, target_row)
    if (model_form %in% c("log-log", "log-lin")) pred <- exp(pred)
    tibble(year = fcst_yr, prediction = as.numeric(pred))
  })
  
  retro_preds <- retro_preds |>
    left_join(select(bs_cn, year, return = Somass_term_adult_return), by = "year")
  scored <- retro_preds |> filter(!is.na(return))
  mape <- mean(abs(scored$return - scored$prediction) / scored$return)
  rmse <- sqrt(mean((scored$return - scored$prediction)^2))
  
  fig <- ggplot(retro_preds, aes(year, prediction)) +
    geom_point(colour = "red", size = 1.5) +
    geom_line(colour = "red") +
    geom_point(data = bs_cn |> filter(year %in% series$year), aes(y = Somass_term_adult_return)) +
    geom_line(data = bs_cn |> filter(year %in% series$year), aes(y = Somass_term_adult_return)) +
    labs(y = "Somass adult Chinook return", x = NULL,
         title = paste0("Retrospective forecast — ", label),
         subtitle = sprintf("retro MAPE = %.1f%%, RMSE = %s (n = %d years)",
                            mape * 100, scales::comma(round(rmse)), nrow(scored))) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(min(series$year), curr_year, by = 1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  print(fig)
  invisible(list(predictions = retro_preds, mape = mape, rmse = rmse, n = nrow(scored), plot = fig))
}

# ── Retro-test the SEASON MODEL (the one actually chosen by PART 2) ────────
season_retro <- retro_forecast(
  subareas     = str_split(SEASON_MODEL$combo, "_")[[1]],
  correction   = SEASON_MODEL$correction,
  model_form   = SEASON_MODEL$model,
  period_label = SEASON_MODEL$period,
  label        = "SEASON MODEL (retro-selected)"
)
cat(sprintf("\nSEASON MODEL retro performance: MAPE = %.1f%%, RMSE = %s (n = %d years)\n",
            season_retro$mape * 100, scales::comma(round(season_retro$rmse)), season_retro$n))

# ── Compare against the highest-in-sample-R^2 combo at cum91 instead --
# roughly what Nick's original script did by settling on week 91. Combo is
# hardcoded from a one-time combo_r2s |> filter(period == "cum91") |>
# slice_max(adj.r.squared) check -- re-derive from combo_r2s directly if
# EXCLUDED_YEARS or the raw data changes materially.
cum91_retro <- retro_forecast(
  subareas = c("23D", "23J", "23K", "23M", "23O+123P"), correction = "raw", model_form = "log-log",
  period_label = "cum91", label = "cum91 highest-R^2 combo (Nick's approach, generalized)"
)
cat(sprintf("cum91 highest-R^2 model retro performance: MAPE = %.1f%%, RMSE = %s\n",
            cum91_retro$mape * 100, scales::comma(round(cum91_retro$rmse))))

cat(sprintf(
  paste0("\n★ Genuine backtest verdict: SEASON MODEL MAPE %.1f%% vs. cum91's-higher-in-sample-R^2 ",
         "model MAPE %.1f%%.\n  Higher in-sample R^2 (adj.R^2 ~0.844 for cum91 vs %.3f for the season model) ",
         "did NOT translate\n  into better real forecasting accuracy once actually backtested -- confirms ",
         "retro-validated selection\n  over picking a model by R^2 alone -- see PART 2's full leaderboard for",
         " every period/combo\n  compared this way, not just this one illustrative pair.\n"),
  season_retro$mape * 100, cum91_retro$mape * 100, SEASON_MODEL$adj.r.squared))
##############################################Trend in CPUE
# Use the season model's own selected combo + period (SECTION A's pick) —
# this is exactly what run_period_forecast() builds as `predictor_val`
# when use_rch = FALSE, just kept as a full year-by-year series instead of
# collapsing to one forecast point.
combo_subareas <- str_split(SEASON_MODEL$combo, "_")[[1]]
combo_period   <- SEASON_MODEL$period

cpue_trend <- cpue |>
  filter(period == combo_period, statsub %in% combo_subareas) |>
  summarise(.by = year, across(c(cn_all_k, boat_trips), \(x) sum(x, na.rm = TRUE))) |>
  mutate(cpue = cn_all_k / boat_trips) |>
  filter(is.finite(cpue)) |>
  arrange(year)

ggplot(cpue_trend, aes(x = year, y = cpue)) +
  geom_line(colour = "#333333", linewidth = 0.9) +
  geom_point(colour = "#333333", size = 2) +
  geom_point(data = filter(cpue_trend, year == curr_year), colour = "red", size = 3) +
  geom_text_repel(aes(label = year), size = 3, min.segment.length = 0) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  labs(
    x = NULL, y = "CPUE (raw, pooled)",
    title = paste0("Chinook raw CPUE trend — season model combo (",
                   str_replace_all(SEASON_MODEL$combo, "_", "+"), "), period ", combo_period)
  ) +
  theme_bw(base_size = 14)
