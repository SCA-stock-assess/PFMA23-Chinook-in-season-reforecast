# PFMA23-Chinook-in-season-reforecast

## What this repo does

Reforecasts the terminal adult Chinook return to the Somass River mid-season,
using recreational catch-per-unit-effort (CPUE) data from creel interviews in
Barkley Sound (PFMA 23) plus SEAK troll CWT recovery data. Each year gets its
own dated folder (`2024/`, `2025/`, `2026/`, ...) containing that season's
scripts and the assembled data workbook. Season folders are largely
self-contained; don't assume code in one year's folder runs against another
year's workbook without checking column names first.

## Current season (2026)

- **Canonical script:** `2026/NEW2026_CPUE_reforecast.R`. Run this one.
- **Data:** `2026/CN_return_predictors_assemblyMaster.xlsx`
  - Sheet `CREEL Interview Summary 2026` — raw interview-level creel data
    (downloaded from CREST), one row per interview.
  - Sheet `CN_return_predictors` — Somass terminal adult return history
    (`Somass_term_adult_return`), updated once per year; current year's cell
    is the literal string `"NA"` until the season's return is known.
- Older `03-code_*.R`, `04-code_*.R`, `05-code_*.R` files in `2026/` are
  superseded scratch work from earlier iterations — safe to delete, don't
  build on them.
- **Confirmed working end-to-end as of a live run this season**: SEASON_MODEL
  lands on `23C+23E+23J+23M`, period `cum83`, raw CPUE, log-log, retro
  MAPE=16.3%, adj.R²=0.821, n=16. If a future run lands anywhere else, treat
  that as a signal to investigate (data materially changed, or a regression)
  rather than assuming it's automatically correct.

## How the pipeline is structured (and why)

`NEW2026_CPUE_reforecast.R` has three stages:

1. **SECTION A — model selection.** PART 1 searches every 4-5 subarea
   combination x raw/RCH-corrected CPUE x 4 functional forms, for EVERY
   stat-week period (not restricted to the earliest-available ones). PART 2
   runs a **genuine expanding-window retro test** (`retro_ols()`) across all
   of PART 1's candidates and picks ONE model for the season, decided by
   lowest retro MAPE among candidates with adj.R² ≥ `MIN_SEASON_ADJ_R2`
   (0.5), restricted to log-log form. Runs once, caches its pick to
   `season_model_<year>.rds`. Re-running the script loads the cached model
   instead of re-searching.
   **There is no single-subarea search and no LOOCV step in this script
   anymore** — both were removed once confirmed unnecessary (see "Removed,
   on purpose" below). If you ever change what columns `SEASON_MODEL`
   carries, delete the cached `season_model_<year>.rds` before re-running —
   an old-schema cache can load silently without erroring and mislabel
   stale numbers as current.

2. **SECTION B — weekly forecast.** Cheap. Takes the cached model's fixed
   subareas/correction/functional form and, for each stat-week milestone
   (wk83only, wk83Cum = cum83, wk84Cum, wk91Cum, wk92Cum), refits it with
   whatever data exists and forecasts the current year. This is what changes
   week to week — the model choice does not.

   `wk83only` (period `"83"`, single week) and `wk83Cum` (period `"cum83"`,
   weeks 82+83) are BOTH in the `results` list on purpose, not a duplicate —
   `cum83` is the period `SEASON_MODEL` was actually selected/retro-validated
   at, so it's the one whose subtitle "R²" and "season-model retro MAPE" are
   guaranteed to refer to the same regression. `"83"` alone is a rougher,
   earlier-look predictor with its own separately-refit (weaker) R² — its
   subtitle still borrows `SEASON_MODEL$mape` (the cum83 backtest number)
   because that's the only retro MAPE this script computes, so read
   `wk83only`'s MAPE line as "how the season model has done historically,"
   not as a retro MAPE for period `"83"` itself.

   Right after the `results` list, a `post_season_review` table lines up
   every milestone's cpue/adj.R²/fit/PI side by side once `curr_year`'s
   actual return is known (adds an APE column once it is). Adapted from
   Nick's `rf_rev` block, generalized to read straight from `results`
   instead of hardcoding each weekly model object and CPUE column by name.
   Rows are skipped (not NA-filled) for any period `curr_year` hasn't
   reached yet, via the same `forecast IS NULL` check `run_period_forecast()`
   already uses.

3. **SECTION C — retrospective analysis / visualization.** `retro_forecast()`
   is the same expanding-window backtest as SECTION A's `retro_ols()`, just
   returning the full prediction series and a plot instead of summary stats
   — it's not re-deciding anything, purely visualizing the pick SECTION A
   already made. Adapted from Nick Brown's original retro code, generalized
   to any combo/correction/form.

**Key validated finding, worth remembering — this is *why* SECTION A
selects by retro MAPE instead of R² (or LOOCV, back when that was computed):**
a full search across every period and every 4-5 subarea combo found the
season model (cum83, `23C+23E+23J+23M`, raw CPUE) is the outright winner by
retro MAPE (16.3%) — nothing from cum91/cum92 is competitive (all sit
≥26%). LOOCV alone would NOT have caught this while it was still in the
script: the highest-in-sample-R² cum91 combo (adj.R² ≈0.84, vs. ≈0.82 for
the season model) had a LOOCV MAPE nearly tied with the season model's
(24.2% vs. 24.3%) — only once genuinely retro-tested did its real error show
up as nearly double (29.4%). LOOCV lets a fit see future years, which let
that cum91 candidate look artificially reliable. This is also the direct
answer to "shouldn't we use week 91 like Nick did, since it has the best
R²?" — checked explicitly, and no: with today's larger dataset, week
91-based models do not hold up once honestly backtested.

**Do not re-run model selection mid-season and take whatever tops that
week's leaderboard.** With over a thousand combinations tested, several
candidates for any given period routinely land within a percentage point or
two of each other in retro MAPE — not always a decisive gap. Re-searching
weekly just chases that noise and produces a different "winner" for reasons
that have nothing to do with real forecasting skill, which undermines both
accuracy and the report's credibility (subareas shouldn't visibly change
week to week for no stats-backed reason). Select once via the full retro
search, hold it for the season. To force a fresh selection (e.g. a new
season), delete `season_model_<year>.rds`.

### Removed, on purpose — don't re-add without a reason

The pipeline went through a "getting too long and complex" pass. Two things
were deliberately cut after confirming they added cost without adding value:

- **Single-subarea search** (`cpue_r2s`, a `fast_ols()` pass over individual
  subareas). Existed only to show "pooling beats a single subarea," which
  was never in question and never fed into `SEASON_MODEL` selection.
- **LOOCV** (`loocv_ols()`, `loocv_r2s`). Useful once, as the tool that
  first revealed retro MAPE and LOOCV MAPE could diverge sharply for the
  same model (the cum91 case above) — but it was never the deciding
  criterion, only a side-by-side diagnostic column. Once that finding was
  banked here and in SECTION C's narrative, the live computation was
  redundant. If a future model shows a suspiciously good retro MAPE
  relative to its R², that's the same instinct that caught the cum91 case
  and the low-R² retro-MAPE flukes below — investigate by hand rather than
  assuming the machinery would still catch it, since it's no longer running
  automatically.

## Data gotchas already fixed — don't rediscover these

- **2001 excluded.** In-season management changes that year; predecessor's
  note, root cause not otherwise documented. `EXCLUDED_YEARS <- c(2001)`.
- **DNA lookup table corridor-code mismatch (fixed in the workbook).** The
  `CREEL DNA LU` tab listed corridor-area RCH proportions under OLD subarea
  codes (23I, 23N, 23P) while creel interviews have always used the CURRENT
  corridor codes (123R, 123T, 123P) — silently failing every RCH join for
  those three codes, for all 26 years of data. Not a real data gap; fixed by
  relabeling the DNA LU tab. 23G/23H/23L remain genuinely unassessed (no
  PROP_RCH data exists for those — confirmed separately, that gap is real).
  Separately confirmed: `cn_all_k * prop_rch` equals `rch_cn_k` exactly for
  every interview row (85,519/85,519 match) — an older predecessor script
  used `cpue * prop_rch` instead of `rch_cn_k / boat_trips`; these are the
  same number computed two different ways, not a methodology difference.
- **Subarea reconciliation:** 23G/23P/23O/123P/23L → `23O+123P`;
  23H/23Q/23N/123T → `23Q+123T`; 23I → `123R` (no-op safeguard, 23I never
  actually appears in the raw data 2000-2026).
- **"123X" codes that are NOT PFMA 23.** The raw interview data contains many
  more "123"-prefixed codes than just the corridor ones — 123G, 123N, 123U,
  123H, 123O, 123L, 123A, 123D. These all carry `region == "Area 123"`, a
  genuinely separate, unrelated management area (not PFMA 23's corridor) —
  123U alone has ~2,500 rows across every year 2000-2026. Only 123T/123R/123P
  are PFMA 23's corridor subareas and belong in this analysis; the regex
  `123(?=(T|R|P))` correctly excludes the rest. Don't "fix" this as a
  data-completeness gap — it isn't one. (This was nearly misdiagnosed as a
  bug once — the row counts look alarming until you check the `region`
  field.)
- **Cumulative periods sum constituent weeks with `na.rm = TRUE`, applied
  consistently to every period now.** Early in the season, a week that
  hasn't happened yet is silently treated as zero catch rather than
  missing — this can quietly deflate an in-season forecast instead of
  failing loudly. The completeness guard in `run_period_forecast()` /
  `retro_forecast()` checks curr_year actually has interviews for every
  week a period needs before forecasting, and skips (with a `warning()`)
  otherwise — this guard is the actual safety net, independent of how
  `cpue`'s columns handle NAs. Don't remove it as "unnecessary."
- **`PERIOD_WEEKS` is defined exactly once**, near the top of the script,
  and used both to build every column in `cpue` and as the completeness
  guard everywhere else. This used to be two separately hand-typed lists
  that drifted out of sync (a missing `"cum83"` entry once silently
  disabled a completeness guard in SECTION C), and Nick's original script
  computed `cum83` with plain `+` while `cum84`/`cum91`/`cum92` used
  `na.rm = TRUE` sum — an actual inconsistency in what "cumulative" meant
  between periods. If you need a new period, add ONE entry to
  `PERIOD_WEEKS` — never hand-type a week list anywhere else in the file.
- **Combo search must test raw AND RCH-corrected CPUE together, never just
  one.** The true top model can be either, depending on period — filtering
  to one correction type ahead of time silently misses the real winner (this
  happened once already; don't repeat it).
- **Retro MAPE itself is not immune to overfitting** when minimized across
  ~1,300 combos x 4 forms x multiple periods on a ~16-year scored sample.
  Confirmed twice while developing this: a low-R² combo (adj.R²=0.42) won
  outright on raw retro MAPE with a LOOCV MAPE of 73% (back when LOOCV was
  still computed) — `MIN_SEASON_ADJ_R2` guards against this. Separately, a
  "linear" fit of the correct combo (R²=0.586) edged out its own log-log
  version (R²=0.826) by about a point of retro MAPE — noise, not signal,
  which is why the final pick is restricted to log-log specifically.
- **`week_XX_cpue` columns in `CN_return_predictors`** (used by the older,
  pre-2026 production script) are NOT reproducible from raw interview data
  with a simple "all PFMA 23 subareas" aggregation — the `CREEL pivots`
  sheet documents they're built from a specific curated subarea subset (23E,
  23F, 23K, 23M, 23J, 23D, 23O, 23Q + historical 23G/23H/23L). If trying to
  match historical report figures against those columns, use that subset,
  not the full subarea list.

## R conventions in this codebase

- `tidyverse` + native pipe `|>` throughout, not magrittr `%>%` (except in
  older pre-2026 scripts, which still use `%>%`).
- Closed-form OLS (`fast_ols()` / `retro_ols()`) instead of `lm()` for
  brute-force searches at scale — same math as `lm(y ~ x)`, no
  formula/model-frame overhead. `retro_ols()` loops per forecast-year (no
  closed-form shortcut exists for an expanding window) but is cheap enough
  per-candidate that the full search still finishes in roughly a minute.
  Only refit with actual `lm()` once a single model has been selected, so
  `predict(..., interval = "pred")` can hand back a real prediction
  interval.
- **Always qualify `scales::comma()` / `scales::percent()` explicitly.**
  Bare `comma()`/`percent()` only resolves if `library(scales)` happened to
  already run in that exact R session — easy to break by sourcing a script
  out of order or copy-pasting a block in isolation. Already got bitten by
  this once; don't reintroduce bare calls.
- 75% prediction intervals (`level = .75`) throughout, matching the
  original production script's convention — not the more common 95%.
- Log-log (`log(return) ~ log(cpue)`) has won essentially every model
  comparison run this season, with zero exceptions once low-R² flukes are
  excluded, but the search still tests all four functional forms (linear /
  log-lin / lin-log / log-log) rather than assuming log-log going in — only
  the FINAL pick is restricted to log-log, and that restriction is
  evidence-based (see "Retro MAPE itself is not immune..." above), not an
  assumption baked in from the start.
- **`run_period_forecast()` and `retro_forecast()` are deliberately
  self-contained.** They derive `winning_subareas`/`use_rch`/`model_form`
  from `SEASON_MODEL` *inside* the function body, rather than reading
  module-level globals set earlier in the script. An earlier draft used
  globals for this and broke with `object 'MODEL_FORM' not found` whenever
  SECTION A didn't fully run first. If you add new helper functions that
  depend on `SEASON_MODEL`, follow the same pattern: derive what you need
  from `SEASON_MODEL` at the top of the function, and check
  `exists("SEASON_MODEL")` with a clear `stop()` message.

## Troubleshooting

- **`SEASON_MODEL not found`** (from `run_period_forecast()`'s own check):
  means `SEASON_MODEL` was never created in this R session. Almost always
  because the script was re-run partially (e.g. just the bottom
  `results <- list(...)` block) instead of sourced from the top. Source the
  *entire* file fresh. If that still fails, check the console for an error
  partway through SECTION A's search (paste it rather than just the final
  error) and confirm whether `season_model_<year>.rds` exists in the same
  folder as the xlsx.
- **`NA/NaN/Inf in 'x'` from `lm.fit()`**: a subarea can have a genuine
  zero-catch year (e.g. 123R has had some). `build_model_formula()`'s
  log-log/lin-log cases use `log(predictor_val + 0.0001)`, not bare
  `log(predictor_val)`, specifically to guard against this — if this error
  reappears, check whether that offset got dropped somewhere.
- **A "winner" with implausibly low R² or an unfamiliar functional form**
  (e.g. `linear` instead of `log-log`, or a combo involving `123R` alone):
  before trusting it, check `season_model_<year>.rds` is actually fresh
  (delete and re-run if in doubt) and cross-check against the "Confirmed
  working end-to-end" result noted at the top of this file. This has
  happened twice during development — see "Retro MAPE itself is not immune
  to overfitting" above.

## Open questions / not fully resolved

- A prior season's in-season report (screenshot, not in this repo) stated a
  week-83 forecast of R²=0.76 / 87,000 adults / 75% PI 57,000–134,000 for a
  "CPUE (Somass-origin Chinook)" model. A predecessor script (2024-era,
  found later) revealed the actual method: subareas 23J+23C+23E (RCH-
  corrected, week 83 alone, log-log) — chosen via undocumented manual
  "exploratory analysis," not a systematic search. Re-running that exact
  method against today's data gives R²=0.08, not 0.74 — the DNA-corrected
  RCH values for that combo have materially changed since 2024 (one visible
  cause: a `rch_cn_k = 0` for 2023 that looks like a DNA-attribution
  anomaly). This is itself a useful data point: a manually-picked model
  that's never re-validated can go stale silently, which is part of the
  justification for this pipeline's rerun-and-recheck approach over a
  once-derived, never-revisited number.
- The report-figure reconstruction script (`04-code_2026_report-figure8.R`,
  if it still exists in `2026/`) was built on an earlier "whole-fishery"
  theory that turned out to be wrong. Superseded — don't use it as reference.

## Working with this repo across Claude Code sessions

This file is what a fresh session should read first instead of re-deriving
the above from scratch. If you (Claude) discover a new data quirk, a bug, or
a methodology decision worth keeping, add it here rather than leaving it
only in conversation history — conversation history doesn't carry over to a
new session; this file does.
