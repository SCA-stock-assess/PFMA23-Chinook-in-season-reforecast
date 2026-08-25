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

## How the pipeline is structured (and why)

`NEW2026_CPUE_reforecast.R` has two stages, deliberately kept separate:

1. **SECTION A — model selection.** Expensive (~10k+ regressions: every
   4-5 subarea combination x raw/RCH-corrected CPUE x 4 functional forms,
   for every stat-week period), ending in a **leave-one-year-out
   cross-validation** pass that picks ONE model for the season. Runs once,
   caches its pick to `season_model_<year>.rds`. Re-running the script
   loads the cached model instead of re-searching.

2. **SECTION B — weekly forecast.** Cheap. Takes the cached model's fixed
   subareas/correction/functional form and, for each stat-week milestone
   (week 83, cum84, cum91, cum92), refits it with whatever data exists and
   forecasts the current year. This is what changes week to week — the
   model choice does not.

**Do not re-run model selection mid-season and take whatever tops that
week's leaderboard.** With 10k+ combinations tested, the top ~10 candidates
for any given period routinely land within thousandths of adj.R² of each
other — statistically indistinguishable. Re-searching weekly just chases
that noise and produces a different "winner" for reasons that have nothing
to do with real forecasting skill, which undermines both accuracy and the
report's credibility (subareas shouldn't visibly change week to week for no
stats-backed reason). Select once via LOOCV, hold it for the season. To
force a fresh selection (e.g. a new season), delete `season_model_<year>.rds`.

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
- **Subarea reconciliation:** 23G/23P/23O/123P/23L → `23O+123P`;
  23H/23Q/23N/123T → `23Q+123T`; 23I → `123R` (no-op safeguard, 23I never
  actually appears in the raw data 2000-2026).
- **Cumulative periods (cum84, cum91, cum92) sum constituent weeks with
  `na.rm = TRUE`.** Early in the season, a week that hasn't happened yet is
  silently treated as zero catch rather than missing — this can quietly
  deflate an in-season forecast instead of failing loudly. The
  `run_period_forecast()` completeness guard checks curr_year actually has
  interviews for every week a period needs before forecasting, and skips
  (with a `warning()`) otherwise. Don't remove this guard as "unnecessary."
- **Combo search must test raw AND RCH-corrected CPUE together, never just
  one.** The true top model can be either, depending on period — filtering
  to one correction type ahead of time silently misses the real winner (this
  happened once already; don't repeat it).
- **`week_XX_cpue` columns in `CN_return_predictors`** (used by the older,
  pre-2026 production script) are NOT reproducible from raw interview data
  with a simple "all PFMA 23 subareas" aggregation — the `CREEL pivots`
  sheet documents they're built from a specific curated subarea subset (23E,
  23F, 23K, 23M, 23J, 23D, 23O, 23Q + historical 23G/23H/23L). If trying to
  match historical report figures against those columns, use that subset,
  not the full subarea list — and even then expect small unexplained gaps
  (see "Open questions" below).

## R conventions in this codebase

- `tidyverse` + native pipe `|>` throughout, not magrittr `%>%` (except in
  older pre-2026 scripts, which still use `%>%`).
- Closed-form OLS (`fast_ols()` / `loocv_ols()`) instead of `lm()` for
  brute-force searches at scale — same math as `lm(y ~ x)`, no
  formula/model-frame overhead. Only refit with actual `lm()` once a single
  model has been selected, so `predict(..., interval = "pred")` can hand
  back a real prediction interval.
- **Always qualify `scales::comma()` / `scales::percent()` explicitly.**
  Bare `comma()`/`percent()` only resolves if `library(scales)` happened to
  already run in that exact R session — easy to break by sourcing a script
  out of order or copy-pasting a block in isolation. Already got bitten by
  this once; don't reintroduce bare calls.
- 75% prediction intervals (`level = .75`) throughout, matching the
  original production script's convention — not the more common 95%.
- Log-log (`log(return) ~ log(cpue)`) has won essentially every model
  comparison run so far across every period checked, but the pipeline still
  tests all four functional forms (linear / log-lin / lin-log / log-log)
  rather than assuming log-log — don't hardcode the form.
- **`run_period_forecast()` is deliberately self-contained.** It derives
  `winning_subareas`/`use_rch`/`model_form` from `SEASON_MODEL` *inside* the
  function body, rather than reading module-level globals set earlier in the
  script. Earlier draft had those as globals (`WINNING_SUBAREAS` etc.) set
  right after SECTION A — that broke with `object 'MODEL_FORM' not found`
  whenever SECTION A didn't fully run first (script interrupted, or someone
  re-ran just the SECTION B block). If you add new helper functions that
  depend on `SEASON_MODEL`, follow the same pattern: derive what you need
  from `SEASON_MODEL` at the top of the function, and check
  `exists("SEASON_MODEL")` with a clear `stop()` message rather than letting
  a missing variable fail three steps removed from the real cause.

## Troubleshooting

- **`object 'MODEL_FORM' not found` (or similar for a derived global):**
  means `SEASON_MODEL` was never created in this R session — SECTION A
  didn't finish (check the console for an earlier error) and
  `season_model_<year>.rds` either doesn't exist yet or failed to load.
  Source the whole script fresh from the top rather than re-running just the
  bottom block. `run_period_forecast()` now fails with an explicit
  `SEASON_MODEL not found` message pointing at this if it happens again.



## Working with this repo across Claude Code sessions

This file is what a fresh session should read first instead of re-deriving
the above from scratch. If you (Claude) discover a new data quirk, a bug, or
a methodology decision worth keeping, add it here rather than leaving it
only in conversation history — conversation history doesn't carry over to a
new session; this file does.
