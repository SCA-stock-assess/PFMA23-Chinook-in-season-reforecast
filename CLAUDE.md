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
   (wk82only, wk83only, wk83Cum = cum83, wk84Cum, wk91Cum, wk92Cum), refits
   it with whatever data exists and forecasts the current year. This is what
   changes week to week — the model choice does not. `wk82only` (period
   `"82"`) was added later in the season as an even-earlier single-week
   look, same caveat as `wk83only` below re: its subtitle's MAPE line.

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

   Right after SECTION B's `ggsave()` calls, two more ad-hoc additions live
   inline in the script (not inside `run_period_forecast()`):
   - A **CPUE trend plot** — `cpue_trend`, plotted but not saved to
     `figures/` — showing raw (always raw, regardless of `SEASON_MODEL`'s
     correction) pooled CPUE by year for the season model's own
     combo+period, i.e. the same `predictor_val` series `run_period_forecast()`
     fits against, just shown as a full history instead of collapsed to one
     forecast point.
   - A **restyled copy of the `wk83Cum` figure** (`figures/2026_wk83Cum_forecast_clean.png`),
     built by taking `results$wk83Cum$plot` and adding `+ labs(...)` to
     override title/axis labels — the ad-hoc pattern for cleaning up a
     figure's title/axis text without editing `run_period_forecast()`
     itself (any `labs()` field you don't name is left as the function
     built it). Copy this pattern for other periods rather than adding new
     labelling parameters to the function.

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
  was never in question and never fed into `SEASON_MODEL` selection. The
  `fast_ols()` function itself outlived this removal as dead code (never
  called — `combo_r2s`'s inline vectorized computation does the same math
  for the search PART 1 actually uses) until it was deleted during the
  2026-08-28 review pass.
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
- **`23A`/`23B` (region `"Area 23 (Alberni Canal)"`) are correctly included,
  not a bug.** `23A` alone is the single largest statsub in the whole
  dataset (~32k rows, ~39% of everything the `asscd_txt` filter keeps) and
  its `region` string says "Alberni Canal" rather than "Barkley" — looked
  like the same class of mislabeling as the `123X` issue above (wrong area
  hiding under a matching numeric prefix) when first spotted. It isn't:
  Alberni Canal and Barkley Sound are both part of PFMA/Area 23, just two
  different internal CREST location labels for the same management area.
  Confirmed directly with the project owner (2026-08-27) — don't re-flag
  this as a scope bug.
- **Cumulative periods sum constituent weeks with `na.rm = TRUE`, applied
  consistently to every period now.** Early in the season, a week that
  hasn't happened yet is silently treated as zero catch rather than
  missing — this can quietly deflate an in-season forecast instead of
  failing loudly. The completeness guard in `run_period_forecast()` /
  `retro_forecast()` checks curr_year actually has interviews for every
  week a period needs before forecasting, and skips (with a `warning()`)
  otherwise — this guard only checks `year == curr_year` (protects the live
  forecast) and only checks whether ANY of the season model's subareas has
  that week (not every one) — both intentional, don't tighten either
  without re-reading the investigation below first.
  **Investigated thoroughly (2026-08-28), since this TODO used to just say
  "not yet quantified":**
  - 21 of 25 fitted training years (2000-2025, excl. 2001) have >=1 of the
    season model's own 4 subareas (23C/23E/23J/23M) missing >=1 of `cum83`'s
    two weeks; only 4/25 years (2002/2004/2006/2022) have a fully complete
    4-subarea x 2-week grid.
  - Requiring full completeness is not viable: fitting on just those 4
    years gives R² ≈ 0.04 (n far too small to mean anything).
  - A blunter "fix" — dropping a subarea's *whole-year* contribution
    whenever it's missing either week, instead of just omitting the
    missing week — is WORSE, not better: retro-fit R² drops from 0.82 to
    0.23, because it guts trip counts in already-thin years (2010:
    11→2 trips, 2012: 3→0, 2014: 7→4), trading a mild representativeness
    issue for much larger variance.
  - **Conclusion: this is not a bug to "fix" by enforcing completeness** —
    with a creel survey this size, opportunistic pooling across whatever
    subarea-weeks actually got sampled is close to necessary. Don't re-flag
    this or try to make the completeness guard check every training year or
    every subarea individually — both were tried by hand and made things
    worse.
  - **The real, still-open risk found instead: every OLS fit in this
    script (`retro_ols`, `combo_r2s`'s inline computation, and the final
    `lm()` refit) is UNWEIGHTED**, even though training-year trip counts
    span roughly 3 (2012) to 178 — a year's CPUE built from 3 interviews
    gets identical regression leverage to one built from 178. Not yet
    addressed. The "Sampling effort by year" plot at the end of the script
    (added 2026-08-28, right after `cpue_trend`) exists specifically to
    make this visible — check it before trusting a close retro-MAPE margin.
- **`sum(x, na.rm = TRUE)` treats an all-`NA` group as a literal `0`, not
  `NA` — a separate, sharper bug found and fixed 2026-08-28.** Subareas
  23G/23H/23L have NO RCH/DNA data at all (100% `NA` `rch_cn_k`, confirmed
  directly against the workbook). Before the fix, summing across an all-`NA`
  group — e.g. the merged `23O+123P`/`23Q+123T` subareas in roughly
  2000-2010, before the DNA program existed — silently produced a
  *fabricated* `rch_cpue = 0`, indistinguishable from "we surveyed this and
  found zero RCH-attributed Chinook," instead of `NA` ("we don't know").
  Concretely reproduced: `23O+123P` in 2000 has real, nonzero catch (29)
  and trips (13) in `cum83`'s two weeks, but `rch_cn_k` was coming out as a
  fake `0` instead of `NA`. Fixed via a shared `sum_or_na()` helper (see
  the R-conventions bullet below) applied everywhere `cn_all_k`/`rch_cn_k`/
  `boat_trips` get summed: `cpue_wide`, `period_cols`, `combo_cpue`
  (PART 1), and the season-model refit `series` in
  `run_period_forecast()`/`retro_forecast()`/`cpue_trend`. Only affects
  RCH-corrected candidates touching the merged `23O+123P`/`23Q+123T`
  subareas — the actual `SEASON_MODEL` (raw CPUE, `23C+23E+23J+23M`) never
  had an all-`NA` group and was unaffected by construction (traced through
  the math by hand), **confirmed empirically 2026-08-28**: deleted
  `season_model_2026.rds` and ran the full script fresh (real R, real
  workbook) — landed on the exact same combo/period/form/correction/MAPE/
  adj.R²/n as before the fix (`23C+23E+23J+23M`, `cum83`, raw, log-log,
  16.3%, 0.821, n=16). Full script also ran end-to-end with no errors
  (SECTION A/B/C, the corrected `cpue_trend`, and the new sampling-effort
  plot all executed cleanly).
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
- Closed-form OLS (`combo_r2s`'s inline vectorized computation / `retro_ols()`)
  instead of `lm()` for brute-force searches at scale — same math as
  `lm(y ~ x)`, no formula/model-frame overhead. `retro_ols()` loops per
  forecast-year (no closed-form shortcut exists for an expanding window)
  but is cheap enough per-candidate that the full search still finishes in
  roughly a minute. Only refit with actual `lm()` once a single model has
  been selected, so `predict(..., interval = "pred")` can hand back a real
  prediction interval.
- **`sum_or_na()`** (defined near the top, before first use) wraps
  `sum(x, na.rm = TRUE)` everywhere `cn_all_k`/`rch_cn_k`/`boat_trips` get
  summed, so an all-`NA` group propagates `NA` instead of the R default of
  a fabricated `0` — see the na.rm bullet below for why this matters. Use
  it (not a bare `sum(..., na.rm = TRUE)`) for any new aggregation over
  these columns.
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
- **FIXED (2026-08-28): `cpue_trend` plot (end of script, after SECTION C)
  now follows `SEASON_MODEL$correction`** the same way `run_period_forecast()`'s
  `predictor_val` does, instead of being hardcoded to raw `cn_all_k /
  boat_trips`. No visible change to this season's output (the season model
  happens to use raw CPUE), but it won't silently mislabel a future
  RCH-corrected season model's trend as raw.
- **TODO: `interview_recoded`'s `asscd_txt` filter drops "Form Incomplete"
  rows (2,333, ~2.7% of raw interviews) entirely** — out of both the catch
  numerator AND the `boat_trips` effort denominator (`boat_trips = n()` of
  the post-filter rows). Flagged during review, not yet resolved (2026-08-27):
  need to confirm with whoever runs CREST whether "Form Incomplete" means
  "boat was interviewed/fished but catch count is untrustworthy" (in which
  case dropping it from `boat_trips` undercounts effort and biases CPUE) or
  "no real interview took place" (in which case current handling is fine).
- **TODO: CPUE is NOT restricted to Chinook-targeted trips.** Checked
  directly (2026-08-27): of the 81,032 rows kept after the `asscd_txt`
  filter, only ~30% have `target_species` containing "Chinook" alone (another
  ~7% are mixed-target rows like "Chinook & Sockeye"), 44% have a blank/NA
  `target_species`, and the rest target Sockeye/Halibut/Coho/Lingcod/Rockfish
  etc. `cn_all_k`/`boat_trips` (and therefore `cpue`) are summed over ALL of
  these regardless of what the trip was targeting — so CPUE mixes Chinook
  catch rates across trips with very different targeting behavior. If the
  mix of targeted species shifts over time or within a season (e.g. more
  Sockeye-directed effort in a big Sockeye year), that would move CPUE
  without any real change in Chinook abundance — a potential confound for a
  model that assumes CPUE tracks Chinook abundance across 25+ years. Not
  yet investigated whether restricting to Chinook-targeted (or Chinook-
  inclusive) trips changes the retro MAPE ranking or the season model pick.
  Note CLAUDE.md already records that an earlier "whole-fishery" theory in
  the superseded report-figure script turned out wrong — worth checking
  whether that's the same issue before assuming it is or isn't.

## Working with this repo across Claude Code sessions

This file is what a fresh session should read first instead of re-deriving
the above from scratch. If you (Claude) discover a new data quirk, a bug, or
a methodology decision worth keeping, add it here rather than leaving it
only in conversation history — conversation history doesn't carry over to a
new session; this file does.
