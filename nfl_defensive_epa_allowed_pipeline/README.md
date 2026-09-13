# NFL Defensive EPA Allowed Pipeline

A reusable R pipeline for evaluating NFL defensive performance using **Expected Points Added (EPA) Allowed**.

The pipeline processes nflfastR play-by-play data, constructs a standardized defensive play population, validates the underlying data, classifies defensive situations, and compares a selected defense against the rest of the NFL.

The current configuration analyzes the **2025 Carolina Panthers defense**.

---

## Overview

EPA Allowed measures the offensive value generated on plays faced by a defense.

Because EPA is recorded from the offense's perspective:

- **Lower EPA Allowed = better defensive performance**
- **Higher EPA Allowed = worse defensive performance**

The pipeline evaluates defensive performance at three levels:

- **Overall**
- **Pass** — pass plays and sacks
- **Run** — rushing plays

It also evaluates performance across standardized game situations to identify potential defensive strengths and weaknesses for further investigation.

---

## Core Metrics

### EPA Allowed per Play

The primary analytical metric:

**EPA Allowed per Play = Total Offensive EPA / Eligible Plays**

This represents the average offensive EPA generated against the defense on each eligible play.

### EPA Difference per 100 Plays

Situational performance is compared against the NFL benchmark using:

**EPA Difference per 100 Plays = (Team EPA/Play - NFL EPA/Play) × 100**

Interpretation:

- **Negative** = better than the NFL benchmark
- **Positive** = worse than the NFL benchmark

### EPA Allowed per Game

Overall defensive EPA is also expressed on a per-game basis to provide a more intuitive measure of season-level impact.

The pipeline reports:

- Team EPA Allowed per Game
- Mean NFL EPA Allowed per Game
- League-Relative EPA Allowed per Game
- Field-Goal Equivalent per Game
- Touchdown Equivalent per Game

---

## NFL Benchmark

The comparison benchmark is calculated using the pooled play-level EPA of **all other NFL defenses**, excluding the team being evaluated.

This prevents the selected team's own performance from influencing its comparison benchmark.

---

## Eligible Play Population

An EPA-eligible play must:

- Be a recorded play
- Have a valid play type
- Not be classified as `no_play`
- Not be a quarterback kneel
- Not be a quarterback spike
- Not be a special-teams play
- Not be a two-point attempt
- Have a valid, finite EPA value

Eligible plays are then classified as:

- **Pass**
- **Run**
- **Sack**

Sacks remain a distinct canonical play type but are included in the **passing population** for EPA analysis.

---

## Situations Analyzed

The pipeline evaluates the selected defense across 12 standardized situations:

| Situation | Definition |
|---|---|
| Overall | All eligible plays |
| 1st & 10 | 1st down with 10 yards to go |
| 2nd & Long | 2nd down with 7+ yards to go |
| 2nd & Medium | 2nd down with 4–6 yards to go |
| 2nd & Short | 2nd down with 3 or fewer yards to go |
| 3rd Down | All 3rd-down plays |
| 4th Down | All 4th-down plays |
| Defense Leading | Offense has a negative score differential |
| Defense Trailing | Offense has a positive score differential |
| Neutral / Tied | Score differential equals zero |
| Offense Own Territory | `yardline_100 > 50` |
| Offense Opponent Territory | `yardline_100 < 50` |

Score differential and field position are interpreted from the **offense's perspective**, consistent with nflfastR play-by-play data.

---

## Statistical Testing

The pipeline uses a **two-sided Welch two-sample t-test** to compare the selected defense's play-level EPA distribution against the pooled EPA distribution of all other defenses.

**Significance level: α = 0.05**

Statistically significant results are classified directionally:

- **Strength** — team EPA Allowed is lower than the NFL benchmark
- **Weakness** — team EPA Allowed is higher than the NFL benchmark

These findings are intended to identify areas for further investigation rather than establish tactical causation.

---

## League Rank

Overall EPA Allowed per Play is calculated for all 32 NFL defenses.

Teams are ranked from lowest to highest EPA Allowed per Play:

**Rank 1 = lowest EPA Allowed per Play = best defensive performance**

---

## Pipeline Structure

```text
nfl_defensive_epa_allowed_pipeline/
│
├── config.R
├── run_pipeline.R
│
├── data_engineering/
│   ├── 01_extract_nfl_regular_season_pbp.R
│   ├── 02_extract_team_defensive_pbp.R
│   ├── 03_build_epa_eligible_plays.R
│   └── 04_classify_epa_situations.R
│
├── quality_assurance/
│   ├── 01_validate_epa_source_and_coverage.R
│   └── 02_validate_epa_eligible_population.R
│
├── data_analysis/
│   ├── 01_build_epa_allowed_table.R
│   └── 02_build_epa_pass_run_tables.R
│
└── data/
    ├── raw/
    ├── processed/
    ├── qa_outputs/
    └── analysis_outputs/
