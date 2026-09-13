# NFL Defensive Success Rate Pipeline

A reusable R pipeline for evaluating NFL defensive performance using **Defensive Success Rate (DSR)**.

The pipeline processes nflfastR play-by-play data, constructs a standardized defensive play population, validates the underlying data, classifies defensive successes and game situations, and compares a selected defense against the rest of the NFL.

The current configuration analyzes the **2025 Carolina Panthers defense**.

---

## Overview

Defensive Success Rate measures how often a defense prevents the offense from achieving a successful play.

Offensive success is defined using down-specific yardage thresholds:

- **1st down:** gain at least 40% of yards to go
- **2nd down:** gain at least 60% of yards to go
- **3rd down:** gain at least 100% of yards to go
- **4th down:** gain at least 100% of yards to go

Defensive success is the inverse of offensive success.

Because the metric is expressed from the defense's perspective:

- **Higher Defensive Success Rate = better defensive performance**
- **Lower Defensive Success Rate = worse defensive performance**

The pipeline evaluates defensive performance at three levels:

- **Overall**
- **Pass** — pass plays and sacks
- **Run** — rushing plays

It also evaluates performance across standardized game situations to identify potential defensive strengths and weaknesses for further investigation.

---

## Core Metrics

### Defensive Success Rate

The primary analytical metric:

**Defensive Success Rate = Defensive Successes / Eligible Plays**

This represents the percentage of eligible plays on which the defense prevents the offense from achieving a successful outcome.

### Difference from NFL Benchmark

Situational performance is compared against the NFL benchmark using:

**Difference = Team Defensive Success Rate - NFL Benchmark Defensive Success Rate**

Interpretation:

- **Positive** = better than the NFL benchmark
- **Negative** = worse than the NFL benchmark

Differences are reported in **percentage points**.

### Defensive Success Index

The pipeline also reports a Defensive Success Index:

**DSI = NFL Benchmark Defensive Success Rate / Team Defensive Success Rate**

Interpretation:

- **DSI < 1.00** = above benchmark
- **DSI = 1.00** = matches benchmark
- **DSI > 1.00** = below benchmark

---

## NFL Benchmark

The comparison benchmark is calculated using the pooled play-level Defensive Success Rate of **all other NFL defenses**, excluding the team being evaluated.

This prevents the selected team's own performance from influencing its comparison benchmark.

---

## Eligible Play Population

An eligible play is an offensive pass or run, including sacks, while excluding:

- Nullified / no-play events
- Quarterback kneels
- Quarterback spikes
- Special-teams plays
- Two-point attempts

Sacks are retained as eligible passing opportunities.

Eligible plays are then classified as:

- **Pass**
- **Run**
- **Sack**

Sacks remain a distinct canonical play type but are included in the **passing population** for Defensive Success Rate analysis. :contentReference[oaicite:2]{index=2} :contentReference[oaicite:3]{index=3}

---

## Success Classification

The pipeline applies down-specific offensive success thresholds:

| Down | Offensive Success Threshold |
|---|---:|
| 1st Down | 40% of yards to go |
| 2nd Down | 60% of yards to go |
| 3rd Down | 100% of yards to go |
| 4th Down | 100% of yards to go |

Defensive success is then calculated as the inverse of offensive success.

Several higher-precedence play outcomes are handled explicitly.

### Defensive Success Overrides

The following are classified as defensive successes:

- Sacks
- Interceptions
- Possession-losing turnovers
- Accepted offensive penalties

### Offensive Success Overrides

A first down awarded by defensive penalty is classified as an offensive success unless a higher-precedence override applies.

The pipeline also validates that:

**Offensive Success + Defensive Success = 1**

for every eligible play. :contentReference[oaicite:4]{index=4}

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

The pipeline uses a **two-sided two-sample proportion test** to compare the selected defense's success rate against the pooled success rate of all other defenses.

**Significance level: α = 0.05**

Statistically significant results are classified directionally:

- **Strength** — team Defensive Success Rate is higher than the NFL benchmark
- **Weakness** — team Defensive Success Rate is lower than the NFL benchmark

These findings are intended to identify areas for further investigation rather than establish tactical causation. :contentReference[oaicite:5]{index=5}

---

## League Rank

Overall Defensive Success Rate is calculated for all NFL defenses.

Teams are ranked from highest to lowest Defensive Success Rate:

**Rank 1 = highest Defensive Success Rate = best defensive performance**

Ties receive the same minimum rank. :contentReference[oaicite:6]{index=6}

---

## Pipeline Structure

```text
nfl_defensive_success_rate_pipeline/
│
├── config.R
├── run_pipeline.R
│
├── data_engineering/
│   ├── 01_extract_nfl_regular_season_pbp.R
│   ├── 02_extract_team_defensive_pbp.R
│   ├── 03_build_eligible_plays.R
│   └── 04_classify_success_and_situations.R
│
├── quality_assurance/
│   ├── 01_reconcile_team_season_totals.R
│   ├── 02_reconcile_team_game_totals.R
│   ├── 03_check_eligible_play_percentage_by_defense.R
│   └── 04_check_success_rate_edge_cases.R
│
├── data_analysis/
│   ├── 01_build_defensive_success_rate_table.R
│   └── 02_build_defensive_success_pass_run_tables.R
│
└── data/
    ├── raw/
    ├── processed/
    ├── qa_outputs/
    └── analysis_outputs/
    ```