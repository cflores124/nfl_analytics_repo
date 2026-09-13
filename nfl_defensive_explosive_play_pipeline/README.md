# NFL Defensive Explosive Play Rate Pipeline

A reusable R pipeline for evaluating NFL defensive performance using **Explosive Play Rate Allowed**.

The pipeline processes nflfastR play-by-play data, constructs a standardized defensive play population, validates the underlying data, classifies explosive plays and game situations, and compares a selected defense against the rest of the NFL.

The current configuration analyzes the **2025 Carolina Panthers defense**.

---

## Overview

Explosive Play Rate measures how frequently a defense allows an eligible offensive play to gain:

- **Pass:** 15 or more yards
- **Run:** 10 or more yards

Because the metric is expressed from the defense's perspective:

- **Lower Explosive Play Rate = better defensive performance**
- **Higher Explosive Play Rate = worse defensive performance**

The pipeline evaluates defensive performance at three levels:

- **Overall**
- **Pass** — pass plays and sacks
- **Run** — rushing plays

Sacks are included in the passing population but cannot be classified as explosive.

The pipeline also evaluates performance across standardized game situations to identify potential defensive strengths and vulnerabilities for further investigation.

---

## Core Metrics

### Explosive Play Rate

The primary analytical metric:

**Explosive Play Rate = Explosive Plays / Eligible Plays**

This represents the percentage of eligible plays on which the defense allows an explosive gain.

### Difference from NFL Benchmark

Situational performance is compared against the NFL benchmark using:

**Difference = Team Explosive Play Rate - NFL Benchmark Explosive Play Rate**

Interpretation:

- **Negative** = better than the NFL benchmark
- **Positive** = worse than the NFL benchmark

Differences are reported in **percentage points**.

### Explosive Vulnerability Index

The pipeline also reports an Explosive Vulnerability Index:

**EVI = Team Explosive Play Rate / NFL Benchmark Explosive Play Rate**

Interpretation:

- **EVI < 1.00** = lower vulnerability than benchmark
- **EVI = 1.00** = matches benchmark
- **EVI > 1.00** = greater vulnerability than benchmark

---

## NFL Benchmark

The comparison benchmark is calculated using the pooled play-level Explosive Play Rate of **all other NFL defenses**, excluding the team being evaluated.

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

Sacks remain a distinct canonical play type but are included in the **passing population** for Explosive Play Rate analysis.

---

## Explosive Play Classification

The pipeline applies play-type-specific explosive thresholds:

| Play Type | Explosive Threshold |
|---|---:|
| Pass | 15+ yards |
| Run | 10+ yards |
| Sack | Never explosive |

A pass is classified as explosive when a non-sack pass gains at least 15 yards.

A run is classified as explosive when it gains at least 10 yards.

Sacks remain in the passing denominator but are always classified as non-explosive.

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

The pipeline uses a **two-sided two-sample proportion test** to compare the selected defense's Explosive Play Rate against the pooled explosive-play rate of all other defenses.

**Significance level: α = 0.05**

Statistically significant results are classified directionally:

- **Strength** — team Explosive Play Rate is lower than the NFL benchmark
- **Vulnerability** — team Explosive Play Rate is higher than the NFL benchmark

These findings are intended to identify areas for further investigation rather than establish tactical causation.

---

## League Rank

Overall Explosive Play Rate is calculated for all NFL defenses.

Teams are ranked from lowest to highest Explosive Play Rate:

**Rank 1 = lowest Explosive Play Rate = best defensive performance**

Ties receive the same minimum rank.

---

## Pipeline Structure

```text
nfl_defensive_explosive_play_pipeline/
│
├── config.R
├── run_pipeline.R
│
├── data_engineering/
│   ├── 01_extract_nfl_regular_season_pbp.R
│   ├── 02_extract_team_defensive_pbp.R
│   ├── 03_build_eligible_plays.R
│   └── 04_classify_explosives_and_situations.R
│
├── quality_assurance/
│   ├── 01_reconcile_team_season_totals.R
│   ├── 02_reconcile_team_game_totals.R
│   ├── 03_check_eligible_play_percentage_by_defense.R
│   └── 04_check_explosive_play_edge_cases.R
│
├── data_analysis/
│   ├── 01_build_explosive_vulnerability_table.R
│   └── 02_build_explosive_pass_run_tables.R
│
└── data/
    ├── raw/
    ├── processed/
    ├── qa_outputs/
    └── analysis_outputs/