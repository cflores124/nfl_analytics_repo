# NFL Analytics Repository

A collection of reproducible NFL analytics pipelines and defensive reports built using **R** and **nflfastR play-by-play data**.

The repository is designed to identify directional statistical findings that can support deeper scouting, film, and tactical analysis.

---

## Defensive Analytics Pipelines

### EPA Allowed

`nfl_defensive_epa_allowed_pipeline/`

Evaluates defensive performance using **Expected Points Added (EPA) Allowed**, including overall, passing, rushing, and situational analysis.

### Defensive Success Rate

`nfl_defensive_success_rate_pipeline/`

Measures how frequently a defense prevents the offense from achieving a successful play based on down-specific success thresholds.

### Explosive Play Rate

`nfl_defensive_explosive_play_pipeline/`

Measures how frequently a defense allows explosive gains, defined as **15+ yards on a pass or 10+ yards on a run**.

Each pipeline includes standardized data engineering, quality assurance, statistical testing, and team-to-NFL comparison methodology.

---

## Defensive Reports

`defensive_reports/`

Contains finished defensive analyses developed from the underlying analytical pipelines, including visualizations and deeper situational analysis.

---

## Current Analysis

The current implementation evaluates the **2025 Carolina Panthers defense**.

All pipelines are designed to be reusable across NFL teams and seasons through centralized configuration.

---

## Data Source

Play-by-play data are obtained through the `nflfastR` R package and the nflverse data ecosystem.

---

## Author

**Cristian Flores**