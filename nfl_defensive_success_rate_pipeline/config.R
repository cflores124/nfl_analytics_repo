# ============================================================
# Configuration
# Reusable NFL Defensive Success Rate Analysis Pipeline
# ============================================================


# ------------------------------------------------------------
# 1. Run configuration
# ------------------------------------------------------------

SEASON <- 2025
TEAM_ABBR <- "CAR"                  # nflverse team abbreviation
TEAM_NAME <- "Carolina Panthers"    # Used for labels and filenames


# ------------------------------------------------------------
# 2. Analytical methodology
# ------------------------------------------------------------

# Offensive success:
#   1st down: gain >= 40% of yards to go
#   2nd down: gain >= 60% of yards to go
#   3rd/4th down: gain >= 100% of yards to go
#
# Defensive success is the inverse of offensive success.
# Sacks and possession-losing turnovers are defensive successes.

FIRST_DOWN_SUCCESS_THRESHOLD <- 0.40
SECOND_DOWN_SUCCESS_THRESHOLD <- 0.60
THIRD_DOWN_SUCCESS_THRESHOLD <- 1.00
FOURTH_DOWN_SUCCESS_THRESHOLD <- 1.00

SIGNIFICANCE_LEVEL <- 0.05


# ------------------------------------------------------------
# 3. Standardized identifiers
# ------------------------------------------------------------

TEAM_SLUG <- TEAM_NAME |>
  tolower() |>
  gsub("[^a-z0-9]+", "_", x = _) |>
  gsub("^_|_$", "", x = _)

TEAM_SEASON_PREFIX <- paste0(TEAM_SLUG, "_", SEASON)


# ------------------------------------------------------------
# 4. Directory paths
# ------------------------------------------------------------

DATA_DIR <- "data"
RAW_DIR <- file.path(DATA_DIR, "raw")
PROCESSED_DIR <- file.path(DATA_DIR, "processed")
QA_OUTPUT_DIR <- file.path(DATA_DIR, "qa_outputs")
ANALYSIS_OUTPUT_DIR <- file.path(DATA_DIR, "analysis_outputs")


# ------------------------------------------------------------
# 5. Raw-data file paths
# ------------------------------------------------------------

NFL_RAW_FILE <- file.path(
  RAW_DIR,
  paste0("nfl_", SEASON, "_regular_season_pbp.csv")
)

TEAM_RAW_FILE <- file.path(
  RAW_DIR,
  paste0(TEAM_SEASON_PREFIX, "_defensive_pbp.csv")
)


# ------------------------------------------------------------
# 6. Processed-data file paths
# ------------------------------------------------------------

NFL_ELIGIBLE_FILE <- file.path(
  PROCESSED_DIR,
  paste0("nfl_", SEASON, "_eligible_plays.csv")
)

TEAM_ELIGIBLE_FILE <- file.path(
  PROCESSED_DIR,
  paste0(TEAM_SEASON_PREFIX, "_eligible_plays.csv")
)

NFL_ANALYSIS_FILE <- file.path(
  PROCESSED_DIR,
  paste0("nfl_", SEASON, "_analysis_plays.csv")
)

TEAM_ANALYSIS_FILE <- file.path(
  PROCESSED_DIR,
  paste0(TEAM_SEASON_PREFIX, "_analysis_plays.csv")
)


# ------------------------------------------------------------
# 7. QA output file paths
# ------------------------------------------------------------

TEAM_SEASON_RECONCILIATION_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_season_reconciliation.csv")
)

TEAM_GAME_RECONCILIATION_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_game_reconciliation.csv")
)

NFL_ELIGIBLE_PERCENTAGE_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0("nfl_", SEASON, "_eligible_percentage_by_defense.csv")
)


# ------------------------------------------------------------
# 8. Analysis output file paths
# ------------------------------------------------------------

TEAM_SUCCESS_RATE_FILE <- file.path(
  ANALYSIS_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_defensive_success_rate_table.csv")
)

TEAM_SUCCESS_PASS_FILE <- file.path(
  ANALYSIS_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_defensive_success_pass_table.csv")
)

TEAM_SUCCESS_RUN_FILE <- file.path(
  ANALYSIS_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_defensive_success_run_table.csv")
)


# ------------------------------------------------------------
# 9. Configuration validation
# ------------------------------------------------------------

if (!is.numeric(SEASON) || length(SEASON) != 1 || is.na(SEASON)) {
  stop("SEASON must be a single numeric value.")
}

if (!is.character(TEAM_ABBR) || length(TEAM_ABBR) != 1 ||
    is.na(TEAM_ABBR) || nchar(TEAM_ABBR) < 2) {
  stop("TEAM_ABBR must be a valid NFL team abbreviation.")
}

if (!is.character(TEAM_NAME) || length(TEAM_NAME) != 1 ||
    is.na(TEAM_NAME) || !nzchar(TEAM_NAME)) {
  stop("TEAM_NAME must be a non-empty character value.")
}

success_thresholds <- c(
  FIRST_DOWN_SUCCESS_THRESHOLD,
  SECOND_DOWN_SUCCESS_THRESHOLD,
  THIRD_DOWN_SUCCESS_THRESHOLD,
  FOURTH_DOWN_SUCCESS_THRESHOLD
)

if (
  any(!is.numeric(success_thresholds)) ||
  any(is.na(success_thresholds)) ||
  any(success_thresholds <= 0 | success_thresholds > 1)
) {
  stop("Success thresholds must be numeric values greater than 0 and no greater than 1.")
}

if (
  !is.numeric(SIGNIFICANCE_LEVEL) ||
  length(SIGNIFICANCE_LEVEL) != 1 ||
  is.na(SIGNIFICANCE_LEVEL) ||
  SIGNIFICANCE_LEVEL <= 0 ||
  SIGNIFICANCE_LEVEL >= 1
) {
  stop("SIGNIFICANCE_LEVEL must be a single numeric value between 0 and 1.")
}


# ------------------------------------------------------------
# 10. Display active configuration
# ------------------------------------------------------------

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "PIPELINE CONFIGURATION\n",
    "============================================================\n\n",
    "Team: %s\n",
    "Team abbreviation: %s\n",
    "Season: %s\n",
    "Success thresholds: 1st %.0f%% | 2nd %.0f%% | 3rd %.0f%% | 4th %.0f%%\n",
    "Passing denominator: pass plays + sacks\n",
    "Running denominator: run plays\n",
    "NFL benchmark: pooled other-defense play-level rate\n",
    "Significance level: %.2f\n",
    "Output prefix: %s\n"
  ),
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  FIRST_DOWN_SUCCESS_THRESHOLD * 100,
  SECOND_DOWN_SUCCESS_THRESHOLD * 100,
  THIRD_DOWN_SUCCESS_THRESHOLD * 100,
  FOURTH_DOWN_SUCCESS_THRESHOLD * 100,
  SIGNIFICANCE_LEVEL,
  TEAM_SEASON_PREFIX
))
