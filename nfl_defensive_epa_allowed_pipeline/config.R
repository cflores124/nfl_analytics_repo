# ============================================================
# Configuration
# Reusable NFL Defensive EPA Allowed Analysis Pipeline
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

# EPA is measured from the offensive perspective.
# Lower EPA Allowed indicates better defensive performance.
#
# Core analytical metric: EPA Allowed per Play
# Overall reporting: EPA per 100 plays, EPA per game,
# league-relative EPA per game, and scoring equivalents
#
# Passing population: pass plays + sacks
# Running population: run plays
# NFL benchmark: pooled play-level EPA across all other defenses


# ------------------------------------------------------------
# 3. Standardized identifiers
# ------------------------------------------------------------

TEAM_SLUG <- TEAM_NAME |>
  tolower() |>
  gsub("[^a-z0-9]+", "_", x = _) |>
  gsub("^_|_$", "", x = _)

TEAM_SEASON_PREFIX <- paste0(TEAM_SLUG, "_", SEASON)
NFL_SEASON_PREFIX <- paste0("nfl_", SEASON)


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
  paste0(NFL_SEASON_PREFIX, "_regular_season_pbp.csv")
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
  paste0(NFL_SEASON_PREFIX, "_epa_eligible_plays.csv")
)

TEAM_ELIGIBLE_FILE <- file.path(
  PROCESSED_DIR,
  paste0(TEAM_SEASON_PREFIX, "_epa_eligible_plays.csv")
)

NFL_ANALYSIS_FILE <- file.path(
  PROCESSED_DIR,
  paste0(NFL_SEASON_PREFIX, "_epa_analysis_plays.csv")
)

TEAM_ANALYSIS_FILE <- file.path(
  PROCESSED_DIR,
  paste0(TEAM_SEASON_PREFIX, "_epa_analysis_plays.csv")
)


# ------------------------------------------------------------
# 7. QA output file paths
# ------------------------------------------------------------

EPA_SOURCE_COVERAGE_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0(NFL_SEASON_PREFIX, "_epa_source_and_coverage.csv")
)

EPA_ELIGIBLE_POPULATION_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0(NFL_SEASON_PREFIX, "_epa_eligible_population.csv")
)


# ------------------------------------------------------------
# 8. Analysis output file paths
# ------------------------------------------------------------

TEAM_EPA_ALLOWED_FILE <- file.path(
  ANALYSIS_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_epa_allowed_table.csv")
)

TEAM_EPA_PASS_FILE <- file.path(
  ANALYSIS_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_epa_allowed_pass_table.csv")
)

TEAM_EPA_RUN_FILE <- file.path(
  ANALYSIS_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_epa_allowed_run_table.csv")
)


# ------------------------------------------------------------
# 9. Configuration validation
# ------------------------------------------------------------

if (
  !is.numeric(SEASON) ||
  length(SEASON) != 1 ||
  is.na(SEASON) ||
  SEASON != as.integer(SEASON)
) {
  stop("SEASON must be a single integer value.")
}

if (
  !is.character(TEAM_ABBR) ||
  length(TEAM_ABBR) != 1 ||
  is.na(TEAM_ABBR) ||
  !nzchar(TEAM_ABBR)
) {
  stop("TEAM_ABBR must be a non-empty character value.")
}

if (
  !is.character(TEAM_NAME) ||
  length(TEAM_NAME) != 1 ||
  is.na(TEAM_NAME) ||
  !nzchar(TEAM_NAME)
) {
  stop("TEAM_NAME must be a non-empty character value.")
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
    "Core analytical metric: EPA Allowed per Play\n",
    "EPA perspective: offense\n",
    "Direction: lower is better for the defense\n",
    "Passing population: pass plays + sacks\n",
    "Running population: run plays\n",
    "NFL benchmark: pooled other-defense play-level EPA\n",
    "Output prefix: %s\n"
  ),
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  TEAM_SEASON_PREFIX
))
