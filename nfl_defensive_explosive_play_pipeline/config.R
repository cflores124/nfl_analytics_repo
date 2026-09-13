# ============================================================
# Configuration
# Reusable NFL Defensive Explosive-Play Analysis Pipeline
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

# Explosive thresholds:
#   Pass: non-sack pass gaining >= threshold
#   Run:  run gaining >= threshold
EXPLOSIVE_PASS_YARDS <- 15
EXPLOSIVE_RUN_YARDS <- 10


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

TEAM_VULNERABILITY_FILE <- file.path(
  ANALYSIS_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_explosive_vulnerability_table.csv")
)

# Passing opportunities include pass plays and sacks.
# Sacks remain in the denominator but cannot be explosive.
TEAM_EXPLOSIVE_PASS_FILE <- file.path(
  ANALYSIS_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_explosive_pass_table.csv")
)

TEAM_EXPLOSIVE_RUN_FILE <- file.path(
  ANALYSIS_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_explosive_run_table.csv")
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

if (!is.numeric(EXPLOSIVE_PASS_YARDS) ||
    length(EXPLOSIVE_PASS_YARDS) != 1 ||
    is.na(EXPLOSIVE_PASS_YARDS) ||
    EXPLOSIVE_PASS_YARDS <= 0) {
  stop("EXPLOSIVE_PASS_YARDS must be a single numeric value greater than zero.")
}

if (!is.numeric(EXPLOSIVE_RUN_YARDS) ||
    length(EXPLOSIVE_RUN_YARDS) != 1 ||
    is.na(EXPLOSIVE_RUN_YARDS) ||
    EXPLOSIVE_RUN_YARDS <= 0) {
  stop("EXPLOSIVE_RUN_YARDS must be a single numeric value greater than zero.")
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
    "Explosive pass threshold: %s yards\n",
    "Explosive run threshold: %s yards\n",
    "Passing denominator: pass plays + sacks\n",
    "Output prefix: %s\n"
  ),
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  EXPLOSIVE_PASS_YARDS,
  EXPLOSIVE_RUN_YARDS,
  TEAM_SEASON_PREFIX
))
