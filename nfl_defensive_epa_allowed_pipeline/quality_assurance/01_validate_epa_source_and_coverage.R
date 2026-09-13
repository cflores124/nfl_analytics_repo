# ============================================================
# 01 - Validate EPA Source and Coverage
# Reusable NFL Defensive EPA Allowed Analysis Pipeline
# ============================================================
#
# Validates the authoritative NFL regular-season source and
# configured team extraction before EPA population analysis.
#
# Checks:
#   1. Required source fields are present
#   2. Source contains only the configured season
#   3. EPA values are numeric where present
#   4. Defensive-team and game coverage are valid
#   5. Team extraction matches the authoritative NFL source
#
# This script validates source integrity and coverage but does
# not define or validate the EPA-eligible play population.
# ============================================================


# ------------------------------------------------------------
# 1. Load configuration and packages
# ------------------------------------------------------------

if (!file.exists("config.R")) {
  stop(
    "config.R was not found. Open nfl_defensive_epa_allowed_pipeline.Rproj ",
    "from the project root before running this script."
  )
}

source("config.R")

required_packages <- c("dplyr", "readr", "tibble")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running the pipeline."
  )
}


# ------------------------------------------------------------
# 2. Validate input files
# ------------------------------------------------------------

required_files <- c(
  NFL_RAW_FILE,
  TEAM_RAW_FILE
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required raw file(s):\n",
    paste0("  - ", missing_files, collapse = "\n"),
    "\n\nRun data_engineering/01_extract_nfl_regular_season_pbp.R and ",
    "data_engineering/02_extract_team_defensive_pbp.R first."
  )
}


# ------------------------------------------------------------
# 3. Load raw datasets
# ------------------------------------------------------------

nfl_raw <- readr::read_csv(
  NFL_RAW_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

team_raw <- readr::read_csv(
  TEAM_RAW_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

if (nrow(nfl_raw) == 0 || nrow(team_raw) == 0) {
  stop("One or more raw datasets contain zero rows.")
}


# ------------------------------------------------------------
# 4. Validate required source fields
# ------------------------------------------------------------

required_columns <- c(
  "game_id",
  "season",
  "season_type",
  "week",
  "posteam",
  "defteam",
  "play_type",
  "epa"
)

validate_source <- function(data, label) {
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  seasons <- unique(stats::na.omit(data$season))
  
  if (length(seasons) != 1 || seasons != SEASON) {
    stop(
      label,
      " must contain only season ",
      SEASON,
      "."
    )
  }
  
  season_types <- unique(stats::na.omit(data$season_type))
  
  if (length(season_types) != 1 || season_types != "REG") {
    stop(
      label,
      " must contain only regular-season data."
    )
  }
  
  if (!is.numeric(data$epa)) {
    stop(
      label,
      " EPA field must be numeric."
    )
  }
  
  if (any(is.infinite(data$epa), na.rm = TRUE)) {
    stop(
      label,
      " contains infinite EPA values."
    )
  }
}

validate_source(nfl_raw, "NFL raw dataset")
validate_source(team_raw, "Team raw dataset")


# ------------------------------------------------------------
# 5. Validate defensive-team coverage
# ------------------------------------------------------------

nfl_defensive_rows <- nfl_raw |>
  dplyr::filter(!is.na(defteam))

defenses_present <- sort(unique(nfl_defensive_rows$defteam))

if (length(defenses_present) != 32) {
  stop(
    "NFL raw dataset contains ",
    length(defenses_present),
    " defensive teams; expected 32."
  )
}

if (!TEAM_ABBR %in% defenses_present) {
  stop(
    "Configured team ",
    TEAM_ABBR,
    " was not found in the NFL raw defensive-team values."
  )
}

team_values <- unique(stats::na.omit(team_raw$defteam))

if (length(team_values) != 1 || team_values != TEAM_ABBR) {
  stop(
    "Team raw dataset must contain only ",
    TEAM_ABBR,
    " defensive plays."
  )
}


# ------------------------------------------------------------
# 6. Validate team extraction against NFL source
# ------------------------------------------------------------

expected_team_rows <- nfl_raw |>
  dplyr::filter(
    !is.na(defteam),
    defteam == TEAM_ABBR
  )

if (nrow(team_raw) != nrow(expected_team_rows)) {
  stop(
    "Team raw extraction does not match the configured team's ",
    "population in the NFL raw dataset. Expected ",
    nrow(expected_team_rows),
    " rows; found ",
    nrow(team_raw),
    "."
  )
}

nfl_team_games <- sort(unique(expected_team_rows$game_id))
team_games <- sort(unique(team_raw$game_id))

if (!identical(nfl_team_games, team_games)) {
  stop(
    "Team raw extraction does not contain the same games as the ",
    "configured team's population in the NFL raw dataset."
  )
}


# ------------------------------------------------------------
# 7. Build defensive coverage summary
# ------------------------------------------------------------

coverage_summary <- nfl_defensive_rows |>
  dplyr::group_by(defteam) |>
  dplyr::summarise(
    raw_rows = dplyr::n(),
    games = dplyr::n_distinct(game_id),
    weeks = dplyr::n_distinct(week, na.rm = TRUE),
    epa_present = sum(!is.na(epa)),
    epa_missing = sum(is.na(epa)),
    epa_coverage_pct = round(
      100 * epa_present / raw_rows,
      2
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(defteam)


# ------------------------------------------------------------
# 8. Validate game and week coverage
# ------------------------------------------------------------

invalid_game_coverage <- coverage_summary |>
  dplyr::filter(games == 0)

if (nrow(invalid_game_coverage) > 0) {
  stop(
    "One or more defenses contain zero games in the raw source."
  )
}

team_coverage <- coverage_summary |>
  dplyr::filter(defteam == TEAM_ABBR)

if (nrow(team_coverage) != 1) {
  stop(
    "Unable to identify exactly one coverage row for ",
    TEAM_ABBR,
    "."
  )
}


# ------------------------------------------------------------
# 9. Summarize EPA source availability
# ------------------------------------------------------------

nfl_epa_present <- sum(!is.na(nfl_raw$epa))
nfl_epa_missing <- sum(is.na(nfl_raw$epa))

team_epa_present <- sum(!is.na(team_raw$epa))
team_epa_missing <- sum(is.na(team_raw$epa))

nfl_epa_coverage_pct <- round(
  100 * nfl_epa_present / nrow(nfl_raw),
  2
)

team_epa_coverage_pct <- round(
  100 * team_epa_present / nrow(team_raw),
  2
)


# ------------------------------------------------------------
# 10. Save QA output
# ------------------------------------------------------------

dir.create(
  QA_OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  coverage_summary,
  EPA_SOURCE_COVERAGE_FILE
)


# ------------------------------------------------------------
# 11. Report completion
# ------------------------------------------------------------

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "EPA SOURCE AND COVERAGE VALIDATION COMPLETE\n",
    "============================================================\n\n",
    "Team: %s (%s)\n",
    "Season: %s\n\n",
    "NFL source:\n",
    "  Raw rows: %s\n",
    "  Defensive teams: %s\n",
    "  Unique games: %s\n",
    "  EPA present: %s\n",
    "  EPA missing: %s\n",
    "  EPA coverage: %.2f%%\n\n",
    "%s source:\n",
    "  Raw rows: %s\n",
    "  Unique games: %s\n",
    "  EPA present: %s\n",
    "  EPA missing: %s\n",
    "  EPA coverage: %.2f%%\n\n",
    "Team extraction matches NFL source: YES\n\n",
    "Saved to: %s\n"
  ),
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  nrow(nfl_raw),
  length(defenses_present),
  dplyr::n_distinct(nfl_raw$game_id),
  nfl_epa_present,
  nfl_epa_missing,
  nfl_epa_coverage_pct,
  TEAM_NAME,
  nrow(team_raw),
  dplyr::n_distinct(team_raw$game_id),
  team_epa_present,
  team_epa_missing,
  team_epa_coverage_pct,
  EPA_SOURCE_COVERAGE_FILE
))