# ============================================================
# 02 - Validate EPA-Eligible Population
# Reusable NFL Defensive EPA Allowed Analysis Pipeline
# ============================================================
#
# Audits the EPA-eligible play population produced by the
# data engineering pipeline.
#
# Checks:
#   1. All eligible rows are normal plays
#   2. No excluded event types remain
#   3. Team eligible-play percentage is compared league-wide
#
# This script audits but does not reconstruct or modify the
# EPA-eligible play population.
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

required_packages <- c("dplyr", "readr")

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
# 2. Load NFL raw and eligible datasets
# ------------------------------------------------------------

required_files <- c(
  NFL_RAW_FILE,
  NFL_ELIGIBLE_FILE
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing required file(s):\n",
    paste0("  - ", missing_files, collapse = "\n"),
    "\n\nRun data engineering stages 01-03 first."
  )
}

nfl_raw <- readr::read_csv(
  NFL_RAW_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

nfl_eligible <- readr::read_csv(
  NFL_ELIGIBLE_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

if (
  nrow(nfl_raw) == 0 ||
  nrow(nfl_eligible) == 0
) {
  stop(
    "NFL raw or EPA-eligible dataset contains zero rows."
  )
}


# ------------------------------------------------------------
# 3. Validate required QA fields
# ------------------------------------------------------------

required_raw_columns <- c(
  "defteam"
)

required_eligible_columns <- c(
  "defteam",
  "play",
  "play_type",
  "qb_kneel",
  "qb_spike",
  "special_teams_play",
  "two_point_attempt"
)

missing_raw_columns <- setdiff(
  required_raw_columns,
  names(nfl_raw)
)

missing_eligible_columns <- setdiff(
  required_eligible_columns,
  names(nfl_eligible)
)

if (length(missing_raw_columns) > 0) {
  stop(
    "NFL raw dataset is missing required QA columns: ",
    paste(missing_raw_columns, collapse = ", ")
  )
}

if (length(missing_eligible_columns) > 0) {
  stop(
    "NFL EPA-eligible dataset is missing required QA columns: ",
    paste(missing_eligible_columns, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 4. Validate normal-play population
# ------------------------------------------------------------

invalid_play_rows <- sum(
  is.na(nfl_eligible$play) |
    nfl_eligible$play != 1
)

if (invalid_play_rows > 0) {
  stop(
    "EPA-eligible population contains ",
    invalid_play_rows,
    " row(s) not identified as normal plays."
  )
}


# ------------------------------------------------------------
# 5. Check for excluded-event leakage
# ------------------------------------------------------------

excluded_event_counts <- c(
  no_plays = sum(
    nfl_eligible$play_type == "no_play",
    na.rm = TRUE
  ),
  kneels = sum(
    nfl_eligible$qb_kneel == 1,
    na.rm = TRUE
  ),
  spikes = sum(
    nfl_eligible$qb_spike == 1,
    na.rm = TRUE
  ),
  special_teams = sum(
    nfl_eligible$special_teams_play == 1,
    na.rm = TRUE
  ),
  two_point_attempts = sum(
    nfl_eligible$two_point_attempt == 1,
    na.rm = TRUE
  )
)

if (any(excluded_event_counts > 0)) {
  stop(
    "Excluded events remain in the EPA-eligible population:\n",
    paste(
      names(excluded_event_counts),
      excluded_event_counts,
      sep = ": ",
      collapse = "\n"
    )
  )
}


# ------------------------------------------------------------
# 6. Build league-wide eligible-play distribution
# ------------------------------------------------------------

raw_counts <- nfl_raw |>
  dplyr::filter(
    !is.na(defteam)
  ) |>
  dplyr::count(
    defteam,
    name = "raw_rows"
  )

eligible_counts <- nfl_eligible |>
  dplyr::filter(
    !is.na(defteam)
  ) |>
  dplyr::count(
    defteam,
    name = "eligible_plays"
  )

eligible_distribution <- raw_counts |>
  dplyr::left_join(
    eligible_counts,
    by = "defteam"
  ) |>
  dplyr::mutate(
    eligible_plays = dplyr::coalesce(
      eligible_plays,
      0L
    ),
    eligible_percentage = round(
      100 * eligible_plays / raw_rows,
      2
    )
  ) |>
  dplyr::arrange(
    dplyr::desc(eligible_percentage)
  ) |>
  dplyr::mutate(
    league_rank = dplyr::row_number()
  )


# ------------------------------------------------------------
# 7. Identify configured team
# ------------------------------------------------------------

team_summary <- eligible_distribution |>
  dplyr::filter(
    defteam == TEAM_ABBR
  )

if (nrow(team_summary) != 1) {
  stop(
    "Unable to identify ",
    TEAM_ABBR,
    " in the league-wide eligible-play distribution."
  )
}


# ------------------------------------------------------------
# 8. Save QA output
# ------------------------------------------------------------

dir.create(
  QA_OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  eligible_distribution,
  EPA_ELIGIBLE_POPULATION_FILE
)


# ------------------------------------------------------------
# 9. Report completion
# ------------------------------------------------------------

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "EPA-ELIGIBLE POPULATION VALIDATION COMPLETE\n",
    "============================================================\n\n",
    "Team: %s (%s)\n",
    "Season: %s\n\n",
    
    "NFL eligible plays: %s\n\n",
    
    "%s eligible-play percentage: %.2f%%\n",
    "League rank: %s of %s\n\n",
    
    "Normal-play population: PASS\n",
    "Excluded-event leakage: PASS\n\n",
    
    "Saved to: %s\n"
  ),
  
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  
  nrow(nfl_eligible),
  
  TEAM_NAME,
  team_summary$eligible_percentage,
  team_summary$league_rank,
  nrow(eligible_distribution),
  
  EPA_ELIGIBLE_POPULATION_FILE
))
