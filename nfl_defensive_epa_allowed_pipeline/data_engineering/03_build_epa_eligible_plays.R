# ============================================================
# 03 - Build EPA-Eligible Plays
# Reusable NFL Defensive EPA Allowed Analysis Pipeline
# ============================================================
#
# Builds the team and NFL EPA-eligible play populations.
#
# Eligible play:
#   Normal offensive play identified by nflfastR's play flag;
#   excludes nullified/no-play events, kneels, spikes,
#   special teams, and two-point attempts.
#
# All retained plays must contain valid nflverse EPA.
#
# Inputs:
#   NFL_RAW_FILE
#   TEAM_RAW_FILE
#
# Outputs:
#   NFL_ELIGIBLE_FILE
#   TEAM_ELIGIBLE_FILE
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
# 2. Validate input files
# ------------------------------------------------------------

required_files <- c(
  NFL_RAW_FILE,
  TEAM_RAW_FILE
)

missing_files <- required_files[
  !file.exists(required_files)
]

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
# 4. Validate required fields
# ------------------------------------------------------------

required_columns <- c(
  "season",
  "game_id",
  "defteam",
  "play",
  "play_type",
  "qb_kneel",
  "qb_spike",
  "special_teams_play",
  "two_point_attempt",
  "epa"
)

validate_input <- function(data, label) {
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  seasons <- unique(
    stats::na.omit(data$season)
  )
  
  if (length(seasons) != 1 || seasons != SEASON) {
    stop(
      label,
      " must contain only season ",
      SEASON,
      "."
    )
  }
}

validate_input(
  nfl_raw,
  "NFL raw dataset"
)

validate_input(
  team_raw,
  "Team raw dataset"
)

team_values <- unique(
  stats::na.omit(team_raw$defteam)
)

if (
  length(team_values) != 1 ||
  team_values != TEAM_ABBR
) {
  stop(
    "Team raw dataset must contain only ",
    TEAM_ABBR,
    " defensive plays."
  )
}

if (!TEAM_ABBR %in% nfl_raw$defteam) {
  stop(
    "Configured team ",
    TEAM_ABBR,
    " was not found in the NFL raw dataset."
  )
}


# ------------------------------------------------------------
# 5. Define EPA-eligible play population
# ------------------------------------------------------------

build_epa_eligible_plays <- function(data) {
  
  data |>
    dplyr::filter(
      play == 1,
      !is.na(play_type),
      play_type != "no_play",
      is.na(qb_kneel) | qb_kneel != 1,
      is.na(qb_spike) | qb_spike != 1,
      is.na(special_teams_play) | special_teams_play != 1,
      is.na(two_point_attempt) | two_point_attempt != 1,
      !is.na(epa),
      is.finite(epa)
    )
}


# ------------------------------------------------------------
# 6. Build team and NFL eligible datasets
# ------------------------------------------------------------

nfl_eligible <- build_epa_eligible_plays(
  nfl_raw
)

team_eligible <- build_epa_eligible_plays(
  team_raw
)

if (
  nrow(nfl_eligible) == 0 ||
  nrow(team_eligible) == 0
) {
  stop(
    "One or more EPA-eligible datasets contain zero rows."
  )
}


# ------------------------------------------------------------
# 7. Validate eligible populations
# ------------------------------------------------------------

validate_eligible_population <- function(data, label) {
  
  no_play_rows <- !is.na(data$play_type) &
    data$play_type == "no_play"
  
  invalid_epa <- is.na(data$epa) |
    !is.finite(data$epa)
  
  if (any(no_play_rows)) {
    stop(
      label,
      " contains ",
      sum(no_play_rows),
      " nullified/no-play event(s)."
    )
  }
  
  if (any(invalid_epa)) {
    stop(
      label,
      " contains ",
      sum(invalid_epa),
      " eligible play(s) with missing or non-finite EPA."
    )
  }
}

validate_eligible_population(
  nfl_eligible,
  "NFL EPA-eligible dataset"
)

validate_eligible_population(
  team_eligible,
  "Team EPA-eligible dataset"
)


# ------------------------------------------------------------
# 8. Validate team/NFL consistency
# ------------------------------------------------------------

expected_team_rows <- sum(
  !is.na(nfl_eligible$defteam) &
    nfl_eligible$defteam == TEAM_ABBR
)

if (nrow(team_eligible) != expected_team_rows) {
  stop(
    "Team EPA-eligible dataset does not match the configured team's ",
    "population in the NFL EPA-eligible dataset. Expected ",
    expected_team_rows,
    " rows; found ",
    nrow(team_eligible),
    "."
  )
}


# ------------------------------------------------------------
# 9. Summarize eligible populations
# ------------------------------------------------------------

summarize_population <- function(
    raw_data,
    eligible_data
) {
  
  data.frame(
    raw_rows = nrow(raw_data),
    eligible_plays = nrow(eligible_data),
    excluded_rows = nrow(raw_data) - nrow(eligible_data),
    games = dplyr::n_distinct(eligible_data$game_id)
  )
}

nfl_summary <- summarize_population(
  nfl_raw,
  nfl_eligible
)

team_summary <- summarize_population(
  team_raw,
  team_eligible
)


# ------------------------------------------------------------
# 10. Save processed datasets
# ------------------------------------------------------------

dir.create(
  PROCESSED_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  nfl_eligible,
  NFL_ELIGIBLE_FILE
)

readr::write_csv(
  team_eligible,
  TEAM_ELIGIBLE_FILE
)


# ------------------------------------------------------------
# 11. Report completion
# ------------------------------------------------------------

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "EPA-ELIGIBLE PLAY BUILD COMPLETE\n",
    "============================================================\n\n",
    "Team: %s (%s)\n",
    "Season: %s\n\n",
    
    "NFL population:\n",
    "  Raw rows: %s\n",
    "  Eligible plays: %s\n",
    "  Excluded rows: %s\n",
    "  Games: %s\n\n",
    
    "%s population:\n",
    "  Raw rows: %s\n",
    "  Eligible plays: %s\n",
    "  Excluded rows: %s\n",
    "  Games: %s\n\n",
    
    "NFL output: %s\n",
    "Team output: %s\n"
  ),
  
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  
  nfl_summary$raw_rows,
  nfl_summary$eligible_plays,
  nfl_summary$excluded_rows,
  nfl_summary$games,
  
  TEAM_NAME,
  team_summary$raw_rows,
  team_summary$eligible_plays,
  team_summary$excluded_rows,
  team_summary$games,
  
  NFL_ELIGIBLE_FILE,
  TEAM_ELIGIBLE_FILE
))
