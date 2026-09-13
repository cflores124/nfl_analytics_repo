# ============================================================
# 03 - Build Eligible Play Datasets
# Reusable NFL Defensive Explosive-Play Analysis Pipeline
# ============================================================
#
# Creates eligible offensive scrimmage-play datasets for the
# configured team's defense and the full NFL regular season.
#
# Eligible play:
#   Pass or run, including sacks, excluding kneels, spikes,
#   special-teams plays, and two-point attempts.
#
# Inputs:
# data/raw/{team}_{season}_defensive_pbp.csv
# data/raw/nfl_{season}_regular_season_pbp.csv
#
# Outputs:
# data/processed/{team}_{season}_eligible_plays.csv
# data/processed/nfl_{season}_eligible_plays.csv
# ============================================================


# ------------------------------------------------------------
# 1. Load configuration
# ------------------------------------------------------------

if (!file.exists("config.R")) {
  stop(
    "config.R was not found. Open nfl_defensive_explosive_play_pipeline.Rproj ",
    "from the project root before running this script."
  )
}

source("config.R")


# ------------------------------------------------------------
# 2. Check required packages
# ------------------------------------------------------------

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
# 3. Validate inputs
# ------------------------------------------------------------

if (!file.exists(TEAM_RAW_FILE)) {
  stop(
    "Team defensive raw PBP file was not found: ", TEAM_RAW_FILE,
    "\nRun data_engineering/02_extract_team_defensive_pbp.R first."
  )
}

if (!file.exists(NFL_RAW_FILE)) {
  stop(
    "NFL regular-season raw PBP file was not found: ", NFL_RAW_FILE,
    "\nRun data_engineering/01_extract_nfl_regular_season_pbp.R first."
  )
}

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "BUILD ELIGIBLE PLAY DATASETS\n",
    "============================================================\n\n",
    "Team: %s (%s)\n",
    "Season: %s\n",
    "Team input: %s\n",
    "NFL input: %s\n\n"
  ),
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  TEAM_RAW_FILE,
  NFL_RAW_FILE
))


# ------------------------------------------------------------
# 4. Load source datasets
# ------------------------------------------------------------

team_raw <- readr::read_csv(
  TEAM_RAW_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

nfl_raw <- readr::read_csv(
  NFL_RAW_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

if (nrow(team_raw) == 0) {
  stop("Team raw PBP contains zero rows: ", TEAM_RAW_FILE)
}

if (nrow(nfl_raw) == 0) {
  stop("NFL raw PBP contains zero rows: ", NFL_RAW_FILE)
}

required_columns <- c(
  "game_id",
  "play_type",
  "qb_kneel",
  "qb_spike",
  "special_teams_play",
  "two_point_attempt",
  "sack"
)

missing_team_columns <- setdiff(required_columns, names(team_raw))
missing_nfl_columns <- setdiff(required_columns, names(nfl_raw))

if (length(missing_team_columns) > 0) {
  stop(
    "Team raw PBP is missing required columns: ",
    paste(missing_team_columns, collapse = ", ")
  )
}

if (length(missing_nfl_columns) > 0) {
  stop(
    "NFL raw PBP is missing required columns: ",
    paste(missing_nfl_columns, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 5. Build eligible-play datasets
# ------------------------------------------------------------

build_eligible_plays <- function(data) {
  dplyr::filter(
    data,
    play_type %in% c("pass", "run"),
    is.na(qb_kneel) | qb_kneel != 1,
    is.na(qb_spike) | qb_spike != 1,
    is.na(special_teams_play) | special_teams_play != 1,
    is.na(two_point_attempt) | two_point_attempt != 1
  )
}

team_eligible <- build_eligible_plays(team_raw)
nfl_eligible <- build_eligible_plays(nfl_raw)

if (nrow(team_eligible) == 0) {
  stop("Eligible-play filter produced zero plays for ", TEAM_NAME, ".")
}

if (nrow(nfl_eligible) == 0) {
  stop("Eligible-play filter produced zero NFL plays.")
}


# ------------------------------------------------------------
# 6. Summarize eligible-play datasets
# ------------------------------------------------------------

summarize_eligible_plays <- function(raw, eligible) {
  c(
    raw_rows = nrow(raw),
    eligible_plays = nrow(eligible),
    excluded_rows = nrow(raw) - nrow(eligible),
    pass_plays = sum(eligible$play_type == "pass", na.rm = TRUE),
    run_plays = sum(eligible$play_type == "run", na.rm = TRUE),
    sacks_retained = sum(eligible$sack == 1, na.rm = TRUE),
    unique_games = length(unique(eligible$game_id))
  )
}

team_summary <- summarize_eligible_plays(team_raw, team_eligible)
nfl_summary <- summarize_eligible_plays(nfl_raw, nfl_eligible)


# ------------------------------------------------------------
# 7. Save eligible-play datasets
# ------------------------------------------------------------

dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(team_eligible, TEAM_ELIGIBLE_FILE)
readr::write_csv(nfl_eligible, NFL_ELIGIBLE_FILE)


# ------------------------------------------------------------
# 8. Report completion
# ------------------------------------------------------------

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "%s ELIGIBLE-PLAY SUMMARY\n",
    "============================================================\n\n",
    "Raw PBP rows: %s\n",
    "Eligible plays: %s\n",
    "Excluded rows: %s\n",
    "Pass plays: %s\n",
    "Run plays: %s\n",
    "Sacks retained: %s\n",
    "Unique games: %s\n\n",
    "============================================================\n",
    "%s NFL ELIGIBLE-PLAY SUMMARY\n",
    "============================================================\n\n",
    "Raw PBP rows: %s\n",
    "Eligible plays: %s\n",
    "Excluded rows: %s\n",
    "Pass plays: %s\n",
    "Run plays: %s\n",
    "Sacks retained: %s\n",
    "Unique games: %s\n\n",
    "============================================================\n",
    "ELIGIBLE-PLAY DATASETS CREATED SUCCESSFULLY\n",
    "============================================================\n\n",
    "Team output: %s\n",
    "NFL output: %s\n"
  ),
  toupper(TEAM_NAME),
  team_summary["raw_rows"],
  team_summary["eligible_plays"],
  team_summary["excluded_rows"],
  team_summary["pass_plays"],
  team_summary["run_plays"],
  team_summary["sacks_retained"],
  team_summary["unique_games"],
  SEASON,
  nfl_summary["raw_rows"],
  nfl_summary["eligible_plays"],
  nfl_summary["excluded_rows"],
  nfl_summary["pass_plays"],
  nfl_summary["run_plays"],
  nfl_summary["sacks_retained"],
  nfl_summary["unique_games"],
  TEAM_ELIGIBLE_FILE,
  NFL_ELIGIBLE_FILE
))
