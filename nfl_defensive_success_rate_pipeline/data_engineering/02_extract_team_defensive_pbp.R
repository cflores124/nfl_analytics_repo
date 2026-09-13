# ============================================================
# 02 - Extract Team Defensive Play-by-Play
# Reusable NFL Defensive Success Rate Analysis Pipeline
# ============================================================
#
# Extracts the configured team's defensive plays from the
# authoritative NFL regular-season play-by-play dataset.
#
# Input:
# data/raw/nfl_{season}_regular_season_pbp.csv
#
# Output:
# data/raw/{team}_{season}_defensive_pbp.csv
# ============================================================


# ------------------------------------------------------------
# 1. Load configuration
# ------------------------------------------------------------

if (!file.exists("config.R")) {
  stop(
    "config.R was not found. Open nfl_defensive_success_rate_pipeline.Rproj ",
    "from the project root before running this script."
  )
}

source("config.R")


# ------------------------------------------------------------
# 2. Check required packages
# ------------------------------------------------------------

if (!requireNamespace("readr", quietly = TRUE)) {
  stop("Missing required R package: readr. Install it before running the pipeline.")
}


# ------------------------------------------------------------
# 3. Validate input
# ------------------------------------------------------------

if (!file.exists(NFL_RAW_FILE)) {
  stop(
    "NFL raw play-by-play file was not found: ", NFL_RAW_FILE,
    "\nRun data_engineering/01_extract_nfl_regular_season_pbp.R first."
  )
}

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "TEAM DEFENSIVE PLAY-BY-PLAY EXTRACTION\n",
    "============================================================\n\n",
    "Team: %s (%s)\n",
    "Season: %s\n",
    "Input file: %s\n",
    "Output file: %s\n\n",
    "Reading NFL regular-season play-by-play data...\n\n"
  ),
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  NFL_RAW_FILE,
  TEAM_RAW_FILE
))

nfl_regular_season <- readr::read_csv(
  NFL_RAW_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

if (nrow(nfl_regular_season) == 0) {
  stop("NFL raw dataset contains zero rows: ", NFL_RAW_FILE)
}

required_columns <- c("game_id", "week", "posteam", "defteam")
missing_columns <- setdiff(required_columns, names(nfl_regular_season))

if (length(missing_columns) > 0) {
  stop(
    "NFL raw dataset is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 4. Extract team defensive play-by-play
# ------------------------------------------------------------

team_defensive_pbp <- nfl_regular_season[
  !is.na(nfl_regular_season$defteam) &
    nfl_regular_season$defteam == TEAM_ABBR,
]

if (nrow(team_defensive_pbp) == 0) {
  stop(
    "No defensive play-by-play rows were found for ",
    TEAM_NAME, " (", TEAM_ABBR, "). Check TEAM_ABBR in config.R."
  )
}

team_games <- length(unique(team_defensive_pbp$game_id))
weeks_present <- sort(unique(
  team_defensive_pbp$week[!is.na(team_defensive_pbp$week)]
))


# ------------------------------------------------------------
# 5. Save team defensive dataset
# ------------------------------------------------------------

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(team_defensive_pbp, TEAM_RAW_FILE)


# ------------------------------------------------------------
# 6. Report completion
# ------------------------------------------------------------

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "TEAM DEFENSIVE EXTRACTION COMPLETE\n",
    "============================================================\n\n",
    "Rows: %s\n",
    "Unique games: %s\n",
    "Weeks: %s\n\n",
    "Saved to: %s\n"
  ),
  nrow(team_defensive_pbp),
  team_games,
  paste(weeks_present, collapse = ", "),
  TEAM_RAW_FILE
))
