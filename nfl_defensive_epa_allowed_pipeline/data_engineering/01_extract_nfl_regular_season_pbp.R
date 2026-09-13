# ============================================================
# 01 - Extract NFL Regular-Season Play-by-Play
# Reusable NFL Defensive EPA Allowed Analysis Pipeline
# ============================================================
#
# Downloads play-by-play for the configured season, isolates
# the regular season, and saves the authoritative raw NFL file.
#
# Output:
# data/raw/nfl_{season}_regular_season_pbp.csv
# ============================================================


# ------------------------------------------------------------
# 1. Load configuration
# ------------------------------------------------------------

if (!file.exists("config.R")) {
  stop(
    "config.R was not found. Open nfl_defensive_epa_allowed_pipeline.Rproj ",
    "from the project root before running this script."
  )
}

source("config.R")


# ------------------------------------------------------------
# 2. Check required packages
# ------------------------------------------------------------

required_packages <- c("nflfastR", "readr")

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
# 3. Prepare extraction
# ------------------------------------------------------------

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "NFL REGULAR-SEASON PLAY-BY-PLAY EXTRACTION\n",
    "============================================================\n\n",
    "Season: %s\n",
    "Output file: %s\n\n",
    "Downloading %s NFL play-by-play data...\n\n"
  ),
  SEASON,
  NFL_RAW_FILE,
  SEASON
))


# ------------------------------------------------------------
# 4. Download and validate NFL play-by-play
# ------------------------------------------------------------

pbp <- nflfastR::load_pbp(seasons = SEASON)

if (nrow(pbp) == 0) {
  stop("No play-by-play data were returned for season ", SEASON, ".")
}

required_columns <- c(
  "game_id",
  "season",
  "season_type",
  "week",
  "posteam",
  "defteam",
  "play_type"
)

missing_columns <- setdiff(required_columns, names(pbp))

if (length(missing_columns) > 0) {
  stop(
    "Downloaded play-by-play data are missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 5. Isolate regular season
# ------------------------------------------------------------

nfl_regular_season <- pbp[
  pbp$season == SEASON & pbp$season_type == "REG",
]

if (nrow(nfl_regular_season) == 0) {
  stop("No regular-season plays were found for ", SEASON, ".")
}


# ------------------------------------------------------------
# 6. Confirm configured team is present
# ------------------------------------------------------------

team_mask <- !is.na(nfl_regular_season$defteam) &
  nfl_regular_season$defteam == TEAM_ABBR

if (!any(team_mask)) {
  stop(
    "Configured team ", TEAM_ABBR,
    " was not found in the ", SEASON,
    " regular-season defensive data. Check TEAM_ABBR and SEASON in config.R."
  )
}

unique_games <- length(unique(nfl_regular_season$game_id))
team_games <- length(unique(nfl_regular_season$game_id[team_mask]))

weeks_present <- sort(unique(
  nfl_regular_season$week[!is.na(nfl_regular_season$week)]
))


# ------------------------------------------------------------
# 7. Save raw dataset
# ------------------------------------------------------------

readr::write_csv(nfl_regular_season, NFL_RAW_FILE)


# ------------------------------------------------------------
# 8. Report completion
# ------------------------------------------------------------

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "NFL REGULAR-SEASON EXTRACTION COMPLETE\n",
    "============================================================\n\n",
    "Rows: %s\n",
    "Unique games: %s\n",
    "Weeks: %s\n",
    "%s defensive PBP rows: %s\n",
    "%s games: %s\n\n",
    "Saved to: %s\n"
  ),
  nrow(nfl_regular_season),
  unique_games,
  paste(weeks_present, collapse = ", "),
  TEAM_NAME,
  sum(team_mask),
  TEAM_NAME,
  team_games,
  NFL_RAW_FILE
))
