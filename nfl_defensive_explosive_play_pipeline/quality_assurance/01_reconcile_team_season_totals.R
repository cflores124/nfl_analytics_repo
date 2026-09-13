# ============================================================
# 01 - Reconcile Team Defensive Season Totals
# Reusable NFL Defensive Explosive-Play Analysis Pipeline
# ============================================================
#
# Calculates official-style defensive season totals from
# nflverse play-by-play for independent reconciliation against
# an external reference source. (Pro Football Reference)
#
# Input:
# data/raw/{team}_{season}_defensive_pbp.csv
#
# Output:
# data/qa_outputs/{team}_{season}_season_reconciliation.csv
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
# 3. Validate input
# ------------------------------------------------------------

if (!file.exists(TEAM_RAW_FILE)) {
  stop(
    "Team defensive play-by-play file was not found: ", TEAM_RAW_FILE,
    "\nRun data_engineering/02_extract_team_defensive_pbp.R first."
  )
}

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "TEAM SEASON-LEVEL RECONCILIATION\n",
    "============================================================\n\n",
    "Team: %s (%s)\n",
    "Season: %s\n",
    "Input file: %s\n",
    "Output file: %s\n\n"
  ),
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  TEAM_RAW_FILE,
  TEAM_SEASON_RECONCILIATION_FILE
))


# ------------------------------------------------------------
# 4. Load team defensive play-by-play
# ------------------------------------------------------------

team_defensive_pbp <- readr::read_csv(
  TEAM_RAW_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

if (nrow(team_defensive_pbp) == 0) {
  stop("Team defensive PBP contains zero rows: ", TEAM_RAW_FILE)
}

required_columns <- c(
  "game_id",
  "season",
  "week",
  "defteam",
  "pass_attempt",
  "complete_pass",
  "passing_yards",
  "pass_touchdown",
  "interception",
  "rush_attempt",
  "rushing_yards",
  "rush_touchdown",
  "sack",
  "yards_gained",
  "two_point_attempt"
)

missing_columns <- setdiff(required_columns, names(team_defensive_pbp))

if (length(missing_columns) > 0) {
  stop(
    "Team defensive PBP is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 5. Validate team and season
# ------------------------------------------------------------

team_values <- unique(
  team_defensive_pbp$defteam[!is.na(team_defensive_pbp$defteam)]
)

if (length(team_values) != 1 || team_values != TEAM_ABBR) {
  stop(
    "Team defensive PBP does not contain only ",
    TEAM_ABBR, " defensive plays."
  )
}

season_values <- unique(
  team_defensive_pbp$season[!is.na(team_defensive_pbp$season)]
)

if (length(season_values) != 1 || season_values != SEASON) {
  stop(
    "Team defensive PBP does not contain only season ",
    SEASON, "."
  )
}


# ------------------------------------------------------------
# 6. Calculate official-style season totals
# ------------------------------------------------------------

season_reconciliation <- team_defensive_pbp |>
  dplyr::summarise(
    games = dplyr::n_distinct(game_id),
    
    # Official pass attempts exclude sacks and two-point attempts.
    opp_pass_attempts = sum(
      pass_attempt == 1 &
        sack != 1 &
        (is.na(two_point_attempt) | two_point_attempt != 1),
      na.rm = TRUE
    ),
    
    # Official completions exclude two-point attempts.
    opp_completions = sum(
      complete_pass == 1 &
        (is.na(two_point_attempt) | two_point_attempt != 1),
      na.rm = TRUE
    ),
    
    # Official team passing yards are net of sack yardage.
    opp_pass_yards =
      sum(passing_yards, na.rm = TRUE) +
      sum(
        dplyr::if_else(sack == 1, yards_gained, 0),
        na.rm = TRUE
      ),
    
    opp_pass_td = sum(pass_touchdown == 1, na.rm = TRUE),
    opp_interceptions = sum(interception == 1, na.rm = TRUE),
    opp_rush_attempts = sum(rush_attempt == 1, na.rm = TRUE),
    opp_rush_yards = sum(rushing_yards, na.rm = TRUE),
    opp_rush_td = sum(rush_touchdown == 1, na.rm = TRUE)
  ) |>
  dplyr::mutate(
    total_yards_allowed = opp_pass_yards + opp_rush_yards
  )


# ------------------------------------------------------------
# 7. Build reconciliation table
# ------------------------------------------------------------

validation_table <- tibble::tibble(
  metric = c(
    "Games",
    "Opponent Pass Attempts",
    "Opponent Completions",
    "Opponent Passing Yards",
    "Opponent Passing TDs",
    "Opponent Interceptions",
    "Opponent Rush Attempts",
    "Opponent Rushing Yards",
    "Opponent Rushing TDs",
    "Total Yards Allowed"
  ),
  nflverse_value = c(
    season_reconciliation$games,
    season_reconciliation$opp_pass_attempts,
    season_reconciliation$opp_completions,
    season_reconciliation$opp_pass_yards,
    season_reconciliation$opp_pass_td,
    season_reconciliation$opp_interceptions,
    season_reconciliation$opp_rush_attempts,
    season_reconciliation$opp_rush_yards,
    season_reconciliation$opp_rush_td,
    season_reconciliation$total_yards_allowed
  )
)


# ------------------------------------------------------------
# 8. Export reconciliation table
# ------------------------------------------------------------

dir.create(QA_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  validation_table,
  TEAM_SEASON_RECONCILIATION_FILE
)


# ------------------------------------------------------------
# 9. Report results
# ------------------------------------------------------------

weeks_present <- sort(unique(
  team_defensive_pbp$week[!is.na(team_defensive_pbp$week)]
))

cat(sprintf(
  paste0(
    "Source rows: %s\n",
    "Unique games: %s\n",
    "Weeks: %s\n\n",
    "Calculated official-style season totals:\n\n"
  ),
  nrow(team_defensive_pbp),
  dplyr::n_distinct(team_defensive_pbp$game_id),
  paste(weeks_present, collapse = ", ")
))

print(validation_table, n = Inf)

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "SEASON-LEVEL QA TABLE COMPLETE\n",
    "============================================================\n\n",
    "Validation table saved to:\n%s\n\n",
    "Reconcile these values independently against the external ",
    "reference source.\n"
  ),
  TEAM_SEASON_RECONCILIATION_FILE
))
