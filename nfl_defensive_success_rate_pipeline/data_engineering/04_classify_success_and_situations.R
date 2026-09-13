# ============================================================
# 04 - Classify Success and Situations
# Reusable NFL Defensive Success Rate Analysis Pipeline
# ============================================================
#
# Classifies eligible plays for Defensive Success Rate and
# assigns situational flags used by downstream analysis.
#
# Success rules:
#   1st down: 40% of yards to go
#   2nd down: 60% of yards to go
#   3rd/4th down: 100% of yards to go
#
# Precedence:
#   1. Possession-losing turnover -> defensive success
#   2. Sack                       -> defensive success
#   3. Offensive penalty          -> defensive success
#   4. Offensive touchdown        -> offensive success
#   5. First down by penalty      -> offensive success
#   6. Otherwise apply yardage threshold
#
# This script classifies but does not add or remove plays.
# ============================================================


# ------------------------------------------------------------
# 1. Load configuration and packages
# ------------------------------------------------------------

if (!file.exists("config.R")) {
  stop(
    "config.R was not found. Open nfl_defensive_success_rate_pipeline.Rproj ",
    "from the project root."
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
    paste(missing_packages, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 2. Validate and load inputs
# ------------------------------------------------------------

input_files <- c(
  Team = TEAM_ELIGIBLE_FILE,
  NFL = NFL_ELIGIBLE_FILE
)

missing_files <- input_files[!file.exists(input_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing eligible-play file(s): ",
    paste(missing_files, collapse = ", "),
    "\nRun data_engineering/03_build_eligible_plays.R first."
  )
}

team_eligible <- readr::read_csv(
  TEAM_ELIGIBLE_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

nfl_eligible <- readr::read_csv(
  NFL_ELIGIBLE_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

if (nrow(team_eligible) == 0 || nrow(nfl_eligible) == 0) {
  stop("One or more eligible-play datasets contain zero rows.")
}

required_columns <- c(
  "game_id",
  "posteam",
  "play_type",
  "sack",
  "yards_gained",
  "down",
  "ydstogo",
  "interception",
  "fumble_lost",
  "touchdown",
  "first_down_penalty",
  "penalty",
  "penalty_team",
  "score_differential",
  "yardline_100"
)

check_columns <- function(data, label) {
  
  missing <- setdiff(required_columns, names(data))
  
  if (length(missing) > 0) {
    stop(
      label,
      " eligible-play dataset is missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }
}

check_columns(team_eligible, TEAM_NAME)
check_columns(nfl_eligible, paste(SEASON, "NFL"))


# ------------------------------------------------------------
# 3. Define classification logic
# ------------------------------------------------------------

# score_differential is from the offense's perspective:
#   < 0 = defense leading
#   > 0 = defense trailing
#   = 0 = neutral
#
# yardline_100 is from the offense's perspective:
#   > 50 = offense own territory
#   < 50 = offense opponent territory
#   = 50 = midfield and belongs to neither territory category

classify_analysis_plays <- function(data) {
  
  data |>
    dplyr::mutate(
      
      # Analytical play type
      analysis_play_type = dplyr::case_when(
        sack == 1 ~ "sack",
        play_type == "pass" ~ "pass",
        play_type == "run" ~ "run",
        TRUE ~ NA_character_
      ),
      
      # Turnover and penalty flags
      possession_lost = as.integer(
        interception == 1 | fumble_lost == 1
      ),
      
      offensive_penalty = as.integer(
        penalty == 1 &
          !is.na(penalty_team) &
          !is.na(posteam) &
          penalty_team == posteam
      ),
      
      # Offensive success classification
      offensive_success = dplyr::case_when(
        possession_lost == 1L ~ 0L,
        sack == 1 ~ 0L,
        offensive_penalty == 1L ~ 0L,
        touchdown == 1 ~ 1L,
        first_down_penalty == 1 ~ 1L,
        
        down == 1 &
          yards_gained >= FIRST_DOWN_SUCCESS_THRESHOLD * ydstogo ~ 1L,
        
        down == 2 &
          yards_gained >= SECOND_DOWN_SUCCESS_THRESHOLD * ydstogo ~ 1L,
        
        down == 3 &
          yards_gained >= THIRD_DOWN_SUCCESS_THRESHOLD * ydstogo ~ 1L,
        
        down == 4 &
          yards_gained >= FOURTH_DOWN_SUCCESS_THRESHOLD * ydstogo ~ 1L,
        
        TRUE ~ 0L
      ),
      
      defensive_success = 1L - offensive_success,
      
      # Down and distance
      situation_1st_10 =
        as.integer(down == 1 & ydstogo == 10),
      
      situation_2nd_long =
        as.integer(down == 2 & ydstogo >= 7),
      
      situation_2nd_medium =
        as.integer(down == 2 & dplyr::between(ydstogo, 4, 6)),
      
      situation_2nd_short =
        as.integer(down == 2 & ydstogo <= 3),
      
      situation_3rd_down =
        as.integer(down == 3),
      
      situation_4th_down =
        as.integer(down == 4),
      
      # Score state
      situation_leading =
        as.integer(score_differential < 0),
      
      situation_trailing =
        as.integer(score_differential > 0),
      
      situation_neutral_score =
        as.integer(score_differential == 0),
      
      # Field position
      situation_own_territory =
        as.integer(yardline_100 > 50),
      
      situation_opponent_territory =
        as.integer(yardline_100 < 50)
    )
}


# ------------------------------------------------------------
# 4. Classify eligible plays
# ------------------------------------------------------------

team_analysis <- classify_analysis_plays(team_eligible)
nfl_analysis <- classify_analysis_plays(nfl_eligible)


# ------------------------------------------------------------
# 5. Validate classifications
# ------------------------------------------------------------

validate_classifications <- function(input, output, label) {
  
  if (nrow(output) != nrow(input)) {
    stop(label, " classification changed the eligible-play count.")
  }
  
  if (
    any(is.na(output$analysis_play_type)) ||
    any(!output$analysis_play_type %in% c("pass", "run", "sack"))
  ) {
    stop(label, " contains invalid analytical play types.")
  }
  
  if (
    any(is.na(output$offensive_success)) ||
    any(!output$offensive_success %in% c(0L, 1L)) ||
    any(is.na(output$defensive_success)) ||
    any(!output$defensive_success %in% c(0L, 1L))
  ) {
    stop(label, " contains invalid success classifications.")
  }
  
  if (any(
    output$offensive_success + output$defensive_success != 1L
  )) {
    stop(label, " offensive and defensive success are not exact inverses.")
  }
  
  if (any(
    output$analysis_play_type == "sack" &
    output$defensive_success != 1L
  )) {
    stop(label, " contains a sack not classified as defensive success.")
  }
  
  if (any(
    output$possession_lost == 1L &
    output$defensive_success != 1L
  )) {
    stop(
      label,
      " contains a possession-losing turnover not classified ",
      "as defensive success."
    )
  }
  
  if (any(
    output$offensive_penalty == 1L &
    output$defensive_success != 1L
  )) {
    stop(
      label,
      " contains an offensive penalty not classified as defensive success."
    )
  }
  
  second_down_categories <-
    output$situation_2nd_long +
    output$situation_2nd_medium +
    output$situation_2nd_short
  
  if (any(second_down_categories > 1, na.rm = TRUE)) {
    stop(label, " contains overlapping second-down categories.")
  }
  
  known_score <- !is.na(output$score_differential)
  
  score_categories <-
    output$situation_leading +
    output$situation_trailing +
    output$situation_neutral_score
  
  if (any(score_categories[known_score] != 1)) {
    stop(label, " contains an invalid score-state classification.")
  }
  
  known_territory <-
    !is.na(output$yardline_100) &
    output$yardline_100 != 50
  
  territory_categories <-
    output$situation_own_territory +
    output$situation_opponent_territory
  
  if (any(territory_categories[known_territory] != 1)) {
    stop(label, " contains an invalid territory classification.")
  }
}

validate_classifications(
  team_eligible,
  team_analysis,
  TEAM_NAME
)

validate_classifications(
  nfl_eligible,
  nfl_analysis,
  paste(SEASON, "NFL")
)


# ------------------------------------------------------------
# 6. Build summaries
# ------------------------------------------------------------

summarize_classifications <- function(data) {
  
  c(
    plays = nrow(data),
    pass = sum(data$analysis_play_type == "pass"),
    run = sum(data$analysis_play_type == "run"),
    sack = sum(data$analysis_play_type == "sack"),
    offensive_penalty = sum(data$offensive_penalty),
    offensive_success = sum(data$offensive_success),
    defensive_success = sum(data$defensive_success),
    turnovers = sum(data$possession_lost),
    fourth_down = sum(data$situation_4th_down),
    midfield = sum(data$yardline_100 == 50, na.rm = TRUE)
  )
}

team_summary <- summarize_classifications(team_analysis)
nfl_summary <- summarize_classifications(nfl_analysis)


# ------------------------------------------------------------
# 7. Save analysis-ready datasets
# ------------------------------------------------------------

dir.create(
  PROCESSED_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(team_analysis, TEAM_ANALYSIS_FILE)
readr::write_csv(nfl_analysis, NFL_ANALYSIS_FILE)


# ------------------------------------------------------------
# 8. Report completion
# ------------------------------------------------------------

print_summary <- function(label, summary) {
  
  cat(
    label, "\n",
    "  Eligible plays: ", summary["plays"], "\n",
    "  Passes: ", summary["pass"], "\n",
    "  Runs: ", summary["run"], "\n",
    "  Sacks: ", summary["sack"], "\n",
    "  Offensive-penalty plays: ", summary["offensive_penalty"], "\n",
    "  Offensive successes: ", summary["offensive_success"], "\n",
    "  Defensive successes: ", summary["defensive_success"], "\n",
    "  Possession-losing turnovers: ", summary["turnovers"], "\n",
    "  Fourth-down plays: ", summary["fourth_down"], "\n",
    "  Plays at midfield: ", summary["midfield"], "\n\n",
    sep = ""
  )
}

cat(
  "\n============================================================\n",
  "CLASSIFICATION SUMMARY\n",
  "============================================================\n\n",
  sep = ""
)

print_summary(TEAM_NAME, team_summary)
print_summary(paste(SEASON, "NFL"), nfl_summary)

cat(
  "Team output: ", TEAM_ANALYSIS_FILE, "\n",
  "NFL output: ", NFL_ANALYSIS_FILE, "\n",
  sep = ""
)
