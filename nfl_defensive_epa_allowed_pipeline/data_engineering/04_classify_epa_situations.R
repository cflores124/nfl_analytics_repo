# ============================================================
# 04 - Classify EPA Situations
# Reusable NFL Defensive EPA Allowed Analysis Pipeline
# ============================================================
#
# Classifies EPA-eligible plays by analysis play type and
# assigns situational flags used by downstream analysis.
#
# Analysis play types:
#   Pass -> non-sack passing plays
#   Run  -> rushing plays
#   Sack -> passing plays ending in a sack
#
# Situations:
#   Down and distance
#   Score state
#   Field position
#
# Score state is classified from the defense's perspective.
# Field position is classified from the offense's perspective.
#
# EPA remains measured from the offense's perspective.
#
# This script classifies but does not add or remove plays.
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
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
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
  NFL_ELIGIBLE_FILE,
  TEAM_ELIGIBLE_FILE
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing required EPA-eligible file(s):\n",
    paste0("  - ", missing_files, collapse = "\n"),
    "\n\nRun data_engineering/03_build_epa_eligible_plays.R first."
  )
}


# ------------------------------------------------------------
# 3. Load EPA-eligible datasets
# ------------------------------------------------------------

nfl_eligible <- readr::read_csv(
  NFL_ELIGIBLE_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

team_eligible <- readr::read_csv(
  TEAM_ELIGIBLE_FILE,
  show_col_types = FALSE,
  guess_max = Inf
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
# 4. Validate required fields
# ------------------------------------------------------------

required_columns <- c(
  "season",
  "defteam",
  "play_type",
  "sack",
  "down",
  "ydstogo",
  "score_differential",
  "yardline_100",
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
  
  if (
    length(seasons) != 1 ||
    seasons != SEASON
  ) {
    stop(
      label,
      " must contain only season ",
      SEASON,
      "."
    )
  }
  
  if (
    any(is.na(data$play_type)) ||
    any(!data$play_type %in% c("pass", "run"))
  ) {
    stop(
      label,
      " must contain only pass or run play_type values ",
      "with no missing values."
    )
  }
  
  if (
    any(is.na(data$sack)) ||
    any(!data$sack %in% c(0, 1))
  ) {
    stop(
      label,
      " must contain only 0 or 1 sack values ",
      "with no missing values."
    )
  }
  
  if (
    any(is.na(data$epa)) ||
    any(!is.finite(data$epa))
  ) {
    stop(
      label,
      " contains missing or non-finite EPA values."
    )
  }
}

validate_input(
  nfl_eligible,
  "NFL EPA-eligible dataset"
)

validate_input(
  team_eligible,
  "Team EPA-eligible dataset"
)

team_values <- unique(
  stats::na.omit(team_eligible$defteam)
)

if (
  length(team_values) != 1 ||
  team_values != TEAM_ABBR
) {
  stop(
    "Team EPA-eligible dataset must contain only ",
    TEAM_ABBR,
    " defensive plays."
  )
}

if (!TEAM_ABBR %in% nfl_eligible$defteam) {
  stop(
    "Configured team ",
    TEAM_ABBR,
    " was not found in the NFL EPA-eligible dataset."
  )
}


# ------------------------------------------------------------
# 5. Define analytical classifications
# ------------------------------------------------------------

classify_epa_situations <- function(data) {
  
  data |>
    dplyr::mutate(
      
      # Analysis play type
      #
      # Sacks are separated from ordinary passing plays so the
      # same canonical play-type definition is used throughout
      # the defensive analytics pipeline suite.
      analysis_play_type = dplyr::case_when(
        sack == 1 ~ "sack",
        play_type == "pass" ~ "pass",
        play_type == "run" ~ "run",
        TRUE ~ NA_character_
      ),
      
      # Down and distance
      situation_1st_10 = as.integer(
        down == 1 &
          ydstogo == 10
      ),
      
      situation_2nd_long = as.integer(
        down == 2 &
          ydstogo >= 7
      ),
      
      situation_2nd_medium = as.integer(
        down == 2 &
          ydstogo >= 4 &
          ydstogo <= 6
      ),
      
      situation_2nd_short = as.integer(
        down == 2 &
          ydstogo <= 3
      ),
      
      situation_3rd_down = as.integer(
        down == 3
      ),
      
      situation_4th_down = as.integer(
        down == 4
      ),
      
      # Score state from the defense's perspective
      #
      # nflverse score_differential is from the offense's
      # perspective:
      #
      #   < 0 -> offense trailing -> defense leading
      #   > 0 -> offense leading  -> defense trailing
      #   = 0 -> tied
      situation_leading = as.integer(
        score_differential < 0
      ),
      
      situation_trailing = as.integer(
        score_differential > 0
      ),
      
      situation_neutral_score = as.integer(
        score_differential == 0
      ),
      
      # Field position from the offense's perspective
      #
      # yardline_100 > 50 -> offense's own territory
      # yardline_100 < 50 -> opponent territory
      # yardline_100 = 50 -> midfield; neither category
      situation_own_territory = as.integer(
        yardline_100 > 50
      ),
      
      situation_opponent_territory = as.integer(
        yardline_100 < 50
      )
    )
}


# ------------------------------------------------------------
# 6. Classify team and NFL datasets
# ------------------------------------------------------------

nfl_analysis <- classify_epa_situations(
  nfl_eligible
)

team_analysis <- classify_epa_situations(
  team_eligible
)


# ------------------------------------------------------------
# 7. Validate classifications
# ------------------------------------------------------------

situation_columns <- c(
  "situation_1st_10",
  "situation_2nd_long",
  "situation_2nd_medium",
  "situation_2nd_short",
  "situation_3rd_down",
  "situation_4th_down",
  "situation_leading",
  "situation_trailing",
  "situation_neutral_score",
  "situation_own_territory",
  "situation_opponent_territory"
)

validate_classifications <- function(data, label) {
  
  if (
    any(is.na(data$analysis_play_type)) ||
    any(
      !data$analysis_play_type %in%
      c("pass", "run", "sack")
    )
  ) {
    stop(
      label,
      " contains invalid analysis_play_type values."
    )
  }
  
  invalid_situations <- vapply(
    situation_columns,
    function(column) {
      
      values <- data[[column]]
      
      any(
        !is.na(values) &
          !values %in% c(0L, 1L)
      )
    },
    logical(1)
  )
  
  if (any(invalid_situations)) {
    stop(
      label,
      " contains invalid situation indicator(s): ",
      paste(
        situation_columns[invalid_situations],
        collapse = ", "
      )
    )
  }
}

validate_classifications(
  nfl_analysis,
  "NFL EPA analysis dataset"
)

validate_classifications(
  team_analysis,
  "Team EPA analysis dataset"
)


# ------------------------------------------------------------
# 8. Validate population preservation
# ------------------------------------------------------------

if (nrow(nfl_analysis) != nrow(nfl_eligible)) {
  stop(
    "NFL analysis classification changed the EPA-eligible play count."
  )
}

if (nrow(team_analysis) != nrow(team_eligible)) {
  stop(
    "Team analysis classification changed the EPA-eligible play count."
  )
}


# ------------------------------------------------------------
# 9. Validate score-state partition
# ------------------------------------------------------------

validate_score_state_partition <- function(data, label) {
  
  score_state_count <-
    data$situation_leading +
    data$situation_trailing +
    data$situation_neutral_score
  
  valid_score_rows <- !is.na(data$score_differential)
  
  if (
    any(
      score_state_count[valid_score_rows] != 1L
    )
  ) {
    stop(
      label,
      " contains valid score-differential plays that were not ",
      "assigned to exactly one score-state category."
    )
  }
}

validate_score_state_partition(
  nfl_analysis,
  "NFL EPA analysis dataset"
)

validate_score_state_partition(
  team_analysis,
  "Team EPA analysis dataset"
)


# ------------------------------------------------------------
# 10. Validate field-position partition
# ------------------------------------------------------------

validate_field_position_partition <- function(data, label) {
  
  non_midfield <- (
    !is.na(data$yardline_100) &
      data$yardline_100 != 50
  )
  
  midfield <- (
    !is.na(data$yardline_100) &
      data$yardline_100 == 50
  )
  
  territory_count <-
    data$situation_own_territory +
    data$situation_opponent_territory
  
  if (
    any(
      territory_count[non_midfield] != 1L
    )
  ) {
    stop(
      label,
      " contains non-midfield plays that were not assigned ",
      "to exactly one territory category."
    )
  }
  
  if (
    any(
      territory_count[midfield] != 0L
    )
  ) {
    stop(
      label,
      " contains midfield plays assigned to a territory category."
    )
  }
}

validate_field_position_partition(
  nfl_analysis,
  "NFL EPA analysis dataset"
)

validate_field_position_partition(
  team_analysis,
  "Team EPA analysis dataset"
)


# ------------------------------------------------------------
# 11. Summarize classifications
# ------------------------------------------------------------

summarize_classifications <- function(data) {
  
  c(
    total_plays = nrow(data),
    
    pass_plays = sum(
      data$analysis_play_type == "pass"
    ),
    
    run_plays = sum(
      data$analysis_play_type == "run"
    ),
    
    sack_plays = sum(
      data$analysis_play_type == "sack"
    ),
    
    passing_population = sum(
      data$analysis_play_type %in%
        c("pass", "sack")
    ),
    
    midfield_plays = sum(
      !is.na(data$yardline_100) &
        data$yardline_100 == 50
    )
  )
}

nfl_summary <- summarize_classifications(
  nfl_analysis
)

team_summary <- summarize_classifications(
  team_analysis
)


# ------------------------------------------------------------
# 12. Save analysis-ready datasets
# ------------------------------------------------------------

dir.create(
  PROCESSED_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  nfl_analysis,
  NFL_ANALYSIS_FILE
)

readr::write_csv(
  team_analysis,
  TEAM_ANALYSIS_FILE
)


# ------------------------------------------------------------
# 13. Report completion
# ------------------------------------------------------------

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "EPA SITUATION CLASSIFICATION COMPLETE\n",
    "============================================================\n\n",
    
    "Team: %s (%s)\n",
    "Season: %s\n\n",
    
    "NFL population:\n",
    "  Total plays: %s\n",
    "  Pass plays: %s\n",
    "  Run plays: %s\n",
    "  Sack plays: %s\n",
    "  Passing population: %s\n",
    "  Midfield plays: %s\n\n",
    
    "%s population:\n",
    "  Total plays: %s\n",
    "  Pass plays: %s\n",
    "  Run plays: %s\n",
    "  Sack plays: %s\n",
    "  Passing population: %s\n",
    "  Midfield plays: %s\n\n",
    
    "Population preservation: PASS\n",
    "Analysis play-type validation: PASS\n",
    "Score-state classification: PASS\n",
    "Field-position classification: PASS\n\n",
    
    "NFL output: %s\n",
    "Team output: %s\n"
  ),
  
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  
  nfl_summary["total_plays"],
  nfl_summary["pass_plays"],
  nfl_summary["run_plays"],
  nfl_summary["sack_plays"],
  nfl_summary["passing_population"],
  nfl_summary["midfield_plays"],
  
  TEAM_NAME,
  team_summary["total_plays"],
  team_summary["pass_plays"],
  team_summary["run_plays"],
  team_summary["sack_plays"],
  team_summary["passing_population"],
  team_summary["midfield_plays"],
  
  NFL_ANALYSIS_FILE,
  TEAM_ANALYSIS_FILE
))
