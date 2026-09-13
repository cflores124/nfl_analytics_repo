# ============================================================
# 04 - Classify Explosives and Situations
# Reusable NFL Defensive Explosive-Play Analysis Pipeline
# ============================================================
#
# Purpose:
#   Add analytical play type, explosive-play status, and
#   situational flags to the eligible-play datasets.
#
# This script transforms the existing eligible-play population.
# It does not add or remove plays.
#
# Situations:
#   1st & 10
#   2nd & long (7+)
#   2nd & medium (4-6)
#   2nd & short (<=3)
#   3rd down
#   4th down
#   Leading
#   Trailing
#   Neutral score
#   Own territory
#   Opponent territory
#
# "Overall" is calculated downstream from all eligible plays.
#
# Inputs:
#   TEAM_ELIGIBLE_FILE
#   NFL_ELIGIBLE_FILE
#
# Outputs:
#   TEAM_ANALYSIS_FILE
#   NFL_ANALYSIS_FILE
#
# ============================================================


# ------------------------------------------------------------
# 1. Load configuration and packages
# ------------------------------------------------------------

if (!file.exists("config.R")) {
  stop(
    "config.R was not found. Open the project from its root directory.",
    call. = FALSE
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
    ".",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 2. Load eligible-play datasets
# ------------------------------------------------------------

required_files <- c(
  TEAM_ELIGIBLE_FILE,
  NFL_ELIGIBLE_FILE
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing required eligible-play file(s):\n",
    paste0("  - ", missing_files, collapse = "\n"),
    "\n\nRun data_engineering/03_build_eligible_plays.R first.",
    call. = FALSE
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
  stop(
    "One or more eligible-play datasets contain zero rows.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 3. Validate required source fields
# ------------------------------------------------------------
#
# Stage 03 owns eligible-play construction.
#
# This script checks only the source fields required for its
# classifications.
#
# ------------------------------------------------------------

required_columns <- c(
  "play_type",
  "sack",
  "yards_gained",
  "down",
  "ydstogo",
  "score_differential",
  "yardline_100"
)

validate_source_fields <- function(data, label) {
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

validate_source_fields(
  team_eligible,
  "Team eligible-play dataset"
)

validate_source_fields(
  nfl_eligible,
  "NFL eligible-play dataset"
)


# ------------------------------------------------------------
# 4. Classify analysis plays
# ------------------------------------------------------------
#
# Score differential is from the offense's perspective:
#
#   < 0 -> defense leading
#   > 0 -> defense trailing
#   = 0 -> neutral
#
# yardline_100 is from the offense's perspective:
#
#   > 50 -> offense own territory
#   < 50 -> offense opponent territory
#   = 50 -> midfield
#
# ------------------------------------------------------------

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
      
      # Explosive-play classification
      explosive_play = dplyr::case_when(
        analysis_play_type == "pass" &
          yards_gained >= EXPLOSIVE_PASS_YARDS ~ 1L,
        
        analysis_play_type == "run" &
          yards_gained >= EXPLOSIVE_RUN_YARDS ~ 1L,
        
        TRUE ~ 0L
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
          dplyr::between(ydstogo, 4, 6)
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
      
      # Score state
      situation_leading = as.integer(
        score_differential < 0
      ),
      
      situation_trailing = as.integer(
        score_differential > 0
      ),
      
      situation_neutral_score = as.integer(
        score_differential == 0
      ),
      
      # Field position
      situation_own_territory = as.integer(
        yardline_100 > 50
      ),
      
      situation_opponent_territory = as.integer(
        yardline_100 < 50
      )
    )
}


team_analysis <- classify_analysis_plays(
  team_eligible
)

nfl_analysis <- classify_analysis_plays(
  nfl_eligible
)


# ------------------------------------------------------------
# 5. Validate generated classifications
# ------------------------------------------------------------
#
# These are structural checks on fields created by this script.
# Independent football-methodology QA is performed downstream.
#
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

validate_classifications <- function(
    input,
    output,
    label
) {
  
  # Classification must preserve the eligible population.
  if (nrow(output) != nrow(input)) {
    stop(
      label,
      " classification changed the eligible-play row count.",
      call. = FALSE
    )
  }
  
  # Every eligible play must have an analytical play type.
  if (
    any(is.na(output$analysis_play_type)) ||
    any(
      !output$analysis_play_type %in%
      c("pass", "run", "sack")
    )
  ) {
    stop(
      label,
      " contains an invalid analysis_play_type.",
      call. = FALSE
    )
  }
  
  # Generated binary fields must contain only 0 or 1.
  binary_columns <- c(
    "explosive_play",
    situation_columns
  )
  
  invalid_binary <- vapply(
    binary_columns,
    function(column) {
      any(
        is.na(output[[column]]) |
          !output[[column]] %in% c(0L, 1L)
      )
    },
    logical(1)
  )
  
  if (any(invalid_binary)) {
    stop(
      label,
      " contains invalid binary classification field(s): ",
      paste(
        binary_columns[invalid_binary],
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }
  
  # Second-down distance groups must be mutually exclusive.
  second_down_groups <-
    output$situation_2nd_long +
    output$situation_2nd_medium +
    output$situation_2nd_short
  
  if (any(second_down_groups > 1L)) {
    stop(
      label,
      " contains overlapping second-down distance groups.",
      call. = FALSE
    )
  }
  
  # Score-state groups must be mutually exclusive.
  score_groups <-
    output$situation_leading +
    output$situation_trailing +
    output$situation_neutral_score
  
  if (any(score_groups > 1L)) {
    stop(
      label,
      " contains overlapping score-state groups.",
      call. = FALSE
    )
  }
  
  # Territory groups must be mutually exclusive.
  territory_groups <-
    output$situation_own_territory +
    output$situation_opponent_territory
  
  if (any(territory_groups > 1L)) {
    stop(
      label,
      " contains overlapping territory groups.",
      call. = FALSE
    )
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
# 6. Summarize classifications
# ------------------------------------------------------------

summarize_classifications <- function(data) {
  
  c(
    eligible_plays = nrow(data),
    
    passes = sum(
      data$analysis_play_type == "pass"
    ),
    
    runs = sum(
      data$analysis_play_type == "run"
    ),
    
    sacks = sum(
      data$analysis_play_type == "sack"
    ),
    
    explosive_plays = sum(
      data$explosive_play
    ),
    
    third_down_plays = sum(
      data$situation_3rd_down
    ),
    
    fourth_down_plays = sum(
      data$situation_4th_down
    ),
    
    midfield_plays = sum(
      data$yardline_100 == 50,
      na.rm = TRUE
    )
  )
}

team_summary <- summarize_classifications(
  team_analysis
)

nfl_summary <- summarize_classifications(
  nfl_analysis
)


# ------------------------------------------------------------
# 7. Save outputs
# ------------------------------------------------------------

dir.create(
  PROCESSED_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  team_analysis,
  TEAM_ANALYSIS_FILE
)

readr::write_csv(
  nfl_analysis,
  NFL_ANALYSIS_FILE
)


# ------------------------------------------------------------
# 8. Report completion
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  "EXPLOSIVE / SITUATION CLASSIFICATION COMPLETE\n",
  "============================================================\n\n",
  
  TEAM_NAME, "\n",
  "  Eligible plays: ", team_summary["eligible_plays"], "\n",
  "  Passes: ", team_summary["passes"], "\n",
  "  Runs: ", team_summary["runs"], "\n",
  "  Sacks: ", team_summary["sacks"], "\n",
  "  Explosive plays: ", team_summary["explosive_plays"], "\n",
  "  Third-down plays: ", team_summary["third_down_plays"], "\n",
  "  Fourth-down plays: ", team_summary["fourth_down_plays"], "\n",
  "  Midfield plays: ", team_summary["midfield_plays"], "\n\n",
  
  SEASON, " NFL\n",
  "  Eligible plays: ", nfl_summary["eligible_plays"], "\n",
  "  Passes: ", nfl_summary["passes"], "\n",
  "  Runs: ", nfl_summary["runs"], "\n",
  "  Sacks: ", nfl_summary["sacks"], "\n",
  "  Explosive plays: ", nfl_summary["explosive_plays"], "\n",
  "  Third-down plays: ", nfl_summary["third_down_plays"], "\n",
  "  Fourth-down plays: ", nfl_summary["fourth_down_plays"], "\n",
  "  Midfield plays: ", nfl_summary["midfield_plays"], "\n\n",
  
  "Team output: ", TEAM_ANALYSIS_FILE, "\n",
  "NFL output: ", NFL_ANALYSIS_FILE, "\n",
  
  sep = ""
)
