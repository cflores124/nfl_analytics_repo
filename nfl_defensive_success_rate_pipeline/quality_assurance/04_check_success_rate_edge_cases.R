# ============================================================
# 04 - Check Defensive Success Rate Edge Cases
# Reusable NFL Defensive Success Rate Analysis Pipeline
# ============================================================
#
# Purpose:
#
#   Validate edge-case classifications that override or
#   complicate the normal 40/60/100/100 yardage rule.
#
# Primary checks:
#
#   - sacks are defensive successes
#   - interceptions are possession-losing defensive successes
#   - possession losses are explained by interceptions or
#     lost fumbles
#   - accepted offensive penalties are defensive successes
#   - first downs awarded by defensive penalty are offensive
#     successes unless a higher-precedence override applies
#   - offensive and defensive success are exact inverses
#
# Audit output:
#
#   Fumble plays are retained for source-data review because
#   yards_gained can require interpretation on fumbles/laterals.
#
# Input:
#
#   data/processed/{team}_{season}_analysis_plays.csv
#
# Outputs:
#
#   data/qa_outputs/{team}_{season}_success_rate_edge_cases.csv
#   data/qa_outputs/{team}_{season}_success_rate_edge_case_summary.csv
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

required_packages <- c("dplyr", "readr", "tibble")

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
# 2. Define QA outputs and load data
# ------------------------------------------------------------

SUCCESS_RATE_EDGE_CASE_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_success_rate_edge_cases.csv")
)

SUCCESS_RATE_EDGE_CASE_SUMMARY_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_success_rate_edge_case_summary.csv")
)

if (!file.exists(TEAM_ANALYSIS_FILE)) {
  stop(
    "Team analysis-play file was not found: ",
    TEAM_ANALYSIS_FILE,
    "\nRun data_engineering/04_classify_success_and_situations.R first.",
    call. = FALSE
  )
}

team_analysis <- readr::read_csv(
  TEAM_ANALYSIS_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

if (nrow(team_analysis) == 0) {
  stop(
    "Team analysis-play dataset contains zero rows.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 3. Validate required fields
# ------------------------------------------------------------

required_columns <- c(
  "season",
  "game_id",
  "week",
  "posteam",
  "defteam",
  "play_type",
  "analysis_play_type",
  "down",
  "ydstogo",
  "yards_gained",
  "sack",
  "interception",
  "fumble",
  "fumble_lost",
  "touchdown",
  "first_down_penalty",
  "penalty",
  "penalty_team",
  "penalty_yards",
  "possession_lost",
  "offensive_penalty",
  "offensive_success",
  "defensive_success"
)

missing_columns <- setdiff(
  required_columns,
  names(team_analysis)
)

if (length(missing_columns) > 0) {
  stop(
    "Team analysis-play dataset is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    ".",
    call. = FALSE
  )
}

team_values <- unique(stats::na.omit(team_analysis$defteam))
season_values <- unique(stats::na.omit(team_analysis$season))

if (
  length(team_values) != 1 ||
  team_values != TEAM_ABBR
) {
  stop(
    "Team analysis-play dataset must contain only ",
    TEAM_ABBR,
    " defensive plays.",
    call. = FALSE
  )
}

if (
  length(season_values) != 1 ||
  season_values != SEASON
) {
  stop(
    "Team analysis-play dataset must contain only season ",
    SEASON,
    ".",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. Identify edge cases
# ------------------------------------------------------------

edge_cases <- team_analysis |>
  dplyr::mutate(
    qa_sack =
      !is.na(sack) &
      sack == 1,
    
    qa_interception =
      !is.na(interception) &
      interception == 1,
    
    qa_lost_fumble =
      !is.na(fumble_lost) &
      fumble_lost == 1,
    
    qa_fumble =
      !is.na(fumble) &
      fumble == 1,
    
    qa_first_down_penalty =
      !is.na(first_down_penalty) &
      first_down_penalty == 1,
    
    qa_offensive_penalty =
      offensive_penalty == 1
  )


# ------------------------------------------------------------
# 5. Check definite classification rules
# ------------------------------------------------------------

sack_violations <- edge_cases |>
  dplyr::filter(
    qa_sack,
    defensive_success != 1 |
      offensive_success != 0
  )

interception_violations <- edge_cases |>
  dplyr::filter(
    qa_interception,
    possession_lost != 1 |
      defensive_success != 1 |
      offensive_success != 0
  )

possession_loss_violations <- edge_cases |>
  dplyr::filter(
    possession_lost == 1,
    !(
      (!is.na(interception) & interception == 1) |
        (!is.na(fumble_lost) & fumble_lost == 1)
    )
  )

offensive_penalty_violations <- edge_cases |>
  dplyr::filter(
    qa_offensive_penalty,
    defensive_success != 1 |
      offensive_success != 0
  )

first_down_penalty_violations <- edge_cases |>
  dplyr::filter(
    qa_first_down_penalty,
    
    # Higher-precedence classifications are excluded.
    possession_lost == 0,
    sack == 0,
    offensive_penalty == 0,
    
    offensive_success != 1 |
      defensive_success != 0
  )

success_inverse_violations <- edge_cases |>
  dplyr::filter(
    is.na(offensive_success) |
      is.na(defensive_success) |
      offensive_success + defensive_success != 1
  )


# ------------------------------------------------------------
# 6. Build fumble audit
# ------------------------------------------------------------
#
# Fumbles are retained as an audit population rather than
# treated automatically as classification errors.
#
# Lost fumbles should already be handled through possession
# loss. Offensive recoveries may require source-data review
# because yards_gained can be affected by recovery/lateral
# handling.
#
# ------------------------------------------------------------

fumble_audit <- edge_cases |>
  dplyr::filter(
    qa_fumble
  ) |>
  dplyr::mutate(
    qa_category = dplyr::if_else(
      qa_lost_fumble,
      "lost_fumble",
      "offense_recovered_fumble"
    ),
    
    qa_review_reason = dplyr::if_else(
      qa_lost_fumble,
      "Verify offensive possession ownership on lost fumble.",
      paste0(
        "Verify yards_gained represents the intended final ",
        "offensive outcome."
      )
    )
  )


# ------------------------------------------------------------
# 7. Build QA summary
# ------------------------------------------------------------

qa_summary <- tibble::tibble(
  check = c(
    "Eligible plays",
    "Sacks",
    "Sack classification violations",
    "Interceptions",
    "Interception classification violations",
    "Lost fumbles",
    "Possession-losing plays",
    "Unexplained possession-losing plays",
    "Offensive-penalty plays",
    "Offensive-penalty classification violations",
    "First downs by penalty",
    "First-down-by-penalty classification violations",
    "Fumble plays",
    "Success inverse violations"
  ),
  
  value = c(
    nrow(edge_cases),
    
    sum(edge_cases$qa_sack),
    nrow(sack_violations),
    
    sum(edge_cases$qa_interception),
    nrow(interception_violations),
    
    sum(edge_cases$qa_lost_fumble),
    
    sum(
      edge_cases$possession_lost == 1,
      na.rm = TRUE
    ),
    
    nrow(possession_loss_violations),
    
    sum(edge_cases$qa_offensive_penalty),
    nrow(offensive_penalty_violations),
    
    sum(edge_cases$qa_first_down_penalty),
    nrow(first_down_penalty_violations),
    
    sum(edge_cases$qa_fumble),
    nrow(success_inverse_violations)
  )
)


# ------------------------------------------------------------
# 8. Fail on definite classification violations
# ------------------------------------------------------------

definite_violations <- c(
  sack_classification =
    nrow(sack_violations),
  
  interception_classification =
    nrow(interception_violations),
  
  unexplained_possession_loss =
    nrow(possession_loss_violations),
  
  offensive_penalty_classification =
    nrow(offensive_penalty_violations),
  
  first_down_penalty_classification =
    nrow(first_down_penalty_violations),
  
  success_inverse =
    nrow(success_inverse_violations)
)

failed_checks <- definite_violations[
  definite_violations > 0
]

if (length(failed_checks) > 0) {
  
  failure_message <- paste(
    paste0(
      names(failed_checks),
      "=",
      failed_checks
    ),
    collapse = ", "
  )
  
  stop(
    "Success Rate edge-case QA found definite classification violations: ",
    failure_message,
    ".",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 9. Save QA outputs
# ------------------------------------------------------------

review_columns <- c(
  "season",
  "game_id",
  "week",
  "posteam",
  "defteam",
  "play_type",
  "analysis_play_type",
  "down",
  "ydstogo",
  "yards_gained",
  "sack",
  "interception",
  "fumble",
  "fumble_lost",
  "possession_lost",
  "offensive_success",
  "defensive_success",
  "qa_category",
  "qa_review_reason"
)

fumble_audit_output <- fumble_audit |>
  dplyr::select(
    dplyr::all_of(review_columns)
  ) |>
  dplyr::arrange(
    week,
    game_id
  )

dir.create(
  QA_OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  fumble_audit_output,
  SUCCESS_RATE_EDGE_CASE_FILE
)

readr::write_csv(
  qa_summary,
  SUCCESS_RATE_EDGE_CASE_SUMMARY_FILE
)


# ------------------------------------------------------------
# 10. Display results
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  "DEFENSIVE SUCCESS RATE EDGE-CASE QA\n",
  "============================================================\n\n",
  "Team: ", TEAM_NAME, " (", TEAM_ABBR, ")\n",
  "Season: ", SEASON, "\n\n",
  sep = ""
)

print(
  qa_summary,
  n = Inf
)

cat(
  "\n============================================================\n",
  "SUCCESS RATE EDGE-CASE QA COMPLETE\n",
  "============================================================\n\n",
  
  "No definite classification violations were detected.\n\n",
  
  "Fumble audit plays: ", nrow(fumble_audit_output), "\n",
  "Offensive-penalty plays: ",
  sum(edge_cases$qa_offensive_penalty), "\n\n",
  
  "Edge-case audit: ",
  SUCCESS_RATE_EDGE_CASE_FILE, "\n",
  
  "QA summary: ",
  SUCCESS_RATE_EDGE_CASE_SUMMARY_FILE, "\n",
  
  sep = ""
)
