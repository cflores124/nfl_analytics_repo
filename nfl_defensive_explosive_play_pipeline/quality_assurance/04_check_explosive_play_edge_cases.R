# ============================================================
# 04 - Check Explosive Play Edge Cases
# Reusable NFL Defensive Explosive-Play Analysis Pipeline
# ============================================================
#
# Audits explosive-play classifications that are most vulnerable
# to edge-case or source-data behavior.
#
# Explosive thresholds:
#   Pass: yards_gained >= EXPLOSIVE_PASS_YARDS
#   Run:  yards_gained >= EXPLOSIVE_RUN_YARDS
#   Sack: never explosive
#
# Definite QA:
#   - pass/run threshold classification
#   - sack classification
#   - threshold-boundary classification
#
# Audit populations:
#   - penalty-affected eligible plays
#   - fumble/lateral plays
#
# Input:
#   TEAM_ANALYSIS_FILE
#
# Outputs:
#   *_explosive_play_edge_cases.csv
#   *_explosive_play_edge_case_summary.csv
# ============================================================


# ------------------------------------------------------------
# 1. Load configuration and packages
# ------------------------------------------------------------

if (!file.exists("config.R")) {
  stop(
    "config.R was not found. Open ",
    "nfl_defensive_explosive_play_pipeline.Rproj ",
    "from the project root before running this script.",
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
    ". Install them before running the pipeline.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 2. Define outputs and load analysis data
# ------------------------------------------------------------

EXPLOSIVE_EDGE_CASE_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_explosive_play_edge_cases.csv")
)

EXPLOSIVE_EDGE_CASE_SUMMARY_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0(TEAM_SEASON_PREFIX, "_explosive_play_edge_case_summary.csv")
)

if (!file.exists(TEAM_ANALYSIS_FILE)) {
  stop(
    "Team analysis-play file was not found: ",
    TEAM_ANALYSIS_FILE,
    "\nRun data_engineering/04_classify_explosives_and_situations.R first.",
    call. = FALSE
  )
}

team_analysis <- readr::read_csv(
  TEAM_ANALYSIS_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

if (nrow(team_analysis) == 0) {
  stop("Team analysis-play dataset contains zero rows.", call. = FALSE)
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
  "analysis_play_type",
  "yards_gained",
  "sack",
  "interception",
  "fumble",
  "fumble_lost",
  "touchdown",
  "penalty",
  "penalty_team",
  "penalty_yards",
  "explosive_play"
)

missing_columns <- setdiff(required_columns, names(team_analysis))

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

if (length(team_values) != 1 || team_values != TEAM_ABBR) {
  stop(
    "Team analysis-play dataset must contain only ",
    TEAM_ABBR,
    " defensive plays.",
    call. = FALSE
  )
}

if (length(season_values) != 1 || season_values != SEASON) {
  stop(
    "Team analysis-play dataset must contain only season ",
    SEASON,
    ".",
    call. = FALSE
  )
}

if (
  any(is.na(team_analysis$analysis_play_type)) ||
  any(!team_analysis$analysis_play_type %in% c("pass", "run", "sack"))
) {
  stop(
    "analysis_play_type must contain only pass, run, or sack ",
    "with no missing values.",
    call. = FALSE
  )
}

if (
  any(is.na(team_analysis$explosive_play)) ||
  any(!team_analysis$explosive_play %in% c(0L, 1L))
) {
  stop(
    "explosive_play must contain only 0 or 1 with no missing values.",
    call. = FALSE
  )
}

if (any(is.na(team_analysis$yards_gained))) {
  stop(
    "Eligible analysis plays contain missing yards_gained values.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. Independently classify expected explosive plays
# ------------------------------------------------------------

qa <- team_analysis |>
  dplyr::mutate(
    
    expected_explosive = dplyr::case_when(
      analysis_play_type == "sack" ~ 0L,
      
      analysis_play_type == "pass" &
        yards_gained >= EXPLOSIVE_PASS_YARDS ~ 1L,
      
      analysis_play_type == "run" &
        yards_gained >= EXPLOSIVE_RUN_YARDS ~ 1L,
      
      TRUE ~ 0L
    ),
    
    classification_violation =
      explosive_play != expected_explosive,
    
    pass_boundary =
      analysis_play_type == "pass" &
      yards_gained %in% c(
        EXPLOSIVE_PASS_YARDS - 1,
        EXPLOSIVE_PASS_YARDS
      ),
    
    run_boundary =
      analysis_play_type == "run" &
      yards_gained %in% c(
        EXPLOSIVE_RUN_YARDS - 1,
        EXPLOSIVE_RUN_YARDS
      ),
    
    penalty_affected =
      (!is.na(penalty) & penalty == 1) |
      !is.na(penalty_team) |
      (!is.na(penalty_yards) & penalty_yards != 0),
    
    fumble_play =
      !is.na(fumble) &
      fumble == 1
  )


# ------------------------------------------------------------
# 5. Check definite classification violations
# ------------------------------------------------------------

ordinary_violations <- qa |>
  dplyr::filter(
    classification_violation,
    !penalty_affected,
    !fumble_play
  )

sack_violations <- qa |>
  dplyr::filter(
    analysis_play_type == "sack",
    explosive_play != 0L
  )

boundary_violations <- qa |>
  dplyr::filter(
    pass_boundary | run_boundary,
    !penalty_affected,
    !fumble_play,
    classification_violation
  )


# ------------------------------------------------------------
# 6. Build targeted manual-review population
# ------------------------------------------------------------
#
# Penalty and fumble/lateral plays are retained for audit because
# yards_gained may require additional football-context review.
#
# They are not automatic QA failures.
#
# ------------------------------------------------------------

edge_case_audit <- qa |>
  dplyr::filter(
    penalty_affected |
      fumble_play
  ) |>
  dplyr::mutate(
    
    qa_category = dplyr::case_when(
      penalty_affected & fumble_play ~ "penalty_and_fumble",
      penalty_affected ~ "penalty",
      fumble_play ~ "fumble",
      TRUE ~ "other"
    ),
    
    qa_review_required =
      classification_violation
  ) |>
  dplyr::select(
    qa_category,
    season,
    game_id,
    week,
    posteam,
    defteam,
    analysis_play_type,
    yards_gained,
    sack,
    interception,
    fumble,
    fumble_lost,
    touchdown,
    penalty,
    penalty_team,
    penalty_yards,
    explosive_play,
    expected_explosive,
    qa_review_required
  ) |>
  dplyr::arrange(
    week,
    game_id
  )


# ------------------------------------------------------------
# 7. Build QA summary
# ------------------------------------------------------------

qa_summary <- tibble::tibble(
  check = c(
    "Eligible plays",
    "Pass plays",
    "Run plays",
    "Sacks",
    "Explosive plays",
    "Explosive pass plays",
    "Explosive run plays",
    "Pass threshold-boundary plays",
    "Run threshold-boundary plays",
    "Sack classification violations",
    "Ordinary threshold violations",
    "Threshold-boundary violations",
    "Penalty-affected eligible plays",
    "Fumble plays",
    "Penalty/fumble classification conflicts"
  ),
  
  value = c(
    nrow(qa),
    
    sum(qa$analysis_play_type == "pass"),
    sum(qa$analysis_play_type == "run"),
    sum(qa$analysis_play_type == "sack"),
    
    sum(qa$explosive_play == 1L),
    
    sum(
      qa$analysis_play_type == "pass" &
        qa$explosive_play == 1L
    ),
    
    sum(
      qa$analysis_play_type == "run" &
        qa$explosive_play == 1L
    ),
    
    sum(qa$pass_boundary),
    sum(qa$run_boundary),
    
    nrow(sack_violations),
    nrow(ordinary_violations),
    nrow(boundary_violations),
    
    sum(qa$penalty_affected),
    sum(qa$fumble_play),
    
    sum(
      (qa$penalty_affected | qa$fumble_play) &
        qa$classification_violation
    )
  )
)


# ------------------------------------------------------------
# 8. Fail on definite violations
# ------------------------------------------------------------

definite_violations <- c(
  sack_classification = nrow(sack_violations),
  ordinary_threshold = nrow(ordinary_violations),
  threshold_boundary = nrow(boundary_violations)
)

failed_checks <- definite_violations[
  definite_violations > 0
]

if (length(failed_checks) > 0) {
  stop(
    "Explosive Play edge-case QA found definite violations: ",
    paste(
      paste0(names(failed_checks), "=", failed_checks),
      collapse = ", "
    ),
    ".",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 9. Save QA outputs
# ------------------------------------------------------------

dir.create(
  QA_OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  edge_case_audit,
  EXPLOSIVE_EDGE_CASE_FILE
)

readr::write_csv(
  qa_summary,
  EXPLOSIVE_EDGE_CASE_SUMMARY_FILE
)


# ------------------------------------------------------------
# 10. Report results
# ------------------------------------------------------------

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "EXPLOSIVE PLAY EDGE-CASE QA\n",
    "============================================================\n\n",
    "Team: %s (%s)\n",
    "Season: %s\n",
    "Pass threshold: %s+ yards\n",
    "Run threshold: %s+ yards\n\n"
  ),
  TEAM_NAME,
  TEAM_ABBR,
  SEASON,
  EXPLOSIVE_PASS_YARDS,
  EXPLOSIVE_RUN_YARDS
))

print(qa_summary, n = Inf)

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "EXPLOSIVE PLAY EDGE-CASE QA COMPLETE\n",
    "============================================================\n\n",
    "No definite explosive-play classification violations detected.\n",
    "Penalty/fumble audit rows: %s\n",
    "Penalty/fumble classification conflicts: %s\n\n",
    "Edge-case audit:\n%s\n\n",
    "QA summary:\n%s\n"
  ),
  nrow(edge_case_audit),
  sum(edge_case_audit$qa_review_required),
  EXPLOSIVE_EDGE_CASE_FILE,
  EXPLOSIVE_EDGE_CASE_SUMMARY_FILE
))
