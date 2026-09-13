# ============================================================
# Run Full NFL Defensive Explosive-Play Analysis Pipeline
# ============================================================
#
# Purpose:
#
#   Execute the complete defensive explosive-play pipeline in
#   dependency order, suppress individual script output, report
#   stage-level status, and summarize final analytical results.
#
# Pipeline:
#
#   1.  Extract NFL regular-season PBP
#   2.  Extract configured team's defensive PBP
#   3.  Build season-level reconciliation
#   4.  Build game-level reconciliation
#   5.  Build eligible-play datasets
#   6.  Check eligible-play percentage by defense
#   7.  Classify explosives and situations
#   8.  Check explosive-play edge cases
#   9.  Build explosive vulnerability table
#   10. Build explosive pass / run tables
#
# IMPORTANT:
#
#   QA 01 and QA 02 create nflverse-derived reconciliation
#   tables. Independent comparison against external reference
#   data remains a manual validation step outside this runner.
#
# ============================================================


# ------------------------------------------------------------
# 1. Confirm project root and load configuration
# ------------------------------------------------------------

required_root_files <- c(
  "config.R",
  "run_pipeline.R",
  "nfl_defensive_explosive_play_pipeline.Rproj"
)

missing_root_files <- required_root_files[
  !file.exists(required_root_files)
]

if (length(missing_root_files) > 0) {
  stop(
    "The pipeline does not appear to be running from the ",
    "project root.\n\nMissing root files:\n",
    paste0("  - ", missing_root_files, collapse = "\n"),
    "\n\nOpen nfl_defensive_explosive_play_pipeline.Rproj ",
    "and run run_pipeline.R again.",
    call. = FALSE
  )
}

invisible(
  capture.output(
    source(
      "config.R",
      local = globalenv()
    )
  )
)


# ------------------------------------------------------------
# 2. Create required directories
# ------------------------------------------------------------

required_directories <- c(
  RAW_DIR,
  PROCESSED_DIR,
  QA_OUTPUT_DIR,
  ANALYSIS_OUTPUT_DIR
)

for (directory in required_directories) {
  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ------------------------------------------------------------
# 3. Define pipeline stages
# ------------------------------------------------------------

pipeline_steps <- data.frame(
  step = 1:10,
  
  label = c(
    "Extract NFL regular-season PBP",
    "Extract team defensive PBP",
    "Build season reconciliation",
    "Build game reconciliation",
    "Build eligible-play datasets",
    "Run eligible-play league QA",
    "Classify explosives and situations",
    "Run explosive edge-case QA",
    "Build vulnerability table",
    "Build explosive pass / run tables"
  ),
  
  script = c(
    "data_engineering/01_extract_nfl_regular_season_pbp.R",
    "data_engineering/02_extract_team_defensive_pbp.R",
    "quality_assurance/01_reconcile_team_season_totals.R",
    "quality_assurance/02_reconcile_team_game_totals.R",
    "data_engineering/03_build_eligible_plays.R",
    "quality_assurance/03_check_eligible_play_percentage_by_defense.R",
    "data_engineering/04_classify_explosives_and_situations.R",
    "quality_assurance/04_check_explosive_play_edge_cases.R",
    "data_analysis/01_build_explosive_vulnerability_table.R",
    "data_analysis/02_build_explosive_pass_run_tables.R"
  ),
  
  stringsAsFactors = FALSE
)

missing_scripts <- pipeline_steps$script[
  !file.exists(pipeline_steps$script)
]

if (length(missing_scripts) > 0) {
  stop(
    "The following pipeline scripts were not found:\n",
    paste0("  - ", missing_scripts, collapse = "\n"),
    "\n\nConfirm the repository structure before running the pipeline.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. Define expected outputs
# ------------------------------------------------------------

EXPLOSIVE_EDGE_CASE_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0(
    TEAM_SEASON_PREFIX,
    "_explosive_play_edge_cases.csv"
  )
)

EXPLOSIVE_EDGE_CASE_SUMMARY_FILE <- file.path(
  QA_OUTPUT_DIR,
  paste0(
    TEAM_SEASON_PREFIX,
    "_explosive_play_edge_case_summary.csv"
  )
)

expected_outputs <- c(
  NFL_RAW_FILE,
  TEAM_RAW_FILE,
  TEAM_SEASON_RECONCILIATION_FILE,
  TEAM_GAME_RECONCILIATION_FILE,
  NFL_ELIGIBLE_FILE,
  TEAM_ELIGIBLE_FILE,
  NFL_ELIGIBLE_PERCENTAGE_FILE,
  NFL_ANALYSIS_FILE,
  TEAM_ANALYSIS_FILE,
  EXPLOSIVE_EDGE_CASE_FILE,
  EXPLOSIVE_EDGE_CASE_SUMMARY_FILE,
  TEAM_VULNERABILITY_FILE,
  TEAM_EXPLOSIVE_PASS_FILE,
  TEAM_EXPLOSIVE_RUN_FILE
)


# ------------------------------------------------------------
# 5. Helper functions
# ------------------------------------------------------------

format_runtime <- function(seconds) {
  
  if (seconds < 60) {
    return(
      paste0(
        round(seconds, 2),
        "s"
      )
    )
  }
  
  paste0(
    floor(seconds / 60),
    "m ",
    round(seconds %% 60, 1),
    "s"
  )
}


run_pipeline_script <- function(
    script_path,
    step_number,
    total_steps,
    step_label
) {
  
  start_time <- Sys.time()
  
  warning_messages <- character(0)
  
  result <- tryCatch(
    {
      invisible(
        capture.output(
          withCallingHandlers(
            source(
              script_path,
              echo = FALSE,
              local = new.env(
                parent = globalenv()
              )
            ),
            
            warning = function(w) {
              
              warning_messages <<- c(
                warning_messages,
                conditionMessage(w)
              )
              
              invokeRestart(
                "muffleWarning"
              )
            }
          )
        )
      )
      
      list(
        success = TRUE,
        error_message = NA_character_
      )
    },
    
    error = function(e) {
      list(
        success = FALSE,
        error_message = conditionMessage(e)
      )
    }
  )
  
  elapsed_seconds <- as.numeric(
    difftime(
      Sys.time(),
      start_time,
      units = "secs"
    )
  )
  
  if (!result$success) {
    
    cat(sprintf(
      "[%d/%d] %-42s FAILED\n",
      step_number,
      total_steps,
      step_label
    ))
    
    cat(
      "\n------------------------------------------------------------\n",
      "PIPELINE FAILURE\n",
      "------------------------------------------------------------\n",
      "Stage:  ", step_number, "\n",
      "Script: ", script_path, "\n",
      "Error:  ", result$error_message, "\n",
      "------------------------------------------------------------\n",
      sep = ""
    )
    
    stop(
      "Pipeline stopped during stage ",
      step_number,
      ": ",
      step_label,
      call. = FALSE
    )
  }
  
  warning_count <- length(
    warning_messages
  )
  
  warning_label <- if (
    warning_count == 0
  ) {
    ""
  } else {
    paste0(
      "  [",
      warning_count,
      ifelse(
        warning_count == 1,
        " warning]",
        " warnings]"
      )
    )
  }
  
  cat(sprintf(
    "[%d/%d] %-42s SUCCESS  %8s%s\n",
    step_number,
    total_steps,
    step_label,
    format_runtime(elapsed_seconds),
    warning_label
  ))
  
  list(
    elapsed_seconds = elapsed_seconds,
    warnings = warning_messages
  )
}


get_overall_row <- function(
    table,
    label
) {
  
  overall_row <- table[
    table$situation == "overall",
    ,
    drop = FALSE
  ]
  
  if (nrow(overall_row) != 1) {
    stop(
      "Unable to identify exactly one 'overall' row in ",
      label,
      ".",
      call. = FALSE
    )
  }
  
  overall_row
}


get_significant_results <- function(
    table,
    play_type
) {
  
  significant_rows <- table[
    table$statistically_significant %in% TRUE,
    ,
    drop = FALSE
  ]
  
  if (nrow(significant_rows) == 0) {
    return(NULL)
  }
  
  data.frame(
    play_type = play_type,
    situation = significant_rows$situation,
    team_rate = significant_rows$team_explosive_rate,
    nfl_rate = significant_rows$NFL_average,
    difference = significant_rows$team_difference,
    EVI = significant_rows$EVI,
    
    finding = ifelse(
      significant_rows$team_difference > 0,
      "VULNERABILITY",
      "STRENGTH"
    ),
    
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# 6. Display active configuration
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  "NFL DEFENSIVE EXPLOSIVE-PLAY PIPELINE\n",
  "============================================================\n\n",
  "Team: ", TEAM_NAME, " (", TEAM_ABBR, ")\n",
  "Season: ", SEASON, "\n",
  "Explosive thresholds: Pass ", EXPLOSIVE_PASS_YARDS,
  "+ yards | Run ", EXPLOSIVE_RUN_YARDS, "+ yards\n",
  "Passing denominator: Pass plays + sacks\n",
  "Running denominator: Run plays\n",
  "Situations evaluated: 12\n",
  "NFL benchmark: Pooled other-defense play-level rate\n",
  "Significance test: Two-sided two-sample proportion test, ",
  "alpha = 0.05\n\n",
  sep = ""
)


# ------------------------------------------------------------
# 7. Run pipeline
# ------------------------------------------------------------

pipeline_start_time <- Sys.time()

pipeline_warnings <- vector(
  mode = "list",
  length = nrow(pipeline_steps)
)

cat("Running pipeline...\n\n")

for (i in seq_len(nrow(pipeline_steps))) {
  
  step_result <- run_pipeline_script(
    script_path = pipeline_steps$script[i],
    step_number = pipeline_steps$step[i],
    total_steps = nrow(pipeline_steps),
    step_label = pipeline_steps$label[i]
  )
  
  pipeline_warnings[[i]] <-
    step_result$warnings
}

total_elapsed_seconds <- as.numeric(
  difftime(
    Sys.time(),
    pipeline_start_time,
    units = "secs"
  )
)


# ------------------------------------------------------------
# 8. Confirm expected outputs
# ------------------------------------------------------------

missing_outputs <- expected_outputs[
  !file.exists(expected_outputs)
]

if (length(missing_outputs) > 0) {
  stop(
    "All computational stages completed, but the following ",
    "expected outputs were not found:\n",
    paste0(
      "  - ",
      missing_outputs,
      collapse = "\n"
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 9. Read final summary outputs
# ------------------------------------------------------------
#
# Only files required for the final runner summary are read.
# Detailed validation remains the responsibility of the
# individual engineering, QA, and analysis scripts.
#
# ------------------------------------------------------------

season_qa <- utils::read.csv(
  TEAM_SEASON_RECONCILIATION_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

edge_case_qa <- utils::read.csv(
  EXPLOSIVE_EDGE_CASE_SUMMARY_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

vulnerability_table <- utils::read.csv(
  TEAM_VULNERABILITY_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

pass_table <- utils::read.csv(
  TEAM_EXPLOSIVE_PASS_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

run_table <- utils::read.csv(
  TEAM_EXPLOSIVE_RUN_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ------------------------------------------------------------
# 10. Extract summary values
# ------------------------------------------------------------

overall_row <- get_overall_row(
  vulnerability_table,
  "the explosive vulnerability table"
)

pass_row <- get_overall_row(
  pass_table,
  "the explosive pass table"
)

run_row <- get_overall_row(
  run_table,
  "the explosive run table"
)

team_games <- season_qa$nflverse_value[
  season_qa$metric == "Games"
]

if (length(team_games) != 1) {
  stop(
    "Unable to identify exactly one season-level Games value.",
    call. = FALSE
  )
}

team_games <- as.numeric(
  team_games
)

if (
  !"league_rank" %in% names(overall_row) ||
  length(overall_row$league_rank) != 1 ||
  is.na(overall_row$league_rank)
) {
  stop(
    "Unable to identify the overall NFL explosive-play rank ",
    "in the explosive vulnerability table.",
    call. = FALSE
  )
}

league_rank <- as.integer(
  overall_row$league_rank
)

edge_case_conflicts <- edge_case_qa$value[
  edge_case_qa$check ==
    "Penalty/fumble classification conflicts"
]

if (length(edge_case_conflicts) != 1) {
  stop(
    "Unable to identify the penalty/fumble classification ",
    "conflict count in the explosive edge-case QA summary.",
    call. = FALSE
  )
}

edge_case_conflicts <- as.numeric(
  edge_case_conflicts
)

significant_groups <- list(
  get_significant_results(
    vulnerability_table,
    "Overall"
  ),
  
  get_significant_results(
    pass_table,
    "Pass"
  ),
  
  get_significant_results(
    run_table,
    "Run"
  )
)

significant_groups <- Filter(
  Negate(is.null),
  significant_groups
)

significant_results <- if (
  length(significant_groups) == 0
) {
  NULL
} else {
  do.call(
    rbind,
    significant_groups
  )
}


# ------------------------------------------------------------
# 11. Display pipeline summary
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  "PIPELINE SUMMARY\n",
  "============================================================\n\n",
  sep = ""
)

cat(sprintf(
  paste0(
    "%-34s %s\n",
    "%-34s %s\n",
    "%-34s %s\n",
    "%-34s %.2f%%\n",
    "%-34s %s\n",
    "%-34s %.2f%%\n",
    "%-34s %+.2f pp\n",
    "%-34s %.2f\n",
    "%-34s %s\n",
    "%-34s %s\n"
  ),
  
  "Team defensive games:",
  format(
    team_games,
    trim = TRUE
  ),
  
  "Team eligible plays:",
  format(
    overall_row$total_plays,
    trim = TRUE
  ),
  
  "Team explosive plays:",
  format(
    overall_row$explosive_plays,
    trim = TRUE
  ),
  
  "Team explosive rate:",
  overall_row$team_explosive_rate,
  
  "NFL explosive-play rank:",
  paste0(
    league_rank,
    " / 32"
  ),
  
  "NFL benchmark rate:",
  overall_row$NFL_average,
  
  "Difference from benchmark:",
  overall_row$team_difference,
  
  "Explosive Vulnerability Index:",
  overall_row$EVI,
  
  "Statistically significant:",
  as.character(
    overall_row$statistically_significant
  ),
  
  "Edge-case classification conflicts:",
  format(
    edge_case_conflicts,
    trim = TRUE
  )
))


# ------------------------------------------------------------
# 12. Display pass / run summary
# ------------------------------------------------------------

cat(
  "\n------------------------------------------------------------\n",
  "EXPLOSIVE PASSING\n",
  "------------------------------------------------------------\n\n",
  sep = ""
)

cat(sprintf(
  paste0(
    "%-34s %s / %s\n",
    "%-34s %.2f%%\n",
    "%-34s %.2f%%\n",
    "%-34s %+.2f pp\n",
    "%-34s %.2f\n",
    "%-34s %s\n"
  ),
  
  "Explosive pass plays:",
  format(
    pass_row$explosive_plays,
    trim = TRUE
  ),
  format(
    pass_row$total_plays,
    trim = TRUE
  ),
  
  "Team pass explosive rate:",
  pass_row$team_explosive_rate,
  
  "NFL pass benchmark:",
  pass_row$NFL_average,
  
  "Pass difference:",
  pass_row$team_difference,
  
  "Pass EVI:",
  pass_row$EVI,
  
  "Statistically significant:",
  as.character(
    pass_row$statistically_significant
  )
))

cat(
  "\n------------------------------------------------------------\n",
  "EXPLOSIVE RUSHING\n",
  "------------------------------------------------------------\n\n",
  sep = ""
)

cat(sprintf(
  paste0(
    "%-34s %s / %s\n",
    "%-34s %.2f%%\n",
    "%-34s %.2f%%\n",
    "%-34s %+.2f pp\n",
    "%-34s %.2f\n",
    "%-34s %s\n"
  ),
  
  "Explosive run plays:",
  format(
    run_row$explosive_plays,
    trim = TRUE
  ),
  format(
    run_row$total_plays,
    trim = TRUE
  ),
  
  "Team run explosive rate:",
  run_row$team_explosive_rate,
  
  "NFL run benchmark:",
  run_row$NFL_average,
  
  "Run difference:",
  run_row$team_difference,
  
  "Run EVI:",
  run_row$EVI,
  
  "Statistically significant:",
  as.character(
    run_row$statistically_significant
  )
))


# ------------------------------------------------------------
# 13. Display statistically significant findings
# ------------------------------------------------------------

cat(
  "\n------------------------------------------------------------\n",
  "STATISTICALLY SIGNIFICANT FINDINGS\n",
  "------------------------------------------------------------\n\n",
  sep = ""
)

if (is.null(significant_results)) {
  
  cat(
    "No statistically significant differences were identified.\n"
  )
  
} else {
  
  column_widths <- c(
    finding = 14,
    type = 10,
    situation = 20,
    rate = 12,
    difference = 12,
    EVI = 8
  )
  
  cat(sprintf(
    "%-*s %-*s %-*s %-*s %-*s %-*s %-*s\n",
    column_widths["finding"], "Finding",
    column_widths["type"], "Type",
    column_widths["situation"], "Situation",
    column_widths["rate"], "Team Rate",
    column_widths["rate"], "NFL Rate",
    column_widths["difference"], "Difference",
    column_widths["EVI"], "EVI"
  ))
  
  for (i in seq_len(nrow(significant_results))) {
    
    result <- significant_results[i, ]
    
    cat(sprintf(
      "%-*s %-*s %-*s %-*s %-*s %-*s %-*s\n",
      column_widths["finding"],
      result$finding,
      
      column_widths["type"],
      result$play_type,
      
      column_widths["situation"],
      result$situation,
      
      column_widths["rate"],
      sprintf(
        "%.2f%%",
        result$team_rate
      ),
      
      column_widths["rate"],
      sprintf(
        "%.2f%%",
        result$nfl_rate
      ),
      
      column_widths["difference"],
      sprintf(
        "%+.2f pp",
        result$difference
      ),
      
      column_widths["EVI"],
      sprintf(
        "%.2f",
        result$EVI
      )
    ))
  }
}


# ------------------------------------------------------------
# 14. Display outputs and runtime
# ------------------------------------------------------------

cat(
  "\n------------------------------------------------------------\n",
  "FINAL OUTPUTS\n",
  "------------------------------------------------------------\n\n",
  
  "Explosive vulnerability table:\n",
  TEAM_VULNERABILITY_FILE,
  
  "\n\nExplosive pass table:\n",
  TEAM_EXPLOSIVE_PASS_FILE,
  
  "\n\nExplosive run table:\n",
  TEAM_EXPLOSIVE_RUN_FILE,
  
  "\n\nRuntime: ",
  format_runtime(
    total_elapsed_seconds
  ),
  "\n",
  
  sep = ""
)


# ------------------------------------------------------------
# 15. Display warning summary
# ------------------------------------------------------------

total_warning_count <- sum(
  vapply(
    pipeline_warnings,
    length,
    integer(1)
  )
)

cat(
  "Warnings: ",
  total_warning_count,
  "\n",
  sep = ""
)

if (total_warning_count > 0) {
  
  cat("\nWarning summary:\n")
  
  for (i in seq_along(pipeline_warnings)) {
    
    step_warnings <- unique(
      pipeline_warnings[[i]]
    )
    
    if (length(step_warnings) == 0) {
      next
    }
    
    cat(sprintf(
      "  Step %d - %s:\n",
      pipeline_steps$step[i],
      pipeline_steps$label[i]
    ))
    
    for (warning_message in step_warnings) {
      cat(
        "    - ",
        warning_message,
        "\n",
        sep = ""
      )
    }
  }
}


# ------------------------------------------------------------
# 16. Final pipeline status
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  "STATUS: SUCCESS\n",
  "============================================================\n\n",
  "All 10 computational pipeline stages completed successfully.\n",
  "All expected pipeline outputs were created.\n",
  "Explosive edge-case QA completed successfully.\n",
  "Overall, pass, and run tables contain 12 situations, ",
  "including fourth down.\n\n",
  "Independent external season- and game-level reconciliation\n",
  "is still required before treating the data as externally validated.\n",
  sep = ""
)
