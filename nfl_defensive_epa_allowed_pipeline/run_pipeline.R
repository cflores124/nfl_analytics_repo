# ============================================================
# Run Full NFL Defensive EPA Allowed Analysis Pipeline
# ============================================================
#
# Executes the complete Defensive EPA Allowed pipeline from
# data extraction through QA and analysis.
#
# Primary reporting:
#   EPA Allowed per 100 Plays
#   EPA Allowed per Game
#   Mean NFL EPA Allowed per Game
#   League-Relative EPA Allowed per Game
#   Field-Goal / Touchdown Equivalents per Game
#
# Situational comparison:
#   EPA Difference per 100 Plays =
#     (Team EPA/Play - NFL EPA/Play) * 100
#
# Lower EPA Allowed = better defensive performance.
# ============================================================


# ------------------------------------------------------------
# 1. Confirm project root and load configuration
# ------------------------------------------------------------

required_root_files <- c(
  "config.R",
  "run_pipeline.R",
  "nfl_defensive_epa_allowed_pipeline.Rproj"
)

missing_root_files <- required_root_files[
  !file.exists(required_root_files)
]

if (length(missing_root_files) > 0) {
  stop(
    paste0(
      "The pipeline does not appear to be running from the project root.\n\n",
      "Missing root files:\n",
      paste0("  - ", missing_root_files, collapse = "\n"),
      "\n\nOpen nfl_defensive_epa_allowed_pipeline.Rproj ",
      "and run run_pipeline.R again."
    ),
    call. = FALSE
  )
}

invisible(
  capture.output(
    source("config.R", local = globalenv())
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

invisible(
  lapply(
    required_directories,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)


# ------------------------------------------------------------
# 3. Define pipeline stages
# ------------------------------------------------------------

pipeline_steps <- data.frame(
  step = 1:8,
  label = c(
    "Extract NFL regular-season PBP",
    "Extract team defensive PBP",
    "Build EPA-eligible play datasets",
    "Validate EPA source and coverage",
    "Validate EPA-eligible population",
    "Classify EPA situations",
    "Build EPA Allowed table",
    "Build EPA Allowed pass / run tables"
  ),
  script = c(
    "data_engineering/01_extract_nfl_regular_season_pbp.R",
    "data_engineering/02_extract_team_defensive_pbp.R",
    "data_engineering/03_build_epa_eligible_plays.R",
    "quality_assurance/01_validate_epa_source_and_coverage.R",
    "quality_assurance/02_validate_epa_eligible_population.R",
    "data_engineering/04_classify_epa_situations.R",
    "data_analysis/01_build_epa_allowed_table.R",
    "data_analysis/02_build_epa_pass_run_tables.R"
  ),
  stringsAsFactors = FALSE
)

missing_scripts <- pipeline_steps$script[
  !file.exists(pipeline_steps$script)
]

if (length(missing_scripts) > 0) {
  stop(
    paste0(
      "The following pipeline scripts were not found:\n",
      paste0("  - ", missing_scripts, collapse = "\n"),
      "\n\nConfirm the repository structure before running the pipeline."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. Define expected outputs
# ------------------------------------------------------------

expected_outputs <- c(
  NFL_RAW_FILE,
  TEAM_RAW_FILE,
  NFL_ELIGIBLE_FILE,
  TEAM_ELIGIBLE_FILE,
  EPA_SOURCE_COVERAGE_FILE,
  EPA_ELIGIBLE_POPULATION_FILE,
  NFL_ANALYSIS_FILE,
  TEAM_ANALYSIS_FILE,
  TEAM_EPA_ALLOWED_FILE,
  TEAM_EPA_PASS_FILE,
  TEAM_EPA_RUN_FILE
)


# ------------------------------------------------------------
# 5. Define pipeline helpers
# ------------------------------------------------------------

format_runtime <- function(seconds) {
  
  if (seconds < 60) {
    return(paste0(round(seconds, 2), "s"))
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
              local = new.env(parent = globalenv())
            ),
            warning = function(w) {
              warning_messages <<- c(
                warning_messages,
                conditionMessage(w)
              )
              invokeRestart("muffleWarning")
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
    difftime(Sys.time(), start_time, units = "secs")
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
      "------------------------------------------------------------\n\n",
      "Stage:  ", step_number, "\n",
      "Script: ", script_path, "\n",
      "Error:  ", result$error_message, "\n\n",
      sep = ""
    )
    
    stop(
      paste0(
        "Pipeline stopped during stage ",
        step_number,
        ": ",
        step_label
      ),
      call. = FALSE
    )
  }
  
  warning_count <- length(warning_messages)
  warning_label <- ""
  
  if (warning_count > 0) {
    warning_label <- paste0(
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


get_overall_row <- function(table, label) {
  
  if (!"situation" %in% names(table)) {
    stop(
      label,
      " is missing the situation column.",
      call. = FALSE
    )
  }
  
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


build_significant_results <- function(table, play_type, label) {
  
  required_columns <- c(
    "situation",
    "epa_allowed_per_play",
    "nfl_epa_allowed_per_play",
    "epa_difference_per_100",
    "statistically_significant"
  )
  
  missing_columns <- setdiff(required_columns, names(table))
  
  if (length(missing_columns) > 0) {
    stop(
      label,
      " is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (
    any(is.na(table$statistically_significant)) ||
    !all(table$statistically_significant %in% c(TRUE, FALSE))
  ) {
    stop(
      label,
      " contains invalid statistically_significant values.",
      call. = FALSE
    )
  }
  
  significant <- table[
    table$statistically_significant,
    ,
    drop = FALSE
  ]
  
  if (nrow(significant) == 0) {
    return(NULL)
  }
  
  data.frame(
    finding = ifelse(
      significant$epa_difference_per_100 < 0,
      "Strength",
      "Weakness"
    ),
    play_type = play_type,
    situation = significant$situation,
    team_epa = significant$epa_allowed_per_play,
    nfl_epa = significant$nfl_epa_allowed_per_play,
    difference_per_100 = significant$epa_difference_per_100,
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# 6. Display active configuration
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  "NFL DEFENSIVE EPA ALLOWED PIPELINE\n",
  "============================================================\n\n",
  "Team: ", TEAM_NAME, " (", TEAM_ABBR, ")\n",
  "Season: ", SEASON, "\n",
  "Performance metric: EPA Allowed\n",
  "Situational comparison: EPA Difference per 100 Plays\n",
  "EPA perspective: Offense\n",
  "Better performance: Lower EPA Allowed\n",
  "Passing population: Pass plays + sacks\n",
  "Running population: Run plays\n",
  "NFL benchmark: Pooled other-defense play-level EPA\n\n",
  sep = ""
)


# ------------------------------------------------------------
# 7. Run pipeline
# ------------------------------------------------------------

pipeline_start_time <- Sys.time()
pipeline_warnings <- vector("list", nrow(pipeline_steps))

cat("Running pipeline...\n\n")

for (i in seq_len(nrow(pipeline_steps))) {
  
  step_result <- run_pipeline_script(
    script_path = pipeline_steps$script[i],
    step_number = pipeline_steps$step[i],
    total_steps = nrow(pipeline_steps),
    step_label = pipeline_steps$label[i]
  )
  
  pipeline_warnings[[i]] <- step_result$warnings
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
    paste0(
      "All pipeline stages completed, but the following ",
      "expected outputs were not found:\n",
      paste0("  - ", missing_outputs, collapse = "\n")
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 9. Read final analysis outputs
# ------------------------------------------------------------

epa_table <- utils::read.csv(
  TEAM_EPA_ALLOWED_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

pass_table <- utils::read.csv(
  TEAM_EPA_PASS_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

run_table <- utils::read.csv(
  TEAM_EPA_RUN_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

overall_row <- get_overall_row(
  epa_table,
  "the EPA Allowed table"
)

pass_row <- get_overall_row(
  pass_table,
  "the EPA Allowed pass table"
)

run_row <- get_overall_row(
  run_table,
  "the EPA Allowed run table"
)


# ------------------------------------------------------------
# 10. Validate summary fields
# ------------------------------------------------------------
#
# The runner consumes final analytical outputs. It does not
# recalculate metrics owned by the analysis scripts.
#
# ------------------------------------------------------------

overall_summary_columns <- c(
  "epa_allowed_per_play",
  "nfl_epa_allowed_per_play",
  "epa_difference_per_100",
  "league_rank",
  "statistically_significant",
  "epa_allowed_per_game",
  "mean_nfl_epa_allowed_per_game",
  "league_relative_epa_allowed_per_game",
  "field_goal_equivalent_per_game",
  "touchdown_equivalent_per_game"
)

play_type_summary_columns <- c(
  "eligible_plays",
  "total_epa",
  "epa_allowed_per_play",
  "nfl_epa_allowed_per_play",
  "epa_difference_per_100",
  "statistically_significant"
)

summary_tables <- list(
  "EPA Allowed table" = list(
    table = overall_row,
    required = overall_summary_columns
  ),
  "EPA Allowed pass table" = list(
    table = pass_row,
    required = play_type_summary_columns
  ),
  "EPA Allowed run table" = list(
    table = run_row,
    required = play_type_summary_columns
  )
)

for (label in names(summary_tables)) {
  
  summary_table <- summary_tables[[label]]$table
  required_columns <- summary_tables[[label]]$required
  
  missing_columns <- setdiff(
    required_columns,
    names(summary_table)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      label,
      " is missing summary column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}


# ------------------------------------------------------------
# 11. Build statistically significant findings
# ------------------------------------------------------------

significant_tables <- list(
  build_significant_results(
    epa_table,
    "Overall",
    "the EPA Allowed table"
  ),
  build_significant_results(
    pass_table,
    "Pass",
    "the EPA Allowed pass table"
  ),
  build_significant_results(
    run_table,
    "Run",
    "the EPA Allowed run table"
  )
)

significant_tables <- Filter(
  Negate(is.null),
  significant_tables
)

significant_results <- if (length(significant_tables) == 0) {
  NULL
} else {
  do.call(rbind, significant_tables)
}


# ------------------------------------------------------------
# 12. Display overall EPA Allowed summary
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  "PIPELINE SUMMARY\n",
  "============================================================\n\n",
  
  "OVERALL EPA ALLOWED\n",
  "------------------------------------------------------------\n\n",
  sep = ""
)

cat(sprintf(
  paste0(
    "%-42s %+.4f\n",
    "%-42s %+.4f\n",
    "%-42s %+.2f\n",
    "%-42s %s\n",
    "%-42s %s\n"
  ),
  "Team EPA Allowed per Play:",
  overall_row$epa_allowed_per_play,
  
  "NFL Benchmark EPA per Play:",
  overall_row$nfl_epa_allowed_per_play,
  
  "Team EPA Difference per 100 Plays:",
  overall_row$epa_difference_per_100,
  
  "League Rank:",
  format(overall_row$league_rank, trim = TRUE),
  
  "Statistically Significant:",
  as.character(overall_row$statistically_significant)
))


# ------------------------------------------------------------
# 13. Display pass / run EPA Allowed summaries
# ------------------------------------------------------------

display_play_type_summary <- function(
    title,
    opportunity_label,
    row
) {
  
  cat(
    "\n------------------------------------------------------------\n",
    title,
    "\n------------------------------------------------------------\n\n",
    sep = ""
  )
  
  cat(sprintf(
    paste0(
      "%-42s %s\n",
      "%-42s %+.3f\n",
      "%-42s %+.4f\n",
      "%-42s %+.4f\n",
      "%-42s %+.2f\n",
      "%-42s %s\n"
    ),
    opportunity_label,
    format(row$eligible_plays, trim = TRUE),
    
    "Team Total EPA:",
    row$total_epa,
    
    "Team EPA Allowed per Play:",
    row$epa_allowed_per_play,
    
    "NFL Benchmark EPA per Play:",
    row$nfl_epa_allowed_per_play,
    
    "Team EPA Difference per 100 Plays:",
    row$epa_difference_per_100,
    
    "Statistically Significant:",
    as.character(row$statistically_significant)
  ))
}

display_play_type_summary(
  "PASS EPA ALLOWED",
  "Passing Opportunities:",
  pass_row
)

display_play_type_summary(
  "RUN EPA ALLOWED",
  "Running Opportunities:",
  run_row
)


# ------------------------------------------------------------
# 14. Display per-game EPA summary
# ------------------------------------------------------------

cat(
  "\n------------------------------------------------------------\n",
  "PER-GAME EPA\n",
  "------------------------------------------------------------\n\n",
  sep = ""
)

cat(sprintf(
  paste0(
    "%-42s %+.2f\n",
    "%-42s %+.2f\n",
    "%-42s %+.2f\n",
    "%-42s %+.2f\n",
    "%-42s %+.2f\n"
  ),
  "Team EPA Allowed per Game:",
  overall_row$epa_allowed_per_game,
  
  "Mean NFL EPA Allowed per Game:",
  overall_row$mean_nfl_epa_allowed_per_game,
  
  "League-Relative EPA Allowed per Game:",
  overall_row$league_relative_epa_allowed_per_game,
  
  "Team Field-Goal Equivalent per Game:",
  overall_row$field_goal_equivalent_per_game,
  
  "Team Touchdown Equivalent per Game:",
  overall_row$touchdown_equivalent_per_game
))


# ------------------------------------------------------------
# 15. Display statistically significant findings
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
    epa = 12,
    difference = 16
  )
  
  cat(sprintf(
    "%-*s %-*s %-*s %-*s %-*s %-*s\n",
    column_widths["finding"], "Finding",
    column_widths["type"], "Type",
    column_widths["situation"], "Situation",
    column_widths["epa"], "Team EPA",
    column_widths["epa"], "NFL EPA",
    column_widths["difference"], "Diff / 100"
  ))
  
  for (i in seq_len(nrow(significant_results))) {
    
    result <- significant_results[i, ]
    
    cat(sprintf(
      "%-*s %-*s %-*s %-*s %-*s %-*s\n",
      column_widths["finding"],
      result$finding,
      
      column_widths["type"],
      result$play_type,
      
      column_widths["situation"],
      result$situation,
      
      column_widths["epa"],
      sprintf("%+.4f", result$team_epa),
      
      column_widths["epa"],
      sprintf("%+.4f", result$nfl_epa),
      
      column_widths["difference"],
      sprintf("%+.2f", result$difference_per_100)
    ))
  }
}


# ------------------------------------------------------------
# 16. Display outputs, runtime, and warnings
# ------------------------------------------------------------

cat(
  "\n------------------------------------------------------------\n",
  "FINAL OUTPUTS\n",
  "------------------------------------------------------------\n\n",
  "EPA Allowed table:\n", TEAM_EPA_ALLOWED_FILE,
  "\n\nEPA Allowed pass table:\n", TEAM_EPA_PASS_FILE,
  "\n\nEPA Allowed run table:\n", TEAM_EPA_RUN_FILE,
  "\n\nRuntime: ", format_runtime(total_elapsed_seconds), "\n",
  sep = ""
)

total_warning_count <- sum(
  vapply(pipeline_warnings, length, integer(1))
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
    
    step_warnings <- unique(pipeline_warnings[[i]])
    
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
# 17. Final pipeline status
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  "STATUS: SUCCESS\n",
  "============================================================\n\n",
  "All pipeline stages completed successfully.\n",
  "All expected pipeline outputs were created.\n",
  sep = ""
)
