# ============================================================
# 02 - Build EPA Allowed Pass and Run Tables
# ============================================================
#
# Builds separate pass and run EPA Allowed tables across
# 12 standardized situations.
#
# Populations:
#   Pass = pass plays + sacks
#   Run  = run plays
#
# Metric:
#   EPA Allowed per Play = total offensive EPA / eligible plays
#
# Comparison:
#   EPA Difference per 100 Plays =
#     (Team EPA/Play - NFL EPA/Play) * 100
#
# Interpretation:
#   Lower EPA Allowed = better defensive performance
#   Negative difference = better than NFL benchmark
#   Positive difference = worse than NFL benchmark
#
# Statistical test:
#   Two-sided Welch two-sample t-test (alpha = 0.05)
#
# Outputs:
#   TEAM_EPA_PASS_FILE
#   TEAM_EPA_RUN_FILE
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

SIGNIFICANCE_LEVEL <- 0.05

required_packages <- c(
  "dplyr",
  "readr",
  "tibble"
)

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
# 2. Load analysis-ready datasets
# ------------------------------------------------------------

required_files <- c(
  TEAM_ANALYSIS_FILE,
  NFL_ANALYSIS_FILE
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing required analysis file(s):\n",
    paste0("  - ", missing_files, collapse = "\n"),
    "\n\nRun data_engineering/04_classify_epa_situations.R first."
  )
}

team_analysis <- readr::read_csv(
  TEAM_ANALYSIS_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

nfl_analysis <- readr::read_csv(
  NFL_ANALYSIS_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

if (
  nrow(team_analysis) == 0 ||
  nrow(nfl_analysis) == 0
) {
  stop(
    "One or more analysis datasets contain zero rows."
  )
}


# ------------------------------------------------------------
# 3. Define standardized situations
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

situations <- tibble::tibble(
  
  situation = c(
    "overall",
    "1st_&_10",
    "2nd_&_long",
    "2nd_&_medium",
    "2nd_&_short",
    "3rd_down",
    "4th_down",
    "leading",
    "trailing",
    "neutral_score",
    "own_territory",
    "opponent_territory"
  ),
  
  situation_column = c(
    NA_character_,
    situation_columns
  )
)


# ------------------------------------------------------------
# 4. Validate analytical inputs
# ------------------------------------------------------------

required_columns <- c(
  "defteam",
  "analysis_play_type",
  "epa",
  situation_columns
)

validate_analysis_data <- function(data, label) {
  
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

validate_analysis_data(
  team_analysis,
  "Team analysis dataset"
)

validate_analysis_data(
  nfl_analysis,
  "NFL analysis dataset"
)


# ------------------------------------------------------------
# 5. Create pooled other-defense benchmark
# ------------------------------------------------------------

nfl_benchmark <- nfl_analysis |>
  dplyr::filter(
    !is.na(defteam),
    defteam != TEAM_ABBR
  )

if (nrow(nfl_benchmark) == 0) {
  stop(
    "NFL benchmark contains zero plays after excluding ",
    TEAM_ABBR,
    "."
  )
}


# ------------------------------------------------------------
# 6. Define pass and run populations
# ------------------------------------------------------------

filter_play_population <- function(
    data,
    play_group
) {
  
  if (play_group == "pass") {
    return(
      data |>
        dplyr::filter(
          analysis_play_type %in%
            c("pass", "sack")
        )
    )
  }
  
  if (play_group == "run") {
    return(
      data |>
        dplyr::filter(
          analysis_play_type == "run"
        )
    )
  }
  
  stop(
    "Unknown play group: ",
    play_group,
    "."
  )
}


# ------------------------------------------------------------
# 7. Define situational filtering helper
# ------------------------------------------------------------

filter_situation <- function(
    data,
    situation_column
) {
  
  if (is.na(situation_column)) {
    return(data)
  }
  
  data[
    !is.na(data[[situation_column]]) &
      data[[situation_column]] == 1L,
    ,
    drop = FALSE
  ]
}


# ------------------------------------------------------------
# 8. Calculate situational EPA Allowed
# ------------------------------------------------------------

calculate_epa_allowed <- function(
    data,
    play_group
) {
  
  play_population <- filter_play_population(
    data,
    play_group
  )
  
  results <- lapply(
    seq_len(nrow(situations)),
    function(i) {
      
      situation_data <- filter_situation(
        play_population,
        situations$situation_column[i]
      )
      
      eligible_plays <- nrow(
        situation_data
      )
      
      total_epa <- sum(
        situation_data$epa
      )
      
      epa_allowed_per_play <- if (
        eligible_plays > 0
      ) {
        total_epa / eligible_plays
      } else {
        NA_real_
      }
      
      tibble::tibble(
        situation = situations$situation[i],
        eligible_plays = eligible_plays,
        total_epa = total_epa,
        epa_allowed_per_play = epa_allowed_per_play
      )
    }
  )
  
  dplyr::bind_rows(
    results
  )
}


# ------------------------------------------------------------
# 9. Define statistical significance test
# ------------------------------------------------------------

test_statistical_significance <- function(
    team_epa,
    nfl_epa
) {
  
  team_epa <- team_epa[
    !is.na(team_epa) &
      is.finite(team_epa)
  ]
  
  nfl_epa <- nfl_epa[
    !is.na(nfl_epa) &
      is.finite(nfl_epa)
  ]
  
  if (
    length(team_epa) < 2 ||
    length(nfl_epa) < 2
  ) {
    return(FALSE)
  }
  
  if (
    stats::var(team_epa) == 0 &&
    stats::var(nfl_epa) == 0
  ) {
    return(FALSE)
  }
  
  test_result <- tryCatch(
    stats::t.test(
      x = team_epa,
      y = nfl_epa,
      alternative = "two.sided",
      var.equal = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(test_result)) {
    return(FALSE)
  }
  
  isTRUE(
    test_result$p.value <
      SIGNIFICANCE_LEVEL
  )
}


# ------------------------------------------------------------
# 10. Calculate situational statistical significance
# ------------------------------------------------------------

calculate_situation_significance <- function(
    team_data,
    benchmark_data,
    play_group
) {
  
  team_population <- filter_play_population(
    team_data,
    play_group
  )
  
  benchmark_population <- filter_play_population(
    benchmark_data,
    play_group
  )
  
  results <- lapply(
    seq_len(nrow(situations)),
    function(i) {
      
      situation_column <-
        situations$situation_column[i]
      
      team_situation <- filter_situation(
        team_population,
        situation_column
      )
      
      benchmark_situation <- filter_situation(
        benchmark_population,
        situation_column
      )
      
      tibble::tibble(
        situation = situations$situation[i],
        
        statistically_significant =
          test_statistical_significance(
            team_situation$epa,
            benchmark_situation$epa
          )
      )
    }
  )
  
  dplyr::bind_rows(
    results
  )
}


# ------------------------------------------------------------
# 11. Build EPA Allowed table
# ------------------------------------------------------------

build_epa_table <- function(
    team_data,
    benchmark_data,
    play_group
) {
  
  team_epa <- calculate_epa_allowed(
    team_data,
    play_group
  )
  
  nfl_epa <- calculate_epa_allowed(
    benchmark_data,
    play_group
  ) |>
    dplyr::rename(
      nfl_eligible_plays = eligible_plays,
      nfl_total_epa = total_epa,
      nfl_epa_allowed_per_play = epa_allowed_per_play
    )
  
  significance_results <-
    calculate_situation_significance(
      team_data,
      benchmark_data,
      play_group
    )
  
  team_epa |>
    
    dplyr::left_join(
      nfl_epa,
      by = "situation"
    ) |>
    
    dplyr::left_join(
      significance_results,
      by = "situation"
    ) |>
    
    dplyr::mutate(
      
      epa_difference_per_100 = (
        epa_allowed_per_play -
          nfl_epa_allowed_per_play
      ) * 100
    )
}


# ------------------------------------------------------------
# 12. Build pass and run tables
# ------------------------------------------------------------

pass_table <- build_epa_table(
  team_analysis,
  nfl_benchmark,
  "pass"
)

run_table <- build_epa_table(
  team_analysis,
  nfl_benchmark,
  "run"
)


# ------------------------------------------------------------
# 13. Validate analytical outputs
# ------------------------------------------------------------

validate_epa_table <- function(
    table,
    label
) {
  
  if (
    nrow(table) != nrow(situations) ||
    !identical(
      table$situation,
      situations$situation
    )
  ) {
    stop(
      label,
      " situations are missing or out of order."
    )
  }
  
  if (
    any(table$eligible_plays <= 0) ||
    any(table$nfl_eligible_plays <= 0)
  ) {
    stop(
      label,
      " contains one or more situations with zero eligible plays."
    )
  }
  
  if (
    !is.logical(
      table$statistically_significant
    ) ||
    any(
      is.na(
        table$statistically_significant
      )
    )
  ) {
    stop(
      label,
      " statistically_significant must contain only TRUE/FALSE values."
    )
  }
  
  if (
    any(
      !is.finite(
        table$epa_difference_per_100
      )
    )
  ) {
    stop(
      label,
      " contains non-finite EPA Difference per 100 values."
    )
  }
}

validate_epa_table(
  pass_table,
  "EPA Allowed pass table"
)

validate_epa_table(
  run_table,
  "EPA Allowed run table"
)


# ------------------------------------------------------------
# 14. Validate overall pass/run decomposition
# ------------------------------------------------------------

overall_pass <- pass_table |>
  dplyr::filter(
    situation == "overall"
  )

overall_run <- run_table |>
  dplyr::filter(
    situation == "overall"
  )

expected_team_plays <- nrow(
  team_analysis
)

expected_team_total_epa <- sum(
  team_analysis$epa
)

reconstructed_team_plays <-
  overall_pass$eligible_plays +
  overall_run$eligible_plays

reconstructed_team_total_epa <-
  overall_pass$total_epa +
  overall_run$total_epa

if (
  reconstructed_team_plays !=
  expected_team_plays
) {
  stop(
    "Pass and run opportunities do not reconstruct ",
    "the team EPA-eligible population. Expected ",
    expected_team_plays,
    "; calculated ",
    reconstructed_team_plays,
    "."
  )
}

if (
  !isTRUE(
    all.equal(
      reconstructed_team_total_epa,
      expected_team_total_epa,
      tolerance = 1e-10
    )
  )
) {
  stop(
    "Pass and run EPA totals do not reconstruct ",
    "the team total EPA."
  )
}


# ------------------------------------------------------------
# 15. Validate overall EPA differences
# ------------------------------------------------------------

validate_overall_difference <- function(
    table,
    label
) {
  
  overall_row <- table |>
    dplyr::filter(
      situation == "overall"
    )
  
  expected_difference_per_100 <- (
    overall_row$epa_allowed_per_play -
      overall_row$nfl_epa_allowed_per_play
  ) * 100
  
  if (
    !isTRUE(
      all.equal(
        overall_row$epa_difference_per_100,
        expected_difference_per_100,
        tolerance = 1e-10
      )
    )
  ) {
    stop(
      label,
      " overall EPA Difference per 100 was calculated incorrectly."
    )
  }
}

validate_overall_difference(
  pass_table,
  "EPA Allowed pass table"
)

validate_overall_difference(
  run_table,
  "EPA Allowed run table"
)


# ------------------------------------------------------------
# 16. Format pass and run tables
# ------------------------------------------------------------

format_epa_table <- function(table) {
  
  table |>
    dplyr::select(
      situation,
      eligible_plays,
      total_epa,
      epa_allowed_per_play,
      nfl_eligible_plays,
      nfl_total_epa,
      nfl_epa_allowed_per_play,
      epa_difference_per_100,
      statistically_significant
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(
          total_epa,
          epa_allowed_per_play,
          nfl_total_epa,
          nfl_epa_allowed_per_play,
          epa_difference_per_100
        ),
        ~ round(.x, 4)
      )
    )
}

final_pass_table <- format_epa_table(
  pass_table
)

final_run_table <- format_epa_table(
  run_table
)


# ------------------------------------------------------------
# 17. Save outputs
# ------------------------------------------------------------

dir.create(
  ANALYSIS_OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  final_pass_table,
  TEAM_EPA_PASS_FILE
)

readr::write_csv(
  final_run_table,
  TEAM_EPA_RUN_FILE
)


# ------------------------------------------------------------
# 18. Display results
# ------------------------------------------------------------

overall_pass <- final_pass_table |>
  dplyr::filter(
    situation == "overall"
  )

overall_run <- final_run_table |>
  dplyr::filter(
    situation == "overall"
  )

cat(
  "\n============================================================\n",
  toupper(TEAM_NAME), " EPA ALLOWED PASS TABLE\n",
  "============================================================\n\n",
  sep = ""
)

print(
  final_pass_table,
  n = nrow(final_pass_table),
  width = Inf
)

cat(
  "\n============================================================\n",
  toupper(TEAM_NAME), " EPA ALLOWED RUN TABLE\n",
  "============================================================\n\n",
  sep = ""
)

print(
  final_run_table,
  n = nrow(final_run_table),
  width = Inf
)

cat(
  "\n============================================================\n",
  "PASS / RUN SUMMARY\n",
  "============================================================\n\n",
  
  "Passing opportunities: ",
  overall_pass$eligible_plays,
  
  "\nPass total EPA: ",
  round(
    overall_pass$total_epa,
    3
  ),
  
  "\nPass EPA Allowed per Play: ",
  round(
    overall_pass$epa_allowed_per_play,
    4
  ),
  
  "\nPass NFL Benchmark EPA Allowed per Play: ",
  round(
    overall_pass$nfl_epa_allowed_per_play,
    4
  ),
  
  "\nPass EPA Difference per 100 Plays: ",
  round(
    overall_pass$epa_difference_per_100,
    2
  ),
  
  "\n\nRunning opportunities: ",
  overall_run$eligible_plays,
  
  "\nRun total EPA: ",
  round(
    overall_run$total_epa,
    3
  ),
  
  "\nRun EPA Allowed per Play: ",
  round(
    overall_run$epa_allowed_per_play,
    4
  ),
  
  "\nRun NFL Benchmark EPA Allowed per Play: ",
  round(
    overall_run$nfl_epa_allowed_per_play,
    4
  ),
  
  "\nRun EPA Difference per 100 Plays: ",
  round(
    overall_run$epa_difference_per_100,
    2
  ),
  
  "\n\nStatistically different passing situations: ",
  sum(
    final_pass_table$statistically_significant
  ),
  " of ",
  nrow(final_pass_table),
  
  "\nSignificant passing strengths: ",
  sum(
    final_pass_table$statistically_significant &
      final_pass_table$epa_difference_per_100 < 0
  ),
  
  "\nSignificant passing weaknesses: ",
  sum(
    final_pass_table$statistically_significant &
      final_pass_table$epa_difference_per_100 > 0
  ),
  
  "\n\nStatistically different running situations: ",
  sum(
    final_run_table$statistically_significant
  ),
  " of ",
  nrow(final_run_table),
  
  "\nSignificant running strengths: ",
  sum(
    final_run_table$statistically_significant &
      final_run_table$epa_difference_per_100 < 0
  ),
  
  "\nSignificant running weaknesses: ",
  sum(
    final_run_table$statistically_significant &
      final_run_table$epa_difference_per_100 > 0
  ),
  
  "\n\nPass output: ",
  TEAM_EPA_PASS_FILE,
  
  "\nRun output: ",
  TEAM_EPA_RUN_FILE,
  "\n",
  
  sep = ""
)
