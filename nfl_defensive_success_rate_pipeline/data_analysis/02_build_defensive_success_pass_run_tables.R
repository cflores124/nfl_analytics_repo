# ============================================================
# 02 - Build Defensive Success Pass and Run Tables
# Reusable NFL Defensive Success Rate Analysis Pipeline
# ============================================================
#
# Purpose:
#
#   Build separate defensive success pass and defensive success
#   run tables for the configured defense across 12 standardized
#   game situations.
#
# Passing denominator:
#
#   analysis_play_type %in% c("pass", "sack")
#
# Passing numerator:
#
#   defensive_success == 1
#
# Sacks remain passing opportunities and are classified as
# defensive successes.
#
# Running denominator:
#
#   analysis_play_type == "run"
#
# Running numerator:
#
#   defensive_success == 1
#
# NFL benchmark:
#
#   Pooled play-level Defensive Success Rate across all other
#   defenses.
#
# Statistical test:
#
#   Two-sided two-sample proportion test
#   H0: Team Defensive Success Rate = NFL benchmark rate
#   HA: Team Defensive Success Rate != NFL benchmark rate
#   Alpha = SIGNIFICANCE_LEVEL
#
# Outputs:
#
#   *_defensive_success_pass_table.csv
#   *_defensive_success_run_table.csv
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
# 2. Load analysis-ready datasets
# ------------------------------------------------------------

required_files <- c(
  TEAM_ANALYSIS_FILE,
  NFL_ANALYSIS_FILE
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required analysis file(s):\n",
    paste0("  - ", missing_files, collapse = "\n"),
    "\n\nRun data_engineering/04_classify_success_and_situations.R first.",
    call. = FALSE
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

if (nrow(team_analysis) == 0 || nrow(nfl_analysis) == 0) {
  stop("One or more analysis datasets contain zero rows.", call. = FALSE)
}


# ------------------------------------------------------------
# 3. Validate required analytical fields
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

required_columns <- c(
  "season",
  "defteam",
  "analysis_play_type",
  "defensive_success",
  situation_columns
)

validate_input <- function(data, label) {
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  seasons <- unique(stats::na.omit(data$season))
  
  if (length(seasons) != 1 || seasons != SEASON) {
    stop(
      label,
      " must contain only season ",
      SEASON,
      ".",
      call. = FALSE
    )
  }
  
  if (
    any(is.na(data$analysis_play_type)) ||
    any(!data$analysis_play_type %in% c("pass", "run", "sack"))
  ) {
    stop(
      label,
      " contains invalid analysis_play_type values.",
      call. = FALSE
    )
  }
  
  binary_columns <- c(
    "defensive_success",
    situation_columns
  )
  
  invalid_binary <- vapply(
    binary_columns,
    function(column) {
      values <- data[[column]]
      any(is.na(values)) || any(!values %in% c(0L, 1L))
    },
    logical(1)
  )
  
  if (any(invalid_binary)) {
    stop(
      label,
      " contains invalid binary field(s): ",
      paste(binary_columns[invalid_binary], collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

validate_input(team_analysis, "Team analysis dataset")
validate_input(nfl_analysis, "NFL analysis dataset")

team_values <- unique(stats::na.omit(team_analysis$defteam))

if (length(team_values) != 1 || team_values != TEAM_ABBR) {
  stop(
    "Team analysis dataset must contain only ",
    TEAM_ABBR,
    " defensive plays.",
    call. = FALSE
  )
}

if (!TEAM_ABBR %in% nfl_analysis$defteam) {
  stop(
    "Configured team ",
    TEAM_ABBR,
    " was not found in the NFL analysis dataset.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. Create pooled other-defense benchmark
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
    ".",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 5. Define situations
# ------------------------------------------------------------

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
# 6. Define pass and run populations
# ------------------------------------------------------------

filter_play_population <- function(data, play_group) {
  
  if (play_group == "pass") {
    return(
      data |>
        dplyr::filter(
          analysis_play_type %in% c("pass", "sack")
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
    ".",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 7. Calculate situational Defensive Success Rates
# ------------------------------------------------------------

calculate_situation_rates <- function(data, play_group) {
  
  play_population <- filter_play_population(
    data,
    play_group
  )
  
  results <- lapply(
    seq_len(nrow(situations)),
    function(i) {
      
      situation_column <- situations$situation_column[i]
      
      situation_data <- if (is.na(situation_column)) {
        play_population
      } else {
        play_population[
          play_population[[situation_column]] == 1L,
          ,
          drop = FALSE
        ]
      }
      
      total_plays <- nrow(situation_data)
      defensive_successes <- sum(situation_data$defensive_success)
      
      tibble::tibble(
        situation = situations$situation[i],
        defensive_successes = defensive_successes,
        total_plays = total_plays,
        success_rate = if (total_plays > 0) {
          defensive_successes / total_plays
        } else {
          NA_real_
        }
      )
    }
  )
  
  dplyr::bind_rows(results)
}


# ------------------------------------------------------------
# 8. Define statistical significance test
# ------------------------------------------------------------

test_statistical_significance <- function(
    team_successes,
    team_plays,
    nfl_successes,
    nfl_plays
) {
  
  if (
    is.na(team_successes) ||
    is.na(team_plays) ||
    is.na(nfl_successes) ||
    is.na(nfl_plays) ||
    team_plays <= 0 ||
    nfl_plays <= 0
  ) {
    return(FALSE)
  }
  
  test_result <- suppressWarnings(
    stats::prop.test(
      x = c(team_successes, nfl_successes),
      n = c(team_plays, nfl_plays),
      alternative = "two.sided",
      correct = FALSE
    )
  )
  
  isTRUE(test_result$p.value < SIGNIFICANCE_LEVEL)
}


# ------------------------------------------------------------
# 9. Build Defensive Success Rate table
# ------------------------------------------------------------

build_success_table <- function(
    team_data,
    benchmark_data,
    play_group
) {
  
  team_rates <- calculate_situation_rates(
    team_data,
    play_group
  ) |>
    dplyr::rename(
      team_success_rate = success_rate
    )
  
  nfl_rates <- calculate_situation_rates(
    benchmark_data,
    play_group
  ) |>
    dplyr::rename(
      nfl_defensive_successes = defensive_successes,
      nfl_total_plays = total_plays,
      NFL_average = success_rate
    )
  
  team_rates |>
    dplyr::left_join(
      nfl_rates,
      by = "situation"
    ) |>
    dplyr::mutate(
      team_difference = team_success_rate - NFL_average,
      
      DSI = dplyr::if_else(
        !is.na(team_success_rate) & team_success_rate > 0,
        NFL_average / team_success_rate,
        NA_real_
      ),
      
      statistically_significant = mapply(
        FUN = test_statistical_significance,
        team_successes = defensive_successes,
        team_plays = total_plays,
        nfl_successes = nfl_defensive_successes,
        nfl_plays = nfl_total_plays
      )
    )
}


# ------------------------------------------------------------
# 10. Build pass and run tables
# ------------------------------------------------------------

pass_table <- build_success_table(
  team_analysis,
  nfl_benchmark,
  "pass"
)

run_table <- build_success_table(
  team_analysis,
  nfl_benchmark,
  "run"
)


# ------------------------------------------------------------
# 11. Validate analytical outputs
# ------------------------------------------------------------

validate_success_table <- function(table, label) {
  
  if (
    nrow(table) != nrow(situations) ||
    !identical(table$situation, situations$situation)
  ) {
    stop(
      label,
      " situations are missing or out of order.",
      call. = FALSE
    )
  }
  
  if (
    any(table$defensive_successes > table$total_plays) ||
    any(table$nfl_defensive_successes > table$nfl_total_plays)
  ) {
    stop(
      label,
      " contains more defensive successes than total plays.",
      call. = FALSE
    )
  }
  
  if (
    any(
      table$team_success_rate < 0 |
      table$team_success_rate > 1,
      na.rm = TRUE
    ) ||
    any(
      table$NFL_average < 0 |
      table$NFL_average > 1,
      na.rm = TRUE
    )
  ) {
    stop(
      label,
      " contains a Defensive Success Rate outside the valid 0-1 range.",
      call. = FALSE
    )
  }
  
  if (
    !is.logical(table$statistically_significant) ||
    any(is.na(table$statistically_significant))
  ) {
    stop(
      label,
      " statistically_significant must contain only TRUE/FALSE values.",
      call. = FALSE
    )
  }
}

validate_success_table(
  pass_table,
  "Defensive success pass table"
)

validate_success_table(
  run_table,
  "Defensive success run table"
)


# ------------------------------------------------------------
# 12. Validate pass/run decomposition
# ------------------------------------------------------------

overall_pass <- pass_table |>
  dplyr::filter(situation == "overall")

overall_run <- run_table |>
  dplyr::filter(situation == "overall")

if (nrow(overall_pass) != 1 || nrow(overall_run) != 1) {
  stop(
    "Unable to identify exactly one overall row in each table.",
    call. = FALSE
  )
}

reconstructed_plays <-
  overall_pass$total_plays +
  overall_run$total_plays

expected_plays <- nrow(team_analysis)

if (reconstructed_plays != expected_plays) {
  stop(
    "Pass and run populations do not reconstruct the team eligible-play ",
    "population. Expected ",
    expected_plays,
    "; calculated ",
    reconstructed_plays,
    ".",
    call. = FALSE
  )
}

reconstructed_successes <-
  overall_pass$defensive_successes +
  overall_run$defensive_successes

expected_successes <- sum(
  team_analysis$defensive_success
)

if (reconstructed_successes != expected_successes) {
  stop(
    "Pass and run defensive successes do not reconstruct the overall ",
    "defensive-success population. Expected ",
    expected_successes,
    "; calculated ",
    reconstructed_successes,
    ".",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 13. Create presentation-ready tables
# ------------------------------------------------------------

format_success_table <- function(table) {
  
  table |>
    dplyr::mutate(
      team_success_rate = round(team_success_rate * 100, 2),
      NFL_average = round(NFL_average * 100, 2),
      team_difference = round(team_difference * 100, 2),
      DSI = round(DSI, 2)
    ) |>
    dplyr::select(
      situation,
      defensive_successes,
      total_plays,
      team_success_rate,
      NFL_average,
      team_difference,
      DSI,
      statistically_significant
    )
}

final_pass_table <- format_success_table(pass_table)
final_run_table <- format_success_table(run_table)


# ------------------------------------------------------------
# 14. Save outputs
# ------------------------------------------------------------

dir.create(
  ANALYSIS_OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  final_pass_table,
  TEAM_SUCCESS_PASS_FILE
)

readr::write_csv(
  final_run_table,
  TEAM_SUCCESS_RUN_FILE
)


# ------------------------------------------------------------
# 15. Display results
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  toupper(TEAM_NAME), " DEFENSIVE SUCCESS PASS TABLE\n",
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
  toupper(TEAM_NAME), " DEFENSIVE SUCCESS RUN TABLE\n",
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
  
  "Eligible plays: ", expected_plays, "\n",
  "Passing opportunities: ", overall_pass$total_plays, "\n",
  "Running opportunities: ", overall_run$total_plays, "\n",
  "Reconstructed eligible plays: ", reconstructed_plays, "\n\n",
  
  "Defensive successes: ", expected_successes, "\n",
  "Pass defensive successes: ", overall_pass$defensive_successes, "\n",
  "Run defensive successes: ", overall_run$defensive_successes, "\n",
  "Reconstructed defensive successes: ", reconstructed_successes, "\n\n",
  
  "Statistically different passing situations: ",
  sum(final_pass_table$statistically_significant),
  " of ", nrow(final_pass_table), "\n",
  
  "Significant passing strengths: ",
  sum(
    final_pass_table$statistically_significant &
      final_pass_table$team_difference > 0
  ), "\n",
  
  "Significant passing weaknesses: ",
  sum(
    final_pass_table$statistically_significant &
      final_pass_table$team_difference < 0
  ), "\n\n",
  
  "Statistically different running situations: ",
  sum(final_run_table$statistically_significant),
  " of ", nrow(final_run_table), "\n",
  
  "Significant running strengths: ",
  sum(
    final_run_table$statistically_significant &
      final_run_table$team_difference > 0
  ), "\n",
  
  "Significant running weaknesses: ",
  sum(
    final_run_table$statistically_significant &
      final_run_table$team_difference < 0
  ), "\n\n",
  
  "Pass output: ", TEAM_SUCCESS_PASS_FILE, "\n",
  "Run output: ", TEAM_SUCCESS_RUN_FILE, "\n",
  
  sep = ""
)
