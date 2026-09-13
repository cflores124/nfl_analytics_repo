# ============================================================
# 02 - Build Explosive Pass and Run Vulnerability Tables
# Reusable NFL Defensive Explosive-Play Analysis Pipeline
# ============================================================
#
# Purpose:
#
#   Build separate explosive pass and explosive run
#   vulnerability tables for the configured defense across
#   12 standardized game situations.
#
# Passing denominator:
#
#   analysis_play_type %in% c("pass", "sack")
#
# Passing numerator:
#
#   analysis_play_type == "pass" &
#   explosive_play == 1
#
#   Sacks remain passing opportunities but cannot be explosive.
#
# Running denominator:
#
#   analysis_play_type == "run"
#
# Running numerator:
#
#   analysis_play_type == "run" &
#   explosive_play == 1
#
# NFL benchmark:
#
#   Pooled play-level explosive rate across all other defenses.
#
# Metrics:
#
#   team_difference =
#     Team Explosive Rate - NFL Benchmark Rate
#
#   EVI =
#     Team Explosive Rate / NFL Benchmark Rate
#
#   EVI > 1.00 -> greater explosive-play vulnerability
#   EVI = 1.00 -> matches benchmark
#   EVI < 1.00 -> lower explosive-play vulnerability
#
# Statistical test:
#
#   Two-sided two-sample proportion test
#   H0: Team explosive-play rate = NFL benchmark rate
#   HA: Team explosive-play rate != NFL benchmark rate
#   Alpha = 0.05
#
# Inputs:
#
#   TEAM_ANALYSIS_FILE
#   NFL_ANALYSIS_FILE
#
# Outputs:
#
#   TEAM_EXPLOSIVE_PASS_FILE
#   TEAM_EXPLOSIVE_RUN_FILE
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

SIGNIFICANCE_LEVEL <- 0.05

required_packages <- c("dplyr", "readr", "tibble")

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

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing required analysis file(s):\n",
    paste0("  - ", missing_files, collapse = "\n"),
    "\n\nRun data_engineering/",
    "04_classify_explosives_and_situations.R first.",
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
  stop(
    "One or more analysis datasets contain zero rows.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 3. Validate required analytical fields
# ------------------------------------------------------------
#
# Upstream engineering and QA already validate construction of
# the eligible population and explosive-play classifications.
#
# This script validates only the fields required to construct
# the pass/run analytical tables.
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

required_columns <- c(
  "season",
  "defteam",
  "analysis_play_type",
  "explosive_play",
  situation_columns
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
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  seasons <- unique(
    stats::na.omit(data$season)
  )
  
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
    any(
      !data$analysis_play_type %in%
      c("pass", "run", "sack")
    )
  ) {
    stop(
      label,
      " contains invalid analysis_play_type values.",
      call. = FALSE
    )
  }
  
  binary_columns <- c(
    "explosive_play",
    situation_columns
  )
  
  invalid_binary <- vapply(
    binary_columns,
    function(column) {
      
      values <- data[[column]]
      
      any(is.na(values)) ||
        any(!values %in% c(0L, 1L))
    },
    logical(1)
  )
  
  if (any(invalid_binary)) {
    stop(
      label,
      " contains invalid binary field(s): ",
      paste(
        binary_columns[invalid_binary],
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }
}

validate_input(
  team_analysis,
  "Team analysis dataset"
)

validate_input(
  nfl_analysis,
  "NFL analysis dataset"
)

team_values <- unique(
  stats::na.omit(team_analysis$defteam)
)

if (
  length(team_values) != 1 ||
  team_values != TEAM_ABBR
) {
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
    ".",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 7. Calculate situational explosive-play rates
# ------------------------------------------------------------

calculate_situational_rates <- function(
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
      
      situation_column <-
        situations$situation_column[i]
      
      situation_data <- if (
        is.na(situation_column)
      ) {
        play_population
      } else {
        play_population[
          play_population[[situation_column]] == 1L,
          ,
          drop = FALSE
        ]
      }
      
      total_plays <- nrow(
        situation_data
      )
      
      explosive_plays <- sum(
        situation_data$explosive_play
      )
      
      tibble::tibble(
        situation = situations$situation[i],
        explosive_plays = explosive_plays,
        total_plays = total_plays,
        explosive_rate = if (total_plays > 0) {
          explosive_plays / total_plays
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
    team_explosives,
    team_plays,
    nfl_explosives,
    nfl_plays
) {
  
  if (
    is.na(team_explosives) ||
    is.na(team_plays) ||
    is.na(nfl_explosives) ||
    is.na(nfl_plays) ||
    team_plays <= 0 ||
    nfl_plays <= 0
  ) {
    return(FALSE)
  }
  
  test_result <- suppressWarnings(
    stats::prop.test(
      x = c(
        team_explosives,
        nfl_explosives
      ),
      n = c(
        team_plays,
        nfl_plays
      ),
      alternative = "two.sided",
      correct = FALSE
    )
  )
  
  isTRUE(
    test_result$p.value < SIGNIFICANCE_LEVEL
  )
}


# ------------------------------------------------------------
# 9. Build vulnerability table
# ------------------------------------------------------------

build_vulnerability_table <- function(
    team_data,
    benchmark_data,
    play_group
) {
  
  team_rates <- calculate_situational_rates(
    team_data,
    play_group
  ) |>
    dplyr::rename(
      team_explosive_rate = explosive_rate
    )
  
  nfl_rates <- calculate_situational_rates(
    benchmark_data,
    play_group
  ) |>
    dplyr::rename(
      nfl_explosive_plays = explosive_plays,
      nfl_total_plays = total_plays,
      NFL_average = explosive_rate
    )
  
  team_rates |>
    dplyr::left_join(
      nfl_rates,
      by = "situation"
    ) |>
    dplyr::mutate(
      
      team_difference =
        team_explosive_rate -
        NFL_average,
      
      EVI = dplyr::if_else(
        !is.na(NFL_average) &
          NFL_average > 0,
        team_explosive_rate /
          NFL_average,
        NA_real_
      ),
      
      statistically_significant = mapply(
        FUN = test_statistical_significance,
        team_explosives = explosive_plays,
        team_plays = total_plays,
        nfl_explosives = nfl_explosive_plays,
        nfl_plays = nfl_total_plays
      )
    )
}


# ------------------------------------------------------------
# 10. Build pass and run tables
# ------------------------------------------------------------

pass_table <- build_vulnerability_table(
  team_analysis,
  nfl_benchmark,
  "pass"
)

run_table <- build_vulnerability_table(
  team_analysis,
  nfl_benchmark,
  "run"
)


# ------------------------------------------------------------
# 11. Validate analytical outputs
# ------------------------------------------------------------
#
# Validate calculations produced by this script without
# repeating upstream engineering or QA checks.
#
# ------------------------------------------------------------

validate_vulnerability_table <- function(
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
      " situations are missing or out of order.",
      call. = FALSE
    )
  }
  
  if (
    any(
      table$explosive_plays >
      table$total_plays
    ) ||
    any(
      table$nfl_explosive_plays >
      table$nfl_total_plays
    )
  ) {
    stop(
      label,
      " contains explosive-play counts ",
      "greater than total plays.",
      call. = FALSE
    )
  }
  
  if (
    any(
      table$team_explosive_rate < 0 |
      table$team_explosive_rate > 1,
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
      " contains a rate outside the valid 0-1 range.",
      call. = FALSE
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
      " statistically_significant must contain ",
      "only TRUE/FALSE values.",
      call. = FALSE
    )
  }
}

validate_vulnerability_table(
  pass_table,
  "Explosive pass table"
)

validate_vulnerability_table(
  run_table,
  "Explosive run table"
)


# ------------------------------------------------------------
# 12. Validate overall pass/run decomposition
# ------------------------------------------------------------
#
# Pass + sack opportunities and run opportunities must
# reconstruct the complete eligible-play population.
#
# Explosive pass and run counts must reconstruct the complete
# explosive-play population.
#
# ------------------------------------------------------------

overall_pass <- pass_table |>
  dplyr::filter(
    situation == "overall"
  )

overall_run <- run_table |>
  dplyr::filter(
    situation == "overall"
  )

reconstructed_total_plays <-
  overall_pass$total_plays +
  overall_run$total_plays

expected_total_plays <- nrow(
  team_analysis
)

if (
  reconstructed_total_plays !=
  expected_total_plays
) {
  stop(
    "Pass and run denominators do not reconstruct ",
    "the team eligible-play population. Expected ",
    expected_total_plays,
    "; calculated ",
    reconstructed_total_plays,
    ".",
    call. = FALSE
  )
}

reconstructed_explosives <-
  overall_pass$explosive_plays +
  overall_run$explosive_plays

expected_explosives <- sum(
  team_analysis$explosive_play
)

if (
  reconstructed_explosives !=
  expected_explosives
) {
  stop(
    "Explosive pass and run counts do not reconstruct ",
    "the team explosive-play population. Expected ",
    expected_explosives,
    "; calculated ",
    reconstructed_explosives,
    ".",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 13. Create presentation-ready tables
# ------------------------------------------------------------

format_vulnerability_table <- function(table) {
  
  table |>
    dplyr::mutate(
      
      team_explosive_rate = round(
        team_explosive_rate * 100,
        2
      ),
      
      NFL_average = round(
        NFL_average * 100,
        2
      ),
      
      team_difference = round(
        team_difference * 100,
        2
      ),
      
      EVI = round(
        EVI,
        2
      )
    ) |>
    dplyr::select(
      situation,
      explosive_plays,
      total_plays,
      team_explosive_rate,
      NFL_average,
      team_difference,
      EVI,
      statistically_significant
    )
}

final_pass_table <- format_vulnerability_table(
  pass_table
)

final_run_table <- format_vulnerability_table(
  run_table
)


# ------------------------------------------------------------
# 14. Display results
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  toupper(TEAM_NAME),
  " EXPLOSIVE PASS TABLE\n",
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
  toupper(TEAM_NAME),
  " EXPLOSIVE RUN TABLE\n",
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
  "PASS / RUN VALIDATION SUMMARY\n",
  "============================================================\n\n",
  
  "Eligible plays: ",
  expected_total_plays,
  "\n",
  
  "Passing opportunities: ",
  overall_pass$total_plays,
  "\n",
  
  "Running opportunities: ",
  overall_run$total_plays,
  "\n",
  
  "Reconstructed eligible plays: ",
  reconstructed_total_plays,
  "\n\n",
  
  "Explosive plays: ",
  expected_explosives,
  "\n",
  
  "Explosive passes: ",
  overall_pass$explosive_plays,
  "\n",
  
  "Explosive runs: ",
  overall_run$explosive_plays,
  "\n",
  
  "Reconstructed explosive plays: ",
  reconstructed_explosives,
  "\n\n",
  
  "Statistically different passing situations: ",
  sum(final_pass_table$statistically_significant),
  " of ",
  nrow(final_pass_table),
  "\n",
  
  "Significant passing vulnerabilities: ",
  sum(
    final_pass_table$statistically_significant &
      final_pass_table$team_difference > 0
  ),
  "\n",
  
  "Significant passing strengths: ",
  sum(
    final_pass_table$statistically_significant &
      final_pass_table$team_difference < 0
  ),
  "\n\n",
  
  "Statistically different running situations: ",
  sum(final_run_table$statistically_significant),
  " of ",
  nrow(final_run_table),
  "\n",
  
  "Significant running vulnerabilities: ",
  sum(
    final_run_table$statistically_significant &
      final_run_table$team_difference > 0
  ),
  "\n",
  
  "Significant running strengths: ",
  sum(
    final_run_table$statistically_significant &
      final_run_table$team_difference < 0
  ),
  "\n",
  
  sep = ""
)


# ------------------------------------------------------------
# 15. Save outputs
# ------------------------------------------------------------

dir.create(
  ANALYSIS_OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  final_pass_table,
  TEAM_EXPLOSIVE_PASS_FILE
)

readr::write_csv(
  final_run_table,
  TEAM_EXPLOSIVE_RUN_FILE
)

cat(
  "\n============================================================\n",
  "EXPLOSIVE PASS / RUN TABLES CREATED SUCCESSFULLY\n",
  "============================================================\n\n",
  "Team: ", TEAM_NAME, "\n",
  "Season: ", SEASON, "\n",
  "Situations per table: ", nrow(situations), "\n",
  "Pass output: ", TEAM_EXPLOSIVE_PASS_FILE, "\n",
  "Run output: ", TEAM_EXPLOSIVE_RUN_FILE, "\n",
  sep = ""
)
