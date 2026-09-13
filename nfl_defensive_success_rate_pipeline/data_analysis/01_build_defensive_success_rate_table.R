# ============================================================
# 01 - Build Defensive Success Rate Table
# Reusable NFL Defensive Success Rate Analysis Pipeline
# ============================================================
#
# Compares the configured defense's Defensive Success Rate
# with the pooled rate of all other NFL defenses across
# 12 standardized game situations.
#
# Defensive Success Rate:
#   defensive successes / eligible plays
#
# Metrics:
#   team_difference = Team DSR - NFL Benchmark DSR
#   DSI = NFL Benchmark DSR / Team DSR
#
#   DSI > 1.00 -> below benchmark
#   DSI = 1.00 -> matches benchmark
#   DSI < 1.00 -> above benchmark
#
# League rank:
#   Configured team among all NFL defenses by overall DSR.
#
#   Rank 1 = highest Defensive Success Rate
#   Higher rank = lower Defensive Success Rate
#   Ties receive the same minimum rank.
#
# Statistical test:
#   Two-sided two-sample proportion test
#   Alpha is defined in config.R.
#
# Output:
#   TEAM_SUCCESS_RATE_FILE
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
  stop(
    "One or more analysis datasets contain zero rows.",
    call. = FALSE
  )
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
# 6. Calculate situational Defensive Success Rates
# ------------------------------------------------------------

calculate_situation_rates <- function(data) {
  
  results <- lapply(
    seq_len(nrow(situations)),
    function(i) {
      
      situation_column <- situations$situation_column[i]
      
      situation_data <- if (is.na(situation_column)) {
        data
      } else {
        data[
          data[[situation_column]] == 1L,
          ,
          drop = FALSE
        ]
      }
      
      total_plays <- nrow(situation_data)
      defensive_successes <- sum(
        situation_data$defensive_success
      )
      
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
# 7. Calculate overall NFL league rank
# ------------------------------------------------------------

calculate_overall_league_rank <- function(data, team_abbr) {
  
  team_rankings <- data |>
    dplyr::filter(
      !is.na(defteam)
    ) |>
    dplyr::group_by(
      defteam
    ) |>
    dplyr::summarise(
      defensive_successes = sum(defensive_success),
      total_plays = dplyr::n(),
      success_rate = defensive_successes / total_plays,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      league_rank = dplyr::min_rank(
        dplyr::desc(success_rate)
      )
    )
  
  teams_ranked <- dplyr::n_distinct(
    team_rankings$defteam
  )
  
  configured_team_rank <- team_rankings |>
    dplyr::filter(
      defteam == team_abbr
    )
  
  if (nrow(configured_team_rank) != 1) {
    stop(
      "Unable to identify exactly one league-ranking row for ",
      team_abbr,
      ".",
      call. = FALSE
    )
  }
  
  tibble::tibble(
    league_rank = as.integer(
      configured_team_rank$league_rank
    ),
    teams_ranked = teams_ranked
  )
}

overall_league_rank <- calculate_overall_league_rank(
  nfl_analysis,
  TEAM_ABBR
)


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
      x = c(
        team_successes,
        nfl_successes
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
# 9. Build Defensive Success Rate table
# ------------------------------------------------------------

team_rates <- calculate_situation_rates(
  team_analysis
) |>
  dplyr::rename(
    team_success_rate = success_rate
  )

nfl_rates <- calculate_situation_rates(
  nfl_benchmark
) |>
  dplyr::rename(
    nfl_defensive_successes = defensive_successes,
    nfl_total_plays = total_plays,
    NFL_average = success_rate
  )

success_rate_table <- team_rates |>
  dplyr::left_join(
    nfl_rates,
    by = "situation"
  ) |>
  dplyr::mutate(
    team_difference = team_success_rate - NFL_average,
    
    DSI = dplyr::if_else(
      !is.na(team_success_rate) &
        team_success_rate > 0,
      NFL_average / team_success_rate,
      NA_real_
    ),
    
    league_rank = dplyr::if_else(
      situation == "overall",
      overall_league_rank$league_rank,
      NA_integer_
    ),
    
    statistically_significant = mapply(
      FUN = test_statistical_significance,
      team_successes = defensive_successes,
      team_plays = total_plays,
      nfl_successes = nfl_defensive_successes,
      nfl_plays = nfl_total_plays
    )
  )


# ------------------------------------------------------------
# 10. Validate analytical output
# ------------------------------------------------------------

if (
  nrow(success_rate_table) != nrow(situations) ||
  !identical(
    success_rate_table$situation,
    situations$situation
  )
) {
  stop(
    "Defensive Success Rate situations are missing or out of order.",
    call. = FALSE
  )
}

if (
  any(
    success_rate_table$defensive_successes >
    success_rate_table$total_plays
  ) ||
  any(
    success_rate_table$nfl_defensive_successes >
    success_rate_table$nfl_total_plays
  )
) {
  stop(
    "Defensive success count exceeds total plays in at least one situation.",
    call. = FALSE
  )
}

if (
  any(
    success_rate_table$team_success_rate < 0 |
    success_rate_table$team_success_rate > 1,
    na.rm = TRUE
  ) ||
  any(
    success_rate_table$NFL_average < 0 |
    success_rate_table$NFL_average > 1,
    na.rm = TRUE
  )
) {
  stop(
    "A Defensive Success Rate is outside the valid 0-1 range.",
    call. = FALSE
  )
}

if (
  !is.logical(
    success_rate_table$statistically_significant
  ) ||
  any(
    is.na(
      success_rate_table$statistically_significant
    )
  )
) {
  stop(
    "statistically_significant must contain only TRUE/FALSE values.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 11. Validate overall row and league rank
# ------------------------------------------------------------

overall_row <- success_rate_table |>
  dplyr::filter(
    situation == "overall"
  )

expected_team_successes <- sum(
  team_analysis$defensive_success
)

expected_team_plays <- nrow(
  team_analysis
)

expected_nfl_successes <- sum(
  nfl_benchmark$defensive_success
)

expected_nfl_plays <- nrow(
  nfl_benchmark
)

if (
  overall_row$defensive_successes != expected_team_successes ||
  overall_row$total_plays != expected_team_plays
) {
  stop(
    "Overall team counts do not match the team analysis dataset.",
    call. = FALSE
  )
}

if (
  overall_row$nfl_defensive_successes != expected_nfl_successes ||
  overall_row$nfl_total_plays != expected_nfl_plays
) {
  stop(
    "Overall NFL benchmark counts do not match the benchmark dataset.",
    call. = FALSE
  )
}

if (
  is.na(overall_row$league_rank) ||
  overall_row$league_rank < 1 ||
  overall_row$league_rank > overall_league_rank$teams_ranked
) {
  stop(
    "Overall NFL league rank is missing or outside the valid range.",
    call. = FALSE
  )
}

if (
  any(
    !is.na(
      success_rate_table$league_rank[
        success_rate_table$situation != "overall"
      ]
    )
  )
) {
  stop(
    "League rank must be populated only for the overall situation.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 12. Create presentation-ready table
# ------------------------------------------------------------

final_table <- success_rate_table |>
  dplyr::mutate(
    team_success_rate = round(
      team_success_rate * 100,
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
    DSI = round(
      DSI,
      2
    )
  ) |>
  dplyr::select(
    situation,
    defensive_successes,
    total_plays,
    team_success_rate,
    NFL_average,
    team_difference,
    DSI,
    league_rank,
    statistically_significant
  )


# ------------------------------------------------------------
# 13. Save output
# ------------------------------------------------------------

dir.create(
  ANALYSIS_OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  final_table,
  TEAM_SUCCESS_RATE_FILE
)


# ------------------------------------------------------------
# 14. Display results
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  toupper(TEAM_NAME),
  " DEFENSIVE SUCCESS RATE TABLE\n",
  "============================================================\n\n",
  sep = ""
)

print(
  final_table,
  n = nrow(final_table),
  width = Inf
)

cat(
  "\nTeam eligible plays: ",
  expected_team_plays,
  
  "\nTeam defensive successes: ",
  expected_team_successes,
  
  "\nTeam overall DSR: ",
  round(
    expected_team_successes /
      expected_team_plays * 100,
    2
  ),
  "%",
  
  "\nNFL overall DSR rank: ",
  overall_league_rank$league_rank,
  " of ",
  overall_league_rank$teams_ranked,
  
  "\n\nNFL benchmark eligible plays: ",
  expected_nfl_plays,
  
  "\nNFL benchmark defensive successes: ",
  expected_nfl_successes,
  
  "\nNFL benchmark overall DSR: ",
  round(
    expected_nfl_successes /
      expected_nfl_plays * 100,
    2
  ),
  "%",
  
  "\n\nStatistically different situations: ",
  sum(final_table$statistically_significant),
  " of ",
  nrow(final_table),
  
  "\nSignificant strengths: ",
  sum(
    final_table$statistically_significant &
      final_table$team_difference > 0
  ),
  
  "\nSignificant weaknesses: ",
  sum(
    final_table$statistically_significant &
      final_table$team_difference < 0
  ),
  
  "\n\nOutput: ",
  TEAM_SUCCESS_RATE_FILE,
  "\n",
  
  sep = ""
)
