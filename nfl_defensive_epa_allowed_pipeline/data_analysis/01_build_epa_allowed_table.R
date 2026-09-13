# ============================================================
# 01 - Build EPA Allowed Table
# Reusable NFL Defensive EPA Allowed Analysis Pipeline
# ============================================================
#
# Compares the configured defense's EPA Allowed with the pooled
# other-defense NFL benchmark across 12 standardized situations.
#
# Core metric:
#   EPA Allowed per Play = Total Offensive EPA / Eligible Plays
#
# Situational comparison:
#   EPA Difference per 100 Plays =
#     (Team EPA/Play - NFL EPA/Play) * 100
#
# Overall reporting metrics:
#   League Rank
#   EPA Allowed per Game
#   Mean NFL EPA Allowed per Game
#   League-Relative EPA Allowed per Game
#   Field-Goal Equivalent per Game
#   Touchdown Equivalent per Game
#
# League rank:
#   Ranks all 32 defenses by overall EPA Allowed per Play.
#   Rank 1 = lowest / best EPA Allowed per Play.
#
# Lower EPA Allowed = better defensive performance.
#
# Statistical test:
#   Two-sided Welch two-sample t-test
#
# Output:
#   TEAM_EPA_ALLOWED_FILE
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

required_packages <- c("dplyr", "readr", "tibble")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
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

if (nrow(team_analysis) == 0 || nrow(nfl_analysis) == 0) {
  stop("One or more analysis datasets contain zero rows.")
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
#
# Upstream engineering and QA already validate EPA eligibility
# and situation construction. Only analysis-required fields are
# checked here.
#
# ------------------------------------------------------------

required_columns <- c(
  "game_id",
  "defteam",
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
  
  if (any(is.na(data$epa)) || any(!is.finite(data$epa))) {
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
    " defensive plays."
  )
}

if (!TEAM_ABBR %in% nfl_analysis$defteam) {
  stop(
    "Configured team ",
    TEAM_ABBR,
    " was not found in the NFL analysis dataset."
  )
}


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
# 6. Define analytical helpers
# ------------------------------------------------------------

filter_situation <- function(data, situation_column) {
  
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


calculate_epa_allowed <- function(data) {
  
  dplyr::bind_rows(
    lapply(
      seq_len(nrow(situations)),
      function(i) {
        
        situation_data <- filter_situation(
          data,
          situations$situation_column[i]
        )
        
        eligible_plays <- nrow(situation_data)
        total_epa <- sum(situation_data$epa)
        
        tibble::tibble(
          situation = situations$situation[i],
          eligible_plays = eligible_plays,
          total_epa = total_epa,
          epa_allowed_per_play = if (eligible_plays > 0) {
            total_epa / eligible_plays
          } else {
            NA_real_
          }
        )
      }
    )
  )
}


test_statistical_significance <- function(team_epa, nfl_epa) {
  
  team_epa <- team_epa[
    is.finite(team_epa)
  ]
  
  nfl_epa <- nfl_epa[
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
  
  !is.null(test_result) &&
    isTRUE(
      test_result$p.value < SIGNIFICANCE_LEVEL
    )
}


calculate_situation_significance <- function(
    team_data,
    benchmark_data
) {
  
  dplyr::bind_rows(
    lapply(
      seq_len(nrow(situations)),
      function(i) {
        
        situation_column <- situations$situation_column[i]
        
        team_situation <- filter_situation(
          team_data,
          situation_column
        )
        
        benchmark_situation <- filter_situation(
          benchmark_data,
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
  )
}


# ------------------------------------------------------------
# 7. Calculate situational EPA Allowed
# ------------------------------------------------------------

team_epa <- calculate_epa_allowed(
  team_analysis
)

nfl_epa <- calculate_epa_allowed(
  nfl_benchmark
) |>
  dplyr::rename(
    nfl_eligible_plays = eligible_plays,
    nfl_total_epa = total_epa,
    nfl_epa_allowed_per_play = epa_allowed_per_play
  )

significance_results <- calculate_situation_significance(
  team_analysis,
  nfl_benchmark
)


# ------------------------------------------------------------
# 8. Calculate overall EPA reporting metrics
# ------------------------------------------------------------
#
# Calculates league rank and per-game EPA reporting metrics.
# Overall-only metrics are attached only to the overall row.
#
# Lower EPA Allowed = better; league rank 1 = best.
# ------------------------------------------------------------

team_games <- dplyr::n_distinct(
  team_analysis$game_id
)

team_total_epa <- sum(
  team_analysis$epa
)

if (team_games <= 0) {
  stop(
    "Unable to identify any games for the configured defense."
  )
}

league_team_epa <- nfl_analysis |>
  dplyr::filter(
    !is.na(defteam)
  ) |>
  dplyr::group_by(defteam) |>
  dplyr::summarise(
    eligible_plays = dplyr::n(),
    total_epa = sum(epa),
    games = dplyr::n_distinct(game_id),
    epa_allowed_per_play = total_epa / eligible_plays,
    epa_allowed_per_game = total_epa / games,
    .groups = "drop"
  ) |>
  dplyr::mutate(
    league_rank = dplyr::min_rank(
      epa_allowed_per_play
    )
  )

if (
  nrow(league_team_epa) != 32 ||
  any(league_team_epa$eligible_plays <= 0) ||
  any(league_team_epa$games <= 0) ||
  any(!is.finite(league_team_epa$epa_allowed_per_play)) ||
  any(!is.finite(league_team_epa$epa_allowed_per_game))
) {
  stop(
    "Unable to calculate valid overall EPA metrics for all 32 defenses."
  )
}

configured_team_metrics <- league_team_epa |>
  dplyr::filter(
    defteam == TEAM_ABBR
  )

if (nrow(configured_team_metrics) != 1) {
  stop(
    "Unable to identify exactly one league-level EPA row for ",
    TEAM_ABBR,
    "."
  )
}

team_epa_per_game <- team_total_epa / team_games

mean_nfl_epa_per_game <- mean(
  league_team_epa$epa_allowed_per_game
)

overall_metrics <- tibble::tibble(
  league_rank = configured_team_metrics$league_rank,
  team_games = team_games,
  epa_allowed_per_game = team_epa_per_game,
  mean_nfl_epa_allowed_per_game = mean_nfl_epa_per_game,
  league_relative_epa_allowed_per_game =
    team_epa_per_game - mean_nfl_epa_per_game,
  field_goal_equivalent_per_game =
    team_epa_per_game / 3,
  touchdown_equivalent_per_game =
    team_epa_per_game / 7
)


# ------------------------------------------------------------
# 9. Build EPA Allowed table
# ------------------------------------------------------------

epa_allowed_table <- team_epa |>
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
    ) * 100,
    
    league_rank = dplyr::if_else(
      situation == "overall",
      overall_metrics$league_rank,
      NA_integer_
    ),
    
    team_games = dplyr::if_else(
      situation == "overall",
      overall_metrics$team_games,
      NA_integer_
    ),
    
    epa_allowed_per_game = dplyr::if_else(
      situation == "overall",
      overall_metrics$epa_allowed_per_game,
      NA_real_
    ),
    
    mean_nfl_epa_allowed_per_game = dplyr::if_else(
      situation == "overall",
      overall_metrics$mean_nfl_epa_allowed_per_game,
      NA_real_
    ),
    
    league_relative_epa_allowed_per_game = dplyr::if_else(
      situation == "overall",
      overall_metrics$league_relative_epa_allowed_per_game,
      NA_real_
    ),
    
    field_goal_equivalent_per_game = dplyr::if_else(
      situation == "overall",
      overall_metrics$field_goal_equivalent_per_game,
      NA_real_
    ),
    
    touchdown_equivalent_per_game = dplyr::if_else(
      situation == "overall",
      overall_metrics$touchdown_equivalent_per_game,
      NA_real_
    )
  )


# ------------------------------------------------------------
# 10. Validate analytical output
# ------------------------------------------------------------

if (
  nrow(epa_allowed_table) != nrow(situations) ||
  !identical(
    epa_allowed_table$situation,
    situations$situation
  )
) {
  stop(
    "EPA Allowed situations are missing or out of order."
  )
}

if (
  any(epa_allowed_table$eligible_plays <= 0) ||
  any(epa_allowed_table$nfl_eligible_plays <= 0)
) {
  stop(
    "One or more situations contain zero eligible plays."
  )
}

if (
  !is.logical(
    epa_allowed_table$statistically_significant
  ) ||
  any(
    is.na(
      epa_allowed_table$statistically_significant
    )
  )
) {
  stop(
    "statistically_significant must contain only TRUE/FALSE values."
  )
}

if (
  any(
    !is.finite(
      epa_allowed_table$epa_difference_per_100
    )
  )
) {
  stop(
    "EPA Difference per 100 contains non-finite values."
  )
}


# ------------------------------------------------------------
# 11. Validate overall row
# ------------------------------------------------------------
#
# Only calculations created in this script are checked here.
# Upstream engineering and QA logic is not reconstructed.
#
# ------------------------------------------------------------

overall_row <- epa_allowed_table |>
  dplyr::filter(
    situation == "overall"
  )

if (nrow(overall_row) != 1) {
  stop(
    "EPA Allowed table must contain exactly one overall row."
  )
}

expected_nfl_total_epa <- sum(
  nfl_benchmark$epa
)

if (
  overall_row$eligible_plays != nrow(team_analysis) ||
  !isTRUE(
    all.equal(
      overall_row$total_epa,
      team_total_epa,
      tolerance = 1e-10
    )
  )
) {
  stop(
    "Overall team EPA values do not match the source data."
  )
}

if (
  overall_row$nfl_eligible_plays != nrow(nfl_benchmark) ||
  !isTRUE(
    all.equal(
      overall_row$nfl_total_epa,
      expected_nfl_total_epa,
      tolerance = 1e-10
    )
  )
) {
  stop(
    "Overall NFL EPA values do not match the benchmark data."
  )
}

overall_metric_columns <- names(
  overall_metrics
)

actual_overall_metrics <- overall_row |>
  dplyr::select(
    dplyr::all_of(
      overall_metric_columns
    )
  )

if (
  !isTRUE(
    all.equal(
      as.data.frame(actual_overall_metrics),
      as.data.frame(overall_metrics),
      tolerance = 1e-10,
      check.attributes = FALSE
    )
  )
) {
  stop(
    "One or more overall EPA reporting metrics are incorrect."
  )
}

non_overall_metrics <- epa_allowed_table |>
  dplyr::filter(
    situation != "overall"
  ) |>
  dplyr::select(
    dplyr::all_of(
      overall_metric_columns
    )
  )

if (
  !all(
    vapply(
      non_overall_metrics,
      function(x) all(is.na(x)),
      logical(1)
    )
  )
) {
  stop(
    "Overall EPA reporting metrics must appear only on the overall row."
  )
}


# ------------------------------------------------------------
# 12. Create final analytical table
# ------------------------------------------------------------

final_table <- epa_allowed_table |>
  dplyr::select(
    situation,
    eligible_plays,
    total_epa,
    epa_allowed_per_play,
    nfl_eligible_plays,
    nfl_total_epa,
    nfl_epa_allowed_per_play,
    epa_difference_per_100,
    league_rank,
    statistically_significant,
    team_games,
    epa_allowed_per_game,
    mean_nfl_epa_allowed_per_game,
    league_relative_epa_allowed_per_game,
    field_goal_equivalent_per_game,
    touchdown_equivalent_per_game
  ) |>
  dplyr::mutate(
    dplyr::across(
      where(is.double),
      ~ round(.x, 4)
    )
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
  TEAM_EPA_ALLOWED_FILE
)


# ------------------------------------------------------------
# 14. Display results
# ------------------------------------------------------------

overall_row <- final_table |>
  dplyr::filter(
    situation == "overall"
  )

cat(
  "\n============================================================\n",
  toupper(TEAM_NAME), " EPA ALLOWED TABLE\n",
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
  overall_row$eligible_plays,
  
  "\nTeam total EPA: ",
  round(overall_row$total_epa, 3),
  
  "\nTeam EPA Allowed per Play: ",
  round(overall_row$epa_allowed_per_play, 4),
  
  "\nNFL benchmark EPA Allowed per Play: ",
  round(overall_row$nfl_epa_allowed_per_play, 4),
  
  "\nEPA Difference per 100 Plays: ",
  round(overall_row$epa_difference_per_100, 2),
  
  "\nLeague rank: ",
  overall_row$league_rank,
  
  "\nStatistically significant: ",
  overall_row$statistically_significant,
  
  "\n\nTeam games: ",
  overall_row$team_games,
  
  "\nTeam EPA Allowed per Game: ",
  round(overall_row$epa_allowed_per_game, 2),
  
  "\nMean NFL EPA Allowed per Game: ",
  round(overall_row$mean_nfl_epa_allowed_per_game, 2),
  
  "\nLeague-Relative EPA Allowed per Game: ",
  round(overall_row$league_relative_epa_allowed_per_game, 2),
  
  "\nField-Goal Equivalent per Game: ",
  round(overall_row$field_goal_equivalent_per_game, 2),
  
  "\nTouchdown Equivalent per Game: ",
  round(overall_row$touchdown_equivalent_per_game, 2),
  
  "\n\nStatistically different situations: ",
  sum(final_table$statistically_significant),
  " of ",
  nrow(final_table),
  
  "\nSignificant strengths: ",
  sum(
    final_table$statistically_significant &
      final_table$epa_difference_per_100 < 0
  ),
  
  "\nSignificant weaknesses: ",
  sum(
    final_table$statistically_significant &
      final_table$epa_difference_per_100 > 0
  ),
  
  "\n\nOutput: ",
  TEAM_EPA_ALLOWED_FILE,
  "\n",
  sep = ""
)
