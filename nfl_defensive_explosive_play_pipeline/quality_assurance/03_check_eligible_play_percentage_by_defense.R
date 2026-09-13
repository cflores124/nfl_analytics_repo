# ============================================================
# 03 - Check Eligible-Play Percentage by NFL Defense
# Reusable NFL Defensive Explosive-Play Analysis Pipeline
# ============================================================
#
# Compares the configured team's eligible-play percentage
# against the other 31 NFL defenses as a sanity check on the
# eligible population produced by the data-engineering pipeline.
#
# Eligible percentage:
# eligible plays / raw defensive PBP rows * 100
#
# Comparison statistics:
# configured team vs. other 31 defenses
#
# League rank:
# configured team among all 32 defenses
#
# Inputs:
# data/raw/nfl_{season}_regular_season_pbp.csv
# data/processed/nfl_{season}_eligible_plays.csv
#
# Output:
# data/qa_outputs/nfl_{season}_eligible_percentage_by_defense.csv
# ============================================================


# ------------------------------------------------------------
# 1. Load configuration
# ------------------------------------------------------------

if (!file.exists("config.R")) {
  stop(
    "config.R was not found. Open nfl_defensive_explosive_play_pipeline.Rproj ",
    "from the project root before running this script."
  )
}

source("config.R")


# ------------------------------------------------------------
# 2. Check required packages
# ------------------------------------------------------------

required_packages <- c("dplyr", "readr")
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
# 3. Validate inputs
# ------------------------------------------------------------

if (!file.exists(NFL_RAW_FILE)) {
  stop(
    "NFL raw PBP file was not found: ", NFL_RAW_FILE,
    "\nRun data_engineering/01_extract_nfl_regular_season_pbp.R first."
  )
}

if (!file.exists(NFL_ELIGIBLE_FILE)) {
  stop(
    "NFL eligible-play file was not found: ", NFL_ELIGIBLE_FILE,
    "\nRun data_engineering/03_build_eligible_plays.R first."
  )
}

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "ELIGIBLE-PLAY PERCENTAGE BY DEFENSE QA\n",
    "============================================================\n\n",
    "Season: %s\n",
    "Configured team: %s (%s)\n",
    "Comparison: configured team vs. other NFL defenses\n",
    "Raw NFL input: %s\n",
    "Eligible NFL input: %s\n",
    "Output: %s\n\n"
  ),
  SEASON,
  TEAM_NAME,
  TEAM_ABBR,
  NFL_RAW_FILE,
  NFL_ELIGIBLE_FILE,
  NFL_ELIGIBLE_PERCENTAGE_FILE
))


# ------------------------------------------------------------
# 4. Load NFL datasets
# ------------------------------------------------------------

nfl_raw <- readr::read_csv(
  NFL_RAW_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

nfl_eligible <- readr::read_csv(
  NFL_ELIGIBLE_FILE,
  show_col_types = FALSE,
  guess_max = Inf
)

if (nrow(nfl_raw) == 0) {
  stop("NFL raw PBP contains zero rows: ", NFL_RAW_FILE)
}

if (nrow(nfl_eligible) == 0) {
  stop("NFL eligible-play dataset contains zero rows: ", NFL_ELIGIBLE_FILE)
}

required_columns <- c("season", "defteam")

missing_raw_columns <- setdiff(required_columns, names(nfl_raw))
missing_eligible_columns <- setdiff(required_columns, names(nfl_eligible))

if (length(missing_raw_columns) > 0) {
  stop(
    "NFL raw PBP is missing required columns: ",
    paste(missing_raw_columns, collapse = ", ")
  )
}

if (length(missing_eligible_columns) > 0) {
  stop(
    "NFL eligible-play dataset is missing required columns: ",
    paste(missing_eligible_columns, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 5. Validate season and configured team
# ------------------------------------------------------------

raw_seasons <- unique(
  nfl_raw$season[!is.na(nfl_raw$season)]
)

eligible_seasons <- unique(
  nfl_eligible$season[!is.na(nfl_eligible$season)]
)

if (length(raw_seasons) != 1 || raw_seasons != SEASON) {
  stop(
    "NFL raw PBP does not contain only season ",
    SEASON, "."
  )
}

if (length(eligible_seasons) != 1 || eligible_seasons != SEASON) {
  stop(
    "NFL eligible-play dataset does not contain only season ",
    SEASON, "."
  )
}

if (!TEAM_ABBR %in% nfl_raw$defteam) {
  stop(
    "Configured team ", TEAM_ABBR,
    " was not found in the raw NFL defensive-team values."
  )
}

if (!TEAM_ABBR %in% nfl_eligible$defteam) {
  stop(
    "Configured team ", TEAM_ABBR,
    " was not found in the NFL eligible-play dataset."
  )
}

if (nrow(nfl_eligible) > nrow(nfl_raw)) {
  stop(
    "NFL eligible-play dataset contains more rows than the raw NFL dataset. ",
    "Investigate the upstream eligible-play construction."
  )
}


# ------------------------------------------------------------
# 6. Calculate eligible-play percentage by defense
# ------------------------------------------------------------

raw_by_defense <- nfl_raw |>
  dplyr::filter(!is.na(defteam)) |>
  dplyr::count(
    defteam,
    name = "raw_pbp_rows"
  )

eligible_by_defense <- nfl_eligible |>
  dplyr::filter(!is.na(defteam)) |>
  dplyr::count(
    defteam,
    name = "eligible_plays"
  )

defense_eligibility <- raw_by_defense |>
  dplyr::left_join(
    eligible_by_defense,
    by = "defteam"
  ) |>
  dplyr::mutate(
    eligible_plays = dplyr::coalesce(eligible_plays, 0L),
    eligible_pct = eligible_plays / raw_pbp_rows * 100
  )


# ------------------------------------------------------------
# 7. Validate defense-level results
# ------------------------------------------------------------

if (any(
  defense_eligibility$eligible_plays >
  defense_eligibility$raw_pbp_rows
)) {
  stop(
    "At least one defense has more eligible plays than raw PBP rows."
  )
}

if (any(
  defense_eligibility$eligible_pct < 0 |
  defense_eligibility$eligible_pct > 100
)) {
  stop(
    "At least one defense has an invalid eligible-play percentage."
  )
}

team_result <- defense_eligibility |>
  dplyr::filter(defteam == TEAM_ABBR)

if (nrow(team_result) != 1) {
  stop(
    "Expected exactly one QA row for ",
    TEAM_ABBR, ", but found ", nrow(team_result), "."
  )
}


# ------------------------------------------------------------
# 8. Build other-defense reference population
# ------------------------------------------------------------

reference_defenses <- defense_eligibility |>
  dplyr::filter(defteam != TEAM_ABBR)

if (nrow(reference_defenses) == 0) {
  stop(
    "No reference defenses remain after excluding ",
    TEAM_ABBR, "."
  )
}

reference_summary <- reference_defenses |>
  dplyr::summarise(
    defenses = dplyr::n(),
    mean_pct = mean(eligible_pct, na.rm = TRUE),
    median_pct = median(eligible_pct, na.rm = TRUE),
    sd_pct = sd(eligible_pct, na.rm = TRUE),
    min_pct = min(eligible_pct, na.rm = TRUE),
    max_pct = max(eligible_pct, na.rm = TRUE)
  )

reference_sd <- reference_summary$sd_pct

if (is.na(reference_sd) || reference_sd == 0) {
  stop(
    "Other-defense eligible-play percentage standard deviation ",
    "is zero or missing."
  )
}


# ------------------------------------------------------------
# 9. Calculate configured-team comparison statistics
# ------------------------------------------------------------

team_eligible_pct <- team_result$eligible_pct

team_difference <- (
  team_eligible_pct -
    reference_summary$mean_pct
)

team_z_score <- (
  team_difference /
    reference_sd
)


# ------------------------------------------------------------
# 10. Calculate 32-team league rank
# ------------------------------------------------------------

defense_eligibility <- defense_eligibility |>
  dplyr::mutate(
    rank_high_to_low = rank(
      -eligible_pct,
      ties.method = "min"
    )
  ) |>
  dplyr::arrange(
    rank_high_to_low,
    defteam
  )

team_result <- defense_eligibility |>
  dplyr::filter(defteam == TEAM_ABBR)

league_defenses <- nrow(defense_eligibility)


# ------------------------------------------------------------
# 11. Add configured-team QA statistics to output
# ------------------------------------------------------------

defense_eligibility <- defense_eligibility |>
  dplyr::mutate(
    difference_vs_other_31 = dplyr::if_else(
      defteam == TEAM_ABBR,
      team_difference,
      NA_real_
    ),
    
    z_score_vs_other_31 = dplyr::if_else(
      defteam == TEAM_ABBR,
      team_z_score,
      NA_real_
    )
  )


# ------------------------------------------------------------
# 12. Export league QA table
# ------------------------------------------------------------

dir.create(
  QA_OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  defense_eligibility,
  NFL_ELIGIBLE_PERCENTAGE_FILE
)


# ------------------------------------------------------------
# 13. Report results
# ------------------------------------------------------------

cat(
  "============================================================\n",
  SEASON, " ELIGIBLE-PLAY PERCENTAGE BY DEFENSE\n",
  "============================================================\n\n",
  sep = ""
)

print(defense_eligibility, n = Inf)

cat(
  "\n============================================================\n",
  "OTHER-DEFENSE REFERENCE DISTRIBUTION\n",
  "============================================================\n\n",
  sep = ""
)

print(reference_summary)

cat(sprintf(
  paste0(
    "\n============================================================\n",
    "%s SANITY CHECK\n",
    "============================================================\n\n",
    "Eligible-play percentage: %.2f%%\n",
    "Other %s defenses mean: %.2f%%\n",
    "Other %s defenses median: %.2f%%\n",
    "Other %s defenses SD: %.2f percentage points\n",
    "Difference from other %s mean: %.2f percentage points\n",
    "Z-score vs. other %s defenses: %.3f\n",
    "League rank, high to low: %s of %s\n\n",
    "QA table saved to:\n%s\n"
  ),
  toupper(TEAM_NAME),
  team_eligible_pct,
  reference_summary$defenses,
  reference_summary$mean_pct,
  reference_summary$defenses,
  reference_summary$median_pct,
  reference_summary$defenses,
  reference_summary$sd_pct,
  reference_summary$defenses,
  team_difference,
  reference_summary$defenses,
  team_z_score,
  team_result$rank_high_to_low,
  league_defenses,
  NFL_ELIGIBLE_PERCENTAGE_FILE
))
