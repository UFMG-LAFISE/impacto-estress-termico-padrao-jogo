## ============================================================================
##  preparar_dados_modelos_novo.R  —  shared data preparation for the copa2026-sna TEAM-LEVEL
##  passing-network (SNA) mixed-effects analyses. Team-level counterpart of
##  `_wc2026_prep.R` of ~/Downloads/wc2026-physical-performance (player-level); sourced by the team-level model
##  scripts in this folder.
##
##  Requires `target_variable` (character, one of the 4 SNA DVs) to exist
##  before sourcing. Optional knobs (set before sourcing): `open_stadiums_only`
##  (default TRUE — same team decision as the player-level study: the MAIN
##  analysis is restricted to open-air (roofless) venues; venue rule
##  `roof_type == "open"`).
##  Produces in the calling environment:
##     df          — analysis sample (team-match rows, DV present)
##     df_env      — weather-complete subset of df (for the environmental models)
##     build_fixed_team()  — structural fixed-effects string builder (drops
##                            single-level factors), team-level covariate set
##     montar_amostra_modelos() — reusable sample builder
##
##  WHY THIS IS A SEPARATE FILE, NOT AN EXTENSION OF `_wc2026_prep.R`: the
##  grain is different (team-match, not player-match), so the player-specific
##  filters/covariates in the original (GK exclusion, min_minutes, position,
##  age, started) have NO team-level equivalent and are simply absent here —
##  copying `_wc2026_prep.R` and deleting those lines would leave a script
##  that LOOKS like it still supports player concepts it no longer does; a
##  fresh, explicit file is clearer about what team-level analysis actually is.
##
##  NOTE ON NAMING: dataset columns ending in `_c` are DEGREES CELSIUS
##  (wbgt_c, air_temp_c, dew_point_c, ...), NOT centred. Centred predictors
##  created here use the `_cen` suffix, matching `_wc2026_prep.R`.
## ============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(forcats)
})

stopifnot(exists("target_variable"))

SNA_DVS <- c("densidade_ponderada", "clustering_rede_ponderado",
             "centralizacao_grau", "distancia_media_ponderada",
             "centralizacao_grau_saida", "centralizacao_grau_entrada")
if (!target_variable %in% SNA_DVS)
  stop("target_variable '", target_variable, "' is not one of the 4 SNA DVs: ",
       paste(SNA_DVS, collapse = ", "))

## --- Locate + read the dataset (once) ---------------------------------------
.data_candidates <- c(
  if (exists(".prep_dir")) file.path(.prep_dir, "dados", "dataset_modelos_copa2026_novo.csv"),
  "dados/dataset_modelos_copa2026_novo.csv",
  "modelos/dados/dataset_modelos_copa2026_novo.csv")
DATA_PATH <- .data_candidates[file.exists(.data_candidates)][1]
if (is.na(DATA_PATH)) stop("dataset_modelos_copa2026_novo.csv not found relative to ", getwd())
df_raw <- readr::read_csv(DATA_PATH, show_col_types = FALSE, progress = FALSE)

.cen <- function(x) { x <- as.numeric(x); x - mean(x, na.rm = TRUE) }

## --- Weather source: indoor-corrected `_adj` columns vs raw ERA5 ------------
## Same logic/default as `_wc2026_prep.R` — analyse the indoor-corrected
## columns by default (set use_adjusted_weather <- FALSE before sourcing to
## fall back to raw ERA5).
if (!exists("use_adjusted_weather")) use_adjusted_weather <- TRUE
.wsrc <- function(base) {
  adj <- paste0(base, "_adj")
  if (isTRUE(use_adjusted_weather) && adj %in% names(df_raw)) adj else base
}
WBGT_SRC <- .wsrc("wbgt_c");           TA_SRC  <- .wsrc("air_temp_c")
RH_SRC   <- .wsrc("rel_humidity_pct"); RAD_SRC <- .wsrc("solar_radiation")

## --- Reusable sample builder ------------------------------------------------
##  Filters (DV present, optional open-venue-only restriction), sets
##  factors/reference levels, and builds the centred `_cen` predictors ON the
##  returned sample. Returns list(df, df_env).
montar_amostra_modelos <- function(dv, open_stadiums_only = TRUE) {
  if (!dv %in% names(df_raw))
    stop("DV '", dv, "' is not a column in the dataset.")
  d <- df_raw %>% filter(!is.na(.data[[dv]]))
  if (isTRUE(open_stadiums_only))
    d <- d %>% filter(roof_type == "open")   # main-analysis rule: open-air venues only

  d <- d %>%
    mutate(
      team_id          = factor(team),
      match_number     = factor(match_number),
      tournament_stage = fct_relevel(factor(tournament_stage), "Group"),
      time_of_day      = fct_relevel(factor(time_of_day), "afternoon"),
      WBGT_cen = .cen(.data[[WBGT_SRC]]), Ta_cen  = .cen(.data[[TA_SRC]]),
      RH_cen   = .cen(.data[[RH_SRC]]),
      RAD_cen  = .cen(.data[[RAD_SRC]]) / 100,  # units of 100 W/m2, matches player-level scaling
      alt_cen  = .cen(altitude_m) / 100,        # units of 100 m, matches player-level scaling
      rank_cen = .cen(rank_diff)
    ) %>%
    group_by(team_id) %>%
    arrange(match_datetime, .by_group = TRUE) %>%
    mutate(team_match_seq = factor(row_number())) %>%   # per-team match order
    ungroup()

  d_env <- d %>%
    filter(!is.na(.data[[WBGT_SRC]]), !is.na(.data[[TA_SRC]]),
           !is.na(.data[[RH_SRC]]), !is.na(.data[[RAD_SRC]])) %>%
    droplevels()
  list(df = d, df_env = d_env)
}

## --- Structural fixed-effects builder --------------------------------------
##  Team-level covariate set: the player-level `base` was
##  (alt_cen, tournament_stage, time_of_day, rank_cen, position, age_cen, started);
##  position/age/started have no team-level meaning and are dropped, leaving
##  the 4 match/team-context covariates that DO carry over unchanged.
build_fixed_team <- function(data, extra = character(0),
                             base = c("alt_cen", "tournament_stage", "time_of_day",
                                      "rank_cen")) {
  keep <- vapply(base, function(v) {
    x <- data[[v]]
    if (is.factor(x) || is.character(x)) nlevels(droplevels(factor(x))) >= 2 else TRUE
  }, logical(1))
  dropped <- base[!keep]
  if (length(dropped))
    message("build_fixed_team(): dropping single-level term(s) in this sample: ",
            paste(dropped, collapse = ", "))
  paste(c(base[keep], extra), collapse = " + ")
}

## --- Build the primary analysis sample with the current knobs ---------------
if (!exists("open_stadiums_only")) open_stadiums_only <- TRUE

.built <- montar_amostra_modelos(target_variable, open_stadiums_only = open_stadiums_only)
df <- .built$df; df_env <- .built$df_env

## --- Sample-size + centring-reference report -------------------------------
cat(sprintf(
  "COPA2026 SNA prep | DV = %s\n  Analysis sample (DV present): %d rows | %d matches | %d teams\n  Weather-complete (df_env):    %d rows | %d matches | %d teams\n",
  target_variable,
  nrow(df),     nlevels(factor(df$match_number)),     nlevels(factor(df$team_id)),
  nrow(df_env), nlevels(factor(df_env$match_number)), nlevels(factor(df_env$team_id))
))
cat(sprintf(
  "  Centring reference means: WBGT=%.2f C | Ta=%.2f C | RH=%.1f%% | R=%.0f W/m2 | alt=%.0f m | rank_diff=%.1f\n",
  mean(df[[WBGT_SRC]], na.rm = TRUE), mean(df[[TA_SRC]], na.rm = TRUE),
  mean(df[[RH_SRC]], na.rm = TRUE), mean(df[[RAD_SRC]], na.rm = TRUE),
  mean(df$altitude_m, na.rm = TRUE), mean(df$rank_diff, na.rm = TRUE)))
## --- Roof/venue-exclusion report --------------------------------------------
.roofed_matches <- df_raw %>% filter(roof_type != "open") %>%
  distinct(match_number, city, roof_type)
cat(sprintf(
  "  Roof rule: open_stadiums_only = %s (rule: roof_type == \"open\") \n  Non-open venues in the full dataset: %d matches across %d venues -> %s\n",
  open_stadiums_only,
  nrow(.roofed_matches), dplyr::n_distinct(.roofed_matches$city),
  paste(sprintf("%s (%s)", unique(.roofed_matches$city), .roofed_matches$roof_type[!duplicated(.roofed_matches$city)]), collapse = ", ")
))
cat(sprintf("  Weather source: %s (WBGT<-%s)\n",
            if (isTRUE(use_adjusted_weather)) "indoor-corrected *_adj" else "raw ERA5",
            WBGT_SRC))

## --- Repetition depth per team (the key open question from the design
##  discussion: is `team_id` as a random intercept even supportable?) --------
.team_depth <- df %>% count(team_id, name = "n_matches") %>% arrange(n_matches)
cat(sprintf(
  "  Matches per team in this sample: min=%d | median=%.0f | max=%d (n teams=%d)\n",
  min(.team_depth$n_matches), median(.team_depth$n_matches), max(.team_depth$n_matches),
  nrow(.team_depth)))
