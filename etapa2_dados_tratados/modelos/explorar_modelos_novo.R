## ============================================================================
##  explorar_modelos.R — EXPLORATORY pass for the 4 team-level SNA DVs
##  (densidade_ponderada, clustering_rede_ponderado, centralizacao_grau,
##  distancia_media_ponderada). NOT a final report — this is the empirical
##  evidence-gathering step the design discussion called for, before locking
##  (a) which random-effects structure to adopt (team_id + match_number
##  crossed, vs match_number only) and (b) which distribution family
##  (Gaussian vs Gamma GLMM) per DV, mirroring exactly how the player-level
##  study locked its own family choice (see `modelo-linear-misto-WC2026.Rmd`,
##  "Family comparison" section) before it was hardcoded into
##  `render_all_dvs.R`'s `gaussian_family()`.
##
##  For each DV x weather-spec (WBGT alone; Ta+RH+Radiation) x RE-structure
##  (crossed; match-only) x family (Gaussian; Gamma log-link), fits the model
##  by ML (AIC-comparable across families) and reports AIC + convergence +
##  singular-fit status in one table. Run from `modelos/`:
##      Rscript explorar_modelos.R
## ============================================================================

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr); library(glmmTMB); library(performance)
}))

.prep_dir <- getwd()  # run from modelos/
SNA_DVS <- c("densidade_ponderada", "clustering_rede_ponderado",
             "centralizacao_grau", "distancia_media_ponderada")

RE_STRUCTURES <- list(
  crossed     = "(1 | team_id) + (1 | match_number)",
  match_only  = "(1 | match_number)"
)

results <- list()

for (dv in SNA_DVS) {

  target_variable <- dv
  open_stadiums_only <- TRUE
  use_adjusted_weather <- TRUE
  source("preparar_dados_modelos_novo.R")   # builds df, df_env, build_fixed_team() for this dv

  weather_specs <- list(
    WBGT          = "WBGT_cen",
    `Ta_RH_Rad`   = "Ta_cen + RH_cen + RAD_cen"
  )

  for (wname in names(weather_specs)) {
    fixed <- build_fixed_team(df_env, extra = weather_specs[[wname]])

    for (rename in names(RE_STRUCTURES)) {
      re <- RE_STRUCTURES[[rename]]
      ff <- as.formula(paste(dv, "~", fixed, "+", re))

      for (fam_name in c("gaussian", "gamma")) {
        fam <- if (fam_name == "gaussian") gaussian() else Gamma(link = "log")
        fit <- tryCatch(
          glmmTMB(ff, family = fam, data = df_env, REML = FALSE),
          error = function(e) e
        )
        row <- data.frame(
          dv = dv, weather_spec = wname, re_structure = rename, family = fam_name,
          n = nrow(df_env), n_matches = nlevels(factor(df_env$match_number)),
          n_teams = nlevels(factor(df_env$team_id))
        )
        if (inherits(fit, "error")) {
          row$AIC <- NA_real_; row$converged <- FALSE; row$singular <- NA
          row$var_team <- NA_real_; row$var_match <- NA_real_; row$var_resid <- NA_real_
          row$note <- paste("FIT ERROR:", conditionMessage(fit))
        } else {
          row$AIC <- tryCatch(AIC(fit), error = function(e) NA_real_)
          row$converged <- isTRUE(fit$fit$convergence == 0)
          vc <- tryCatch(glmmTMB::VarCorr(fit)$cond, error = function(e) NULL)
          get_var <- function(nm) if (!is.null(vc) && nm %in% names(vc)) as.numeric(diag(vc[[nm]])) else NA_real_
          row$var_team  <- get_var("team_id")
          row$var_match <- get_var("match_number")
          row$var_resid <- tryCatch(sigma(fit)^2, error = function(e) NA_real_)
          row$singular <- tryCatch(performance::check_singularity(fit), error = function(e) NA)
          row$note <- ""
        }
        results[[length(results) + 1]] <- row
      }
    }
  }
  cat("\n", strrep("-", 78), "\n\n", sep = "")
}

res <- bind_rows(results)

## Best (lowest-AIC) combo per DV x weather_spec, among converged, non-singular fits
best <- res %>%
  filter(converged, !isTRUE(singular), !is.na(AIC)) %>%
  group_by(dv, weather_spec) %>%
  slice_min(AIC, n = 1) %>%
  ungroup()

out_dir <- "resultados"
dir.create(out_dir, showWarnings = FALSE)
write.csv(res, file.path(out_dir, "exploracao_modelos_novo.csv"), row.names = FALSE)

cat("\n\n=== FULL COMPARISON TABLE ===\n\n")
print(res %>%
        mutate(AIC = round(AIC, 1), var_team = round(var_team, 4),
               var_match = round(var_match, 6), var_resid = round(var_resid, 4)) %>%
        select(dv, weather_spec, re_structure, family, AIC, singular, var_team, var_match, var_resid, converged),
      row.names = FALSE, width = 220)

cat("\n\n=== BEST (lowest AIC, converged, non-singular) PER DV x WEATHER SPEC ===\n\n")
print(best %>% select(dv, weather_spec, re_structure, family, AIC, n, n_matches, n_teams) %>%
        mutate(AIC = round(AIC, 1)),
      row.names = FALSE, width = 200)

cat("\n\nWrote resultados/exploracao_modelos_novo.csv\n")
