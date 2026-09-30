## ============================================================================
##  ajustar_modelos_finais.R — FINAL (locked-spec) models for the 4 team-level
##  SNA DVs, following the decisions from `explorar_modelos.R`:
##
##    Random effects: (1 | team_id) ONLY. The crossed structure
##    (1|team_id)+(1|match_number) was tested for all 4 DVs x both weather
##    specs (8 combinations); match_number's variance was numerically 0 in
##    EVERY one (see resultados/exploracao_modelos.csv) -- at team grain,
##    match_number groups only 2 rows that already share identical weather/
##    context fixed effects, so it carries no residual signal. Dropping it is
##    not a compromise; the crossed and team-only fits are the same model.
##
##    Family per DV (locked by ML AIC comparison, same >=4 rule as the
##    player-level study):
##      densidade_ponderada        -> gaussian  (gamma AIC WORSE by ~12)
##      clustering_rede_ponderado  -> gaussian  (gamma AIC worse by ~1.3, < 4)
##      centralizacao_grau         -> gamma (log link)  (AIC improvement ~14)
##      distancia_media_ponderada  -> gamma (log link)  (AIC improvement ~76)
##
##  For each DV, fits BOTH environmental specifications (WBGT alone; Ta + RH +
##  Radiation), by REML (final reporting standard, matching the player-level
##  study), and reports: coefficient table (exponentiated for gamma models),
##  marginal/conditional R^2 (Nakagawa), a collinearity (VIF) check, and
##  DHARMa residual diagnostics (uniformity/dispersion/outlier/quantile tests
##  + a saved QQ/residual plot). Not yet a manuscript-ready Rmd/Word report
##  (that is the natural next step once these numbers are reviewed) -- this
##  is the full statistical content in plain, inspectable form.
##
##  Run from `modelos/`:  Rscript ajustar_modelos_finais.R
## ============================================================================

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr); library(glmmTMB); library(performance); library(broom.mixed); library(scales)
}))

.prep_dir <- getwd()
DV_FAMILY <- c(
  densidade_ponderada       = "gaussian",
  clustering_rede_ponderado = "gaussian",
  centralizacao_grau        = "gamma",
  distancia_media_ponderada = "gamma"
)
RE_FINAL <- "(1 | team_id)"
WEATHER_SPECS <- list(WBGT = "WBGT_cen", `Ta_RH_Rad` = "Ta_cen + RH_cen + RAD_cen")

fig_dir <- "resultados/figuras"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
all_coefs <- list()
all_fit_stats <- list()

for (dv in names(DV_FAMILY)) {

  target_variable <- dv
  open_stadiums_only <- TRUE
  use_adjusted_weather <- TRUE
  source("preparar_dados_modelos.R")

  fam_name <- DV_FAMILY[[dv]]
  fam <- if (fam_name == "gaussian") gaussian() else Gamma(link = "log")
  exponentiate <- fam_name == "gamma"

  cat(sprintf("\n\n%s\n### %s  (family = %s, RE = %s)\n%s\n", strrep("=", 78), dv, fam_name, RE_FINAL, strrep("=", 78)))

  for (wname in names(WEATHER_SPECS)) {
    fixed <- build_fixed_team(df_env, extra = WEATHER_SPECS[[wname]])
    ff <- as.formula(paste(dv, "~", fixed, "+", RE_FINAL))
    cat(sprintf("\n--- %s : %s ---\nFormula: %s\n", dv, wname, deparse(ff)))

    m <- glmmTMB(ff, family = fam, data = df_env, REML = TRUE)

    ## coefficient table
    ct <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE, exponentiate = exponentiate) %>%
      transmute(dv = dv, weather_spec = wname, term,
                estimate = round(estimate, 4),
                conf.low = round(conf.low, 4), conf.high = round(conf.high, 4),
                statistic = round(statistic, 2),
                p.value = scales::pvalue(p.value, accuracy = 0.001))
    print(as.data.frame(ct), row.names = FALSE)
    all_coefs[[paste(dv, wname)]] <- ct

    ## fit stats: R2, VIF, random-effect variance, dispersion
    r2 <- tryCatch(performance::r2_nakagawa(m),
                    error = function(e) list(R2_marginal = NA, R2_conditional = NA))
    vif_tab <- tryCatch(performance::check_collinearity(m), error = function(e) NULL)
    vc_team <- as.numeric(diag(glmmTMB::VarCorr(m)$cond$team_id))
    icc_team <- tryCatch(performance::icc(m)$ICC_adjusted, error = function(e) NA)

    cat(sprintf(
      "\nR2 marginal = %.3f | R2 conditional = %.3f | var(team_id) = %.4f | ICC(team) = %.3f | N = %d (%d teams, %d matches)\n",
      r2$R2_marginal, r2$R2_conditional, vc_team, icc_team,
      nobs(m), nlevels(df_env$team_id), nlevels(df_env$match_number)))
    if (!is.null(vif_tab)) {
      cat("VIF (collinearity):\n")
      print(as.data.frame(vif_tab)[, c("Term", "VIF")], row.names = FALSE)
    }

    ## DHARMa diagnostics
    sim <- DHARMa::simulateResiduals(m, n = 1000, seed = 1)
    ks  <- DHARMa::testUniformity(sim, plot = FALSE)
    disp <- DHARMa::testDispersion(sim, plot = FALSE)
    out <- DHARMa::testOutliers(sim, plot = FALSE)
    qt  <- DHARMa::testQuantiles(sim, plot = FALSE)
    cat(sprintf(
      "DHARMa: uniformity (KS) p = %.3f | dispersion p = %.3f | outliers p = %.3f | quantile-homogeneity p = %.3f\n",
      ks$p.value, disp$p.value, out$p.value, qt$p.value))

    png_path <- file.path(fig_dir, sprintf("dharma_%s_%s.png", dv, wname))
    png(png_path, width = 1000, height = 500)
    plot(sim)
    dev.off()
    cat(sprintf("Saved diagnostic plot: %s\n", png_path))

    all_fit_stats[[paste(dv, wname)]] <- data.frame(
      dv = dv, weather_spec = wname, family = fam_name,
      R2_marginal = round(r2$R2_marginal, 4), R2_conditional = round(r2$R2_conditional, 4),
      var_team_id = round(vc_team, 4), icc_team = round(icc_team, 3),
      n = nobs(m), n_teams = nlevels(df_env$team_id), n_matches = nlevels(df_env$match_number),
      dharma_uniformity_p = round(ks$p.value, 4), dharma_dispersion_p = round(disp$p.value, 4),
      dharma_outliers_p = round(out$p.value, 4), dharma_quantile_p = round(qt$p.value, 4)
    )
  }
}

out_dir <- "resultados"
dir.create(out_dir, showWarnings = FALSE)
coef_all <- bind_rows(all_coefs)
fit_all  <- bind_rows(all_fit_stats)
write.csv(coef_all, file.path(out_dir, "coeficientes_modelos.csv"), row.names = FALSE)
write.csv(fit_all,  file.path(out_dir, "ajuste_modelos.csv"), row.names = FALSE)

cat("\n\n", strrep("=", 78), "\n=== FIT STATISTICS SUMMARY (all DVs x weather specs) ===\n\n", sep = "")
print(fit_all, row.names = FALSE, width = 220)
cat("\nWrote resultados/coeficientes_modelos.csv and resultados/ajuste_modelos.csv\n")
cat("Diagnostic plots in resultados/figuras/\n")
