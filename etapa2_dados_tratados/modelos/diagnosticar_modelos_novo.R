## ============================================================================
##  diagnosticar_modelos_novo.R — assumption checks for the FINAL
##  team-level models of `ajustar_modelos_finais_novo.R` (same DV_FAMILY /
##  RE_FINAL_MAP / weather specs), with the heat-stress question in front:
##  every check is summarised by whether it threatens the WBGT / Ta / RH / Rad
##  coefficients.
##
##  Per DV x weather spec:
##    1. DHARMa overall: uniformity, dispersion, outliers (bootstrap), quantiles
##    2. DHARMa residuals vs each environmental predictor (testQuantiles) and
##       and variance homogeneity across each categorical covariate (Fligner-Killeen)
##    3. Normality of the team_id random intercepts (Shapiro-Wilk on BLUPs)
##    4. Collinearity (max VIF)
##    5. Leave-one-match-out: range of each environmental coefficient and
##       whether its significance flips when any single match is dropped
##    6. The 5 most extreme team-match rows (scaled residuals)
##
##  Run from `modelos/`:  Rscript diagnosticar_modelos_novo.R
## ============================================================================

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr); library(glmmTMB); library(performance); library(DHARMa)
}))

.prep_dir <- getwd()
DV_FAMILY <- c(densidade_ponderada = "gaussian", clustering_rede_ponderado = "gaussian",
               centralizacao_grau = "gamma", distancia_media_ponderada = "gamma")
RE_FINAL_MAP <- c(densidade_ponderada = "(1 | team_id)", clustering_rede_ponderado = "(1 | team_id)",
                  centralizacao_grau = "(1 | team_id)",
                  distancia_media_ponderada = "(1 | team_id) + (1 | match_number)")
WEATHER_SPECS <- list(WBGT = c("WBGT_cen"), Ta_RH_Rad = c("Ta_cen", "RH_cen", "RAD_cen"))
out_dir <- "resultados/diagnosticos_novo"; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

summ <- list(); loo <- list(); extremes <- list()

for (dv in names(DV_FAMILY)) {
  target_variable <- dv; open_stadiums_only <- TRUE; use_adjusted_weather <- TRUE
  invisible(capture.output(source("preparar_dados_modelos_novo.R")))
  fam <- if (DV_FAMILY[[dv]] == "gaussian") gaussian() else Gamma(link = "log")

  for (wname in names(WEATHER_SPECS)) {
    env_vars <- WEATHER_SPECS[[wname]]
    fixed <- build_fixed_team(df_env, extra = paste(env_vars, collapse = " + "))
    ff <- as.formula(paste(dv, "~", fixed, "+", RE_FINAL_MAP[[dv]]))
    m <- glmmTMB(ff, family = fam, data = df_env, REML = TRUE)
    sim <- simulateResiduals(m, n = 1000, seed = 1)

    p_env <- sapply(env_vars, function(v) testQuantiles(sim, predictor = df_env[[v]], plot = FALSE)$p.value)
    p_cat <- sapply(c("tournament_stage", "time_of_day"), function(v)
      fligner.test(residuals(sim), df_env[[v]])$p.value)
    re_team <- ranef(m)$cond$team_id[, 1]
    vif <- tryCatch(max(check_collinearity(m)$VIF), error = function(e) NA)

    png(file.path(out_dir, sprintf("residuos_vs_ambiente_%s_%s.png", dv, wname)), width = 400 * length(env_vars), height = 400)
    par(mfrow = c(1, length(env_vars)))
    for (v in env_vars) plotResiduals(sim, form = df_env[[v]], xlab = v, main = v)
    dev.off()

    summ[[paste(dv, wname)]] <- data.frame(
      dv = dv, weather_spec = wname, family = DV_FAMILY[[dv]],
      uniformity_p = testUniformity(sim, plot = FALSE)$p.value,
      dispersion_p = testDispersion(sim, plot = FALSE)$p.value,
      outliers_boot_p = testOutliers(sim, type = "bootstrap", plot = FALSE)$p.value,
      quantiles_p = testQuantiles(sim, plot = FALSE)$p.value,
      residuos_vs_ambiente_min_p = min(p_env), residuos_vs_ambiente_worst = names(p_env)[which.min(p_env)],
      resid_homog_cat_min_p = min(p_cat),
      ranef_team_shapiro_p = shapiro.test(re_team)$p.value,
      max_vif = vif)

    ## leave-one-match-out on the environmental coefficients
    full <- summary(m)$coefficients$cond
    for (mt in levels(df_env$match_number)) {
      d <- droplevels(filter(df_env, match_number != mt))
      mm <- tryCatch(glmmTMB(ff, family = fam, data = d, REML = TRUE), error = function(e) NULL)
      if (is.null(mm)) next
      cc <- summary(mm)$coefficients$cond
      for (v in env_vars) loo[[length(loo) + 1]] <- data.frame(
        dv = dv, weather_spec = wname, term = v, dropped_match = mt,
        estimate = cc[v, "Estimate"], p = cc[v, "Pr(>|z|)"],
        full_estimate = full[v, "Estimate"], full_p = full[v, "Pr(>|z|)"])
    }

    r <- residuals(sim)
    idx <- order(abs(r - 0.5), decreasing = TRUE)[1:5]
    extremes[[paste(dv, wname)]] <- data.frame(
      dv = dv, weather_spec = wname, match_number = df_env$match_number[idx],
      team = df_env$team[idx], value = df_env[[dv]][idx],
      total_passes = df_env$total_passes_na_rede[idx], scaled_resid = round(r[idx], 4))
  }
}

summ <- bind_rows(summ); loo <- bind_rows(loo); extremes <- bind_rows(extremes)
loo_sum <- loo %>% group_by(dv, weather_spec, term) %>%
  summarise(full_estimate = first(full_estimate), full_p = first(full_p),
            est_min = min(estimate), est_max = max(estimate), p_max = max(p), p_min = min(p),
            sig_flips = sum((p < 0.05) != (first(full_p) < 0.05)),
            match_max_change = dropped_match[which.max(abs(estimate - first(full_estimate)))],
            .groups = "drop")

write.csv(summ, file.path(out_dir, "pressupostos_resumo_novo.csv"), row.names = FALSE)
write.csv(loo, file.path(out_dir, "sensibilidade_sem_um_jogo_novo.csv"), row.names = FALSE)
write.csv(loo_sum, file.path(out_dir, "sensibilidade_sem_um_jogo_resumo_novo.csv"), row.names = FALSE)
write.csv(extremes, file.path(out_dir, "residuos_extremos_novo.csv"), row.names = FALSE)

options(width = 250)
cat("\n=== ASSUMPTIONS (p < 0.05 = violation) ===\n"); print(mutate(summ, across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
cat("\n=== LEAVE-ONE-MATCH-OUT (environmental terms) ===\n"); print(mutate(loo_sum, across(where(is.numeric), ~ signif(.x, 3))), row.names = FALSE)
cat("\n=== MOST EXTREME ROWS ===\n"); print(extremes, row.names = FALSE)
