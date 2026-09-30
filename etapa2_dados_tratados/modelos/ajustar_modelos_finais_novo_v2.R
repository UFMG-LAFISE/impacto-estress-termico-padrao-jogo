## ============================================================================
##  ajustar_modelos_finais_novo_v2.R — FINAL models for the 4 team-level SNA DVs
##  on the NOVO dataset, v2: same as `ajustar_modelos_finais_novo.R` except for
##  the two DVs that failed the assumption checks in
##  `diagnosticar_modelos_novo.R` (choice made in
##  `testar_modelos_alternativos_novo.R`, resultados/diagnosticos_novo/):
##
##    densidade_ponderada        gaussian, (1|team_id)
##                               + dispformula ~ tournament_stage
##                               (residual variance differs Group vs Playoffs;
##                                Fligner p = 0.007 -> 0.70, AIC 387 -> 379)
##    clustering_rede_ponderado  gaussian, (1|team_id)                 [unchanged]
##    centralizacao_grau_saida   gamma(log), (1|team_id)               [added 28/09/2026]
##                               + dispformula ~ tournament_stage
##                               (passes GIVEN: Def. 6.34 + Remark 6.10 on the weighted
##                                out-degree, Def. 4.7; plain Gamma failed variance
##                                homogeneity, Fligner p = 0.007; AIC 483 -> 482)
##    centralizacao_grau_entrada gamma(log), (1|team_id)               [added 28/09/2026]
##                               (passes RECEIVED: Def. 6.34 formula on the weighted
##                                degree prestige, Def. 4.25; passes all checks)
##    Both replace centralizacao_grau (given + received), no longer analysed.
##    Family chosen in `testar_modelos_alternativos_novo.R` (section centralizacao).
##    distancia_media_ponderada  Student-t on log(distancia), (1|team_id)
##                               (Gamma failed dispersion/quantiles/RE normality,
##                                driven by Paraguay M31/M89 -- real, very few
##                                passes; t tails absorb them. AIC -75.3 -> -82.3)
##
##  Random effects: (1 | team_id) in ALL four DVs (updated 28/09/2026 after
##  `testar_efeitos_aleatorios_novo_v2.R`): team_id needed everywhere (boundary
##  LRT p = 0.003-0.036); match_number variance ~0 / singular in every DV --
##  including distancia, which carried (1|match_number) from the Gamma model;
##  under the t family its variance collapses to 0, so estimates are identical.
##
##  The DV itself is still computed exactly as Def. 6.12 of Clemente et al.
##  (2016); only the model's response scale changes (log). Coefficients of the
##  gamma and t-on-log models are exponentiated: exp(beta) = multiplicative
##  change per unit of the predictor.
##
##  Fits by REML; reports coefficients, R2 (Nakagawa, where computable), VIF,
##  DHARMa (+ Fligner-Killeen by category, Shapiro on team intercepts), and a
##  leave-one-match-out check on the environmental coefficients.
##
##  Run from `modelos/`:  Rscript ajustar_modelos_finais_novo_v2.R
## ============================================================================

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr); library(glmmTMB); library(performance); library(broom.mixed); library(scales); library(DHARMa)
}))

.prep_dir <- getwd()
SPEC <- list(
  densidade_ponderada       = list(family = "gaussian", response = "densidade_ponderada",
                                   re = "(1 | team_id)", disp = ~ tournament_stage),
  clustering_rede_ponderado = list(family = "gaussian", response = "clustering_rede_ponderado",
                                   re = "(1 | team_id)", disp = ~ 1),
  centralizacao_grau_saida  = list(family = "gamma", response = "centralizacao_grau_saida",
                                   re = "(1 | team_id)", disp = ~ tournament_stage),
  centralizacao_grau_entrada = list(family = "gamma", response = "centralizacao_grau_entrada",
                                   re = "(1 | team_id)", disp = ~ 1),
  distancia_media_ponderada = list(family = "t_log", response = "log(distancia_media_ponderada)",
                                   re = "(1 | team_id)", disp = ~ 1)
)
fam_of <- function(f) switch(f, gaussian = gaussian(), gamma = Gamma(link = "log"), t_log = t_family())
WEATHER_SPECS <- list(WBGT = c("WBGT_cen"), Ta_RH_Rad = c("Ta_cen", "RH_cen", "RAD_cen"))

out_dir <- "resultados"; fig_dir <- "resultados/figuras_novo_v2"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
all_coefs <- list(); all_fit <- list(); all_loo <- list()

for (dv in names(SPEC)) {
  s <- SPEC[[dv]]
  target_variable <- dv; open_stadiums_only <- TRUE; use_adjusted_weather <- TRUE
  source("preparar_dados_modelos_novo.R")
  fam <- fam_of(s$family); exponentiate <- s$family %in% c("gamma", "t_log")
  has_match_re <- grepl("match_number", s$re, fixed = TRUE)
  disp_txt <- paste(deparse(s$disp), collapse = "")

  cat(sprintf("\n\n%s\n### %s  (family = %s, response = %s, RE = %s, disp = %s)\n%s\n",
              strrep("=", 78), dv, s$family, s$response, s$re, disp_txt, strrep("=", 78)))

  for (wname in names(WEATHER_SPECS)) {
    env_vars <- WEATHER_SPECS[[wname]]
    fixed <- build_fixed_team(df_env, extra = paste(env_vars, collapse = " + "))
    ff <- as.formula(paste(s$response, "~", fixed, "+", s$re))
    cat(sprintf("\n--- %s : %s ---\nFormula: %s | dispformula: %s\n", dv, wname, deparse(ff), disp_txt))

    m <- glmmTMB(ff, family = fam, dispformula = s$disp, data = df_env, REML = TRUE)
    if (!isTRUE(m$fit$convergence == 0)) warning("convergence problem: ", dv, " ", wname)

    ct <- broom.mixed::tidy(m, effects = "fixed", component = "cond", conf.int = TRUE,
                            exponentiate = exponentiate) %>%
      transmute(dv = dv, weather_spec = wname, term,
                estimate = round(estimate, 4), conf.low = round(conf.low, 4), conf.high = round(conf.high, 4),
                statistic = round(statistic, 2), p.value = scales::pvalue(p.value, accuracy = 0.001))
    print(as.data.frame(ct), row.names = FALSE)
    all_coefs[[paste(dv, wname)]] <- ct

    r2 <- tryCatch(suppressWarnings(performance::r2_nakagawa(m)),
                   error = function(e) list(R2_marginal = NA, R2_conditional = NA))
    if (is.null(r2) || all(is.na(unlist(r2)))) r2 <- list(R2_marginal = NA, R2_conditional = NA)
    if (s$family == "t_log" || disp_txt != "~1") {
      ## Nakagawa R2 by hand (validated against performance:: on the models it can
      ## handle), on the link scale, when performance:: cannot be used:
      ##  - t_family: performance/insight use sigma (not sigma^2) as residual variance
      ##    (verified 28/09/2026) -> var_resid = sigma^2 * df / (df - 2);
      ##  - dispformula != ~1 (residual variance by tournament stage): performance::
      ##    returns NA -> var_resid = mean over observations of the residual variance:
      ##    Gaussian sigma_i^2; Gamma(log) log(1 + sigma_i^2) (the lognormal
      ##    approximation performance:: uses for Gamma).
      X <- model.matrix(m, component = "cond")
      var_f <- var(as.vector(X %*% fixef(m)$cond))
      var_re <- sum(sapply(VarCorr(m)$cond, function(v) as.numeric(diag(v))))
      sig_i <- predict(m, type = "disp")
      var_e <- switch(s$family,
        t_log    = { df_t <- as.numeric(family_params(m)); sigma(m)^2 * df_t / (df_t - 2) },
        gaussian = mean(sig_i^2),
        gamma    = mean(log(1 + sig_i^2)))
      r2 <- list(R2_marginal = var_f / (var_f + var_re + var_e),
                 R2_conditional = (var_f + var_re) / (var_f + var_re + var_e))
    }
    vif <- tryCatch(max(performance::check_collinearity(m)$VIF), error = function(e) NA)
    vc_team <- as.numeric(diag(glmmTMB::VarCorr(m)$cond$team_id))
    vc_match <- if (has_match_re) as.numeric(diag(glmmTMB::VarCorr(m)$cond$match_number)) else NA_real_

    sim  <- simulateResiduals(m, n = 1000, seed = 1)
    ks   <- testUniformity(sim, plot = FALSE)$p.value
    disp <- testDispersion(sim, plot = FALSE)$p.value
    set.seed(1)   # bootstrap outlier test resamples internally -- fix the seed so the p-value is reproducible
    outl <- testOutliers(sim, type = "bootstrap", plot = FALSE)$p.value
    qt   <- suppressWarnings(testQuantiles(sim, plot = FALSE)$p.value)
    p_env <- sapply(env_vars, function(v) suppressWarnings(testQuantiles(sim, predictor = df_env[[v]], plot = FALSE)$p.value))
    p_cat <- min(sapply(c("tournament_stage", "time_of_day"), function(v) fligner.test(residuals(sim), df_env[[v]])$p.value))
    p_re  <- shapiro.test(ranef(m)$cond$team_id[, 1])$p.value

    cat(sprintf(paste0("\nR2 marginal = %s | R2 conditional = %s | var(team_id) = %.4f | var(match_number) = %s | max VIF = %.2f | N = %d (%d teams, %d matches)\n",
                       "DHARMa: uniformity p = %.3f | dispersion p = %.3f | outliers(boot) p = %.3f | quantiles p = %.3f | resid vs env min p = %.3f\n",
                       "Variance homogeneity by category (Fligner) min p = %.3f | team intercepts normality (Shapiro) p = %.3f\n"),
                format(round(r2$R2_marginal, 3)), format(round(r2$R2_conditional, 3)), vc_team,
                ifelse(has_match_re, sprintf("%.4f", vc_match), "NA (not in model)"), vif,
                nobs(m), nlevels(df_env$team_id), nlevels(df_env$match_number),
                ks, disp, outl, qt, min(p_env), p_cat, p_re))

    png(file.path(fig_dir, sprintf("dharma_%s_%s_novo_v2.png", dv, wname)), width = 1000, height = 500)
    plot(sim); dev.off()

    all_fit[[paste(dv, wname)]] <- data.frame(
      dv = dv, weather_spec = wname, family = s$family, response = s$response,
      re_structure = s$re, dispformula = disp_txt,
      R2_marginal = round(as.numeric(r2$R2_marginal), 4), R2_conditional = round(as.numeric(r2$R2_conditional), 4),
      var_team_id = round(vc_team, 4), var_match_number = round(vc_match, 4), max_vif = round(vif, 2),
      n = nobs(m), n_teams = nlevels(df_env$team_id), n_matches = nlevels(df_env$match_number),
      dharma_uniformity_p = round(ks, 4), dharma_dispersion_p = round(disp, 4),
      dharma_outliers_boot_p = round(outl, 4), dharma_quantile_p = round(qt, 4),
      residuos_vs_ambiente_min_p = round(min(p_env), 4), fligner_cat_min_p = round(p_cat, 4),
      ranef_team_shapiro_p = round(p_re, 4))

    ## leave-one-match-out on the environmental coefficients
    full <- summary(m)$coefficients$cond
    for (mt in levels(df_env$match_number)) {
      mm <- tryCatch(glmmTMB(ff, family = fam, dispformula = s$disp,
                             data = droplevels(filter(df_env, match_number != mt)), REML = TRUE),
                     error = function(e) NULL)
      if (is.null(mm)) next
      ## a refit counts only if the optimizer converged and the Hessian gave SEs
      conv <- isTRUE(mm$fit$convergence == 0) && !any(is.na(sqrt(diag(vcov(mm)$cond))))
      cc <- summary(mm)$coefficients$cond
      for (v in env_vars) all_loo[[length(all_loo) + 1]] <- data.frame(
        dv = dv, weather_spec = wname, term = v, dropped_match = mt, converged = conv,
        estimate = cc[v, "Estimate"], p = cc[v, "Pr(>|z|)"],
        full_estimate = full[v, "Estimate"], full_p = full[v, "Pr(>|z|)"])
    }
  }
}

coef_all <- bind_rows(all_coefs); fit_all <- bind_rows(all_fit); loo <- bind_rows(all_loo)
loo_sum <- loo %>% filter(converged) %>% group_by(dv, weather_spec, term) %>%
  summarise(full_estimate = first(full_estimate), full_p = first(full_p),
            est_min = min(estimate), est_max = max(estimate), p_min = min(p), p_max = max(p),
            n_fits = n(), sig_flips = sum((p < 0.05) != (first(full_p) < 0.05)),
            flipping_matches = paste(dropped_match[(p < 0.05) != (first(full_p) < 0.05)], collapse = " "),
            .groups = "drop") %>%
  left_join(loo %>% filter(!converged) %>% group_by(dv, weather_spec, term) %>%
              summarise(nonconverged_dropped = paste(unique(dropped_match), collapse = " "), .groups = "drop"),
            by = c("dv", "weather_spec", "term"))

write.csv(coef_all, file.path(out_dir, "coeficientes_modelos_novo_v2.csv"), row.names = FALSE)
write.csv(fit_all,  file.path(out_dir, "ajuste_modelos_novo_v2.csv"), row.names = FALSE)
write.csv(loo,      file.path(out_dir, "sensibilidade_sem_um_jogo_novo_v2.csv"), row.names = FALSE)
write.csv(loo_sum,  file.path(out_dir, "sensibilidade_sem_um_jogo_resumo_novo_v2.csv"), row.names = FALSE)

options(width = 250)
cat("\n\n", strrep("=", 78), "\n=== FIT / ASSUMPTIONS SUMMARY ===\n\n", sep = "")
print(fit_all %>% select(dv, weather_spec, family, R2_marginal, R2_conditional, starts_with("dharma"),
                         residuos_vs_ambiente_min_p, fligner_cat_min_p, ranef_team_shapiro_p, max_vif), row.names = FALSE)
cat("\n=== LEAVE-ONE-MATCH-OUT (environmental terms) ===\n\n")
print(loo_sum %>% mutate(across(where(is.numeric), ~ signif(.x, 3))), row.names = FALSE)
cat("\nWrote resultados/coeficientes_modelos_novo_v2.csv, ajuste_modelos_novo_v2.csv, sensibilidade_sem_um_jogo*_novo_v2.csv; plots in resultados/figuras_novo_v2/\n")
