## ============================================================================
##  testar_efeitos_aleatorios_novo_v2.R — does each v2 model need team_id,
##  match_number, both, or neither as random intercepts?
##
##  Same family / response / dispformula as `ajustar_modelos_finais_novo_v2.R`
##  (centralization split into out/in on 28/09/2026 -- centralizacao_grau,
##  given + received, is no longer analysed);
##  only the random-effects structure changes:
##    none  : no random effect (fixed effects only, reference)
##    team  : (1 | team_id)
##    match : (1 | match_number)
##    both  : (1 | team_id) + (1 | match_number)
##  All fits by REML (same fixed effects -> REML AIC/LR comparable across RE
##  structures). Likelihood-ratio tests for a single variance component use the
##  boundary correction p = 0.5 * P(chi2_1 > LR) (Self & Liang 1987).
##  Also reports variance estimates, singular fit, DHARMa checks and the
##  heat-stress coefficient under each structure.
##
##  Run from `modelos/`:  Rscript testar_efeitos_aleatorios_novo_v2.R
## ============================================================================

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr); library(glmmTMB); library(performance); library(DHARMa)
}))
.prep_dir <- getwd()
SPEC <- list(
  densidade_ponderada       = list(family = "gaussian", response = "densidade_ponderada", disp = ~ tournament_stage),
  clustering_rede_ponderado = list(family = "gaussian", response = "clustering_rede_ponderado", disp = ~ 1),
  centralizacao_grau_saida  = list(family = "gamma",    response = "centralizacao_grau_saida", disp = ~ tournament_stage),
  centralizacao_grau_entrada = list(family = "gamma",   response = "centralizacao_grau_entrada", disp = ~ 1),
  distancia_media_ponderada = list(family = "t_log",    response = "log(distancia_media_ponderada)", disp = ~ 1)
)
fam_of <- function(f) switch(f, gaussian = gaussian(), gamma = Gamma(link = "log"), t_log = t_family())
RE <- c(none = "", team = "+ (1 | team_id)", match = "+ (1 | match_number)",
        both = "+ (1 | team_id) + (1 | match_number)")
WEATHER <- list(WBGT = "WBGT_cen", Ta_RH_Rad = "Ta_cen + RH_cen + RAD_cen")
HEAT <- c(WBGT = "WBGT_cen", Ta_RH_Rad = "Ta_cen")
out <- list(); lrt <- list()

for (dv in names(SPEC)) {
  s <- SPEC[[dv]]
  target_variable <- dv; open_stadiums_only <- TRUE; use_adjusted_weather <- TRUE
  invisible(capture.output(source("preparar_dados_modelos_novo.R")))
  for (w in names(WEATHER)) {
    fixed <- build_fixed_team(df_env, extra = WEATHER[[w]])
    fits <- list()
    for (r in names(RE)) {
      ff <- as.formula(paste(s$response, "~", fixed, RE[[r]]))
      m <- tryCatch(glmmTMB(ff, family = fam_of(s$family), dispformula = s$disp, data = df_env, REML = TRUE),
                    error = function(e) NULL)
      fits[[r]] <- m
      if (is.null(m)) { out[[length(out) + 1]] <- data.frame(dv = dv, weather_spec = w, re = r, note = "fit error"); next }
      vc <- if (r == "none") list() else VarCorr(m)$cond
      gv <- function(nm) if (nm %in% names(vc)) as.numeric(diag(vc[[nm]])) else NA_real_
      sim <- simulateResiduals(m, n = 1000, seed = 1)
      cc <- summary(m)$coefficients$cond[HEAT[[w]], ]
      out[[length(out) + 1]] <- data.frame(
        dv = dv, weather_spec = w, re = r,
        converged = isTRUE(m$fit$convergence == 0) && !any(is.na(sqrt(diag(vcov(m)$cond)))),
        singular = if (r == "none") FALSE else isTRUE(performance::check_singularity(m)),
        REML_logLik = as.numeric(logLik(m)), AIC_REML = AIC(m),
        var_team = gv("team_id"), var_match = gv("match_number"),
        dharma_uniformity_p = testUniformity(sim, plot = FALSE)$p.value,
        dharma_dispersion_p = testDispersion(sim, plot = FALSE)$p.value,
        dharma_quantile_p = suppressWarnings(testQuantiles(sim, plot = FALSE)$p.value),
        heat_term = HEAT[[w]], heat_est = unname(cc["Estimate"]), heat_se = unname(cc["Std. Error"]),
        heat_p = unname(cc["Pr(>|z|)"]), note = "")
    }
    lr <- function(big, small, tested) {
        if (is.null(fits[[big]]) || is.null(fits[[small]])) return(NULL)
        L <- max(0, 2 * (as.numeric(logLik(fits[[big]])) - as.numeric(logLik(fits[[small]]))))
        data.frame(dv = dv, weather_spec = w, comparison = paste(big, "vs", small),
                   tests = tested, LR = L, p_boundary = 0.5 * pchisq(L, 1, lower.tail = FALSE))
    }
    lrt <- c(lrt, list(lr("team", "none", "team_id (alone)"), lr("match", "none", "match_number (alone)"),
                       lr("both", "team", "match_number given team_id"), lr("both", "match", "team_id given match_number")))
  }
}

res <- bind_rows(out); lrt <- bind_rows(lrt)
write.csv(res, "resultados/efeitos_aleatorios_novo_v2.csv", row.names = FALSE)
write.csv(lrt, "resultados/efeitos_aleatorios_lrt_novo_v2.csv", row.names = FALSE)
options(width = 250)
print(res %>% mutate(across(where(is.numeric), ~ signif(.x, 4))) %>% select(-note, -heat_term), row.names = FALSE)
cat("\n"); print(lrt %>% mutate(across(where(is.numeric), ~ signif(.x, 4))), row.names = FALSE)
