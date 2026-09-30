## ============================================================================
##  testar_modelos_alternativos_novo.R — candidate models for the DVs whose
##  final model failed assumption checks in `diagnosticar_modelos_novo.R`
##  (distancia_media_ponderada: dispersion/quantile/RE-normality failures;
##  densidade_ponderada: residual variance heterogeneity across a categorical
##  covariate + mildly non-normal team intercepts).
##
##  Each candidate is judged by (a) DHARMa tests + RE normality, (b) AIC on the
##  ORIGINAL response scale (log-response models get the Jacobian correction
##  +2*sum(log y) so they are comparable with the Gamma fits), and (c) the
##  heat-stress coefficient (WBGT_cen / Ta_cen), which is the study's question.
##  ML fits for AIC; the reported heat coefficient comes from the same ML fit.
##
##  Section "centralizacao" (added 28/09/2026): family choice for the two
##  centralization DVs that replaced centralizacao_grau (given + received) --
##  centralizacao_grau_saida (passes given) and centralizacao_grau_entrada
##  (passes received) -- among Gaussian, Gamma(log), log-normal, Student-t on
##  log(Y), and Gaussian/Gamma with residual variance by tournament stage, all
##  with (1 | team_id).
##
##  Sections: distancia, densidade, centralizacao (default: all). Running only
##  some sections REPLACES just those DVs' rows in the existing output CSV:
##      Rscript testar_modelos_alternativos_novo.R centralizacao
##
##  Run from `modelos/`:  Rscript testar_modelos_alternativos_novo.R [secoes]
## ============================================================================

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr); library(glmmTMB); library(DHARMa)
}))
.prep_dir <- getwd()
WEATHER_SPECS <- list(WBGT = "WBGT_cen", Ta_RH_Rad = "Ta_cen + RH_cen + RAD_cen")
HEAT <- c(WBGT = "WBGT_cen", Ta_RH_Rad = "Ta_cen")
out <- list()
SECOES <- commandArgs(trailingOnly = TRUE)
if (!length(SECOES)) SECOES <- c("distancia", "densidade", "centralizacao")

fit_eval <- function(dv, cand, wname, ff, fam, disp = ~1, log_resp = FALSE, data) {
  m <- tryCatch(glmmTMB(ff, family = fam, dispformula = disp, data = data, REML = FALSE),
                error = function(e) NULL)
  if (is.null(m) || !isTRUE(m$fit$convergence == 0)) return(data.frame(dv = dv, candidate = cand, weather_spec = wname, note = "did not converge"))
  sim <- simulateResiduals(m, n = 1000, seed = 1)
  set.seed(1)   # bootstrap outlier test resamples internally
  y <- data[[dv]]
  re <- ranef(m)$cond$team_id[, 1]
  cc <- summary(m)$coefficients$cond[HEAT[[wname]], ]
  cat_p <- min(sapply(c("tournament_stage", "time_of_day"), function(v) fligner.test(residuals(sim), data[[v]])$p.value))
  data.frame(dv = dv, candidate = cand, weather_spec = wname,
             AIC_orig_scale = AIC(m) + if (log_resp) 2 * sum(log(y)) else 0,
             uniformity_p = testUniformity(sim, plot = FALSE)$p.value,
             dispersion_p = testDispersion(sim, plot = FALSE)$p.value,
             quantiles_p = suppressWarnings(testQuantiles(sim, plot = FALSE)$p.value),
             outliers_boot_p = testOutliers(sim, type = "bootstrap", plot = FALSE)$p.value,
             resid_homog_cat_min_p = cat_p,
             ranef_team_shapiro_p = shapiro.test(re)$p.value,
             heat_term = HEAT[[wname]], heat_est = unname(cc["Estimate"]), heat_p = unname(cc["Pr(>|z|)"]),
             note = "")
}

## --- distancia_media_ponderada ---------------------------------------------
if ("distancia" %in% SECOES) {
target_variable <- "distancia_media_ponderada"; open_stadiums_only <- TRUE; use_adjusted_weather <- TRUE
invisible(capture.output(source("preparar_dados_modelos_novo.R")))
d <- df_env %>% mutate(log_dist = log(distancia_media_ponderada))
for (wname in names(WEATHER_SPECS)) {
  fx <- build_fixed_team(d, extra = WEATHER_SPECS[[wname]])
  f <- function(y, re) as.formula(paste(y, "~", fx, "+", re))
  TM <- "(1 | team_id) + (1 | match_number)"; T1 <- "(1 | team_id)"
  dv <- "distancia_media_ponderada"
  out[[length(out)+1]] <- fit_eval(dv, "A0 gamma(log), team+match [atual]", wname, f(dv, TM), Gamma(link = "log"), data = d)
  out[[length(out)+1]] <- fit_eval(dv, "A1 gamma(log), team", wname, f(dv, T1), Gamma(link = "log"), data = d)
  out[[length(out)+1]] <- fit_eval(dv, "A2 gamma(inverse), team+match", wname, f(dv, TM), Gamma(link = "inverse"), data = d)
  out[[length(out)+1]] <- fit_eval(dv, "A3 lognormal (gaussian on log y), team+match", wname, f("log_dist", TM), gaussian(), log_resp = TRUE, data = d)
  out[[length(out)+1]] <- fit_eval(dv, "A4 lognormal, team", wname, f("log_dist", T1), gaussian(), log_resp = TRUE, data = d)
  out[[length(out)+1]] <- fit_eval(dv, "A5 student-t on log y, team+match", wname, f("log_dist", TM), t_family(), log_resp = TRUE, data = d)
  out[[length(out)+1]] <- fit_eval(dv, "A6 gamma(log), team+match, disp ~ tournament_stage + time_of_day", wname, f(dv, TM), Gamma(link = "log"),
                                   disp = ~ tournament_stage + time_of_day, data = d)
  out[[length(out)+1]] <- fit_eval(dv, "A7 lognormal, team+match, disp ~ tournament_stage + time_of_day", wname, f("log_dist", TM), gaussian(),
                                   disp = ~ tournament_stage + time_of_day, log_resp = TRUE, data = d)
}

}

## --- densidade_ponderada -----------------------------------------------------
if ("densidade" %in% SECOES) {
target_variable <- "densidade_ponderada"; open_stadiums_only <- TRUE; use_adjusted_weather <- TRUE
invisible(capture.output(source("preparar_dados_modelos_novo.R")))
d <- df_env
for (v in c("tournament_stage", "time_of_day"))
  cat(sprintf("densidade raw SD by %s: %s\n", v,
              paste(sprintf("%s=%.2f (n=%d)", levels(d[[v]]), tapply(d$densidade_ponderada, d[[v]], sd), as.integer(table(d[[v]]))), collapse = " | ")))
for (wname in names(WEATHER_SPECS)) {
  fx <- build_fixed_team(d, extra = WEATHER_SPECS[[wname]])
  ff <- as.formula(paste("densidade_ponderada ~", fx, "+ (1 | team_id)"))
  dv <- "densidade_ponderada"
  out[[length(out)+1]] <- fit_eval(dv, "D0 gaussian, team [atual]", wname, ff, gaussian(), data = d)
  out[[length(out)+1]] <- fit_eval(dv, "D1 gaussian, team, disp ~ tournament_stage", wname, ff, gaussian(), disp = ~ tournament_stage, data = d)
  out[[length(out)+1]] <- fit_eval(dv, "D2 gaussian, team, disp ~ time_of_day", wname, ff, gaussian(), disp = ~ time_of_day, data = d)
  out[[length(out)+1]] <- fit_eval(dv, "D3 gaussian, team, disp ~ tournament_stage + time_of_day", wname, ff, gaussian(), disp = ~ tournament_stage + time_of_day, data = d)
  out[[length(out)+1]] <- fit_eval(dv, "D4 gamma(log), team", wname, ff, Gamma(link = "log"), data = d)
}
}

## --- centralizacao_grau_saida / centralizacao_grau_entrada -------------------
if ("centralizacao" %in% SECOES) {
for (dv in c("centralizacao_grau_saida", "centralizacao_grau_entrada")) {
  target_variable <- dv; open_stadiums_only <- TRUE; use_adjusted_weather <- TRUE
  invisible(capture.output(source("preparar_dados_modelos_novo.R")))
  d <- df_env; d$log_y <- log(d[[dv]])
  for (wname in names(WEATHER_SPECS)) {
    fx <- build_fixed_team(d, extra = WEATHER_SPECS[[wname]])
    f <- function(y) as.formula(paste(y, "~", fx, "+ (1 | team_id)"))
    out[[length(out)+1]] <- fit_eval(dv, "C0 gaussian, team", wname, f(dv), gaussian(), data = d)
    out[[length(out)+1]] <- fit_eval(dv, "C1 gamma(log), team", wname, f(dv), Gamma(link = "log"), data = d)
    out[[length(out)+1]] <- fit_eval(dv, "C2 lognormal (gaussian on log y), team", wname, f("log_y"), gaussian(), log_resp = TRUE, data = d)
    out[[length(out)+1]] <- fit_eval(dv, "C3 student-t on log y, team", wname, f("log_y"), t_family(), log_resp = TRUE, data = d)
    out[[length(out)+1]] <- fit_eval(dv, "C4 gaussian, team, disp ~ tournament_stage", wname, f(dv), gaussian(), disp = ~ tournament_stage, data = d)
    out[[length(out)+1]] <- fit_eval(dv, "C5 gamma(log), team, disp ~ tournament_stage", wname, f(dv), Gamma(link = "log"), disp = ~ tournament_stage, data = d)
  }
}
}

res <- bind_rows(out)
OUT_CSV <- "resultados/diagnosticos_novo/modelos_alternativos_novo.csv"
if (file.exists(OUT_CSV) && !setequal(SECOES, c("distancia", "densidade", "centralizacao"))) {
  prev <- read.csv(OUT_CSV, stringsAsFactors = FALSE)
  res <- bind_rows(prev %>% filter(!dv %in% unique(res$dv)), res)
}
write.csv(res, OUT_CSV, row.names = FALSE)
options(width = 250)
print(res %>% mutate(across(where(is.numeric), ~ signif(.x, 3))), row.names = FALSE)
