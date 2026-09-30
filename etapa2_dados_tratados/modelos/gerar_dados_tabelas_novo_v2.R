## ============================================================================
##  gerar_dados_tabelas_novo_v2.R — collects everything the manuscript tables need
##  from the v2 final models (`ajustar_modelos_finais_novo_v2.R`, same SPEC):
##    - descriptives of the analysis sample (open-air venues, n = 130)
##    - R2 (Nakagawa & Schielzeth 2013) computed by hand for the two models
##      `performance::r2_nakagawa` cannot handle (densidade with dispformula;
##      conditional R2 of the Student-t distance model), on the link scale:
##        R2m = var_f / (var_f + sum var_RE + var_resid)
##        R2c = (var_f + sum var_RE) / (same denominator)
##      var_resid = mean(sigma_i^2) for the heteroscedastic Gaussian model and
##      sigma^2 * df / (df - 2) for the Student-t model.
##  Writes resultados/tabelas_novo_v2/*.csv (read by gerar_tabelas_word_novo_v2.py).
##  Run from `modelos/`:  Rscript gerar_dados_tabelas_novo_v2.R
## ============================================================================

suppressWarnings(suppressPackageStartupMessages({ library(dplyr); library(glmmTMB) }))
.prep_dir <- getwd()
out_dir <- "resultados/tabelas_novo_v2"; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## --- descriptives of the analysis sample -------------------------------------
target_variable <- "densidade_ponderada"; open_stadiums_only <- TRUE; use_adjusted_weather <- TRUE
invisible(capture.output(source("preparar_dados_modelos_novo.R")))
vars <- c(densidade_ponderada = "densidade_ponderada", clustering_rede_ponderado = "clustering_rede_ponderado",
          centralizacao_grau_saida = "centralizacao_grau_saida", centralizacao_grau_entrada = "centralizacao_grau_entrada",
          distancia_media_ponderada = "distancia_media_ponderada",
          total_passes_na_rede = "total_passes_na_rede",
          wbgt = WBGT_SRC, temp = TA_SRC, rh = RH_SRC, rad = RAD_SRC, altitude = "altitude_m")
desc <- bind_rows(lapply(names(vars), function(k) {
  x <- df_env[[vars[[k]]]]
  data.frame(var = k, source_column = vars[[k]], n = sum(!is.na(x)), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
             median = median(x, na.rm = TRUE), min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
}))
sample_info <- data.frame(n_rows = nrow(df_env), n_matches = nlevels(df_env$match_number), n_teams = nlevels(df_env$team_id),
                          n_group = sum(df_env$tournament_stage == "Group"), n_playoffs = sum(df_env$tournament_stage != "Group"),
                          n_afternoon = sum(df_env$time_of_day == "afternoon"), n_evening = sum(df_env$time_of_day != "afternoon"))
write.csv(desc, file.path(out_dir, "descritivas.csv"), row.names = FALSE)
write.csv(sample_info, file.path(out_dir, "amostra.csv"), row.names = FALSE)

## --- manual R2 for the two models performance:: cannot handle ---------------
r2_manual <- function(m, resid_var) {
  X <- model.matrix(m, component = "cond")   # fixed-effects design of the conditional model
  var_f <- var(as.vector(X %*% fixef(m)$cond))
  var_re <- sum(sapply(VarCorr(m)$cond, function(v) as.numeric(diag(v))))
  tot <- var_f + var_re + resid_var
  c(R2_marginal = var_f / tot, R2_conditional = (var_f + var_re) / tot)
}
W <- list(WBGT = "WBGT_cen", Ta_RH_Rad = "Ta_cen + RH_cen + RAD_cen")
r2 <- list()
for (w in names(W)) {
  target_variable <- "densidade_ponderada"; invisible(capture.output(source("preparar_dados_modelos_novo.R")))
  fx <- build_fixed_team(df_env, extra = W[[w]])
  m <- glmmTMB(as.formula(paste("densidade_ponderada ~", fx, "+ (1 | team_id)")), family = gaussian(),
               dispformula = ~ tournament_stage, data = df_env, REML = TRUE)
  sig <- predict(m, type = "disp")
  r2[[length(r2) + 1]] <- data.frame(dv = "densidade_ponderada", weather_spec = w, t(r2_manual(m, mean(sig^2))),
                                     note = "manual: var_resid = mean(sigma_i^2), sigma by tournament_stage")
  target_variable <- "distancia_media_ponderada"; invisible(capture.output(source("preparar_dados_modelos_novo.R")))
  fx <- build_fixed_team(df_env, extra = W[[w]])
  m <- glmmTMB(as.formula(paste("log(distancia_media_ponderada) ~", fx, "+ (1 | team_id)")),
               family = t_family(), data = df_env, REML = TRUE)
  df_t <- as.numeric(family_params(m)); s <- sigma(m)
  r2[[length(r2) + 1]] <- data.frame(dv = "distancia_media_ponderada", weather_spec = w,
                                     t(r2_manual(m, if (df_t > 2) s^2 * df_t / (df_t - 2) else NA)),
                                     note = sprintf("manual, log scale: var_resid = sigma^2*df/(df-2), df = %.2f", df_t))
}
r2 <- bind_rows(r2)
write.csv(r2, file.path(out_dir, "r2_manual.csv"), row.names = FALSE)
print(sample_info); print(desc, digits = 3); print(r2, digits = 3)
