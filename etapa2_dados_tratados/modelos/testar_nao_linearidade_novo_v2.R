## ============================================================================
##  testar_nao_linearidade_novo_v2.R — is the heat-stress effect on each SNA DV
##  better described by a non-linear functional form than by the linear term
##  used in `ajustar_modelos_finais_novo_v2.R`?
##
##  Same family / response / dispformula / (1 | team_id) as the v2 models; only
##  the heat term changes (WBGT in Model A; air temperature in Model B, with
##  humidity and radiation kept linear):
##    linear : heat                          (v2 model)
##    ns2    : natural spline, 2 df          <- PRIMARY test, pre-specified
##    ns3    : natural spline, 3 df          (sensitivity)
##    quad   : heat + heat^2                 (sensitivity)
##    seg    : heat + pmax(heat - c, 0), breakpoint c profiled over a grid
##             (exploratory; its LR test is NOT a valid chi-square test because
##              c is estimated -- reported by AIC only, counting c as 1 parameter)
##  ML fits (fixed effects differ between forms). Student-t fits start the df
##  parameter at 5 (default start made some of them diverge). Likelihood-ratio test of each
##  nested form vs linear; Holm correction across the 5 DVs within each model
##  for the primary test. Heat varies at MATCH level: 65 matches carry the
##  information, only 14 above 28 C WBGT -- curves are weakly identified at the
##  hot end.
##
##  Writes resultados/nao_linearidade_novo_v2.csv and, per model, a panel of
##  fitted curves (linear vs ns2, 95% CI, population level, reference
##  covariates) in resultados/figuras_nao_linearidade_novo_v2/.
##
##  Run from `modelos/`:  Rscript testar_nao_linearidade_novo_v2.R
## ============================================================================

suppressWarnings(suppressPackageStartupMessages({ library(dplyr); library(glmmTMB); library(splines) }))
.prep_dir <- getwd()
SPEC <- list(
  densidade_ponderada        = list(family = "gaussian", response = "densidade_ponderada", disp = ~ tournament_stage),
  clustering_rede_ponderado  = list(family = "gaussian", response = "clustering_rede_ponderado", disp = ~ 1),
  centralizacao_grau_saida   = list(family = "gamma",    response = "centralizacao_grau_saida", disp = ~ tournament_stage),
  centralizacao_grau_entrada = list(family = "gamma",    response = "centralizacao_grau_entrada", disp = ~ 1),
  distancia_media_ponderada  = list(family = "t_log",    response = "log(distancia_media_ponderada)", disp = ~ 1)
)
DV_LABEL <- c(densidade_ponderada = "Densidade", clustering_rede_ponderado = "Clustering",
              centralizacao_grau_saida = "Centralização (saída)", centralizacao_grau_entrada = "Centralização (entrada)",
              distancia_media_ponderada = "Distância média")
fam_of <- function(f) switch(f, gaussian = gaussian(), gamma = Gamma(link = "log"), t_log = t_family())
MODELS <- list(WBGT = list(heat = "WBGT_cen", other = character(0), xlab = "WBGT (°C)"),
               Ta_RH_Rad = list(heat = "Ta_cen", other = c("RH_cen", "RAD_cen"), xlab = "Temperatura do ar (°C)"))
fig_dir <- "resultados/figuras_nao_linearidade_novo_v2"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
res <- list(); curves <- list()

for (w in names(MODELS)) {
  M <- MODELS[[w]]
  for (dv in names(SPEC)) {
    s <- SPEC[[dv]]
    target_variable <- dv; open_stadiums_only <- TRUE; use_adjusted_weather <- TRUE
    invisible(capture.output(source("preparar_dados_modelos_novo.R")))
    d <- df_env; x <- d[[M$heat]]
    heat_mean <- mean(d[[if (w == "WBGT") WBGT_SRC else TA_SRC]])
    ## spline bases built by hand (fixed knots) so the prediction grid uses the same basis
    k2 <- quantile(x, 1/2); k3 <- quantile(x, c(1/3, 2/3)); bk <- range(x)
    B2 <- ns(x, knots = k2, Boundary.knots = bk); B3 <- ns(x, knots = k3, Boundary.knots = bk)
    d$h_ns2_1 <- B2[, 1]; d$h_ns2_2 <- B2[, 2]
    d$h_ns3_1 <- B3[, 1]; d$h_ns3_2 <- B3[, 2]; d$h_ns3_3 <- B3[, 3]
    d$h_sq <- as.numeric(scale(x^2))   # standardised: same model, better-conditioned optimisation
    base <- build_fixed_team(d, extra = paste(M$other, collapse = " + "))
    base <- sub(" \\+ $", "", base)
    ok <- function(m) !is.null(m) && isTRUE(m$fit$convergence == 0)
    ## Student-t fits by ML need a sensible starting value for the df parameter
    ## (psi = log df): from the default start the optimizer can drift to df ~ 0
    ## ("false convergence", logLik NA). Start at df = 5, close to the v2 estimate.
    fit <- function(heat_terms, data = d) {
      ff <- as.formula(paste(s$response, "~", base, "+", heat_terms, "+ (1 | team_id)"))
      st <- if (s$family == "t_log") list(psi = log(5)) else NULL
      tryCatch(suppressWarnings(
        glmmTMB(ff, family = fam_of(s$family), dispformula = s$disp, data = data, REML = FALSE, start = st)),
        error = function(e) NULL)
    }
    fits <- list(linear = fit(M$heat), ns2 = fit("h_ns2_1 + h_ns2_2"),
                 ns3 = fit("h_ns3_1 + h_ns3_2 + h_ns3_3"), quad = fit(paste(M$heat, "+ h_sq")))
    ## segmented: profile the breakpoint over the central 80% of the heat range
    grid <- seq(quantile(x, 0.10), quantile(x, 0.90), length.out = 25); best <- NULL; best_c <- NA
    for (cc in grid) { d$h_hinge <- pmax(x - cc, 0); m <- fit(paste(M$heat, "+ h_hinge"))
      if (ok(m) && (is.null(best) || logLik(m) > logLik(best))) { best <- m; best_c <- cc } }
    fits$seg <- best
    ll0 <- as.numeric(logLik(fits$linear)); k0 <- attr(logLik(fits$linear), "df")
    for (f in names(fits)) {
      m <- fits[[f]]
      if (!ok(m)) { res[[length(res) + 1]] <- data.frame(dv = dv, weather_spec = w, heat_var = M$heat, form = f, note = "did not converge"); next }
      ll <- as.numeric(logLik(m)); k <- attr(logLik(m), "df") + (f == "seg")
      lr <- 2 * (ll - ll0); ddf <- k - k0
      res[[length(res) + 1]] <- data.frame(
        dv = dv, weather_spec = w, heat_var = M$heat, form = f, logLik = ll, n_par = k,
        AIC = -2 * ll + 2 * k, dAIC_vs_linear = (-2 * ll + 2 * k) - (-2 * ll0 + 2 * k0),
        LR = if (f == "linear") NA else lr, df = if (f == "linear") NA else ddf,
        p_vs_linear = if (f %in% c("linear", "seg")) NA else pchisq(lr, ddf, lower.tail = FALSE),
        breakpoint = if (f == "seg") best_c + heat_mean else NA, note = "")
    }
    ## fitted curves (population level, other covariates at reference/mean = 0)
    xg <- seq(bk[1], bk[2], length.out = 60)
    nd <- data.frame(alt_cen = 0, rank_cen = 0, RH_cen = 0, RAD_cen = 0, WBGT_cen = 0, Ta_cen = 0,
                     tournament_stage = factor("Group", levels = levels(d$tournament_stage)),
                     time_of_day = factor("afternoon", levels = levels(d$time_of_day)), team_id = NA)[rep(1, 60), ]
    nd[[M$heat]] <- xg
    G2 <- ns(xg, knots = k2, Boundary.knots = bk); nd$h_ns2_1 <- G2[, 1]; nd$h_ns2_2 <- G2[, 2]
    inv <- if (s$family == "gaussian") identity else exp
    for (f in c("linear", "ns2")) if (ok(fits[[f]])) {
      p <- predict(fits[[f]], newdata = nd, re.form = NA, type = "link", se.fit = TRUE)
      curves[[length(curves) + 1]] <- data.frame(dv = dv, weather_spec = w, form = f, heat = xg + heat_mean,
        fit = inv(p$fit), lo = inv(p$fit - 1.96 * p$se.fit), hi = inv(p$fit + 1.96 * p$se.fit))
    }
    curves[[length(curves) + 1]] <- data.frame(dv = dv, weather_spec = w, form = "obs", heat = x + heat_mean,
                                               fit = d[[dv]], lo = NA, hi = NA)
  }
}

res <- bind_rows(res)
res <- res %>% group_by(weather_spec, form) %>%
  mutate(p_holm = if (first(form) == "ns2") p.adjust(p_vs_linear, method = "holm") else NA_real_) %>% ungroup()
curves <- bind_rows(curves)
write.csv(res, "resultados/nao_linearidade_novo_v2.csv", row.names = FALSE)
write.csv(curves %>% filter(form != "obs"), "resultados/nao_linearidade_curvas_novo_v2.csv", row.names = FALSE)

for (w in names(MODELS)) {
  png(file.path(fig_dir, sprintf("curvas_%s_novo_v2.png", w)), width = 1500, height = 950, res = 130)
  par(mfrow = c(2, 3), mar = c(4.2, 4.2, 2.6, 1))
  for (dv in names(SPEC)) {
    cv <- curves %>% filter(dv == !!dv, weather_spec == w)
    ob <- cv %>% filter(form == "obs"); li <- cv %>% filter(form == "linear"); sp <- cv %>% filter(form == "ns2")
    r <- res %>% filter(dv == !!dv, weather_spec == w, form == "ns2")
    plot(ob$heat, ob$fit, pch = 16, col = "#00000030", cex = 0.8, xlab = MODELS[[w]]$xlab, ylab = DV_LABEL[[dv]],
         main = sprintf("%s\nspline vs linear: p = %.3f (Holm %.3f)", DV_LABEL[[dv]], r$p_vs_linear, r$p_holm), cex.main = 0.95)
    polygon(c(sp$heat, rev(sp$heat)), c(sp$lo, rev(sp$hi)), col = "#2a78d630", border = NA)
    lines(li$heat, li$fit, lwd = 2, lty = 2, col = "#555555"); lines(sp$heat, sp$fit, lwd = 2.5, col = "#2a78d6")
  }
  plot.new(); legend("center", c("Spline (2 gl), IC 95%", "Linear (modelo v2)", "Observações"), bty = "n",
                     lwd = c(2.5, 2, NA), lty = c(1, 2, NA), pch = c(NA, NA, 16), col = c("#2a78d6", "#555555", "#00000060"), cex = 1.1)
  dev.off()
}

options(width = 220)
print(res %>% select(-note, -heat_var, -logLik) %>% mutate(across(where(is.numeric), ~ signif(.x, 3))) %>% as.data.frame(), row.names = FALSE)
