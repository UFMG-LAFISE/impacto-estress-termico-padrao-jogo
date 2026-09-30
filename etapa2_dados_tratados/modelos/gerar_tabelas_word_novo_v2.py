#!/usr/bin/env python3
"""
Builds `resultados/tabelas_novo_v2/tabelas_modelos_novo_v2.docx` — manuscript
tables for the v2 team-level models (padrão de jogo x estresse térmico).

Inputs (all produced by the R scripts, nothing is refitted here):
  resultados/coeficientes_modelos_novo_v2.csv         (ajustar_modelos_finais_novo_v2.R)
  resultados/ajuste_modelos_novo_v2.csv            (idem)
  resultados/sensibilidade_sem_um_jogo_novo_v2.csv         (idem; one row per refit)
  resultados/tabelas_novo_v2/{descritivas,amostra,r2_manual}.csv (gerar_dados_tabelas_novo_v2.R)

Needs python-docx and pandas. Run from `modelos/`:
    python3 gerar_tabelas_word_novo_v2.py
"""

from pathlib import Path

import pandas as pd
from docx import Document
from docx.enum.section import WD_ORIENT
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Cm, Pt

RES = Path("resultados")
TAB = RES / "tabelas_novo_v2"
OUT = TAB / "tabelas_modelos_novo_v2.docx"

DVS = ["densidade_ponderada", "clustering_rede_ponderado", "centralizacao_grau_saida",
       "centralizacao_grau_entrada", "distancia_media_ponderada"]
DV_LABEL = {
    "densidade_ponderada": "Densidade",
    "clustering_rede_ponderado": "Clustering",
    "centralizacao_grau_saida": "Centralização (saída)",
    "centralizacao_grau_entrada": "Centralização (entrada)",
    "distancia_media_ponderada": "Distância média",
}
DV_EFFECT = {  # scale of the reported estimate
    "densidade_ponderada": "β",
    "clustering_rede_ponderado": "β",
    "centralizacao_grau_saida": "exp(β)",
    "centralizacao_grau_entrada": "exp(β)",
    "distancia_media_ponderada": "exp(β)",
}
TERM_LABEL = {
    "(Intercept)": "Intercepto",
    "WBGT_cen": "WBGT (°C)",
    "Ta_cen": "Temperatura do ar (°C)",
    "RH_cen": "Umidade relativa (%)",
    "RAD_cen": "Radiação solar (100 W/m²)",
    "alt_cen": "Altitude (100 m)",
    "rank_cen": "Diferença de ranking FIFA",
    "tournament_stagePlayoffs": "Fase eliminatória (ref.: grupos)",
    "time_of_dayevening": "Período noturno (ref.: tarde)",
}
ENV_TERMS = {"WBGT_cen", "Ta_cen", "RH_cen", "RAD_cen"}
TERM_ORDER = ["(Intercept)", "WBGT_cen", "Ta_cen", "RH_cen", "RAD_cen",
              "alt_cen", "rank_cen", "tournament_stagePlayoffs", "time_of_dayevening"]
FAMILY_LABEL = {
    "gaussian": "Gaussiana (identidade)",
    "gamma": "Gama (log)",
    "t_log": "t de Student no log(Y)",
}
RE_LABEL = {
    "(1 | team_id)": "Seleção",
    "(1 | team_id) + (1 | match_number)": "Seleção + jogo",
}
SPEC_LABEL = {"WBGT": "Modelo A (WBGT)", "Ta_RH_Rad": "Modelo B (Ta + UR + Rad)"}


def num(x, d=3):
    """Brazilian decimal comma."""
    if pd.isna(x):
        return "—"
    return f"{x:.{d}f}".replace(".", ",").replace("-", "\u2212")


def pval(p):
    if isinstance(p, str):
        p = p.strip()
        return p.replace("<0.001", "<\u00a00,001").replace(".", ",")
    if pd.isna(p):
        return "—"
    return "<\u00a00,001" if p < 0.001 else num(p, 3)


def p_is_sig(p):
    if isinstance(p, str):
        return p.strip().startswith("<") or float(p) < 0.05
    return (not pd.isna(p)) and p < 0.05


def set_cell(cell, text, bold=False, italic=False, size=9, align=WD_ALIGN_PARAGRAPH.CENTER):
    cell.text = ""
    p = cell.paragraphs[0]
    p.alignment = align
    p.paragraph_format.space_before = Pt(1)
    p.paragraph_format.space_after = Pt(1)
    run = p.add_run(text)
    run.bold = bold
    run.italic = italic
    run.font.size = Pt(size)


def borders(table, rows_with_bottom):
    """APA-like: top rule, rule under header, bottom rule; no vertical lines."""
    tbl = table._tbl
    tblPr = tbl.tblPr
    b = OxmlElement("w:tblBorders")
    for edge in ("top", "bottom"):
        el = OxmlElement(f"w:{edge}")
        el.set(qn("w:val"), "single")
        el.set(qn("w:sz"), "8")
        el.set(qn("w:color"), "000000")
        b.append(el)
    for edge in ("left", "right", "insideH", "insideV"):
        el = OxmlElement(f"w:{edge}")
        el.set(qn("w:val"), "nil")
        b.append(el)
    tblPr.append(b)
    for ri in rows_with_bottom:
        for cell in table.rows[ri].cells:
            tcPr = cell._tc.get_or_add_tcPr()
            tb = OxmlElement("w:tcBorders")
            el = OxmlElement("w:bottom")
            el.set(qn("w:val"), "single")
            el.set(qn("w:sz"), "4")
            el.set(qn("w:color"), "000000")
            tb.append(el)
            tcPr.append(tb)


def caption(doc, n, text):
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(12)
    r = p.add_run(f"Tabela {n}. ")
    r.bold = True
    r.font.size = Pt(10)
    r2 = p.add_run(text)
    r2.italic = True
    r2.font.size = Pt(10)


def note(doc, text):
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(6)
    r = p.add_run("Nota. ")
    r.italic = True
    r.font.size = Pt(8)
    r2 = p.add_run(text)
    r2.font.size = Pt(8)


def new_table(doc, nrows, ncols, widths_cm):
    t = doc.add_table(rows=nrows, cols=ncols)
    t.alignment = WD_TABLE_ALIGNMENT.CENTER
    t.autofit = False
    for j, w in enumerate(widths_cm):
        t.columns[j].width = Cm(w)   # tblGrid: LibreOffice/Word layout use this, not only cell widths
    for row in t.rows:
        for j, w in enumerate(widths_cm):
            row.cells[j].width = Cm(w)
    return t


def landscape(section):
    section.orientation = WD_ORIENT.LANDSCAPE
    section.page_width, section.page_height = Cm(29.7), Cm(21.0)
    for m in ("left_margin", "right_margin", "top_margin", "bottom_margin"):
        setattr(section, m, Cm(2))


def main():
    coef = pd.read_csv(RES / "coeficientes_modelos_novo_v2.csv", dtype={"p.value": str})
    fit = pd.read_csv(RES / "ajuste_modelos_novo_v2.csv")
    loo_all = pd.read_csv(RES / "sensibilidade_sem_um_jogo_novo_v2.csv")
    desc = pd.read_csv(TAB / "descritivas.csv").set_index("var")
    amostra = pd.read_csv(TAB / "amostra.csv").iloc[0]
    r2m = pd.read_csv(TAB / "r2_manual.csv")

    # R2 from performance:: where available; manual (validated against performance::
    # on the clustering model) for densidade (performance:: cannot handle dispformula)
    # and distancia (performance:: uses sigma instead of sigma^2 for the t family)
    for _, r in r2m.iterrows():
        sel = (fit.dv == r.dv) & (fit.weather_spec == r.weather_spec)
        fit.loc[sel, "R2_marginal"] = r.R2_marginal
        fit.loc[sel, "R2_conditional"] = r.R2_conditional

    doc = Document()
    st = doc.styles["Normal"]
    st.font.name = "Times New Roman"
    st.font.size = Pt(10)
    landscape(doc.sections[0])

    h = doc.add_paragraph()
    rr = h.add_run("Tabelas de resultados — padrão de jogo (redes de passes) e estresse térmico, Copa do Mundo FIFA 2026")
    rr.bold = True
    rr.font.size = Pt(12)
    p = doc.add_paragraph()
    p.add_run(
        f"Amostra analisada: {int(amostra.n_rows)} observações seleção-jogo, {int(amostra.n_matches)} jogos em estádios "
        f"abertos, {int(amostra.n_teams)} seleções. Modelos lineares generalizados mistos (glmmTMB, REML). "
        "Métricas de rede calculadas conforme Clemente, Martins & Mendes (2016)."
    ).font.size = Pt(9)

    # ---------------- Tabela 1: descritivas ----------------
    caption(doc, 1, "Estatística descritiva das variáveis de padrão de jogo e das condições ambientais na amostra analisada.")
    rows = [
        ("Padrão de jogo", None),
        ("Densidade ponderada", "densidade_ponderada", 3),
        ("Clustering da rede", "clustering_rede_ponderado", 3),
        ("Centralização de grau — saída (passes dados)", "centralizacao_grau_saida", 2),
        ("Centralização de grau — entrada (passes recebidos)", "centralizacao_grau_entrada", 2),
        ("Distância média ponderada", "distancia_media_ponderada", 3),
        ("Total de passes na rede", "total_passes_na_rede", 0),
        ("Condições ambientais", None),
        ("WBGT (°C)", "wbgt", 1),
        ("Temperatura do ar (°C)", "temp", 1),
        ("Umidade relativa (%)", "rh", 1),
        ("Radiação solar (W/m²)", "rad", 0),
        ("Altitude (m)", "altitude", 0),
    ]
    t = new_table(doc, len(rows) + 1, 6, [7.0, 2.6, 2.6, 2.6, 2.6, 2.6])
    for j, hd in enumerate(["Variável", "Média", "DP", "Mediana", "Mín.", "Máx."]):
        set_cell(t.rows[0].cells[j], hd, bold=True, align=WD_ALIGN_PARAGRAPH.LEFT if j == 0 else WD_ALIGN_PARAGRAPH.CENTER)
    for i, r in enumerate(rows, start=1):
        if r[1] is None:
            set_cell(t.rows[i].cells[0], r[0], italic=True, align=WD_ALIGN_PARAGRAPH.LEFT)
            for j in range(1, 6):
                set_cell(t.rows[i].cells[j], "")
            continue
        lab, key, d = r
        s = desc.loc[key]
        set_cell(t.rows[i].cells[0], "   " + lab, align=WD_ALIGN_PARAGRAPH.LEFT)
        for j, col in enumerate(["mean", "sd", "median", "min", "max"], start=1):
            set_cell(t.rows[i].cells[j], num(s[col], d))
    borders(t, [0])
    note(doc,
         f"n = {int(amostra.n_rows)} (fase de grupos = {int(amostra.n_group)}; fase eliminatória = {int(amostra.n_playoffs)}; "
         f"tarde = {int(amostra.n_afternoon)}; noite = {int(amostra.n_evening)}). DP = desvio-padrão. "
         "Redes 11×11 (passes de cada substituto somados à posição do titular substituído). Densidade (Def. 6.8), "
         "clustering (Def. 6.22) e distância média (Def. 6.12) de Clemente et al. (2016); centralização de saída = Def. 6.34 "
         "(Remark 6.10) sobre o grau de saída ponderado (Def. 4.7); centralização de entrada = fórmula da Def. 6.34 sobre o "
         "prestígio de grau ponderado (Def. 4.25). Densidade e centralizações não são limitadas a [0, 1] na versão ponderada.")

    # ---------------- Tabelas 2 e 3: coeficientes ----------------
    for n_tab, spec in [(2, "WBGT"), (3, "Ta_RH_Rad")]:
        doc.add_page_break()
        caption(doc, n_tab, f"Efeitos fixos do {SPEC_LABEL[spec]} sobre as variáveis de padrão de jogo.")
        c = coef[coef.weather_spec == spec]
        terms = [tt for tt in TERM_ORDER if tt in set(c.term)]
        W23 = [3.0] + [3.25, 1.29] * len(DVS)
        t = new_table(doc, len(terms) + 2, 1 + 2 * len(DVS), W23)
        set_cell(t.rows[0].cells[0], "", bold=True)
        set_cell(t.rows[1].cells[0], "Preditor", bold=True, align=WD_ALIGN_PARAGRAPH.LEFT)
        for k, dv in enumerate(DVS):
            a, b = t.rows[0].cells[1 + 2 * k], t.rows[0].cells[2 + 2 * k]
            m = a.merge(b)
            set_cell(m, DV_LABEL[dv], bold=True)
            set_cell(t.rows[1].cells[1 + 2 * k], f"{DV_EFFECT[dv]} [IC 95%]", bold=True)
            set_cell(t.rows[1].cells[2 + 2 * k], "p", bold=True, italic=True)
        for i, term in enumerate(terms, start=2):
            lab = TERM_LABEL.get(term, term)
            set_cell(t.rows[i].cells[0], lab, bold=term in ENV_TERMS, align=WD_ALIGN_PARAGRAPH.LEFT)
            for k, dv in enumerate(DVS):
                r = c[(c.dv == dv) & (c.term == term)]
                if r.empty:
                    set_cell(t.rows[i].cells[1 + 2 * k], "—")
                    set_cell(t.rows[i].cells[2 + 2 * k], "—")
                    continue
                r = r.iloc[0]
                d = 4 if dv == "clustering_rede_ponderado" or (term in ENV_TERMS | {"rank_cen", "alt_cen"} and DV_EFFECT[dv] == "β") else 3
                sig = p_is_sig(r["p.value"]) and term != "(Intercept)"
                set_cell(t.rows[i].cells[1 + 2 * k],
                         f"{num(r.estimate, d)} [{num(r['conf.low'], d)}; {num(r['conf.high'], d)}]", bold=sig, size=7.5)
                set_cell(t.rows[i].cells[2 + 2 * k], pval(r["p.value"]), bold=sig, size=7)
        # fit rows
        extra = [("R² marginal", "R2_marginal"), ("R² condicional", "R2_conditional")]
        for lab, col in extra:
            row = t.add_row()
            for j, w in enumerate(W23):
                row.cells[j].width = Cm(w)
            set_cell(row.cells[0], lab, align=WD_ALIGN_PARAGRAPH.LEFT)
            for k, dv in enumerate(DVS):
                f = fit[(fit.dv == dv) & (fit.weather_spec == spec)].iloc[0]
                m = row.cells[1 + 2 * k].merge(row.cells[2 + 2 * k])
                set_cell(m, num(f[col], 3))
        borders(t, [1, len(terms) + 1])
        env_txt = ("WBGT = índice de bulbo úmido e termômetro de globo" if spec == "WBGT"
                   else "Ta = temperatura do ar; UR = umidade relativa; Rad = radiação solar")
        note(doc,
             f"{env_txt}. Preditores contínuos centrados na média da amostra; intercepto = valor esperado nas condições "
             "médias, fase de grupos, período da tarde. Diferença de ranking FIFA = ranking da seleção − ranking do adversário "
             "(valores positivos = seleção pior ranqueada). Densidade e clustering: modelo Gaussiano, β = variação absoluta por "
             "unidade do preditor. Centralizações de saída e de entrada: modelo Gama com ligação log; distância média: modelo t "
             "de Student sobre log(distância); para esses, exp(β) = razão multiplicativa por unidade do preditor (1 = sem efeito). "
             "Efeitos aleatórios: intercepto por seleção em todas as variáveis (o intercepto por jogo teve variância nula). "
             "Densidade e centralização de saída com variância residual estimada separadamente por fase do torneio. "
             "R² de Nakagawa & Schielzeth (2013); para densidade, centralização de saída e distância média, calculado "
             "manualmente na escala do preditor linear. Negrito = p < 0,05 (exceto intercepto).")

    # ---------------- Tabela 4: especificação e pressupostos ----------------
    doc.add_page_break()
    caption(doc, 4, "Especificação dos modelos e verificação dos pressupostos (valores de p; p < 0,05 indica violação).")
    heads = ["Variável", "Modelo", "Distribuição", "Efeitos aleatórios", "Uniformidade", "Dispersão", "Outliers",
             "Quantis", "Resíd. × ambiente", "Homog. variância", "Normalidade EA", "VIF máx."]
    widths = [2.6, 1.4, 3.4, 2.3, 2.3, 1.8, 1.7, 1.6, 2.1, 2.0, 2.3, 1.5]
    t = new_table(doc, 1 + len(fit), len(heads), widths)
    for j, hd in enumerate(heads):
        set_cell(t.rows[0].cells[j], hd, bold=True, size=8)
    i = 1
    for dv in DVS:
        for spec in ["WBGT", "Ta_RH_Rad"]:
            f = fit[(fit.dv == dv) & (fit.weather_spec == spec)].iloc[0]
            fam = FAMILY_LABEL[f.family] + (" + var. por fase" if "tournament_stage" in str(f.dispformula) else "")
            vals = [DV_LABEL[dv], "A" if spec == "WBGT" else "B", fam, RE_LABEL[f.re_structure],
                    num(f.dharma_uniformity_p), num(f.dharma_dispersion_p), num(f.dharma_outliers_boot_p),
                    num(f.dharma_quantile_p), num(f.residuos_vs_ambiente_min_p), num(f.fligner_cat_min_p),
                    num(f.ranef_team_shapiro_p), num(f.max_vif, 2)]
            for j, v in enumerate(vals):
                set_cell(t.rows[i].cells[j], v, size=8,
                         align=WD_ALIGN_PARAGRAPH.LEFT if j in (0, 2, 3) else WD_ALIGN_PARAGRAPH.CENTER)
            i += 1
    borders(t, [0])
    note(doc,
         "Resíduos simulados (DHARMa, 1000 simulações): uniformidade (Kolmogorov-Smirnov), dispersão, outliers "
         "(bootstrap) e homogeneidade dos quantis; Resíd. × ambiente = menor p do teste de quantis dos resíduos contra "
         "cada variável ambiental; Homog. variância = menor p do teste de Fligner-Killeen dos resíduos entre fases do "
         "torneio e entre períodos do jogo; Normalidade EA = Shapiro-Wilk dos interceptos aleatórios por seleção; "
         "VIF = fator de inflação da variância. Os modelos originais de densidade (Gaussiano com variância única) e de "
         "distância média (Gama) violaram pressupostos e foram substituídos pelas especificações acima, selecionadas por "
         "AIC e diagnóstico de resíduos; na centralização de saída, a Gama com variância única violou a homogeneidade "
         "de variância (p = 0,007) e foi adotada a variância residual por fase.")

    # ---------------- Tabela 5: sensibilidade ----------------
    doc.add_page_break()
    caption(doc, 5, "Análise de sensibilidade: estimativas dos efeitos ambientais reajustando cada modelo sem um jogo de cada vez (65 reajustes).")
    heads = ["Variável", "Modelo", "Preditor", "Estimativa (modelo completo)", "p (modelo completo)",
             "Faixa da estimativa", "Faixa de p", "Mudanças de significância"]
    widths = [3.0, 1.6, 4.4, 3.2, 2.4, 4.2, 3.4, 3.2]
    order = [(dv, spec, term) for dv in DVS for spec, terms in
             [("WBGT", ["WBGT_cen"]), ("Ta_RH_Rad", ["Ta_cen", "RH_cen", "RAD_cen"])] for term in terms]
    t = new_table(doc, 1 + len(order), len(heads), widths)
    for j, hd in enumerate(heads):
        set_cell(t.rows[0].cells[j], hd, bold=True, size=8)
    for i, (dv, spec, term) in enumerate(order, start=1):
        rr = loo_all[(loo_all.dv == dv) & (loo_all.weather_spec == spec) & (loo_all.term == term)]
        full_est, full_p = rr.full_estimate.iloc[0], rr.full_p.iloc[0]
        valid = rr[rr.converged].dropna(subset=["p"])   # only refits that converged
        flips = int(((valid.p < 0.05) != (full_p < 0.05)).sum())
        sig_full = full_p < 0.05
        vals = [DV_LABEL[dv], "A" if spec == "WBGT" else "B", TERM_LABEL[term],
                num(full_est, 4), pval(full_p),
                f"{num(valid.estimate.min(), 4)} a {num(valid.estimate.max(), 4)}",
                f"{pval(valid.p.min())} a {pval(valid.p.max())}",
                f"{flips} de {len(valid)}" + (" *" if len(valid) < len(rr) else "")]
        for j, v in enumerate(vals):
            set_cell(t.rows[i].cells[j], v, size=8, bold=(j >= 3 and sig_full),
                     align=WD_ALIGN_PARAGRAPH.LEFT if j in (0, 2) else WD_ALIGN_PARAGRAPH.CENTER)
    borders(t, [0])
    note(doc,
         "Estimativas na escala do preditor linear (para centralização e distância média, log). Mudanças de "
         "significância = número de reajustes em que o preditor passou a ter p < 0,05 (ou deixou de ter) em relação ao "
         "modelo completo. * Na distância média, o reajuste sem o jogo 60 não convergiu (Modelos A e B) e foi excluído; "
         "os 64 reajustes restantes não mudaram a significância. Negrito = efeito "
         "significativo no modelo completo.")

    TAB.mkdir(parents=True, exist_ok=True)
    doc.save(OUT)
    print(f"Wrote {OUT}")


if __name__ == "__main__":
    main()
