#!/usr/bin/env python3
"""
Uso:
    venv/bin/python extrair_estatisticas_jogo.py

Extrai a pagina "Match Summary - Key Statistics" (pagina 3) de cada relatorio
PMSR da FIFA e grava `estatisticas_jogo_copa2026.csv`: uma linha por time por
jogo, com todas as estatisticas da pagina (posse, gols, xG, finalizacoes,
passes, line breaks, pressoes, distancia total, distancia na zona 4 etc.).

O time da esquerda e o da direita sao identificados pela posicao (x) do nome na
pagina, e os nomes sao trocados pelo nome oficial usado nos modelos
(dataset por jogador do wc2026-physical-performance, com acentos -- o mesmo
insumo do contexto dos modelos). Cada rotulo da
tabela e procurado explicitamente: se algum nao for encontrado, o erro vai para
a coluna `erro` daquele jogo e o script segue -- nao grava valor adivinhado.

A zona 5 (>25 km/h) NAO esta nesta pagina; so aparece nas tabelas por jogador
("Physical Data"), sem total do time.
"""

import csv
import logging
import re
import sys
from pathlib import Path

import pdfplumber

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "comum"))
from contexto_jogos import PLAYER_MATCH_CSV  # noqa: E402

logging.getLogger("pdfminer").setLevel(logging.ERROR)

BASE_DIR = Path(__file__).parent
REPORTS_DIR = Path.home() / "Documents" / "analises-grafos-copa" / "Technical Reports" / "Technical Reports"
CSV_PATH = BASE_DIR / "estatisticas_jogo_copa2026.csv"
TEAMS_CSV = PLAYER_MATCH_CSV   # nomes oficiais dos times (com acento), por jogo

NUM = r"(-?\d+(?:\.\d+)?)"
# rotulo da pagina -> (colunas, regex da linha "valor_esq ROTULO valor_dir")
ROWS = [
    (("gols",), rf"^{NUM} Goals {NUM}$"),
    (("xg",), rf"^{NUM} xG \(Expected Goals\) {NUM}$"),
    (("finalizacoes", "finalizacoes_no_alvo"), rf"^{NUM} \({NUM}\) Attempts at Goal \(On Target\) {NUM} \({NUM}\)$"),
    (("passes_total", "passes_completos"), rf"^{NUM} \({NUM}\) Total Passes \(Complete\) {NUM} \({NUM}\)$"),
    (("acerto_passe_pct",), rf"^{NUM} ?% Pass Completion % {NUM} ?%$"),
    (("line_breaks_completos",), rf"^{NUM} Completed Line Breaks {NUM}$"),
    (("line_breaks_defensivos",), rf"^{NUM} Defensive Line Breaks {NUM}$"),
    (("recepcoes_ultimo_terco",), rf"^{NUM} Receptions in the Final Third {NUM}$"),
    (("cruzamentos",), rf"^{NUM} Crosses {NUM}$"),
    (("progressoes_bola",), rf"^{NUM} Ball Progressions {NUM}$"),
    (("pressoes_defensivas", "pressoes_diretas"),
     rf"^{NUM} \({NUM}\) Defensive Pressures Applied \(Direct Pressures\) {NUM} \({NUM}\)$"),
    (("recuperacoes_forcadas",), rf"^{NUM} Forced Turnovers {NUM}$"),
    (("segundas_bolas",), rf"^{NUM} Second Balls {NUM}$"),
    (("distancia_total_km",), rf"^{NUM} km Total Distance Covered {NUM} km$"),
    (("distancia_zona4_km",), rf"^{NUM} km Zone 4 . Low Speed Sprinting: 20-25 km/h {NUM} km$"),
]
STAT_COLS = ["posse_pct", "posse_em_disputa_pct"] + [c for cols, _ in ROWS for c in cols]
FIELDS = ["jogo", "time", "adversario", "lado", *STAT_COLS, "arquivo", "erro"]


def normalize(name: str) -> str:
    return re.sub(r"[^a-z]", "", name.lower())


def find_page(pdf):
    for i, page in enumerate(pdf.pages[:6]):
        if "Key Statistics" in (page.extract_text() or ""):
            return page
    raise ValueError("pagina 'Key Statistics' nao encontrada (PDF so imagem?)")


def team_names(page, official):
    """Nomes do time da esquerda e da direita: palavras da linha do placar,
    separadas pelo meio da pagina, casadas com os nomes oficiais do jogo."""
    words = page.extract_words()
    title = next(w for w in words if w["text"] == "Summary")
    # a linha dos nomes fica logo abaixo do titulo e acima de "Possession"
    poss = next(w for w in words if w["text"] == "Possession")
    band = [w for w in words if title["bottom"] < w["top"] < poss["top"] and not re.fullmatch(r"[\d–-]+", w["text"])]
    mid = page.width / 2
    left = normalize(" ".join(w["text"] for w in band if w["x1"] < mid))
    right = normalize(" ".join(w["text"] for w in band if w["x0"] > mid))
    out = []
    for side in (left, right):
        match = [t for t in official if normalize(t) == side]
        if len(match) != 1:
            raise ValueError(f"time '{side}' nao casa com os nomes oficiais do jogo {official}")
        out.append(match[0])
    return out


def parse_stats(text):
    lines = [l.strip() for l in text.splitlines()]
    left, right = {}, {}
    m = next((re.match(rf"^Total {NUM}% {NUM}% {NUM}% Total$", l) for l in lines
              if re.match(rf"^Total {NUM}% {NUM}% {NUM}% Total$", l)), None)
    if not m:
        raise ValueError("linha de posse nao encontrada")
    left["posse_pct"], mid, right["posse_pct"] = map(float, m.groups())
    left["posse_em_disputa_pct"] = right["posse_em_disputa_pct"] = mid
    for cols, pattern in ROWS:
        hits = [re.match(pattern, l) for l in lines if re.match(pattern, l)]
        if len(hits) != 1:
            raise ValueError(f"linha de {cols[0]} encontrada {len(hits)} vez(es)")
        vals = [float(v) for v in hits[0].groups()]
        k = len(cols)
        for j, c in enumerate(cols):
            left[c], right[c] = vals[j], vals[k + j]
    return left, right


def main() -> int:
    if CSV_PATH.exists():
        print(f"{CSV_PATH.name} ja existe -- apague ou renomeie antes de rodar de novo.")
        return 1
    teams = {}
    with open(TEAMS_CSV, encoding="utf-8") as f:
        for row in csv.DictReader(f):
            teams.setdefault(int(row["match_number"]), set()).add(row["team"])

    out = []
    for path in sorted(REPORTS_DIR.glob("*.pdf"), key=lambda p: int(re.search(r"M(\d+)", p.name).group(1))):
        jogo = int(re.search(r"M(\d+)", path.name).group(1))
        try:
            with pdfplumber.open(path) as pdf:
                page = find_page(pdf)
                t_left, t_right = team_names(page, sorted(teams[jogo]))
                s_left, s_right = parse_stats(page.extract_text())
            for time, adv, lado, s in [(t_left, t_right, "esquerda", s_left), (t_right, t_left, "direita", s_right)]:
                out.append({"jogo": jogo, "time": time, "adversario": adv, "lado": lado, **s,
                            "arquivo": path.name, "erro": ""})
        except Exception as e:
            out.append({"jogo": jogo, "arquivo": path.name, "erro": str(e)})

    with open(CSV_PATH, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=FIELDS)
        writer.writeheader()
        writer.writerows(out)
    erros = [r for r in out if r["erro"]]
    print(f"{len(out)} linhas gravadas em {CSV_PATH.name} ({len(erros)} com erro)")
    for r in erros:
        print(f"  jogo {r['jogo']} ({r['arquivo']}): {r['erro']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
