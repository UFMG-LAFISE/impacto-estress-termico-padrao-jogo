#!/usr/bin/env python3
"""
Uso:
    venv/bin/python calcular_metricas_novo.py

Calcula as metricas macro (metrics.py) sobre as matrizes 11x11 do
novo dataset (`novo-dataset/01_matrizes_passing_networks/`), em que os
passes de cada reserva ja estao somados na posicao do titular que ele
substituiu. Grava em `resultados_copa2026_novo.csv` -- NAO mexe em
os resultados da etapa 1.

Alem das 4 metricas de `compute_all`, calcula a centralizacao de grau
separada em SAIDA (passes dados, Def. 6.34 + Def. 4.7) e ENTRADA (passes
recebidos, Def. 6.34 aplicada a Def. 4.25) -- as duas usadas nos modelos
v2. `centralizacao_grau` (dados + recebidos) continua no CSV so para os
scripts da versao _novo (v1); nao e mais usada na analise.

O nome do time (`time`) vem do dataset por jogador do projeto
wc2026-physical-performance (`comum/contexto_jogos.PLAYER_MATCH_CSV`, o mesmo
insumo do contexto dos modelos), pra casar exatamente com ele na hora de
juntar com as variaveis ambientais (os nomes de arquivo perdem acentos:
"C_te_d_Ivoire", "Cura_ao").

Se uma matriz falhar nas conferencias (11x11, diagonal vazia, totais de
linha/coluna), o erro vai pra coluna `erro` e o script segue pros outros.
"""

import csv
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "comum"))
import metrics as mx  # noqa: E402
from contexto_jogos import PLAYER_MATCH_CSV  # noqa: E402

BASE_DIR = Path(__file__).parent
MATRIZES_DIR = BASE_DIR / "novo-dataset" / "01_matrizes_passing_networks"
CSV_PATH = BASE_DIR / "resultados_copa2026_novo.csv"
TEAMS_CSV = PLAYER_MATCH_CSV   # nomes oficiais dos times (com acento), por jogo
FIELDS = [
    "jogo", "time", "arquivo_matriz",
    "n_jogadores", "total_passes_na_rede",
    "densidade_ponderada", "clustering_rede_ponderado",
    "centralizacao_grau", "distancia_media_ponderada",
    "centralizacao_grau_saida", "centralizacao_grau_entrada",
    "erro",
]


def normalize(name: str) -> str:
    """So letras ASCII minusculas -- 'Côte d'Ivoire' e 'C_te_d_Ivoire' viram 'ctedivoire'."""
    return re.sub(r"[^a-z]", "", name.lower())


def load_matrix(path: Path) -> dict:
    """Le a matriz no formato do novo dataset e confere a estrutura antes
    de calcular qualquer coisa -- trava ruidosamente em vez de adivinhar."""
    with open(path, encoding="utf-8") as f:
        rows = [r for r in csv.reader(f) if any(c.strip() for c in r)]
    header, body, col_totals = rows[0], rows[1:-1], rows[-1]
    players = header[1:-1]
    if header[0] != "" or header[-1] != "TOTAL_DADO" or col_totals[0] != "TOTAL_RECEBIDO":
        raise ValueError("cabecalho/rodape fora do formato esperado")
    if len(players) != 11 or len(body) != 11:
        raise ValueError(f"matriz nao e 11x11 ({len(body)} linhas x {len(players)} colunas)")
    if [r[0] for r in body] != players:
        raise ValueError("nomes das linhas nao batem com os das colunas")

    matrix = {}
    for i, r in enumerate(body):
        if len(r) != len(header):
            raise ValueError(f"linha de {r[0]} tem {len(r)} celulas, esperado {len(header)}")
        if r[1 + i] != "":
            raise ValueError(f"diagonal preenchida na linha de {r[0]}")
        vals = {players[j]: int(x) for j, x in enumerate(r[1:-1]) if j != i}
        if sum(vals.values()) != int(r[-1]):
            raise ValueError(f"soma da linha de {r[0]} nao bate com TOTAL_DADO")
        matrix[r[0]] = vals
    for j, p in enumerate(players):
        if sum(matrix[src].get(p, 0) for src in players) != int(col_totals[1 + j]):
            raise ValueError(f"soma da coluna de {p} nao bate com TOTAL_RECEBIDO")
    return matrix


def main() -> int:
    if CSV_PATH.exists():
        print(f"{CSV_PATH.name} ja existe -- apague ou renomeie antes de rodar de novo.")
        return 1

    teams = {}  # (jogo, nome normalizado) -> nome oficial
    with open(TEAMS_CSV, encoding="utf-8") as f:
        for row in csv.DictReader(f):
            teams[(int(row["match_number"]), normalize(row["team"]))] = row["team"]

    out = []
    for path in sorted(MATRIZES_DIR.glob("*.csv")):
        # time = o que vem depois de "__" (ou de "-V2_" nos arquivos do jogo 10)
        jogo = int(re.match(r"PMSR-M(\d+)", path.name).group(1))
        team_file = re.search(r"(?:__|-V2_)(.+)$", path.stem).group(1)
        team_key = normalize(team_file)
        row = {"jogo": jogo, "time": teams.get((jogo, team_key), team_file),
               "arquivo_matriz": str(path.relative_to(BASE_DIR)), "erro": ""}
        try:
            if (jogo, team_key) not in teams:
                raise ValueError("time nao encontrado no dataset_player_match.csv")
            matrix = load_matrix(path)
            row.update(mx.compute_all(matrix))
            G = mx.build_digraph(matrix)
            row["centralizacao_grau_saida"] = round(mx.degree_centralization_out(G), 4)
            row["centralizacao_grau_entrada"] = round(mx.degree_centralization_in(G), 4)
        except Exception as e:
            row["erro"] = str(e)
        out.append(row)

    out.sort(key=lambda r: (r["jogo"], r["time"]))
    with open(CSV_PATH, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=FIELDS)
        writer.writeheader()
        writer.writerows(out)

    erros = [r for r in out if r["erro"]]
    print(f"{len(out)} times gravados em {CSV_PATH.name} ({len(erros)} com erro)")
    for r in erros:
        print(f"  jogo {r['jogo']} {r['time']}: {r['erro']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
