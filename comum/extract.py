"""
Extrai a matriz de passes (rede de passes) das paginas "Passing Networks"
de um relatorio FIFA PMSR em PDF, usando a posicao geometrica (x, y) de
cada palavra na pagina -- nao regex sobre texto quebrado em linhas.

Por que por coordenada e nao por texto: o pdftotext (e qualquer extracao
"linear" de texto) pode embaralhar colunas quando ha uma tabela lateral
("Top 5 Passers") do lado da matriz principal, e a diagonal em branco
(jogador consigo mesmo) desloca a contagem de valores por linha. Usar
x0/x1 real de cada palavra elimina os dois problemas: cada numero e
associado a coluna cujo centro esta mais perto dele, nao a "a n-esima
posicao da linha".
"""

import csv
import logging
import re
from dataclasses import dataclass, field

import pdfplumber

logging.getLogger("pdfminer").setLevel(logging.ERROR)  # silencia avisos de FontBBox


@dataclass
class TeamMatrix:
    team: str
    page_number: int  # 1-indexed, como aparece no leitor de PDF
    players: list       # nomes completos, na ordem das colunas/linhas
    jersey: dict         # nome completo -> numero da camisa
    matrix: dict          # nome completo -> {nome completo -> passes (int)}


def save_matrix_csv(team_matrix: "TeamMatrix", output_path: str) -> None:
    """Salva a matriz NxN extraida como CSV, no mesmo formato da tabela do
    PDF (linha = quem deu o passe, coluna = quem recebeu), com o numero
    da camisa no cabecalho e totais de linha/coluna pra conferencia rapida
    -- e pra comparar direto com a pagina 'Passing Networks' do relatorio."""
    players = team_matrix.players
    header = [""] + [f"{team_matrix.jersey.get(p, '')} {p}".strip() for p in players] + ["TOTAL_DADO"]

    with open(output_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(header)
        col_totals = {p: 0 for p in players}
        for src in players:
            row_vals = [team_matrix.matrix[src][dst] if dst != src else "" for dst in players]
            row_total = sum(v for v in row_vals if v != "")
            for dst in players:
                if dst != src:
                    col_totals[dst] += team_matrix.matrix[src][dst]
            label = f"{team_matrix.jersey.get(src, '')} {src}".strip()
            writer.writerow([label] + row_vals + [row_total])
        writer.writerow(
            ["TOTAL_RECEBIDO"] + [col_totals[p] for p in players] + [sum(col_totals.values())]
        )


def find_passing_network_pages(pdf_path: str) -> list:
    """Devolve os numeros de pagina (1-indexed) que contem a tabela
    'Passing Networks' -- normalmente 2 paginas por partida, uma por time."""
    pages_found = []
    with pdfplumber.open(pdf_path) as pdf:
        for i, page in enumerate(pdf.pages):
            text = page.extract_text() or ""
            if "Passing Networks" in text and "Passes From" in text:
                pages_found.append(i + 1)
    return pages_found


def _cluster_x(values, gap=8.0):
    """Agrupa uma lista de coordenadas x (centros) em clusters, unindo
    tudo que esta a menos de `gap` pontos de distancia. Devolve a lista
    de centros de cluster, ordenada da esquerda pra direita."""
    values = sorted(values)
    clusters = []
    current = [values[0]]
    for v in values[1:]:
        if v - current[-1] <= gap:
            current.append(v)
        else:
            clusters.append(sum(current) / len(current))
            current = [v]
    clusters.append(sum(current) / len(current))
    return clusters


def extract_team_matrix(pdf_path: str, page_number: int) -> TeamMatrix:
    with pdfplumber.open(pdf_path) as pdf:
        page = pdf.pages[page_number - 1]
        # x_tolerance baixo: por padrao (3pt) o pdfplumber as vezes funde
        # 2-3 sobrenomes vizinhos num so token quando o espacamento entre
        # colunas fica apertado (ex: "GALLARDOALVARADOGUTIERREZ"); 1.5pt
        # evita isso sem quebrar nomes de verdade em letras soltas.
        words = page.extract_words(use_text_flow=False, keep_blank_chars=False, x_tolerance=1.5)

    # ache o titulo "Passing Networks" (ancora vertical) e o nome do time
    # (aparece na mesma linha do titulo, alinhado a direita)
    title_words = [w for w in words if w["text"] in ("Passing", "Networks")]
    if not title_words:
        raise ValueError(f"pagina {page_number} nao parece ter 'Passing Networks'")
    title_top = min(w["top"] for w in title_words)
    team_candidates = [w for w in words if abs(w["top"] - title_top) < 5 and w["text"] not in ("Passing", "Networks")]
    team_name = " ".join(w["text"] for w in sorted(team_candidates, key=lambda w: w["x0"]))

    # a pagina tem uma caixa lateral "Top 5 Player to Player Passers" que
    # ocupa toda a altura da tabela (nao so a linha do titulo) -- os
    # proprios nomes/porcentagens das linhas dessa caixa (ex: "LEE Hanbeom
    # 3.5%") ficam bem mais a esquerda que o titulo "Top 5" (a coluna
    # "Player" da caixa comeca em x=763.6, o titulo em x=810.4). Usar so o
    # titulo como referencia deixa esses nomes vazarem pra dentro da area
    # de dados quando a caixa cresce (times com mais reservas/paginas
    # diferentes). Por isso ancoramos no rotulo mais a esquerda da caixa
    # ("Player"), nao no titulo -- essa posicao e fixa no template do
    # relatorio, testada em varios jogos.
    SIDEBAR_MARKERS = {"Top", "Player", "Passed", "Passers"}
    marker_x0s = [w["x0"] for w in words if w["text"] in SIDEBAR_MARKERS]
    sidebar_x_cutoff = min(marker_x0s, default=10**9) - 5

    # linhas de dados: comecam com um numero de camisa (1-2 digitos) na
    # margem esquerda, seguido do nome do jogador, seguido de numeros.
    # Usamos o padrao "linha inteira comeca com um inteiro isolado bem a
    # esquerda" pra achar onde a tabela de dados comeca verticalmente.
    body_words = [w for w in words if w["top"] > title_top + 30 and w["x0"] < sidebar_x_cutoff]

    # --- passo 1: descobrir os centros de coluna a partir dos VALORES
    # numericos de todas as linhas (16 dos 17 jogadores aparecem em cada
    # linha, entao a uniao cobre as 17 colunas com folga) ---
    numeric_words = [w for w in body_words if re.fullmatch(r"\d+", w["text"])]
    # descarta numeros que sejam o proprio numero de camisa (ficam bem a
    # esquerda, sozinhos antes do nome) -- feito depois de achar linhas.

    # agrupa palavras por linha (mesmo "top", tolerancia pequena)
    rows_by_top = {}
    for w in body_words:
        key = round(w["top"] / 2) * 2  # bucket de 2pt pra tolerar jitter
        rows_by_top.setdefault(key, []).append(w)

    row_tops = sorted(rows_by_top.keys())

    # centros de coluna: usa todos os numeros que NAO sao o primeiro
    # token da linha (primeiro token numerico da linha = camisa)
    col_x_centers_raw = []
    row_records = []  # (top, jersey, name_words, value_words)
    for top in row_tops:
        row_words = sorted(rows_by_top[top], key=lambda w: w["x0"])
        if not row_words:
            continue
        # primeiro token precisa ser um numero (camisa) pra ser linha de dado
        if not re.fullmatch(r"\d+", row_words[0]["text"]):
            continue
        jersey = row_words[0]["text"]
        rest = row_words[1:]
        # nome = tokens ate o primeiro numero puro; valores = daí em diante
        name_tokens = []
        value_tokens = []
        seen_number = False
        for w in rest:
            if not seen_number and re.fullmatch(r"\d+", w["text"]):
                seen_number = True
            if seen_number:
                value_tokens.append(w)
            else:
                name_tokens.append(w)
        if not name_tokens or not value_tokens:
            continue
        row_records.append((top, jersey, name_tokens, value_tokens))
        for w in value_tokens:
            col_x_centers_raw.append((w["x0"] + w["x1"]) / 2)

    if not row_records:
        raise ValueError(f"nenhuma linha de dados encontrada na pagina {page_number}")

    col_centers = _cluster_x(col_x_centers_raw, gap=10.0)

    # --- passo 2: nomes das colunas a partir das linhas de cabecalho
    # (palavras entre o titulo e a primeira linha de dado) ---
    first_data_top = row_records[0][0]
    header_words = [w for w in body_words if w["top"] < first_data_top - 5]
    # ignora a caixa lateral "Top 5 Player to Player Passers"
    header_words = [w for w in header_words if w["x0"] < col_centers[-1] + 40]

    col_name_parts = {i: [] for i in range(len(col_centers))}
    for w in header_words:
        cx = (w["x0"] + w["x1"]) / 2
        idx = min(range(len(col_centers)), key=lambda i: abs(col_centers[i] - cx))
        if abs(col_centers[idx] - cx) > 15:
            continue  # rotulo da tabela ("# Passes From to") ou lixo da caixa lateral, nao e nome de coluna
        col_name_parts[idx].append(w)

    col_names = []
    for i in range(len(col_centers)):
        parts = sorted(col_name_parts[i], key=lambda w: (w["top"], w["x0"]))
        name = " ".join(w["text"] for w in parts).strip()
        col_names.append(name)

    if any(not n for n in col_names):
        missing = [i for i, n in enumerate(col_names) if not n]
        raise ValueError(f"colunas sem nome identificado (indices {missing}) na pagina {page_number}")

    n = len(col_names)
    if len(row_records) != n:
        raise ValueError(
            f"pagina {page_number}: {n} colunas mas {len(row_records)} linhas de dado -- "
            "matriz nao-quadrada, confira manualmente."
        )

    # --- passo 3a: acha a qual coluna cada LINHA corresponde. O nome de
    # cabecalho (col_names) pode vir com espacamento diferente do nome da
    # linha quando o sobrenome tem hifen e a coluna e estreita (ex: o
    # cabecalho quebra "OKON-ENGSTLER" em "OKON" + "ENGSTLER" em linhas
    # diferentes, virando "OKON ENGSTLER" sem o hifen) -- por isso o
    # casamento ignora diferenca de hifen/espaco. O nome final de cada
    # jogador usado na matriz e o da LINHA (sempre em uma linha so, mais
    # confiavel que o cabecalho, que pode quebrar em 2-3 linhas).
    self_idx_by_row = []
    final_names = [None] * n
    jersey_by_col = [None] * n
    for top, jersey, name_tokens, value_tokens in row_records:
        row_name_text = " ".join(w["text"] for w in name_tokens).strip()
        self_idx = _match_player_name(row_name_text, col_names)
        if self_idx is None:
            raise ValueError(f"nao consegui casar o jogador da linha '{row_name_text}' com nenhuma coluna")
        if final_names[self_idx] is not None:
            raise ValueError(
                f"duas linhas casaram com a mesma coluna ('{final_names[self_idx]}' e "
                f"'{row_name_text}') na pagina {page_number} -- confira manualmente."
            )
        final_names[self_idx] = row_name_text
        jersey_by_col[self_idx] = jersey
        self_idx_by_row.append((self_idx, value_tokens))

    players = final_names
    jersey_by_name = {players[i]: jersey_by_col[i] for i in range(n)}
    matrix = {name: {other: 0 for other in players} for name in players}

    # --- passo 3b: agora sim, associa cada valor numerico a coluna mais
    # proxima entre as colunas != a propria (self) ---
    for self_idx, value_tokens in self_idx_by_row:
        row_player = players[self_idx]
        others = [i for i in range(n) if i != self_idx]
        value_tokens_sorted = sorted(value_tokens, key=lambda w: w["x0"])
        if len(value_tokens_sorted) != len(others):
            raise ValueError(
                f"linha de '{row_player}' tem {len(value_tokens_sorted)} valores, "
                f"esperado {len(others)} (n-1). Confira a pagina {page_number} manualmente."
            )
        for col_idx, w in zip(others, value_tokens_sorted):
            cx = (w["x0"] + w["x1"]) / 2
            if abs(col_centers[col_idx] - cx) > 25:
                raise ValueError(
                    f"valor '{w['text']}' da linha '{row_player}' esta longe do centro "
                    f"esperado da coluna '{players[col_idx]}' (diff={abs(col_centers[col_idx]-cx):.1f}pt) "
                    f"-- extracao pode estar desalinhada, confira a pagina {page_number}."
                )
            matrix[row_player][players[col_idx]] = int(w["text"])

    return TeamMatrix(
        team=team_name,
        page_number=page_number,
        players=players,
        jersey=jersey_by_name,
        matrix=matrix,
    )


def _match_player_name(row_name_text: str, col_names: list):
    """Casa o nome de uma linha (pode vir com grafia levemente diferente
    de espacos) com o indice da coluna correspondente."""
    # qualquer caractere nao-alfanumerico vira espaco: cobre hifen normal
    # E os casos em que a fonte do relatorio codifica o hifen como um
    # glifo de area de uso privado do Unicode (ex: "OKONENGSTLER"),
    # que aparece quando o cabecalho quebra um sobrenome-com-hifen em 2
    # linhas ("OKON" / "ENGSTLER") e a reconstrucao junta com esse
    # caractere no meio em vez do hifen visivel da linha de dado.
    norm = lambda s: re.sub(r"[^A-Za-z0-9]+", " ", s).strip().upper()
    target = norm(row_name_text)
    for i, name in enumerate(col_names):
        if norm(name) == target:
            return i
    # fallback: contencao (cobre casos de espacamento diferente)
    for i, name in enumerate(col_names):
        if norm(name) in target or target in norm(name):
            return i
    return None


def extract_match(pdf_path: str) -> list:
    """Extrai as matrizes dos dois times de um relatorio de partida."""
    pages = find_passing_network_pages(pdf_path)
    if len(pages) != 2:
        raise ValueError(
            f"esperava 2 paginas de 'Passing Networks', achei {len(pages)} ({pages}) em {pdf_path}"
        )
    return [extract_team_matrix(pdf_path, p) for p in pages]


if __name__ == "__main__":
    import sys
    import json

    pdf_path = sys.argv[1]
    teams = extract_match(pdf_path)
    for t in teams:
        print(f"\n=== {t.team} (pagina {t.page_number}) ===")
        print(f"{len(t.players)} jogadores: {', '.join(t.players)}")
        total = sum(v for row in t.matrix.values() for v in row.values())
        print(f"soma total de passes na matriz: {total}")
