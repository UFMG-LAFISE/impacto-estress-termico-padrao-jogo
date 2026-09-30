#!/usr/bin/env python3
"""
Uso:
    python3 analisar_jogo.py relatorio1.pdf [relatorio2.pdf ...]
    python3 analisar_jogo.py pasta_com_pdfs/*.pdf

Extrai a matriz de passes de cada relatorio PMSR da FIFA (paginas
"Passing Networks"), calcula as 4 metricas macro escolhidas pra comparar
padrao de jogo entre selecoes, e ACUMULA o resultado em
`resultados_copa2026.csv` (uma linha por time por jogo -- roda de novo
pra cada novo relatorio que sair durante o torneio, sem apagar o que ja
tem).

Alem disso, salva a matriz crua (NxN, no mesmo formato da tabela do PDF)
de cada time em `matrizes/<arquivo>__<time>.csv`, pra conferencia manual
linha a linha contra o relatorio original.

Se um PDF falhar (layout inesperado, pagina faltando etc.), o erro fica
registrado na coluna `erro` da mesma tabela e o script continua pros
proximos arquivos -- nao para o lote inteiro por causa de 1 jogo com
problema.
"""

import csv
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "comum"))
import extract as ex  # noqa: E402
import metrics as mx  # noqa: E402

BASE_DIR = Path(__file__).parent
CSV_PATH = BASE_DIR / "resultados_copa2026.csv"
MATRIZES_DIR = BASE_DIR / "matrizes"
FIELDS = [
    "arquivo", "pagina", "time",
    "n_jogadores", "total_passes_na_rede",
    "densidade_ponderada", "clustering_rede_ponderado",
    "centralizacao_grau", "distancia_media_ponderada",
    "arquivo_matriz",
    "erro",
]


def load_existing_keys() -> set:
    """(arquivo, time) ja processados, pra nao duplicar se rodar 2x o mesmo pdf."""
    if not CSV_PATH.exists():
        return set()
    with open(CSV_PATH, newline="", encoding="utf-8") as f:
        return {(row["arquivo"], row["time"]) for row in csv.DictReader(f)}


def _safe_name(text: str) -> str:
    return re.sub(r"[^A-Za-z0-9_-]+", "_", text).strip("_")


def analisar_um_pdf(pdf_path: Path, existing_keys: set) -> list:
    rows = []
    try:
        teams = ex.extract_match(str(pdf_path))
    except Exception as e:
        rows.append({
            "arquivo": pdf_path.name, "pagina": "", "time": "",
            "n_jogadores": "", "total_passes_na_rede": "",
            "densidade_ponderada": "", "clustering_rede_ponderado": "",
            "centralizacao_grau": "", "distancia_media_ponderada": "",
            "arquivo_matriz": "",
            "erro": f"falha na extracao: {e}",
        })
        return rows

    for t in teams:
        key = (pdf_path.name, t.team)
        if key in existing_keys:
            print(f"  [ja processado, pulando] {pdf_path.name} / {t.team}")
            continue

        row = {"arquivo": pdf_path.name, "pagina": t.page_number, "time": t.team, "erro": ""}

        matrix_filename = f"{_safe_name(pdf_path.stem)}__{_safe_name(t.team)}.csv"
        matrix_path = MATRIZES_DIR / matrix_filename
        try:
            ex.save_matrix_csv(t, str(matrix_path))
            row["arquivo_matriz"] = f"matrizes/{matrix_filename}"
        except Exception as e:
            row["arquivo_matriz"] = ""
            row["erro"] = f"falha ao salvar csv da matriz: {e}"

        try:
            row.update(mx.compute_all(t.matrix))
        except Exception as e:
            existing_err = row["erro"]
            row["erro"] = (existing_err + " | " if existing_err else "") + f"falha no calculo das metricas: {e}"

        rows.append(row)
    return rows


def main(pdf_paths: list):
    MATRIZES_DIR.mkdir(exist_ok=True)
    existing_keys = load_existing_keys()
    write_header = not CSV_PATH.exists()

    n_ok = 0
    n_erro = 0

    with open(CSV_PATH, "a", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=FIELDS)
        if write_header:
            writer.writeheader()

        for i, raw_path in enumerate(pdf_paths, 1):
            pdf_path = Path(raw_path)
            print(f"\n[{i}/{len(pdf_paths)}] Processando {pdf_path.name} ...")
            rows = analisar_um_pdf(pdf_path, existing_keys)
            for row in rows:
                writer.writerow(row)
                f.flush()
                if row["erro"]:
                    n_erro += 1
                    print(f"  [ERRO] {row['time'] or '(time nao identificado)'}: {row['erro']}")
                else:
                    n_ok += 1
                    print(f"  OK: {row['time']} -> densidade={row['densidade_ponderada']}, "
                          f"clustering={row['clustering_rede_ponderado']}, "
                          f"centralizacao={row['centralizacao_grau']}, "
                          f"distancia={row['distancia_media_ponderada']}")

    print(f"\n{'='*60}")
    print(f"Concluido: {n_ok} times processados com sucesso, {n_erro} com erro.")
    print(f"Resultados: {CSV_PATH}")
    print(f"Matrizes individuais: {MATRIZES_DIR}/")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    main(sys.argv[1:])
