#!/usr/bin/env python3
"""
Builds `dados/dataset_modelos_copa2026_novo.csv` — same join as
`montar_dataset_modelos.py`, but reading the SNA metrics computed on the
NEW dataset (`etapa2_dados_tratados/novo-dataset/`, 11x11 matrices where each
substitute's passes are merged into the position of the starter they
replaced), i.e. `etapa2_dados_tratados/resultados_copa2026_novo.csv`.

The context side (match/team/weather columns) comes from
`comum/contexto_jogos.load_context`, shared with stage 1.

Run from `modelos/`:  python3 montar_dataset_modelos_novo.py
"""

import sys
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "comum"))
from contexto_jogos import PLAYER_MATCH_CSV, load_context  # noqa: E402

# DVs of the v2 models; centralizacao_grau (given + received) is kept only for
# the v1 (_novo) scripts -- the analysis now uses the out/in split
SNA_DV_COLS = [
    "densidade_ponderada", "clustering_rede_ponderado",
    "centralizacao_grau", "distancia_media_ponderada",
    "centralizacao_grau_saida", "centralizacao_grau_entrada",
]

HERE = Path(__file__).parent
SNA_CSV = HERE / ".." / "resultados_copa2026_novo.csv"   # etapa2_dados_tratados/
OUT_CSV = HERE / "dados" / "dataset_modelos_copa2026_novo.csv"


def load_sna(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path).rename(columns={"jogo": "match_number", "time": "team"})
    keep = ["match_number", "team", "n_jogadores", "total_passes_na_rede", *SNA_DV_COLS, "erro"]
    return df[keep]


def main():
    if OUT_CSV.exists():
        print(f"{OUT_CSV.name} already exists -- delete or rename it before re-running.")
        return 1

    sna = load_sna(SNA_CSV)
    ctx = load_context(PLAYER_MATCH_CSV)
    print(f"SNA metrics:      {len(sna)} team-match rows ({sna['match_number'].nunique()} matches)")
    print(f"Context (dedup):  {len(ctx)} team-match rows ({ctx['match_number'].nunique()} matches)")

    merged = ctx.merge(sna, on=["match_number", "team"], how="outer", indicator=True)
    only_context = merged[merged["_merge"] == "left_only"]
    only_sna = merged[merged["_merge"] == "right_only"]
    if len(only_context) or len(only_sna):
        print(f"  WARNING: {len(only_context)} context rows without SNA, "
              f"{len(only_sna)} SNA rows without context:")
        for _, r in pd.concat([only_context, only_sna]).iterrows():
            print(f"     match {int(r['match_number'])}: {r['team']} ({r['_merge']})")

    merged = merged.drop(columns=["_merge"])
    merged = merged.sort_values(["match_number", "side"]).reset_index(drop=True)
    merged.to_csv(OUT_CSV, index=False)
    print(f"\nWrote {OUT_CSV} ({len(merged)} rows x {len(merged.columns)} cols)")

    print("\nDV completeness (non-null):")
    for dv in SNA_DV_COLS:
        print(f"  {dv}: {merged[dv].notna().sum()}/{len(merged)}")
    n_open = merged[merged["roof_type"] == "open"]
    print(f"\nOpen-air subset (roof_type == 'open'): {len(n_open)} team-match rows "
          f"({n_open['match_number'].nunique()} matches)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
