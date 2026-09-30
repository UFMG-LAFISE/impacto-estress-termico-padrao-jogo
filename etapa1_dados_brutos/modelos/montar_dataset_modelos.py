#!/usr/bin/env python3
"""
Builds `dados/dataset_modelos_copa2026.csv` — one row per TEAM-MATCH, joining the
passing-network (social network analysis) metrics computed in the sibling
project `copa2026-sna/` with this project's match/team/weather context.

Why this exists as a separate script (not folded into `build_dataset.py`):
the passing-network metrics come from a DIFFERENT source pipeline (SNA
extraction of the "Passing Networks" page, which `parse_tactical.py`
explicitly skips — see its docstring) and live at a different grain
(team-match, not player-match). This script does NOT touch
`dataset_player_match.csv` or any existing pipeline step; it only reads
from it (already-consolidated match/team/weather context, deduplicated
down to one row per team-match) and joins in the SNA metrics.

Inputs:
  - ../resultados_copa2026.csv      (copa2026-sna/; 4 SNA metrics, team x match)
  - ~/Downloads/wc2026-physical-performance/pipeline/data/dataset_player_match.csv
                                    (context, deduplicated)

Output:
  - dados/dataset_modelos_copa2026.csv
  - prints a join/QC report (row counts, unmatched keys, NA pattern)

Run from `modelos/`:  python3 montar_dataset_modelos.py
"""

import re
import sys
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "comum"))
from contexto_jogos import PLAYER_MATCH_CSV, load_context  # noqa: E402

HERE = Path(__file__).parent
SNA_CSV = HERE / ".." / "resultados_copa2026.csv"   # copa2026-sna/
OUT_CSV = HERE / "dados" / "dataset_modelos_copa2026.csv"

SNA_DV_COLS = [
    "densidade_ponderada", "clustering_rede_ponderado",
    "centralizacao_grau", "distancia_media_ponderada",
]


def extract_match_number(arquivo: str) -> int:
    m = re.search(r"M(\d+)", arquivo)
    if not m:
        raise ValueError(f"could not extract match number from '{arquivo}'")
    return int(m.group(1))


def load_sna(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    df["match_number"] = df["arquivo"].apply(extract_match_number)
    df = df.rename(columns={"time": "team"})
    # rows where extract_match() failed for the whole PDF (e.g. match 10, the
    # image-only report) log ONE row with no team identified -- not a real
    # team-match observation, drop before joining so it doesn't show up as a
    # spurious unmatched key.
    df = df[df["team"].notna()]
    keep = ["match_number", "team", "n_jogadores", "total_passes_na_rede", *SNA_DV_COLS, "erro"]
    return df[keep]


def main():
    sna = load_sna(SNA_CSV)
    ctx = load_context(PLAYER_MATCH_CSV)

    print(f"SNA metrics:      {len(sna)} team-match rows ({sna['match_number'].nunique()} matches)")
    print(f"Context (dedup):  {len(ctx)} team-match rows ({ctx['match_number'].nunique()} matches)")

    # left join on context (keep every team-match that exists in the physical
    # study's dataset, even if SNA extraction failed for it -- e.g. match 10,
    # the image-only PDF -- so the gap is visible as NA, not a silently
    # missing row)
    merged = ctx.merge(sna, on=["match_number", "team"], how="left", indicator=True)

    only_context = merged[merged["_merge"] == "left_only"]
    only_sna_keys = set(zip(sna["match_number"], sna["team"])) - set(zip(ctx["match_number"], ctx["team"]))

    print(f"\nJoined:           {len(merged)} team-match rows")
    if len(only_context):
        print(f"  -> {len(only_context)} team-match rows have NO SNA metrics "
              f"(expected: match 10, Germany-Curaçao, image-only PDF):")
        for _, r in only_context.iterrows():
            print(f"     match {int(r['match_number'])}: {r['team']}")
    if only_sna_keys:
        print(f"  WARNING: {len(only_sna_keys)} SNA rows had no matching context row "
              f"(unexpected -- check team-name spelling): {sorted(only_sna_keys)}")

    n_erro = merged["erro"].notna().sum() if "erro" in merged else 0
    if n_erro:
        print(f"  -> {n_erro} rows carry an SNA extraction error message (see 'erro' column)")

    merged = merged.drop(columns=["_merge"])
    merged = merged.sort_values(["match_number", "side"]).reset_index(drop=True)

    OUT_CSV.parent.mkdir(exist_ok=True)
    merged.to_csv(OUT_CSV, index=False)
    print(f"\nWrote {OUT_CSV} ({len(merged)} rows x {len(merged.columns)} cols)")

    print("\nDV completeness (non-null):")
    for dv in SNA_DV_COLS:
        n = merged[dv].notna().sum()
        print(f"  {dv}: {n}/{len(merged)}")

    n_open = merged[merged["roof_type"] == "open"]
    print(f"\nOpen-air subset (roof_type == 'open'): {len(n_open)} team-match rows "
          f"({n_open['match_number'].nunique()} matches), "
          f"{n_open['densidade_ponderada'].notna().sum()} with SNA metrics present")


if __name__ == "__main__":
    sys.exit(main())
