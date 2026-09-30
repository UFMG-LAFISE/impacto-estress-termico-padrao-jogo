"""
Contexto de cada time em cada jogo (fase, horario, estadio, altitude, ranking,
condicoes ambientais etc.), lido do dataset por jogador do projeto
wc2026-physical-performance e reduzido a uma linha por time por jogo.
Usado pelos scripts que montam o dataset dos modelos nas etapas 1 e 2.

So LE o arquivo do outro projeto; nao grava nada la.
"""

from pathlib import Path

import pandas as pd

PLAYER_MATCH_CSV = (Path.home() / "Downloads" / "wc2026-physical-performance"
                    / "pipeline" / "data" / "dataset_player_match.csv")

# context columns to carry over from dataset_player_match.csv, deduplicated
# to team-match grain (deliberately EXCLUDES every player-specific column:
# player_id, jersey, position, age, played_minutes, physical performance,
# and the per-player tactical/technical table — none of those describe the
# team as a whole, and are out of scope for this join).
CONTEXT_COLS = [
    "match_number", "match_id",
    "stage_label", "tournament_stage", "stage_round", "group",
    "date", "kickoff_local", "match_datetime", "time_of_day",
    "stadium", "stadium_real_name", "city", "country",
    "altitude_m", "roof_type", "roof_state", "lat", "lon",
    "half1_end_min", "half2_end_min", "home_score", "away_score",
    "team", "opponent", "side",
    "fifa_rank", "opp_fifa_rank", "rank_diff", "confederation", "opp_confederation",
    "final_placement",
    "air_temp_c", "dew_point_c", "rel_humidity_pct", "abs_humidity_g_m3",
    "solar_radiation", "wind_speed_ms", "nat_wet_bulb_c", "globe_temp_c", "wbgt_c",
    "air_temp_c_adj", "dew_point_c_adj", "rel_humidity_pct_adj", "abs_humidity_g_m3_adj",
    "solar_radiation_adj", "wind_speed_ms_adj", "nat_wet_bulb_c_adj", "globe_temp_c_adj",
    "wbgt_c_adj", "weather_adjustment",
    "travel_km_prev", "days_rest_prev", "delta_Ta_prev", "delta_Ta_prev_adj",
]


def load_context(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path, usecols=CONTEXT_COLS)

    # sanity check: every one of these columns must be constant within a
    # (match_number, team) group before we collapse player rows down to one
    # team-match row -- if not, drop_duplicates would silently keep an
    # arbitrary row and hide a real data problem.
    n_before = df.groupby(["match_number", "team"]).ngroups
    dedup = df.drop_duplicates(["match_number", "team"])
    reconstructed = df.merge(dedup, on=["match_number", "team"], suffixes=("", "_dedup"))
    mismatches = []
    for col in CONTEXT_COLS:
        if col in ("match_number", "team"):
            continue
        left, right = reconstructed[col], reconstructed[f"{col}_dedup"]
        bad = ~(left.eq(right) | (left.isna() & right.isna()))
        if bad.any():
            mismatches.append((col, int(bad.sum())))
    if mismatches:
        raise ValueError(
            "context columns are NOT constant within a team-match (dedup would "
            f"silently pick an arbitrary value): {mismatches}"
        )
    assert dedup.groupby(["match_number", "team"]).ngroups == n_before
    return dedup
