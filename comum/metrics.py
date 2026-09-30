"""
As 4 metricas macro (nivel de rede/time inteiro) escolhidas pra comparar
padrao de jogo entre selecoes na Copa 2026 -- todas na versao ponderada e
direcionada (peso = numero de passes, direcao = quem deu pra quem), que e
o tipo de grafo real dos dados do relatorio da FIFA.

Correspondencia com Clemente, Martins & Mendes (2016):
  - densidade_ponderada        -> Definicao 6.8  (p. 82)
  - clustering_rede_ponderado  -> Definicao 6.22 (p. 86, dentro da secao 6.5 Clique)
  - centralizacao_grau         -> Definicao 6.34 (p. 90), generalizada pra peso pelo Remark 6.10
  - distancia_media_ponderada  -> Definicao 6.12 (p. 84)
"""

import math

import networkx as nx


def build_digraph(matrix: dict) -> nx.DiGraph:
    """matrix: {jogador_origem: {jogador_destino: numero_de_passes}}"""
    G = nx.DiGraph()
    G.add_nodes_from(matrix.keys())
    for src, row in matrix.items():
        for dst, w in row.items():
            if w and w > 0:
                G.add_edge(src, dst, weight=w)
    return G


def weighted_density(G: nx.DiGraph) -> float:
    """Definicao 6.8: soma de todos os pesos / n(n-1)."""
    n = G.number_of_nodes()
    total_weight = sum(d["weight"] for _, _, d in G.edges(data=True))
    return total_weight / (n * (n - 1))


def whole_network_clustering(G: nx.DiGraph) -> float:
    """Definicao 6.22: media do coeficiente de agrupamento ponderado e
    direcionado de cada jogador (formula de Fagiolo 2007 -- a mesma
    referencia que o livro cita nas Definicoes 4.41-4.43; o
    nx.clustering(G, weight=...) para digrafos implementa essa formula)."""
    per_node = nx.clustering(G, weight="weight")
    return sum(per_node.values()) / len(per_node)


def strength(G: nx.DiGraph) -> dict:
    """Forca (grau ponderado) de cada jogador = passes dados + recebidos."""
    out_s = {u: sum(d["weight"] for _, _, d in G.out_edges(u, data=True)) for u in G.nodes}
    in_s = {u: sum(d["weight"] for _, _, d in G.in_edges(u, data=True)) for u in G.nodes}
    return {u: out_s[u] + in_s[u] for u in G.nodes}


def degree_centralization(G: nx.DiGraph) -> float:
    """Definicao 6.34, generalizada pro caso ponderado (Remark 6.10):
    GC_D = soma[C_D(n*) - C_D(ni)] / [(n-2)(n-1)], usando a forca (grau
    ponderado) no lugar do grau simples. Perto de 1 = rede "em estrela"
    (concentrada em poucos jogadores); perto de 0 = conectividade
    homogenea entre os jogadores."""
    s = strength(G)
    n = len(s)
    if n <= 2:
        raise ValueError("centralizacao de grupo exige pelo menos 3 jogadores")
    max_s = max(s.values())
    numerator = sum(max_s - v for v in s.values())
    return numerator / ((n - 2) * (n - 1))


def _group_centralization(values: dict) -> float:
    """Formula da Definicao 6.34: soma[C(n*) - C(ni)] / [(n-2)(n-1)]."""
    n = len(values)
    if n <= 2:
        raise ValueError("centralizacao de grupo exige pelo menos 3 jogadores")
    max_v = max(values.values())
    return sum(max_v - v for v in values.values()) / ((n - 2) * (n - 1))


def degree_centralization_out(G: nx.DiGraph) -> float:
    """Centralizacao de grau de SAIDA (passes dados): Definicao 6.34 + Remark 6.10
    (p. 90), com o grau de saida ponderado do digrafo -- Definicao 4.7 (p. 59):
    C_D-out(ni) = soma_j a_ij."""
    return _group_centralization(
        {u: sum(d["weight"] for _, _, d in G.out_edges(u, data=True)) for u in G.nodes})


def degree_centralization_in(G: nx.DiGraph) -> float:
    """Centralizacao de grau de ENTRADA (passes recebidos): mesma formula da
    Definicao 6.34 aplicada ao prestigio de grau ponderado do digrafo --
    Definicao 4.25 (p. 65): P_D-in(ni) = soma_j a_ji. O livro nao define um
    indice de prestigio de grau para a rede inteira; esta e a extensao por
    analogia com a Definicao 6.34."""
    return _group_centralization(
        {u: sum(d["weight"] for _, _, d in G.in_edges(u, data=True)) for u in G.nodes})


def weighted_average_distance(G: nx.DiGraph) -> float:
    """Definicao 6.12: distancia media geodesica, usando 1/peso como
    'custo' de cada aresta (mais passes = caminho mais 'barato'/curto).
    Segue o Remark 6.1/6.2 do livro: se um par de jogadores nao tem
    caminho dirigido entre si, a distancia desse par vira
    max(distancias finitas) + 1, em vez de ser descartada ou dar erro."""
    n = G.number_of_nodes()
    Gc = G.copy()
    for u, v, d in Gc.edges(data=True):
        d["cost"] = 1.0 / d["weight"]

    nodes = list(Gc.nodes)
    finite_distances = []
    pending_pairs = []
    for u in nodes:
        lengths = nx.single_source_dijkstra_path_length(Gc, u, weight="cost")
        for v in nodes:
            if u == v:
                continue
            if v in lengths:
                finite_distances.append(lengths[v])
            else:
                pending_pairs.append((u, v))

    if not finite_distances:
        raise ValueError("grafo sem nenhum caminho dirigido entre jogadores -- confira os dados")

    fallback = max(finite_distances) + 1
    total = sum(finite_distances) + fallback * len(pending_pairs)
    return (2 / (n * (n - 1))) * total


def compute_all(matrix: dict) -> dict:
    """Roda as 4 metricas de uma vez, a partir da matriz de passes de um time."""
    G = build_digraph(matrix)
    if G.number_of_nodes() == 0:
        raise ValueError("matriz vazia")
    return {
        "n_jogadores": G.number_of_nodes(),
        "total_passes_na_rede": sum(d["weight"] for _, _, d in G.edges(data=True)),
        "densidade_ponderada": round(weighted_density(G), 4),
        "clustering_rede_ponderado": round(whole_network_clustering(G), 4),
        "centralizacao_grau": round(degree_centralization(G), 4),
        "distancia_media_ponderada": round(weighted_average_distance(G), 4),
    }


if __name__ == "__main__":
    # teste rapido com um grafo de brinquedo (triangulo simples)
    toy = {
        "A": {"B": 10, "C": 1},
        "B": {"A": 8, "C": 1},
        "C": {"A": 1, "B": 1},
    }
    print(compute_all(toy))
