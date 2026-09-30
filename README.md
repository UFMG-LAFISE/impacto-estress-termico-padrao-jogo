# Impacto do estresse térmico no padrão de jogo — Copa do Mundo FIFA 2026

**Pergunta:** as condições ambientais, em especial o estresse térmico (WBGT), alteram o
padrão de jogo das seleções na Copa do Mundo FIFA 2026?

O padrão de jogo é medido por **análise de redes sociais (SNA)** das redes de passes
publicadas pela FIFA nos relatórios pós-jogo (*Post-Match Summary Reports*, PMSR). As
métricas seguem **Clemente, Martins & Mendes (2016)**, *Social Network Analysis Applied to
Team Sports Analysis* (Springer). O efeito das condições ambientais é estimado com
**modelos lineares mistos generalizados** (glmmTMB, R).

Projeto do Laboratório de Fisiologia do Exercício (LAFISE/UFMG).

---

## As três etapas da análise

O trabalho foi feito em três etapas. As duas primeiras usam as mesmas métricas e os
mesmos modelos; o que muda é a base de dados. A terceira acrescenta variáveis novas.

| Etapa | Pasta | Base de dados | Situação |
|---|---|---|---|
| **1. Dados brutos** | [`etapa1_dados_brutos/`](etapa1_dados_brutos) | Matrizes extraídas automaticamente dos PDFs, sem o tratamento correto | Encerrada — mantida só como registro |
| **2. Dados tratados** | [`etapa2_dados_tratados/`](etapa2_dados_tratados) | Matrizes 11×11 corrigidas e conferidas | **Resultados atuais** |
| **3. Variáveis da FIFA** | [`etapa3_variaveis_fifa/`](etapa3_variaveis_fifa) | Estatísticas de jogo da página "Key Statistics" dos relatórios | Em andamento |

### Etapa 1 — dados brutos, sem o tratamento correto

As matrizes de passes foram extraídas automaticamente das páginas *Passing Networks* dos
PDFs com **todos os jogadores que entraram em campo**. Cada reserva virou um nó separado,
então as redes tinham tamanhos diferentes entre times e jogos, e as métricas não eram
comparáveis. Os modelos desta etapa foram refeitos na etapa 2 e **não devem ser usados
como resultado**.

- `analisar_jogo.py` — extrai as matrizes dos PDFs e calcula as métricas.
- `matrizes/` — uma matriz por time por jogo (todos os jogadores).
- `resultados_copa2026.csv` — métricas de rede por time por jogo.
- `modelos/` — dataset dos modelos, scripts em R e resultados (sem sufixo).

### Etapa 2 — base de dados tratada (resultados atuais)

Matrizes **11×11**: os passes de cada reserva foram somados na posição do titular que ele
substituiu, com base nas listas de substituições. A base foi conferida matriz a matriz
(dimensão, diagonal vazia, soma de linhas e colunas contra os totais do relatório) e três
arquivos foram corrigidos a partir dos PDFs:

- **Gana, jogo 45** — faltava a célula da diagonal, e os valores estavam deslocados uma coluna.
- **Croácia, jogo 22** — o titular era Petar Musa, e não Igor Matanovic (os números já estavam certos).
- **Egito, jogo 40** — a 6ª substituição não estava na lista, e duas células estavam trocadas.

Os arquivos originais estão em `novo-dataset/originais_antes_correcao/`.

- `novo-dataset/01_matrizes_passing_networks/` — 208 matrizes (104 jogos) e as listas de substituições.
- `calcular_metricas_novo.py` → `resultados_copa2026_novo.csv` — métricas de rede.
- `modelos/` — dataset, modelos, diagnósticos e tabelas:

| Script | O que faz | Resultados |
|---|---|---|
| `montar_dataset_modelos_novo.py` | junta as métricas com o contexto dos jogos | `dados/dataset_modelos_copa2026_novo.csv` |
| `preparar_dados_modelos_novo.R` | amostra, fatores e preditores centrados (usado pelos demais) | — |
| `explorar_modelos_novo.R`, `ajustar_modelos_finais_novo.R` | 1ª versão dos modelos (Gaussiana/Gama) | `resultados/*_novo.csv` |
| `diagnosticar_modelos_novo.R` | pressupostos da 1ª versão | `resultados/diagnosticos_novo/` |
| `testar_modelos_alternativos_novo.R` | escolha da distribuição onde os pressupostos falharam | `resultados/diagnosticos_novo/modelos_alternativos_novo.csv` |
| `testar_efeitos_aleatorios_novo_v2.R` | seleção e/ou jogo como efeito aleatório? | `resultados/efeitos_aleatorios*_novo_v2.csv` |
| **`ajustar_modelos_finais_novo_v2.R`** | **modelos finais**, pressupostos e sensibilidade (sem um jogo por vez) | `resultados/*_novo_v2.csv`, `figuras_novo_v2/` |
| `testar_nao_linearidade_novo_v2.R` | forma não linear do efeito do calor (splines) | `resultados/nao_linearidade*_novo_v2.csv`, `figuras_nao_linearidade_novo_v2/` |
| `gerar_dados_tabelas_novo_v2.R`, `gerar_tabelas_word_novo_v2.py` | tabelas do artigo | `resultados/tabelas_novo_v2/tabelas_modelos_novo_v2.docx` |

### Etapa 3 — novas variáveis dos relatórios da FIFA

Extração da página 3 (*Match Summary – Key Statistics*) de cada relatório: posse de bola,
gols, xG, finalizações, passes, *line breaks*, pressões defensivas, **distância total** e
**distância na zona 4 (20–25 km/h)**, por time em cada jogo. A zona 5 (> 25 km/h) não
existe no nível do time nesses relatórios.

- `extrair_estatisticas_jogo.py` → `estatisticas_jogo_copa2026.csv` (103 jogos; o PDF do
  jogo 10 é só imagem).
- Validação: gols iguais ao placar oficial nos 206 registros; posse somando 100%;
  distância total e zona 4 iguais à soma dos jogadores do projeto de desempenho físico
  (diferença ≤ 0,05 km).

**Próximos passos:** análise de mediação (calor → corrida em alta intensidade → padrão de
jogo) e posse de bola como variável de controle.

---

## Métricas de rede (nível macro)

Rede ponderada e direcionada: *a<sub>ij</sub>* = número de passes do jogador *i* para o
jogador *j*; *n* = 11. Todas as métricas dão um valor por time por jogo.

| Métrica | Cálculo | Livro (Clemente et al., 2016) |
|---|---|---|
| Densidade ponderada | Σ<sub>i</sub>Σ<sub>j≠i</sub> a<sub>ij</sub> / n(n−1) | Def. 6.8, p. 82 |
| Clustering da rede | média do clustering de cada jogador (Fagiolo, 2007), pesos divididos pelo maior peso do time | Def. 6.22, p. 86; Def. 4.42, p. 72 |
| Centralização de saída (passes dados) | Σ[C(n\*) − C(n<sub>i</sub>)] / (n−2)(n−1), C = passes dados | Def. 6.34 + Remark 6.10, p. 90; Def. 4.7, p. 59 |
| Centralização de entrada (passes recebidos) | mesma fórmula, com os passes recebidos | fórmula da Def. 6.34 aplicada à Def. 4.25, p. 65 (extensão por analogia) |
| Distância média ponderada | 2/n(n−1) × Σ<sub>i</sub>Σ<sub>j≠i</sub> d(n<sub>i</sub>,n<sub>j</sub>), custo de cada passe = 1/a<sub>ij</sub> | Def. 6.12, p. 84; Def. 4.12, p. 60 |

As funções estão em [`comum/metrics.py`](comum/metrics.py).

## Modelos (etapa 2, versão final)

Amostra: 130 observações seleção-jogo, 65 jogos em estádios abertos, 47 seleções.

- **Modelo A:** Y ~ WBGT + altitude + fase do torneio + período do jogo + diferença de ranking FIFA + (1 | seleção)
- **Modelo B:** igual ao A, com temperatura do ar + umidade relativa + radiação solar no lugar do WBGT

| Métrica | Distribuição | Variância residual |
|---|---|---|
| Densidade | Gaussiana | por fase do torneio |
| Clustering | Gaussiana | única |
| Centralização de saída | Gama (log) | por fase do torneio |
| Centralização de entrada | Gama (log) | única |
| Distância média | t de Student sobre log(Y) | única |

Todos os modelos passaram nos testes de pressupostos (DHARMa, homogeneidade de variância,
normalidade dos efeitos aleatórios, VIF). O efeito aleatório de jogo foi testado e teve
variância nula em todas as métricas.

**Resultado principal:** o estresse térmico aumentou o **clustering** da rede (WBGT:
β = 0,0034 por °C, p < 0,001; temperatura do ar: β = 0,0031 por °C, p < 0,001), de forma
robusta à retirada de qualquer jogo e sem indício de não linearidade. As demais métricas
não foram afetadas pelo calor e foram explicadas principalmente pela diferença de ranking
FIFA.

---

## Estrutura do repositório

```
comum/                    código usado por mais de uma etapa
  extract.py                extração das matrizes de passes dos PDFs
  metrics.py                métricas de rede
  contexto_jogos.py         contexto de cada jogo (clima, fase, ranking...)
etapa1_dados_brutos/
etapa2_dados_tratados/
etapa3_variaveis_fifa/
requirements.txt          pacotes Python
```

## Como reproduzir

**Dados externos, não incluídos no repositório:**

1. **Relatórios PMSR da FIFA (PDF)** — necessários só para extrair matrizes e estatísticas
   de novo (etapas 1 e 3). Os scripts procuram os arquivos em
   `~/Documents/analises-grafos-copa/Technical Reports/Technical Reports/`.
2. **`dataset_player_match.csv`** do projeto *wc2026-physical-performance* (LAFISE/UFMG),
   com as condições ambientais e o contexto de cada jogo. Os scripts o procuram em
   `~/Downloads/wc2026-physical-performance/pipeline/data/`. O caminho está definido em
   [`comum/contexto_jogos.py`](comum/contexto_jogos.py).

**Python** (3.12):

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

**R** (4.x), pacotes: `glmmTMB`, `DHARMa`, `performance`, `broom.mixed`, `dplyr`,
`forcats`, `readr`, `scales`, `splines`.

Os scripts de cada pasta `modelos/` devem ser executados de dentro dela, por exemplo:

```bash
cd etapa2_dados_tratados/modelos
Rscript ajustar_modelos_finais_novo_v2.R
```

## Referências

- Clemente, F. M., Martins, F. M. L., & Mendes, R. S. (2016). *Social Network Analysis Applied to Team Sports Analysis*. Springer. https://doi.org/10.1007/978-3-319-25855-3
- Fagiolo, G. (2007). Clustering in complex directed networks. *Physical Review E, 76*(2), 026107.
- Opsahl, T., Agneessens, F., & Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. *Social Networks, 32*(3), 245–251.
- Wasserman, S., & Faust, K. (1994). *Social Network Analysis: Methods and Applications*. Cambridge University Press.

## Licença

MIT — ver [`LICENSE`](LICENSE).
