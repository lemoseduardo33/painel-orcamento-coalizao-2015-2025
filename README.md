# Painel mensal de orçamento e coalizão, 2015–2025 (ICG+, v1.1)

Painel mensal (132 meses, jan/2015–dez/2025) que reúne, numa linha por mês, a camada política da coalizão de governo (pastas, partidos, cadeiras na Câmara, coalescência) e os componentes orçamentários do Executivo federal: discricionário ministerial (RP2+RP3) e emendas parlamentares (RP6, com as transferências especiais, RP7, RP8 e RP9), em R$ milhões corrigidos pelo IPCA. É a base de dados do índice ICG+, construído para a dissertação *Parlamentarismo Orçamentário: a evolução do poder do Congresso Nacional sobre o orçamento da União e a sua relação com o custo de coordenação da coalizão*, do Mestrado Profissional em Poder Legislativo (Cefor/Câmara dos Deputados).

## Estrutura

| Pasta / arquivo | Conteúdo |
| --- | --- |
| `dados/painel_mensal_icg_plus_v1.1_2015_2025.csv` | Painel congelado v1.1: 132 linhas × 100 variáveis (separador `;`, UTF-8) |
| `dicionario/dicionario_painel_mensal_v1.1.csv` | Descrição, unidade e fonte de cada variável |
| `scripts/` | Scripts em R que geram o painel a partir dos dados brutos, na ordem de execução |

## Conteúdo do painel

- **Calendário e regras:** ano, mês, tendência, governo, presidente, dummies de dezembro, período eleitoral, ano de eleição geral, EC 86/2015, EC 100/2019, EC 105/2019 (transferências especiais) e vigência do RP9.
- **Coalizão em duas definições:** D1 (partidos com ao menos uma pasta de cota partidária) e D2 (só pastas que o partido indicou ou aceitou como suas). Para cada uma: nº de pastas e de partidos, cadeiras na Câmara no último dia do mês e coalescência (Amorim Neto, 2000).
- **Discricionário ministerial (RP2+RP3):** total, por RP, com e sem créditos extraordinários, parcela da Presidência e decomposição pelo grupo da pasta que controla cada órgão (partido do presidente, outros partidos, sem partido). A v1.1 acrescenta a série principal sem o auxílio emergencial e sem a cessão onerosa (`disc_min_d45` e `dm45_*`).
- **Emendas:** RP6 (Pix e demais), RP7, RP8 e RP9, reescaladas ao total anual do SIOP; RP8 e RP9 por grupo partidário do apoiador (D1 e D2).
- **Participações** de cada componente no discricionário total.

## Principais escolhas metodológicas

- **Valores:** R$ milhões a preços constantes pelo IPCA do SIGA Brasil, mês de execução.
- **Reescala SIGA → SIOP:** o SIGA distribui a despesa pelos meses; o total anual de cada órgão, ano e RP é reescalado ao SIOP, que classifica pelo RP vigente. A série sem reescala está no painel (sufixo `_siga`) para robustez.
- **Base ministerial:** gerada a partir da tabela de mandatos publicada em [base-ministerial-brasil-2015-2025](https://github.com/lemoseduardo33/base-ministerial-brasil-2015-2025). O mês fica com quem ocupou a pasta por mais dias; o interino herda a cota do titular; a Vice-Presidência fica fora da contagem.
- **Presidência da República:** a UO 20101 vai para a Casa Civil; as UOs com vínculo identificável vão para a pasta correspondente.
- **RP8/RP9 por partido:** um registro por empenho, rateio igual entre os apoiadores ativos e partido por proxy em cascata. Detalhes e microdados em [emendas-rp8-rp9-apoiamento-partidos-2020-2025](https://github.com/lemoseduardo33/emendas-rp8-rp9-apoiamento-partidos-2020-2025).
- **Créditos extraordinários (v1.1):** voltam ao discricionário ministerial, com duas exclusões declaradas: auxílio emergencial (UO 55101, 2020–2021) e cessão onerosa (UOs 32101 e 73104, 2019).

As decisões estão descritas e justificadas no capítulo de Método e no apêndice de decisões da dissertação.

## Como reproduzir

Os scripts partem dos dados brutos, que não estão neste repositório por tamanho (cerca de 330 MB). Todos vêm de fontes públicas e podem ser baixados de novo:

| Fonte | Consulta | Pasta esperada |
| --- | --- | --- |
| SIGA Brasil (Senado Federal) | Execução mensal por órgão, UO e RP, Poder Executivo, 2015–2025; RP6 sem filtro de Poder, 2020–2025 | `dados_brutos/siga/` |
| SIOP (Ministério do Planejamento e Orçamento) | Execução anual por órgão, UO e RP; painel de emendas (RP6 a RP9) | `dados_brutos/siop/` |
| Portal da Transparência (CGU) | Apoiamento a emendas parlamentares, 2020–2025 | `dados_brutos/apoiamentos/` |
| API de Dados Abertos da Câmara | Deputados e histórico (legislaturas 55 a 57); lideranças | `dados_brutos/camara/`, `dados_brutos/lideres/` (baixados pelos scripts 04 e 06a) |
| API de Dados Abertos do Senado | Lideranças | `dados_brutos/lideres/` |

Extrações feitas em 18/10/2026. Diretório de trabalho: uma pasta-raiz com `dados_brutos/`, `ministerios/` (tabelas de mandatos e crosswalk) e `icg_plus_v2/scripts/`; as saídas vão para `icg_plus_v2/saidas/NN_etapa/`. Rodar com locale UTF-8.

Pacotes R: `data.table`, `stringr`, `stringi`, `dplyr`, `tidyr`, `readr`, `readxl`, `purrr`, `zoo`, `jsonlite`.

| Script | O que faz |
| --- | --- |
| `01_conciliacao_orcamentaria.R` | Concilia SIGA (mensal) e SIOP (anual) por órgão, RP e ano; calcula o fator de reescala |
| `02_diagnostico_apoiamentos.R` | Confere se as planilhas de apoiamento reproduzem a execução de RP8 e RP9 do SIOP |
| `04_lideres_e_coapoio.R` | Lideranças da Câmara e do Senado e proxy partidária dos apoiadores |
| `05_rp6_pix_mensal.R` | Série mensal do RP6 com as transferências especiais |
| `06a_camara_bancadas_download.R` | Histórico dos deputados para as cadeiras mensais |
| `06b_presidencia_por_uo.R` | Despesa da Presidência por unidade orçamentária |
| `06_base_ministerial_painel.R` | Base ministerial mensal, coalizão D1 e D2 e controle orçamentário de cada pasta |
| `07_painel_mensal_v1.R` | Junta componentes orçamentários e camada política |
| `08_atribuicao_partidaria_rp8_rp9.R` | Atribuição partidária de RP8 e RP9 |
| `09_congela_painel_v1.R` | Congela o painel v1.0 e gera o dicionário |
| `09b_ajuste_extraordinarios.R` | Painel v1.1 (créditos extraordinários) |

## Limitações e o que não está incluído

- A classificação do tipo de indicação de cada ministro (base da definição D2) e o registro de articulação das nomeações pelas presidências das Casas são insumos analíticos da dissertação e não fazem parte desta publicação. As colunas D2 do painel são publicadas; as colunas da lente de articulação foram retiradas. Por isso o script 06 não roda sem esses insumos.
- A planilha de apoiamentos não traz a data do empenho: a atribuição partidária de RP8/RP9 é anual, distribuída pelos meses conforme o valor mensal de cada RP.
- Boa parte do RP9 (76% a 90%) não tem apoiador registrado e aparece como "não atribuído".

## Como citar

LEMOS, Eduardo. *Painel mensal de orçamento e coalizão, 2015–2025 (ICG+, v1.1)*. Brasília: Mestrado Profissional em Poder Legislativo, Cefor/Câmara dos Deputados, 2026. Base de dados. Disponível em: <https://github.com/lemoseduardo33/painel-orcamento-coalizao-2015-2025>.

## Licença

Dados e documentação: CC BY 4.0. Código: MIT. Ver `LICENSE-DADOS.md` e `LICENSE`.

## Autoria

Eduardo Lemos, mestrando do Mestrado Profissional em Poder Legislativo (Cefor/Câmara dos Deputados). Orientador: Prof. Dr. João Luiz Pereira Marciano.
