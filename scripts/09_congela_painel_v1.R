# =============================================================================
# ICG+ v2 — 09: Congela o painel mensal v1.0 (2015–2025) e gera o dicionário
# -----------------------------------------------------------------------------
# Junta 07 (componentes + camada política) e 08 (RP8/RP9 por grupo partidário).
# Saídas: icg_plus_v2/saidas/09_painel_v1.0/
#   base_painel_mensal_v1.0_2026-10-21.csv   (separador ";", ponto decimal, UTF-8)
#   dicionario_base_painel_mensal_v1.0.csv
#   LEIA-ME.md
# Ordem de execução: 06a (download Câmara) -> 06 -> 06b -> 07 -> 08 -> 09
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(stringr); library(tibble) })
VERSAO <- "v1.0"; DATA <- "2026-10-21"  # congelada após a revisão de tipo_indicacao (21/10/2026)
OUT <- "icg_plus_v2/saidas/09_painel_v1.0"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

p  <- read_delim("icg_plus_v2/saidas/07_painel_v1/painel_componentes.csv", delim = ";", show_col_types = FALSE)
a  <- read_delim("icg_plus_v2/saidas/08_atribuicao_rp8_rp9/03_rp8_rp9_coalizao_mensal.csv", delim = ";", show_col_types = FALSE)
b  <- left_join(p, a, by = c("ano", "mes")) |>
  mutate(across(matches("^rp[89]_D[12]_"), ~ coalesce(.x, 0)),
         presidente = case_when(governo == "Dilma II" ~ "Dilma Rousseff", governo == "Temer" ~ "Michel Temer",
                                governo == "Bolsonaro" ~ "Jair Bolsonaro", TRUE ~ "Luiz Inácio Lula da Silva"),
         versao = VERSAO, data_congelamento = DATA) |>
  relocate(presidente, .after = governo)
stopifnot(nrow(b) == 132, !any(duplicated(b[c("ano", "mes")])))
arq <- sprintf("base_painel_mensal_%s_%s.csv", VERSAO, DATA)
write_delim(b, file.path(OUT, arq), delim = ";", na = "")

# --- Dicionário -----------------------------------------------------------------
desc <- tribble(~padrao, ~descricao, ~unidade, ~fonte,
  "^ano$|^mes$", "Ano e mês do empenho", "", "",
  "^data$", "Primeiro dia do mês", "data", "",
  "^t$", "Tendência (1 = jan/2015)", "índice", "",
  "^governo$|^presidente$", "Governo/presidente no dia 1º do mês (mai/2016 = Temer)", "", "",
  "^dez$", "Dummy de dezembro", "0/1", "",
  "^eleitoral$", "Dummy de período eleitoral: jul–out de anos eleitorais (vedação de transferências voluntárias, Lei 9.504/1997, art. 73)", "0/1", "",
  "^ano_eleicao_geral$", "Dummy de ano de eleição geral (2018, 2022)", "0/1", "",
  "^pos_ec", "Dummies institucionais: EC 86/2015 (RP6 impositivo), EC 100/2019 (RP7 impositivo, a partir de 2020), EC 105/2019 (transferências especiais, a partir de 2020)", "0/1", "",
  "^rp9_vigente$", "Dummy dos anos com RP9 (2020–2022)", "0/1", "",
  "^n_pastas$", "Nº de pastas com status ministerial no mês (sem a Vice-Presidência)", "nº", "base ministerial",
  "^n_pastas_D1$", "Pastas de cota partidária (D1)", "nº", "base ministerial",
  "^n_pastas_D2$", "Pastas de cota partidária com tipo_indicacao = bancada (D2)", "nº", "base ministerial",
  "^n_pastas_sem_partido$", "Pastas cujo titular não tem cota partidária (sem partido)", "nº", "base ministerial",
  "^n_pastas_outro$", "Pastas sem adesão partidária (tipo_indicacao = outro: sem partido ou filiado nomeado sem adesão da legenda)", "nº", "base ministerial",
  "^n_interinos$", "Pastas ocupadas por interino no mês", "nº", "base ministerial",
  "^n_partidos_D[12]$", "Nº de partidos na coalizão (D1 ou D2)", "nº", "base ministerial",
  "^partidos_D[12]$", "Lista dos partidos da coalizão", "texto", "base ministerial",
  "^pastas_partidarias_D[12]$", "Pastas dos partidos da coalizão", "nº", "base ministerial",
  "^cadeiras_coal_D[12]$", "Deputados em exercício dos partidos da coalizão, último dia do mês", "nº", "Câmara, Dados Abertos",
  "^total_exercicio_D[12]$", "Deputados em exercício no último dia do mês", "nº", "Câmara, Dados Abertos",
  "^pct_cadeiras_D[12]$", "Cadeiras da coalizão na Câmara", "%", "Câmara, Dados Abertos",
  "^coalescencia_D[12]$", "Coalescência (Amorim Neto 2000): 1 − ½Σ|pastas_i − cadeiras_i|, nas parcelas da coalizão", "0–1", "base ministerial + Câmara",
  "^disc_min$", "Discricionário ministerial (RP2+RP3) nos órgãos com status ministerial, reescalado ao total anual do SIOP por órgão (D-06)", "R$ mi IPCA", "SIGA + SIOP",
  "^disc_min_siga$", "Idem, sem reescala (robustez)", "R$ mi IPCA", "SIGA",
  "^disc_min_extraord$", "Parcela do discricionário ministerial em créditos extraordinários (D-08)", "R$ mi IPCA", "SIGA + SIOP",
  "^disc_min_sem_extraord$", "Discricionário ministerial sem créditos extraordinários (robustez)", "R$ mi IPCA", "SIGA + SIOP",
  "^n_pastas_art_casa$|^n_pastas_art_(camara|senado)$|^n_pastas_D2_art_casa$", "2ª lente (D-27): pastas cuja nomeação foi articulada por presidente/liderança da Câmara ou do Senado (total, por Casa e dentro de D2)", "nº", "ministerios/articulacao_casa.csv",
  "^partidos_so_art_casa$|^pct_cadeiras_so_art_casa$", "2ª lente: partidos (e suas cadeiras, % da Câmara) cuja presença no gabinete passa só por pastas articuladas pelas Casas", "texto / %", "base ministerial + Câmara",
  "^partidos_art_casa$|^articuladores$", "2ª lente: partidos das pastas articuladas e quem articulou", "texto", "ministerios/articulacao_casa.csv",
  "^dm_art_casa$", "2ª lente: discricionário ministerial (RP2+RP3) em órgãos controlados por pastas articuladas pelas Casas", "R$ mi IPCA", "base ministerial + SIGA",
  "^disc_min_presidencia$", "Parcela do discricionário ministerial executada pela Presidência da República (órgão 20000); na série principal, a UO 20101 vai para a Casa Civil (D-25); robustez: tudo para o partido do presidente", "R$ mi IPCA", "SIGA + SIOP",
  "^disc_min_rp[23]$", "Discricionário ministerial por RP", "R$ mi IPCA", "SIGA + SIOP",
  "^rp6$", "Emendas individuais (RP6), inclusive transferências especiais; 2020+ pela extração sem filtro de Poder (D-10)", "R$ mi IPCA", "SIGA",
  "^rp6_pix$|^rp6_demais$", "RP6 por modalidade (Pix = transferência especial, órgão 73000)", "R$ mi IPCA", "SIGA",
  "^rp[789]$", "Emendas de bancada (RP7), de comissão (RP8) e de relator (RP9), reescaladas ao SIOP por ano", "R$ mi IPCA", "SIGA + SIOP",
  "^rp[789]_siga$", "Idem, sem reescala", "R$ mi IPCA", "SIGA",
  "^emendas$", "RP6+RP7+RP8+RP9", "R$ mi IPCA", "",
  "^disc_total$", "Discricionário total = disc_min + emendas (denominador das participações; exclui operações especiais 71000–75000)", "R$ mi IPCA", "",
  "^sh_", "Participação no discricionário total", "proporção", "",
  "^dm_", "Discricionário ministerial por grupo da pasta que controla o órgão (D1): partido do presidente, outros partidos, sem partido", "R$ mi IPCA", "base ministerial + SIGA",
  "^dmD2_", "Idem, D2: separa as pastas de cota partidária que não são de bancada (D1_fora_D2_outro = filiado nomeado sem adesão do partido)", "R$ mi IPCA", "base ministerial + SIGA",
  "^rp[89]_D[12]_", "RP8/RP9 por grupo do apoiador (rateio igual, partido em cascata; D-12 a D-17). Participação anual × coalizão do mês; nao_atribuido = sem apoiador", "R$ mi IPCA", "Portal da Transparência (apoiamentos)",
  "^versao$|^data_congelamento$", "Versão e data de congelamento", "", "")
dic <- tibble(variavel = names(b)) |>
  rowwise() |>
  mutate(i = which(str_detect(variavel, desc$padrao))[1],
         descricao = desc$descricao[i], unidade = desc$unidade[i], fonte = desc$fonte[i]) |>
  ungroup() |> select(-i)
if (any(is.na(dic$descricao))) { print(dic |> filter(is.na(descricao))); stop("variável sem descrição") }
write_delim(dic, file.path(OUT, sprintf("dicionario_base_painel_mensal_%s.csv", VERSAO)), delim = ";")
cat(sprintf("ok: %s (%d linhas × %d variáveis)\n", arq, nrow(b), ncol(b)))
