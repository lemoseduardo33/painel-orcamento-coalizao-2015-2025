# =============================================================================
# ICG+ v2 — 06: Base ministerial mensal, coalizão D1/D2 e controle orçamentário
# -----------------------------------------------------------------------------
# Entradas
#   ministerios/mandatos_ministeriais_2015_2025.csv     1 linha por mandato
#   ministerios/tipo_indicacao_preclassificacao.csv     tipo_indicacao (+ decisao_revisao)
#   ministerios/crosswalk_orcamentario_2015_2025.csv    órgão da LOA -> pasta que o comanda
#   icg_plus_v2/saidas/01_conciliacao/10_painel_mensal_siga_orgao_rp_v0.csv
#   dados_brutos/camara/historico_deputados_leg55_57.csv (script 06a)
# Saídas (icg_plus_v2/saidas/06_base_ministerial/)
#   01_pasta_mes.csv            pasta × mês: titular, partido, cota, tipo (camada política)
#   02_coalizao_mensal.csv      mês: nº de pastas, partidos D1/D2, cadeiras, coalescência
#   03_partido_mes.csv          partido × mês: pastas (D1/D2) e cadeiras
#   04_orgao_mes_controle.csv   órgão orçamentário × mês: pasta controladora, cota, tipo, grupo
#   05_checagens.csv            órgãos com despesa sem controlador etc.
#
# Decisões aplicadas (ver decisoes.md):
#   D-20 mês com troca de ministro -> quem ficou mais dias (empate: o mais recente)
#   D-21 interino herda cota e tipo do titular anterior da pasta (salvo exceções
#        revisadas, que têm tipo próprio na planilha)
#   D-22 D1 = partidos com ao menos uma pasta de cota partidária (Amorim Neto;
#        Bertholini e Pereira); D2 = só pastas com tipo_indicacao = "bancada"
#   D-23 Gabinete da Vice-Presidência fora da contagem e da coalizão
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)
})
INICIO <- as.Date("2015-01-01"); FIM <- as.Date("2025-12-31")
OUT <- "icg_plus_v2/saidas/06_base_ministerial"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
salva <- function(x, f) write_delim(x, file.path(OUT, f), delim = ";",
                                    na = "", escape = "double")  # ponto decimal

VICE <- "GABINETE DA VICE-PRESIDÊNCIA DA REPÚBLICA"
SEM  <- c("SEM PARTIDO", "VAGO")
RENOMEACOES <- tibble::tribble(
  ~de,    ~para,          ~data,
  "PMDB", "MDB",          "2017-12-19",
  "PPS",  "CIDADANIA",    "2019-03-23",
  "PR",   "PL",           "2019-05-07",
  "PRB",  "REPUBLICANOS", "2019-08-14",
  "DEM",  "UNIÃO",        "2022-02-08",
  "PSL",  "UNIÃO",        "2022-02-08") |> mutate(data = as.Date(data))
renomeia <- function(p, d) {
  for (i in seq_len(nrow(RENOMEACOES)))
    p <- ifelse(!is.na(p) & p == RENOMEACOES$de[i] & d >= RENOMEACOES$data[i], RENOMEACOES$para[i], p)
  p
}
partido_presidente <- function(d) case_when(
  d < as.Date("2016-05-12") ~ "PT",
  d < as.Date("2017-12-19") ~ "PMDB",         # PMDB vira MDB em 19/12/2017
  d < as.Date("2019-01-01") ~ "MDB",
  d < as.Date("2019-11-20") ~ "PSL",
  d < as.Date("2021-11-30") ~ "SEM PARTIDO",  # Bolsonaro sem partido
  d < as.Date("2023-01-01") ~ "PL",
  TRUE ~ "PT")
governo <- function(d) case_when(d < as.Date("2016-05-12") ~ "Dilma II",
  d < as.Date("2019-01-01") ~ "Temer", d < as.Date("2023-01-01") ~ "Bolsonaro", TRUE ~ "Lula III")

# -----------------------------------------------------------------------------
# BLOCO 1 — Mandatos + tipo_indicacao
# -----------------------------------------------------------------------------
mand <- read_delim("ministerios/mandatos_ministeriais_2015_2025.csv", delim = ";",
                   col_types = cols(.default = "c")) |>
  mutate(inicio = as.Date(inicio), fim = as.Date(fim))
tipo <- read_delim("ministerios/tipo_indicacao_preclassificacao.csv", delim = ";",
                   col_types = cols(.default = "c")) |>
  # a planilha pode ter sido salva pelo Excel (datas dd/mm/aaaa)
  mutate(inicio = as.Date(inicio, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
         decisao_revisao = na_if(str_trim(coalesce(decisao_revisao, "")), ""),
         tipo_final = coalesce(decisao_revisao, tipo_indicacao)) |>
  select(orgao, nome, inicio, tipo_indicacao = tipo_final, confianca_tipo)
mand <- left_join(mand, tipo, by = c("orgao", "nome", "inicio"))
# D-27: articulação das Casas (2ª lente). O tipo_indicacao desta tabela prevalece
# sobre a planilha de revisão (rodada de 21/10/2026, à tarde).
art <- read_delim("ministerios/articulacao_casa.csv", delim = ";", comment = "#",
                  col_types = cols(.default = "c")) |>
  mutate(inicio = as.Date(inicio)) |>
  select(orgao, nome, inicio, tipo_art = tipo_indicacao, articulador, casa_articulador = casa)
stopifnot(nrow(semi_join(art, mand, by = c("orgao", "nome", "inicio"))) == nrow(art))
mand <- left_join(mand, art, by = c("orgao", "nome", "inicio")) |>
  mutate(tipo_indicacao = coalesce(tipo_art, tipo_indicacao),
         articulador = coalesce(articulador, ""), casa_articulador = coalesce(casa_articulador, "")) |>
  select(-tipo_art)
stopifnot(!any(is.na(mand$tipo_indicacao)))
stopifnot(all(mand$tipo_indicacao %in% c("bancada", "lideranca_casa", "tecnico", "outro", "interino_herda")))
# D-28: tipo final binário. "tecnico" e "outro" viram "outro" (pasta sem adesão partidária).
# A distinção sem partido × filiado continua recuperável por partido_cota.
mand <- mand |> mutate(tipo_indicacao = if_else(tipo_indicacao %in% c("tecnico", "lideranca_casa"), "outro", tipo_indicacao))
stopifnot(all(mand$tipo_indicacao %in% c("bancada", "outro", "interino_herda")))

sobre <- mand |> arrange(orgao, inicio) |> group_by(orgao) |> filter(inicio <= lag(fim)) |> ungroup()
if (nrow(sobre) > 0) { print(sobre); stop("Mandatos sobrepostos") }

# D-21: interino herda cota e tipo do titular anterior na mesma pasta
# (se não houver anterior, do seguinte). A filiação do interino é mantida.
mand <- mand |> arrange(orgao, inicio) |> group_by(orgao) |>
  mutate(eh_herda = tipo_indicacao == "interino_herda",
         cota_tit = if_else(eh_herda, NA_character_, partido_cota),
         tipo_tit = if_else(eh_herda, NA_character_, tipo_indicacao),
         art_tit  = if_else(eh_herda, NA_character_, articulador),
         casa_tit = if_else(eh_herda, NA_character_, casa_articulador)) |>
  fill(cota_tit, tipo_tit, art_tit, casa_tit, .direction = "downup") |>
  mutate(partido_cota   = if_else(eh_herda, cota_tit, partido_cota),
         tipo_indicacao = if_else(eh_herda, tipo_tit, tipo_indicacao),
         articulador    = if_else(eh_herda, art_tit, articulador),
         casa_articulador = if_else(eh_herda, casa_tit, casa_articulador)) |>
  ungroup() |> select(-cota_tit, -tipo_tit, -art_tit, -casa_tit)

# -----------------------------------------------------------------------------
# BLOCO 2 — Expansão diária e regra mensal (D-20)
# -----------------------------------------------------------------------------
dias_mes <- tibble(d = seq(INICIO, FIM, by = "day")) |>
  count(ano = as.integer(format(d, "%Y")), mes = as.integer(format(d, "%m")), name = "dias_no_mes")
dias <- mand |>
  mutate(dia = map2(inicio, fim, ~ seq(.x, .y, by = "day"))) |>
  unnest(dia) |>
  mutate(ano = as.integer(format(dia, "%Y")), mes = as.integer(format(dia, "%m")),
         partido_filiacao = renomeia(partido_filiacao, dia),
         partido_cota     = renomeia(partido_cota, dia)) |>
  left_join(dias_mes, by = c("ano", "mes"))

pasta_mes <- dias |>
  group_by(orgao, ano, mes, dias_no_mes, nome, partido_filiacao, partido_cota,
           interino, tipo_indicacao, confianca, articulador, casa_articulador) |>
  summarise(n_dias = n(), ultimo = max(dia), .groups = "drop") |>
  group_by(orgao, ano, mes) |>
  mutate(dias_inexistente = dias_no_mes - sum(n_dias)) |>
  arrange(desc(n_dias), desc(ultimo), .by_group = TRUE) |>
  slice(1) |> ungroup() |>
  filter(n_dias > dias_inexistente) |>          # pasta criada/extinta no mês
  mutate(data = as.Date(sprintf("%d-%02d-15", ano, mes)),
         governo = governo(data),
         partidaria_D1 = !partido_cota %in% SEM,
         partidaria_D2 = partidaria_D1 & tipo_indicacao == "bancada",
         articulada = articulador != "",
         conta = orgao != VICE) |>
  select(ano, mes, governo, orgao, nome, partido_filiacao, partido_cota, tipo_indicacao,
         articulador, casa_articulador, articulada,
         interino, confianca, n_dias, partidaria_D1, partidaria_D2, conta)
salva(pasta_mes |> arrange(ano, mes, orgao), "01_pasta_mes.csv")

# -----------------------------------------------------------------------------
# BLOCO 3 — Cadeiras na Câmara por partido e mês (último dia do mês)
# Deputado em exercício no último dia do mês, com o partido vigente nessa data.
# -----------------------------------------------------------------------------
f_hist <- "dados_brutos/camara/historico_deputados_leg55_57.csv"
cadeiras <- NULL
if (file.exists(f_hist)) {
  h <- read_csv(f_hist, col_types = cols(.default = "c")) |>
    mutate(dt = as.POSIXct(str_replace(dataHora, "T", " "), tz = "UTC", format = "%Y-%m-%d %H:%M"),
           id = as.integer(id)) |>
    filter(!is.na(dt)) |> arrange(id, dt)
  # Em exercício = situação "Exercício" no último evento até a data
  fim_mes <- dias_mes |> mutate(d = as.POSIXct(sprintf("%d-%02d-%02d 12:00", ano, mes, dias_no_mes), tz = "UTC"))
  # Situação e partido vigentes = último valor não vazio até a data (os eventos
  # "Partido no início da legislatura" vêm sem situação).
  h <- h |> group_by(id) |>
    mutate(sit = situacao, sig = siglaPartido) |>
    fill(sit, sig, .direction = "down") |> ungroup()
  cadeiras <- map_dfr(seq_len(nrow(fim_mes)), function(i) {
    d <- fim_mes$d[i]
    h |> filter(dt <= d) |> group_by(id) |> slice_tail(n = 1) |> ungroup() |>
      filter(sit == "Exercício") |>
      transmute(ano = fim_mes$ano[i], mes = fim_mes$mes[i], id, siglaPartido = sig)
  }) |>
    mutate(data = as.Date(sprintf("%d-%02d-15", ano, mes)),
           partido = recode(siglaPartido, "SDD" = "SOLIDARIEDADE", "SD" = "SOLIDARIEDADE",
                            "PODE" = "PODEMOS", "PTN" = "PODEMOS", "PATRI" = "PATRIOTA",
                            "PEN" = "PATRIOTA", "PTdoB" = "AVANTE"),
           partido = renomeia(partido, data)) |>
    count(ano, mes, partido, name = "cadeiras") |>
    group_by(ano, mes) |> mutate(total_exercicio = sum(cadeiras)) |> ungroup() |>
    # jan/2015 ainda é a 54ª legislatura, não baixada (só 55–57): fica sem cadeiras
    filter(!(ano == 2015 & mes == 1))
  salva(cadeiras, "03b_cadeiras_partido_mes.csv")
}

# -----------------------------------------------------------------------------
# BLOCO 4 — Coalizão mensal: D1 e D2
# Coalescência (Amorim Neto 2000): C = 1 - 1/2 Σ_i |S_i - M_i|, com S_i = parcela
# das pastas partidárias e M_i = parcela das cadeiras da coalizão do partido i.
# -----------------------------------------------------------------------------
pm <- pasta_mes |> filter(conta)
partido_mes <- bind_rows(
  pm |> filter(partidaria_D1) |> count(ano, mes, partido = partido_cota, name = "pastas") |> mutate(def = "D1"),
  pm |> filter(partidaria_D2) |> count(ano, mes, partido = partido_cota, name = "pastas") |> mutate(def = "D2"))
if (!is.null(cadeiras))
  partido_mes <- partido_mes |> left_join(cadeiras |> select(ano, mes, partido, cadeiras, total_exercicio),
                                          by = c("ano", "mes", "partido"))
salva(partido_mes |> arrange(def, ano, mes, partido), "03_partido_mes.csv")

resume_def <- function(D) {
  x <- partido_mes |> filter(def == D)
  out <- x |> group_by(ano, mes) |>
    summarise(n_partidos = n(), partidos = paste(sort(partido), collapse = ", "),
              pastas_partidarias = sum(pastas), .groups = "drop")
  if (!is.null(cadeiras)) {
    co <- x |> semi_join(distinct(cadeiras, ano, mes), by = c("ano", "mes")) |>
      mutate(cadeiras = coalesce(cadeiras, 0L)) |> group_by(ano, mes) |>
      summarise(cadeiras_coal = sum(cadeiras), total_exercicio = max(total_exercicio, na.rm = TRUE),
                coalescencia = 1 - 0.5 * sum(abs(pastas / sum(pastas) -
                                                 if (sum(cadeiras) > 0) cadeiras / sum(cadeiras) else 0)),
                .groups = "drop") |>
      mutate(pct_cadeiras = 100 * cadeiras_coal / total_exercicio)
    out <- left_join(out, co, by = c("ano", "mes"))
  }
  rename_with(out, ~ paste0(.x, "_", D), -c(ano, mes))
}
coal <- pm |> group_by(ano, mes, governo) |>
  summarise(n_pastas = n(), n_pastas_D1 = sum(partidaria_D1), n_pastas_D2 = sum(partidaria_D2),
            n_pastas_sem_partido = sum(partido_cota %in% SEM),
            n_pastas_outro = sum(tipo_indicacao == "outro"),
            n_interinos = sum(interino == "S"),
            # 2ª lente (D-27): pastas cuja nomeação foi articulada pelas presidências/lideranças das Casas
            n_pastas_art_casa = sum(articulada),
            n_pastas_art_camara = sum(articulada & casa_articulador == "Câmara"),
            n_pastas_art_senado = sum(articulada & casa_articulador == "Senado"),
            n_pastas_D2_art_casa = sum(articulada & partidaria_D2),
            partidos_art_casa = paste(sort(unique(partido_cota[articulada & partidaria_D1])), collapse = ", "),
            articuladores = paste(sort(unique(articulador[articulada])), collapse = ", "),
            .groups = "drop") |>
  left_join(resume_def("D1"), by = c("ano", "mes")) |>
  left_join(resume_def("D2"), by = c("ano", "mes"))
# 2ª lente: cadeiras dos partidos cuja presença no gabinete passa SÓ por pastas articuladas pelas Casas
if (!is.null(cadeiras)) {
  so_art <- pm |> filter(partidaria_D1) |> group_by(ano, mes, partido = partido_cota) |>
    summarise(so_art = all(articulada), .groups = "drop") |> filter(so_art) |>
    inner_join(cadeiras, by = c("ano", "mes", "partido")) |>
    group_by(ano, mes) |>
    summarise(partidos_so_art_casa = paste(sort(partido), collapse = ", "),
              pct_cadeiras_so_art_casa = 100 * sum(cadeiras) / max(total_exercicio), .groups = "drop")
  coal <- coal |> left_join(so_art, by = c("ano", "mes")) |>
    mutate(pct_cadeiras_so_art_casa = if_else(is.na(pct_cadeiras_D1), NA_real_, coalesce(pct_cadeiras_so_art_casa, 0)))
}
salva(coal, "02_coalizao_mensal.csv")

# -----------------------------------------------------------------------------
# BLOCO 5 — Camada orçamentária: quem comanda cada órgão da LOA em cada mês
# Órgão SIGA -> (crosswalk, se houver) pasta controladora -> titular do mês.
# PRESIDÊNCIA DA REPÚBLICA (20000) reúne várias pastas: atribuída ao grupo
# "partido do presidente" (regra provisória, a confirmar).
# -----------------------------------------------------------------------------
siga <- read_delim("icg_plus_v2/saidas/01_conciliacao/10_painel_mensal_siga_orgao_rp_v0.csv",
                   delim = ";", locale = locale(decimal_mark = ",", grouping_mark = "."),
                   col_types = cols(.default = "c")) |>
  mutate(across(c(ano, mes, rp), as.integer), orgao_cod = as.integer(orgao_cod),
         across(starts_with(c("empenhado", "liquidado", "pago")),
                ~ parse_double(.x, locale = locale(decimal_mark = ","))))
stopifnot(!anyNA(siga$empenhado_ipca))
xw <- read_delim("ministerios/crosswalk_orcamentario_2015_2025.csv", delim = ";",
                 col_types = cols(.default = "c")) |>
  mutate(inicio = as.Date(inicio), fim = as.Date(fim))
sob_xw <- xw |> arrange(orgao_orcamentario, inicio) |> group_by(orgao_orcamentario) |>
  filter(inicio <= lag(fim)) |> ungroup()
if (nrow(sob_xw) > 0) { print(sob_xw); stop("Crosswalk com períodos sobrepostos") }
NAO_MINISTERIAL <- c("BANCO CENTRAL DO BRASIL - BACEN",
                     "TRANSFERÊNCIAS A ESTADOS, DISTRITO FEDERAL E MUNICÍPIOS", "OPERAÇÕES OFICIAIS DE CRÉDITO", VICE)

orgaos_mes <- siga |> distinct(ano, mes, orgao_cod, orgao) |>
  mutate(data = as.Date(sprintf("%d-%02d-15", ano, mes)))
ctrl <- orgaos_mes |>
  left_join(xw |> select(orgao_orcamentario, inicio, fim, orgao_controlador),
            by = join_by(orgao == orgao_orcamentario, between(data, inicio, fim))) |>
  mutate(pasta = coalesce(orgao_controlador, orgao)) |>
  select(ano, mes, orgao_cod, orgao, pasta) |>
  left_join(pasta_mes |> select(ano, mes, pasta = orgao, nome, partido_cota, tipo_indicacao, articulada),
            by = c("ano", "mes", "pasta")) |>
  mutate(data = as.Date(sprintf("%d-%02d-15", ano, mes)),
         pp = partido_presidente(data),
         grupo = case_when(
           orgao %in% NAO_MINISTERIAL ~ "nao_ministerial",
           orgao == "PRESIDÊNCIA DA REPÚBLICA" ~ "partido_presidente",
           is.na(nome) ~ "sem_controlador",
           partido_cota %in% SEM ~ "sem_partido",
           partido_cota == pp ~ "partido_presidente",
           TRUE ~ "outros_partidos_D1"),
         grupo_D2 = case_when(
           grupo == "outros_partidos_D1" & tipo_indicacao != "bancada" ~ paste0("D1_fora_D2_", tipo_indicacao),
           grupo == "outros_partidos_D1" ~ "outros_partidos_D2",
           TRUE ~ grupo)) |>
  select(-data, -pp)
stopifnot(!any(duplicated(ctrl[c("ano", "mes", "orgao_cod", "orgao")])))
salva(ctrl |> arrange(ano, mes, orgao_cod), "04_orgao_mes_controle.csv")

# Checagem: órgão-mês com despesa discricionária (RP2/RP3) e sem controlador
chk <- siga |> filter(rp %in% 2:3) |> group_by(ano, mes, orgao_cod, orgao) |>
  summarise(emp_ipca = sum(empenhado_ipca), .groups = "drop") |> filter(abs(emp_ipca) > 0) |>
  inner_join(ctrl |> filter(grupo == "sem_controlador") |> select(ano, mes, orgao_cod, pasta),
             by = c("ano", "mes", "orgao_cod")) |>
  group_by(orgao_cod, orgao, pasta) |>
  mutate(am = sprintf("%d-%02d", ano, mes)) |>
  summarise(meses = n(), de = min(am), ate = max(am),
            emp_ipca_bi = round(sum(emp_ipca) / 1e9, 3), .groups = "drop") |> arrange(desc(emp_ipca_bi))
salva(chk, "05_checagem_orgaos_sem_controlador.csv")
cat("Órgãos com RP2/RP3 sem controlador:", nrow(chk), "\n"); print(chk, n = Inf, width = Inf)
