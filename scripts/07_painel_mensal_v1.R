# =============================================================================
# ICG+ v2 — 07: Painel mensal 2015–2025 (versão congelada v1.0)
# -----------------------------------------------------------------------------
# Uma linha por mês (132 meses). Junta:
#   - camada política (script 06): pastas, coalizão D1/D2, cadeiras, coalescência
#   - componentes orçamentários (empenhado, R$ milhões, IPCA do SIGA — D-09)
#   - decomposição do discricionário ministerial por grupo de pasta
#   - variáveis de calendário
# Entradas
#   icg_plus_v2/saidas/01_conciliacao/10_painel_mensal_siga_orgao_rp_v0.csv
#   icg_plus_v2/saidas/01_conciliacao/01_C1_siga_x_siop_ano_rp.csv
#   icg_plus_v2/saidas/01_conciliacao/02c_C2_rp2_rp3_por_orgao_completo.csv
#   icg_plus_v2/saidas/05_rp6_pix/10_painel_mensal_rp6_pix_v0.csv
#   icg_plus_v2/saidas/06_base_ministerial/02_coalizao_mensal.csv
#   icg_plus_v2/saidas/06_base_ministerial/04_orgao_mes_controle.csv
# Saída (insumo do script 09, que congela a v1.0)
#   icg_plus_v2/saidas/07_painel_v1/painel_componentes.csv
#
# Regra D-06 (SIOP define os totais anuais; SIGA dá a distribuição mensal):
#   RP2/RP3: fator = SIOP/SIGA por órgão × ano × RP (tabela 02c), aplicado a todos
#            os meses do órgão. Corrige a diferença "RP da lei × RP vigente"
#            (ex.: Minas e Energia 2019, R$ 11,7 bi de RP2 no SIGA que o SIOP
#            classifica como RP1). Órgãos com |SIGA| < R$ 50 mi no ano: fator 1.
#            Órgãos 71000/74000 (operações especiais) não são ministeriais e
#            ficam fora do discricionário ministerial.
#   RP6:     2020–2025 = extração do SIGA sem filtro de Poder (D-10), já igual
#            ao SIOP, com Pix separado; 2015–2019 = SIGA × fator ano (≈ 1).
#   RP7–RP9: SIGA × fator SIOP/SIGA por ano (RP9 2020: fator 0,92).
#   A série sem reescala é mantida (sufixo _siga) para o teste de robustez.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readr); library(stringr) })
VERSAO <- "v1.0"; DATA_CONGELAMENTO <- "2026-10-21"
OUT <- "icg_plus_v2/saidas/07_painel_v1"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
br <- locale(decimal_mark = ",")
le_br <- function(f) read_delim(f, delim = ";", locale = br, show_col_types = FALSE)

# --- SIGA órgão × mês × RP -----------------------------------------------------
siga <- read_delim("icg_plus_v2/saidas/01_conciliacao/10_painel_mensal_siga_orgao_rp_v0.csv",
                   delim = ";", col_types = cols(.default = "c")) |>
  filter(rp != "NÃO INFORMADO") |>
  mutate(across(c(ano, mes, rp, orgao_cod), as.integer),
         across(c(empenhado, empenhado_ipca), ~ parse_double(.x, locale = br)))
stopifnot(!anyNA(siga$empenhado_ipca))

ctrl <- read_delim("icg_plus_v2/saidas/06_base_ministerial/04_orgao_mes_controle.csv", delim = ";",
                   col_types = cols(.default = "c")) |>
  mutate(across(c(ano, mes, orgao_cod), as.integer)) |>
  mutate(articulada = articulada %in% c("TRUE", "True", "true")) |>
  select(ano, mes, orgao_cod, orgao, grupo, grupo_D2, articulada)

# --- Fatores de reescala SIOP/SIGA --------------------------------------------
f_org <- le_br("icg_plus_v2/saidas/01_conciliacao/02c_C2_rp2_rp3_por_orgao_completo.csv") |>
  mutate(fator = if_else(abs(siga_bi) >= 0.05, siop_bi / siga_bi, 1)) |>
  select(ano, rp, orgao_cod, fator_org = fator)
f_ano <- le_br("icg_plus_v2/saidas/01_conciliacao/01_C1_siga_x_siop_ano_rp.csv") |>
  filter(rp %in% as.character(6:9)) |>
  mutate(rp = as.integer(rp), fator_ano = if_else(siga_emp_bi > 0, siop_emp_bi / siga_emp_bi, 1)) |>
  select(ano, rp, fator_ano)
salva_csv <- function(x, f) write_delim(x, file.path(OUT, f), delim = ";", na = "")
salva_csv(f_org |> filter(abs(fator_org - 1) > 0.01), "00a_fatores_reescala_orgao_rp2_rp3.csv")
salva_csv(f_ano, "00b_fatores_reescala_ano_rp6_rp9.csv")

s <- siga |>
  left_join(f_org, by = c("ano", "rp", "orgao_cod")) |>
  left_join(f_ano, by = c("ano", "rp")) |>
  mutate(fator = case_when(rp %in% 2:3 ~ coalesce(fator_org, 1),
                           rp %in% 6:9 ~ coalesce(fator_ano, 1), TRUE ~ 1),
         emp = empenhado_ipca * fator / 1e6, emp_siga = empenhado_ipca / 1e6) |>
  left_join(ctrl, by = c("ano", "mes", "orgao_cod", "orgao")) |>
  mutate(grupo = coalesce(grupo, "sem_controlador"), grupo_D2 = coalesce(grupo_D2, grupo))
# Presidência (20000) por UO (script 06b, D-25): substitui as linhas RP2/RP3 do órgão
pr <- read_delim("icg_plus_v2/saidas/06_base_ministerial/06_presidencia_uo_mes.csv", delim = ";",
                 show_col_types = FALSE) |>
  mutate(orgao_cod = 20000L, orgao = "PRESIDÊNCIA DA REPÚBLICA") |>
  left_join(f_org, by = c("ano", "rp", "orgao_cod")) |>
  mutate(fator = coalesce(fator_org, 1), emp = empenhado_ipca * fator / 1e6, emp_siga = empenhado_ipca / 1e6)
tot_v0 <- sum(s$emp_siga[s$orgao_cod == 20000 & s$rp %in% 2:3]); tot_uo <- sum(pr$emp_siga)
cat(sprintf("Presidência RP2/RP3: painel v0 = %.1f mi; soma por UO = %.1f mi\n", tot_v0, tot_uo))
stopifnot(abs(tot_v0 - tot_uo) < 1)
s <- bind_rows(s |> filter(!(orgao_cod == 20000 & rp %in% 2:3)),
               pr |> select(ano, mes, orgao_cod, orgao, rp, credito, emp, emp_siga, grupo, grupo_D2))
stopifnot(!any(s$grupo == "sem_controlador" & s$rp %in% 2:3 & abs(s$emp) > 1))

# --- Componentes ---------------------------------------------------------------
mini <- s |> filter(rp %in% 2:3, grupo != "nao_ministerial")
comp <- mini |> group_by(ano, mes) |>
  summarise(disc_min = sum(emp), disc_min_siga = sum(emp_siga),
            disc_min_extraord = sum(emp[credito == "extraordinario"]),
            disc_min_rp2 = sum(emp[rp == 2]), disc_min_rp3 = sum(emp[rp == 3]),
            disc_min_presidencia = sum(emp[orgao_cod == 20000]),
            dm_art_casa = sum(emp[articulada %in% TRUE]), .groups = "drop") |>
  mutate(disc_min_sem_extraord = disc_min - disc_min_extraord)
emd <- s |> filter(rp %in% 6:9) |> group_by(ano, mes, rp) |>
  summarise(v = sum(emp), v_siga = sum(emp_siga), .groups = "drop") |>
  pivot_wider(names_from = rp, values_from = c(v, v_siga), values_fill = 0) |>
  rename(rp6_siga_filtro = v_6, rp7 = v_7, rp8 = v_8, rp9 = v_9,
         rp7_siga = v_siga_7, rp8_siga = v_siga_8, rp9_siga = v_siga_9) |> select(-v_siga_6)
pix <- read_delim("icg_plus_v2/saidas/05_rp6_pix/10_painel_mensal_rp6_pix_v0.csv", delim = ";",
                  locale = br, show_col_types = FALSE) |>
  mutate(tipo = if_else(str_detect(tipo, "pix"), "rp6_pix", "rp6_demais")) |>
  select(ano, mes, tipo, empenhado_ipca_mi) |>
  pivot_wider(names_from = tipo, values_from = empenhado_ipca_mi)
# grupos do discricionário ministerial (D1 e D2)
g1 <- mini |> group_by(ano, mes, grupo) |> summarise(v = sum(emp), .groups = "drop") |>
  pivot_wider(names_from = grupo, values_from = v, names_prefix = "dm_", values_fill = 0)
g2 <- mini |> group_by(ano, mes, grupo_D2) |> summarise(v = sum(emp), .groups = "drop") |>
  pivot_wider(names_from = grupo_D2, values_from = v, names_prefix = "dmD2_", values_fill = 0)

# --- Montagem -----------------------------------------------------------------
cal <- expand_grid(ano = 2015:2025, mes = 1:12) |>
  mutate(data = as.Date(sprintf("%d-%02d-01", ano, mes)),
         t = row_number(),
         governo = case_when(data < as.Date("2016-05-01") ~ "Dilma II", data < as.Date("2019-01-01") ~ "Temer",
                             data < as.Date("2023-01-01") ~ "Bolsonaro", TRUE ~ "Lula III"),
         dez = as.integer(mes == 12),
         eleitoral = as.integer(ano %in% c(2016, 2018, 2020, 2022, 2024) & mes %in% 7:10),
         ano_eleicao_geral = as.integer(ano %in% c(2018, 2022)),
         pos_ec86 = 1L, pos_ec100 = as.integer(ano >= 2020), pos_ec105_pix = as.integer(ano >= 2020),
         rp9_vigente = as.integer(ano %in% 2020:2022))
coal <- read_delim("icg_plus_v2/saidas/06_base_ministerial/02_coalizao_mensal.csv", delim = ";",
                   show_col_types = FALSE) |> select(-governo)

p <- cal |> left_join(coal, by = c("ano", "mes")) |>
  left_join(comp, by = c("ano", "mes")) |> left_join(emd, by = c("ano", "mes")) |>
  left_join(pix, by = c("ano", "mes")) |> left_join(g1, by = c("ano", "mes")) |>
  left_join(g2, by = c("ano", "mes")) |>
  mutate(across(c(starts_with("rp"), starts_with("dm"), starts_with("disc_min")), ~ coalesce(.x, 0)),
         rp6 = if_else(ano >= 2020, rp6_demais + rp6_pix, rp6_siga_filtro),
         rp6_pix = if_else(ano >= 2020, rp6_pix, 0),
         rp6_demais = if_else(ano >= 2020, rp6_demais, rp6_siga_filtro),
         emendas = rp6 + rp7 + rp8 + rp9,
         disc_total = disc_min + emendas,
         across(c(disc_min, disc_min_sem_extraord, rp6, rp7, rp8, rp9, emendas),
                ~ if_else(disc_total != 0, .x / disc_total, NA_real_), .names = "sh_{.col}"),
         across(starts_with("n_partidos_"), ~ coalesce(.x, 0L)),
         versao = VERSAO) |>
  select(-rp6_siga_filtro)
stopifnot(nrow(p) == 132)
write_delim(p, file.path(OUT, "painel_componentes.csv"), delim = ";", na = "")

# --- Conferência anual contra o SIOP -----------------------------------------
conf <- p |> group_by(ano) |>
  summarise(across(c(disc_min, disc_min_siga, disc_min_extraord, rp6, rp6_pix, rp7, rp8, rp9, disc_total),
                   ~ round(sum(.x) / 1e3, 2)), .groups = "drop")
salva_csv(conf, "01_totais_anuais_bi_ipca.csv")
print(conf, width = Inf)
