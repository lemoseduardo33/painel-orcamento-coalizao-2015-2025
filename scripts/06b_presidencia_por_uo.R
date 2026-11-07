# =============================================================================
# ICG+ v2 — 06b: Presidência da República (órgão 20000) separada por UO
# -----------------------------------------------------------------------------
# Casa Civil, SG, Secom, Segov e SRI executam todas na UO 20101; não há como
# separá-las no SIGA. Regra (decisão de 21/10/2026, D-25):
#   - UO 20101 (núcleo da PR)            -> titular da CASA CIVIL (pasta coordenadora)
#   - UOs com vínculo identificável      -> a pasta correspondente (tabela UO_PASTA);
#     se a pasta não existir no mês, vai para a Casa Civil
#   - robustez: órgão inteiro para o partido do presidente (regra anterior)
# Entradas: dados_brutos/siga/siga_AAAA_outubro26.csv; saídas do script 06
# Saída: icg_plus_v2/saidas/06_base_ministerial/06_presidencia_uo_mes.csv
# Ordem: 06 -> 06b -> 07
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(stringr) })
OUT <- "icg_plus_v2/saidas/06_base_ministerial"
num_br <- function(x) { x <- str_trim(x); neg <- str_detect(x, "^-")
  v <- suppressWarnings(as.numeric(str_replace(str_replace_all(str_remove(x, "^-\\s*"), "\\.", ""), ",", ".")))
  v[is.na(v)] <- 0; ifelse(neg, -v, v) }

UO_PASTA <- data.table(
  uo_cod = c("20101", "20118", "20415", "20927", "20204", "20209", "20129", "20201",
             "20122", "20124", "20416"),
  pasta  = c("CASA CIVIL", "GABINETE DE SEGURANÇA INSTITUCIONAL", "SECRETARIA DE COMUNICAÇÃO SOCIAL",
             "CASA CIVIL", "CASA CIVIL", "CASA CIVIL", "CASA CIVIL", "CASA CIVIL",
             "SECRETARIA DE GOVERNO", "SECRETARIA GERAL DA PRESIDÊNCIA DA REPÚBLICA",
             "SECRETARIA GERAL DA PRESIDÊNCIA DA REPÚBLICA"),
  nota   = c("núcleo da PR (todas as secretarias)", "ABIN", "EBC", "Imprensa Nacional", "ITI", "ANPD",
             "Sead (Casa Civil, 2017–18)", "INCRA (Sead/Casa Civil, 2017–18)",
             "SNPM (2018)", "Aquicultura e Pesca (2018)", "EPL (PPI, 2017–18)"))

arqs <- list.files("dados_brutos/siga", pattern = "^siga_\\d{4}_outubro26\\.csv$", full.names = TRUE)
s <- rbindlist(lapply(arqs, function(f) {
  d <- fread(f, encoding = "UTF-8", colClasses = "character")
  setnames(d, function(n) str_remove(n, "^﻿"))
  d[`Órgão Superior Orçamentário (Cód) DESP` == "20000"]
}))
s <- s[, .(ano = as.integer(`Ano SIAFI`), mes = as.integer(`Mês (Número) DES`),
           uo_cod = `UO (Cod) DESP`, uo = `UO Desp`, rp = `Resultado Lei (Cod) DESP`,
           tipo_credito = `Tipo Crédito`, empenhado_ipca = num_br(`Empenhado (IPCA)`))]
s <- s[!str_detect(tipo_credito, "^RP ") & rp %in% c("2", "3") & mes %in% 1:12]  # mês "00" vem vazio
s[, credito := fcase(str_detect(tipo_credito, "EXTRAORD"), "extraordinario",
                     str_detect(tipo_credito, "ESPECIAL"), "especial",
                     str_detect(tipo_credito, "INICIAL"), "inicial_LOA", default = "nao_aplicavel")]
s <- s[, .(empenhado_ipca = sum(empenhado_ipca)), by = .(ano, mes, uo_cod, uo, rp = as.integer(rp), credito)]
s <- merge(s, UO_PASTA[, .(uo_cod, pasta)], by = "uo_cod", all.x = TRUE)
s[is.na(pasta), pasta := "CASA CIVIL"]

pm <- fread(file.path(OUT, "01_pasta_mes.csv"), sep = ";", encoding = "UTF-8")
pm <- pm[, .(ano, mes, pasta = orgao, nome, partido_cota, tipo_indicacao)]
s <- merge(s, pm, by = c("ano", "mes", "pasta"), all.x = TRUE)
# pasta inexistente no mês -> Casa Civil
falta <- s[is.na(nome)]
if (nrow(falta)) {
  falta[, c("nome", "partido_cota", "tipo_indicacao") := NULL]
  falta <- merge(falta[, pasta := "CASA CIVIL"], pm, by = c("ano", "mes", "pasta"), all.x = TRUE)
  s <- rbind(s[!is.na(nome)], falta)
}
if (anyNA(s$nome)) { print(unique(s[is.na(nome), .(ano, mes, uo_cod, pasta)])); stop("sem titular") }
pres <- function(a, m) { d <- as.Date(sprintf("%d-%02d-15", a, m))
  fcase(d < as.Date("2016-05-12"), "PT", d < as.Date("2017-12-19"), "PMDB", d < as.Date("2019-01-01"), "MDB",
        d < as.Date("2019-11-20"), "PSL", d < as.Date("2021-11-30"), "SEM PARTIDO",
        d < as.Date("2023-01-01"), "PL", default = "PT") }
s[, pp := pres(ano, mes)]
s[, grupo := fcase(partido_cota %in% c("SEM PARTIDO", "VAGO"), "sem_partido",
                   partido_cota == pp, "partido_presidente", default = "outros_partidos_D1")]
s[, grupo_D2 := fifelse(grupo == "outros_partidos_D1" & tipo_indicacao != "bancada",
                        paste0("D1_fora_D2_", tipo_indicacao),
                        fifelse(grupo == "outros_partidos_D1", "outros_partidos_D2", grupo))]
s[, pp := NULL]
fwrite(s[order(ano, mes, uo_cod, rp, credito)], file.path(OUT, "06_presidencia_uo_mes.csv"), sep = ";")
print(s[, .(emp_mi = round(sum(empenhado_ipca) / 1e6)), by = .(ano, grupo)][order(ano, grupo)])
