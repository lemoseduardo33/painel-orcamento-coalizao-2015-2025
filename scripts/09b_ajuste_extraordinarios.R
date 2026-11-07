# =============================================================================
# 09b_ajuste_d45.R — Sessão 6: painel v1.1 com a decisão D-45
# Diretório de trabalho = Mestrado/. Rodar depois do 09 e antes do 11.
#
# D-45 (27/10/2026): os créditos extraordinários VOLTAM ao
# discricionário ministerial (revisa a D-31), com duas exclusões declaradas:
#   (a) Auxílio emergencial: créditos extraordinários da UO 55101 (Ministério da
#       Cidadania/MDS, administração direta), RP2/RP3, em 2020 e 2021.
#       Transferência de renda emergencial, não moeda de coalizão. O FNAS (UO
#       55901) fica, porque financia a rede socioassistencial dos municípios.
#   (b) Cessão onerosa: créditos especiais de 2019 nas UOs 32101 (MME, adm.
#       direta: pagamento à Petrobras pela revisão do contrato, out/2019) e
#       73104 (Recursos sob supervisão do MME: repasse de parte do bônus a
#       estados e municípios, dez/2019). Pagamento único e obrigatório.
# Robustez (três versões do Executivo):
#   disc_min_d45       = principal (tudo, menos (a) e (b))
#   disc_min           = todos dentro (inclusive o auxílio e a cessão onerosa)
#   disc_min_d45_fora  = todos os extraordinários fora e sem a cessão onerosa
#
# Valores: IPCA do SIGA × fator de reescala SIOP/SIGA por órgão × ano × RP (D-06),
# a mesma regra do script 07. As colunas dm45_* / dmD2_45_* são os grupos do
# discricionário ministerial (dm_*) sem as exclusões, para o script 12.
# Saídas: icg_plus_v2/saidas/09_painel_v1.1/
# =============================================================================
suppressPackageStartupMessages(library(data.table))
S   <- "icg_plus_v2/saidas"
OUT <- file.path(S, "09_painel_v1.1")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
le <- function(f, ...) { x <- fread(f, sep = ";", encoding = "UTF-8", ...)
  setnames(x, function(n) sub("^﻿", "", n)); x }
num_br <- function(x) {
  x <- trimws(as.character(x)); neg <- grepl("^-\\s*\\d", x)
  x <- sub("^-\\s*", "", x); x <- gsub(".", "", x, fixed = TRUE); x <- sub(",", ".", x, fixed = TRUE)
  v <- suppressWarnings(as.numeric(x)); v[x %in% c("", "-") | is.na(v)] <- 0
  fifelse(neg, -v, v)
}

# --- 1. Exclusões a partir do SIGA bruto -----------------------------------------
le_siga <- function(ano) {
  x <- fread(sprintf("dados_brutos/siga/siga_%d_outubro26.csv", ano), encoding = "UTF-8",
             colClasses = "character")
  setnames(x, function(n) sub("^﻿", "", n))
  x[, .(ano = ano, mes = as.integer(`Mês (Número) DES`),
        orgao_cod = as.integer(`Órgão Superior Orçamentário (Cód) DESP`),
        uo = `UO (Cod) DESP`, rp = `Resultado Lei (Cod) DESP`, credito = `Tipo Crédito`,
        empenhado_ipca = num_br(`Empenhado (IPCA)`), liquidado_ipca = num_br(`Liquidado (IPCA)`),
        pago_ipca = num_br(`Pago (IPCA)`))]
}
siga <- rbindlist(lapply(2019:2021, le_siga))
siga <- siga[rp %in% c("2", "3")][, rp := as.integer(rp)]
excl <- rbind(
  siga[ano %in% 2020:2021 & uo == "55101" & credito == "EXTRAORDINARIO"][, item := "auxilio_emergencial"],
  siga[ano == 2019 & uo %in% c("32101", "73104") & credito == "ESPECIAL"][, item := "cessao_onerosa"])
excl <- excl[, lapply(.SD, sum), by = .(item, ano, mes, orgao_cod, rp),
             .SDcols = c("empenhado_ipca", "liquidado_ipca", "pago_ipca")]

# fator de reescala SIOP/SIGA (mesma regra do 07)
f_org <- le(file.path(S, "01_conciliacao/02c_C2_rp2_rp3_por_orgao_completo.csv"), dec = ",")
f_org <- f_org[, .(ano = as.integer(ano), rp = as.integer(rp), orgao_cod = as.integer(orgao_cod),
                   fator = fifelse(abs(siga_bi) >= 0.05, siop_bi / siga_bi, 1))]
excl <- merge(excl, f_org, by = c("ano", "rp", "orgao_cod"), all.x = TRUE)
excl[is.na(fator), fator := 1]
fwrite(excl, file.path(OUT, "01_exclusoes_d45_orgao_mes.csv"), sep = ";", dec = ",")
print(excl[, .(empenhado_bi_ipca = round(sum(empenhado_ipca) / 1e9, 1),
               reescalado_bi = round(sum(empenhado_ipca * fator) / 1e9, 1)), by = .(item, ano)])

# --- 2. Painel v1.1 -----------------------------------------------------------------
d <- le(file.path(S, "09_painel_v1.0/base_painel_mensal_v1.0_2026-10-21.csv"), dec = ".")
e_mes <- dcast(excl[, .(v = sum(empenhado_ipca * fator) / 1e6), by = .(ano, mes, item)],
               ano + mes ~ item, value.var = "v", fill = 0)
setnames(e_mes, c("auxilio_emergencial", "cessao_onerosa"), c("excl_auxilio", "excl_cessao"))
d <- merge(d, e_mes, by = c("ano", "mes"), all.x = TRUE, sort = FALSE)
for (k in c("excl_auxilio", "excl_cessao")) set(d, which(is.na(d[[k]])), k, 0)
setorder(d, ano, mes)
d[, `:=`(disc_min_d45 = disc_min - excl_auxilio - excl_cessao,
         disc_min_d45_fora = disc_min_sem_extraord - excl_cessao)]

# grupos (dm_*, dmD2_*) sem as exclusões, pelo controlador do órgão no mês
ctrl <- le(file.path(S, "06_base_ministerial/04_orgao_mes_controle.csv"), colClasses = "character")
ctrl <- unique(ctrl[, .(ano = as.integer(ano), mes = as.integer(mes),
                        orgao_cod = as.integer(orgao_cod), grupo, grupo_D2)], by = c("ano", "mes", "orgao_cod"))
eg <- merge(excl[, .(v = sum(empenhado_ipca * fator) / 1e6), by = .(ano, mes, orgao_cod)],
            ctrl, by = c("ano", "mes", "orgao_cod"), all.x = TRUE)
eg <- eg[mes >= 1 & abs(v) > 0]          # mês 00 do SIGA só tem dotação
stopifnot(!anyNA(eg$grupo), !anyNA(eg$grupo_D2))
print(eg[, .(v = round(sum(v) / 1e3, 1)), by = .(ano, grupo, grupo_D2)])
dm  <- grep("^dm_(outros|partido|sem)", names(d), value = TRUE)
dm2 <- grep("^dmD2_(outros|partido|sem|D1)", names(d), value = TRUE)
for (k in dm)  d[, (sub("^dm_", "dm45_", k)) := get(k)]
for (k in dm2) d[, (sub("^dmD2_", "dmD2_45_", k)) := get(k)]
for (i in seq_len(nrow(eg))) {
  r <- eg[i]; w <- which(d$ano == r$ano & d$mes == r$mes)
  c1 <- paste0("dm45_", r$grupo); c2 <- paste0("dmD2_45_", r$grupo_D2)
  stopifnot(c1 %in% names(d), c2 %in% names(d))
  set(d, w, c1, d[[c1]][w] - r$v); set(d, w, c2, d[[c2]][w] - r$v)
}
# conferências
stopifnot(abs(d[, sum(disc_min - disc_min_d45)] - eg[, sum(v)]) < 1)
chk <- d[, abs(rowSums(.SD) - disc_min_d45), .SDcols = patterns("^dm45_")]
cat("Diferença máx. entre soma dos grupos e disc_min_d45 (R$ mi):", round(max(chk), 2), "\n")
d[, versao := "v1.1"]
fwrite(d, file.path(OUT, "base_painel_mensal_v1.1_2026-10-27.csv"), sep = ";", dec = ".")

anual <- d[, .(todos_dentro = sum(disc_min), principal_d45 = sum(disc_min_d45),
               todos_fora = sum(disc_min_d45_fora), auxilio = sum(excl_auxilio),
               cessao = sum(excl_cessao), extraord_total = sum(disc_min_extraord)), by = ano]
anual[, (names(anual)[-1]) := lapply(.SD, function(v) round(v / 1e3, 1)), .SDcols = names(anual)[-1]]
fwrite(anual, file.path(OUT, "02_executivo_tres_versoes_anual_bi.csv"), sep = ";", dec = ",")
print(anual)
