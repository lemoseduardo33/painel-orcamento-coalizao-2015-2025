# =============================================================================
# ICG+ v2 — 05: RP6 mensal com as transferências especiais ("emendas Pix")
#
# Entrada nova: dados_brutos/siga/AAAA_siga_rp6total.csv (2020–2026), exportação
# do SIGA só de RP6 e SEM filtro de Poder, com "Resultado Lei" e "Resultado EOF".
# Emenda Pix = RP6 executado no órgão 73000 (UO "Recursos sob Supervisão do
# Ministério da Fazenda" até 2025; "… do Planejamento e Orçamento" em 2026).
#
# Saídas: icg_plus_v2/saidas/05_rp6_pix/
# =============================================================================

suppressPackageStartupMessages({ library(data.table); library(stringr) })

RAIZ <- if (dir.exists("dados_brutos")) "." else ".."
DB   <- file.path(RAIZ, "dados_brutos")
OUT  <- file.path(RAIZ, "icg_plus_v2", "saidas", "05_rp6_pix")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
salva <- function(x, nome) fwrite(x, file.path(OUT, nome), sep = ";", dec = ",", bom = TRUE)
num_br <- function(x) { x <- str_trim(x); neg <- str_detect(x, "^-")
  v <- suppressWarnings(as.numeric(str_replace(str_replace_all(str_remove(x, "^-\\s*"), "\\.", ""), ",", ".")))
  v[is.na(v)] <- 0; ifelse(neg, -v, v) }
mi <- function(x) round(x / 1e6, 1)

# ------------------------------------------------------------------ leitura ---
arqs <- sort(list.files(file.path(DB, "siga"), "^\\d{4}_siga_rp6total\\.csv$", full.names = TRUE))
r6 <- rbindlist(lapply(arqs, fread, encoding = "UTF-8", colClasses = "character"))
setnames(r6, function(n) str_remove(n, "^﻿"))
r6 <- r6[, .(ano = as.integer(`Ano SIAFI`), mes = as.integer(`Mês (Número) DES`),
             orgao_cod = `Órgão Superior Orçamentário (Cód) DESP`, orgao = `Órgão Superior (UO) DESP`,
             uo = `UO (Ajustada)`, rp_lei = `Resultado Lei (Cod) DESP`, rp_eof = `Resultado EOF (Cod) DESP`,
             dot_ini = num_br(`Dotação Inicial (R$)`), autorizado = num_br(`Autorizado (R$)`),
             empenhado = num_br(`Empenhado (R$)`), empenhado_ipca = num_br(`Empenhado (IPCA)`),
             pago = num_br(`Pago (R$)`), pago_ipca = num_br(`Pago (IPCA)`))]
r6[, tipo := fifelse(orgao_cod == "73000", "pix (transferência especial)", "demais RP6")]

# ---------------------------------------------- 1. conferência com o SIOP -----
se <- fread(file.path(DB, "siop", "siop_emendas_2015a2025_outubro26.csv"), sep = ";",
            encoding = "UTF-8", colClasses = "character")
setnames(se, function(n) str_remove(n, "^﻿"))
se <- se[grepl("^\\d{4}$", Ano) & str_sub(RP, 1, 1) == "6"]
siop <- se[, .(siop_total_mi = mi(sum(num_br(Empenhado))),
               siop_pix_mi = mi(sum(num_br(Empenhado)[str_sub(`Órgão`, 1, 5) == "73000"]))), by = .(ano = as.integer(Ano))]
c1 <- merge(r6[, .(siga_total_mi = mi(sum(empenhado)), siga_pix_mi = mi(sum(empenhado[tipo != "demais RP6"])),
                   rp_eof_diferente_mi = mi(sum(empenhado[rp_eof != "6"]))), by = ano],
            siop, by = "ano", all.x = TRUE)
c1[, `:=`(dif_total_mi = siga_total_mi - siop_total_mi, dif_pix_mi = siga_pix_mi - siop_pix_mi)]
salva(c1[order(ano)], "01_conferencia_siga_rp6_x_siop.csv")

# Fora do Executivo (Judiciário, MPU, Legislativo): quanto de RP6 há em cada ano
salva(r6[, .(empenhado_mi = mi(sum(empenhado))), by = .(ano, orgao_cod, orgao)][empenhado_mi != 0 & orgao_cod < "20000"][order(ano)],
      "01b_rp6_fora_do_executivo.csv")

# ----------------------------------------------- 2. perfil mensal -------------
pm <- r6[mes > 0, .(empenhado = sum(empenhado), empenhado_ipca = sum(empenhado_ipca),
                    pago = sum(pago), pago_ipca = sum(pago_ipca)), by = .(ano, mes, tipo)]
pm[, pct_do_ano := round(100 * empenhado / sum(empenhado), 1), by = .(ano, tipo)]
salva(dcast(pm, tipo + ano ~ mes, value.var = "pct_do_ano", fill = 0), "02_perfil_mensal_pct_do_ano.csv")
salva(dcast(pm, tipo + ano ~ mes, value.var = "empenhado", fun.aggregate = function(x) mi(sum(x)), fill = 0),
      "02b_empenhado_mensal_mi.csv")

# Quanto a regra "perfil do restante do RP6" teria errado (erro absoluto mensal / total do Pix)
cmp <- merge(pm[tipo != "demais RP6", .(ano, mes, pix = empenhado)],
             pm[tipo == "demais RP6", .(ano, mes, perfil = pct_do_ano / 100)], by = c("ano", "mes"), all = TRUE)
cmp[is.na(pix), pix := 0]; cmp[is.na(perfil), perfil := 0]
cmp[, pix_estimado := perfil * sum(pix), by = ano]
salva(cmp[, .(pix_total_mi = mi(sum(pix)), erro_abs_mi = mi(sum(abs(pix - pix_estimado))),
              erro_pct_do_total = round(100 * sum(abs(pix - pix_estimado)) / sum(pix), 0)), by = ano][order(ano)],
      "03_erro_da_regra_proporcional.csv")

# ----------------------------------------------- 3. painel mensal final -------
salva(pm[order(ano, mes, tipo)][, `:=`(empenhado_mi = mi(empenhado), empenhado_ipca_mi = mi(empenhado_ipca),
                                        pago_mi = mi(pago), pago_ipca_mi = mi(pago_ipca))][
        , .(ano, mes, tipo, empenhado_mi, empenhado_ipca_mi, pago_mi, pago_ipca_mi)],
      "10_painel_mensal_rp6_pix_v0.csv")

cat("Concluído:", normalizePath(OUT), "\n")
