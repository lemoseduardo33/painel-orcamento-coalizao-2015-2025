# =============================================================================
# ICG+ v2 — 01: Conciliação orçamentária SIGA × SIOP (2015–2025)
#
# Entradas (dados_brutos/, baixadas em 18/10/2026):
#   siga/siga_AAAA_outubro26.csv           SIGA Brasil, mensal (mês de execução),
#                                          UO × RP × tipo de crédito, R$ e IPCA
#   siop/siop_execucao_2015a2025_outubro26.xlsx  SIOP, anual, Poder Executivo,
#                                          órgão × UO × GND × modalidade × RP × origem do crédito
#   siop/siop_emendas_2015a2025_outubro26.csv    SIOP, anual, emenda a emenda
#
# Saídas: icg_plus_v2/saidas/01_conciliacao/
#
# Rodar no RStudio com o diretório de trabalho na pasta "Mestrado".
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(readxl); library(stringr)
})

RAIZ <- if (dir.exists("dados_brutos")) "." else ".."
DB   <- file.path(RAIZ, "dados_brutos")
OUT  <- file.path(RAIZ, "icg_plus_v2", "saidas", "01_conciliacao")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
salva <- function(x, nome) fwrite(x, file.path(OUT, nome), sep = ";", dec = ",", bom = TRUE)

# Converte números em formato brasileiro, inclusive negativos "- 1.234,56"
num_br <- function(x) {
  x <- str_trim(as.character(x))
  neg <- str_detect(x, "^-\\s*\\d")
  x <- str_remove(x, "^-\\s*")
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace(x, ",", ".")
  v <- suppressWarnings(as.numeric(x))
  v[x %in% c("", "-", NA)] <- 0
  ifelse(neg, -v, v)
}
cod5 <- function(x) str_sub(str_trim(x), 1, 5)
rp1  <- function(x) str_sub(str_trim(x), 1, 1)
bi   <- function(x) round(x / 1e9, 3)

# -----------------------------------------------------------------------------
# 1. Leitura
# -----------------------------------------------------------------------------
arqs <- list.files(file.path(DB, "siga"), pattern = "^siga_\\d{4}.*\\.csv$", full.names = TRUE)
siga <- rbindlist(lapply(arqs, fread, encoding = "UTF-8", colClasses = "character"))
setnames(siga, function(n) str_remove(n, "^﻿"))
mets <- c("PL", "Dotação Inicial", "Autorizado", "Empenhado", "Liquidado", "Pago")
siga <- siga[, .(
  ano        = as.integer(`Ano SIAFI`),
  mes        = as.integer(`Mês (Número) DES`),
  orgao_cod  = `Órgão Superior Orçamentário (Cód) DESP`,
  orgao      = `Órgão Superior (UO) DESP`,
  uo_cod     = `UO (Cod) DESP`,
  uo         = `UO Desp`,
  rp         = `Resultado Lei (Cod) DESP`,
  modalidade_emenda = `Emenda (Modalidade)`,
  tipo_credito = `Tipo Crédito`,
  dot_ini   = num_br(`Dotação Inicial (R$)`), dot_ini_ipca = num_br(`Dotação Inicial (IPCA)`),
  autorizado = num_br(`Autorizado (R$)`),     autorizado_ipca = num_br(`Autorizado (IPCA)`),
  empenhado = num_br(`Empenhado (R$)`),       empenhado_ipca = num_br(`Empenhado (IPCA)`),
  liquidado = num_br(`Liquidado (R$)`),       liquidado_ipca = num_br(`Liquidado (IPCA)`),
  pago      = num_br(`Pago (R$)`),            pago_ipca = num_br(`Pago (IPCA)`))]
siga[, restos_a_pagar := str_detect(tipo_credito, "^RP ")]
siga[, credito := fcase(str_detect(tipo_credito, "EXTRAORD"), "extraordinario",
                        str_detect(tipo_credito, "ESPECIAL"), "especial",
                        str_detect(tipo_credito, "INICIAL"),  "inicial_LOA",
                        default = "nao_aplicavel")]

siop_x <- as.data.table(read_excel(file.path(DB, "siop", "siop_execucao_2015a2025_outubro26.xlsx")))
siop_x <- siop_x[grepl("^\\d{4}$", Ano)]   # descarta linha "Total" e rodapé de filtros
siop_x <- siop_x[, .(
  ano = as.integer(Ano), orgao_cod = cod5(`Órgão Orçamentário`), orgao = `Órgão Orçamentário`,
  uo_cod = cod5(`Unidade Orçamentária`), gnd = `Grupo de Despesa`,
  modalidade = `Modalidade de Aplicação`, rp = rp1(`Resultado Primário`),
  origem_credito = `Origem do Crédito`,
  dot_ini = as.numeric(`Dotação Inicial`), dot_atual = as.numeric(`Dotação Atual`),
  empenhado = as.numeric(Empenhado), liquidado = as.numeric(Liquidado), pago = as.numeric(Pago))]
siop_x[, credito := fcase(str_detect(origem_credito, "Extraordin"), "extraordinario",
                          str_detect(origem_credito, "Especial"), "especial",
                          default = "inicial_LOA")]

siop_e <- fread(file.path(DB, "siop", "siop_emendas_2015a2025_outubro26.csv"),
                sep = ";", encoding = "UTF-8", colClasses = "character")
setnames(siop_e, function(n) str_remove(n, "^﻿"))
siop_e <- siop_e[grepl("^\\d{4}$", Ano)]
siop_e <- siop_e[, .(
  ano = as.integer(Ano), rp = rp1(RP), autor = Autor, tipo_autor = `Tipo Autor`,
  partido = Partido, uf = `UF Autor`, emenda = `Nro. Emenda`,
  orgao_cod = cod5(`Órgão`), orgao = `Órgão`, uo_cod = cod5(UO), acao = `Ação`,
  gnd = GND, modalidade = Modalidade,
  dot_ini = num_br(`Dotação Inicial Emenda`), dot_atual = num_br(`Dotação Atual Emenda`),
  empenhado = num_br(Empenhado), liquidado = num_br(Liquidado), pago = num_br(Pago))]

# -----------------------------------------------------------------------------
# 2. Inventário das bases
# -----------------------------------------------------------------------------
inv <- rbind(
  data.table(base = "SIGA", linhas = nrow(siga), anos = paste(range(siga$ano), collapse = "–"),
             rps = paste(sort(unique(siga$rp)), collapse = ","),
             orgaos = uniqueN(siga$orgao_cod)),
  data.table(base = "SIOP execução", linhas = nrow(siop_x), anos = paste(range(siop_x$ano), collapse = "–"),
             rps = paste(sort(unique(siop_x$rp)), collapse = ","),
             orgaos = uniqueN(siop_x$orgao_cod)),
  data.table(base = "SIOP emendas", linhas = nrow(siop_e), anos = paste(range(siop_e$ano), collapse = "–"),
             rps = paste(sort(unique(siop_e$rp)), collapse = ","),
             orgaos = uniqueN(siop_e$orgao_cod)))
salva(inv, "00_inventario_bases.csv")

# Restos a pagar no SIGA: devem ter empenhado zero
rap <- siga[, .(empenhado_bi = bi(sum(empenhado)), pago_bi = bi(sum(pago))),
            by = .(restos_a_pagar)]
salva(rap, "00b_siga_restos_a_pagar.csv")

# -----------------------------------------------------------------------------
# 3. C1 — Totais por ano × RP (empenhado, dotação inicial, pago)
# -----------------------------------------------------------------------------
s1 <- siga[restos_a_pagar == FALSE, .(siga_emp = sum(empenhado), siga_dot = sum(dot_ini),
                                      siga_pago_exerc = sum(pago)), by = .(ano, rp)]
o1 <- siop_x[, .(siop_emp = sum(empenhado), siop_dot = sum(dot_ini), siop_pago = sum(pago)),
             by = .(ano, rp)]
c1 <- merge(s1, o1, by = c("ano", "rp"), all = TRUE)
for (v in names(c1)[-(1:2)]) set(c1, which(is.na(c1[[v]])), v, 0)
c1[, `:=`(dif_emp_bi = bi(siga_emp - siop_emp),
          dif_emp_pct = round(100 * (siga_emp - siop_emp) / pmax(abs(siop_emp), 1), 2),
          dif_dot_pct = round(100 * (siga_dot - siop_dot) / pmax(abs(siop_dot), 1), 2))]
c1_out <- c1[, .(ano, rp, siga_emp_bi = bi(siga_emp), siop_emp_bi = bi(siop_emp),
                 dif_emp_bi, dif_emp_pct, siga_dot_bi = bi(siga_dot), siop_dot_bi = bi(siop_dot),
                 dif_dot_pct)][order(rp, ano)]
salva(c1_out, "01_C1_siga_x_siop_ano_rp.csv")

# -----------------------------------------------------------------------------
# 4. C2 — Órgãos: presença e diferenças (todos os RPs; foco RP2/RP3)
# -----------------------------------------------------------------------------
s2 <- siga[restos_a_pagar == FALSE, .(siga_emp = sum(empenhado)), by = .(ano, orgao_cod, rp)]
o2 <- siop_x[, .(siop_emp = sum(empenhado), orgao_siop = first(orgao)), by = .(ano, orgao_cod, rp)]
c2 <- merge(s2, o2, by = c("ano", "orgao_cod", "rp"), all = TRUE)
c2[is.na(siga_emp), siga_emp := 0]; c2[is.na(siop_emp), siop_emp := 0]
nomes_org <- unique(rbind(siop_x[, .(orgao_cod, nome = orgao)],
                          siga[, .(orgao_cod, nome = paste(orgao_cod, "-", orgao))]), by = "orgao_cod")
c2 <- merge(c2[, orgao_siop := NULL], nomes_org, by = "orgao_cod", all.x = TRUE)
c2[, dif := siga_emp - siop_emp]

# 4a. Órgãos com execução no SIOP e ausentes no SIGA (e vice-versa), por ano × RP
ausentes <- c2[abs(dif) > 1e6 & (siga_emp == 0 | siop_emp == 0),
               .(ano, rp, orgao_cod, nome, siga_bi = bi(siga_emp), siop_bi = bi(siop_emp))][order(-abs(siop_bi - siga_bi))]
salva(ausentes, "02a_C2_orgaos_ausentes_em_uma_base.csv")

# 4b. Diferenças em RP2 e RP3 por órgão (mais de R$ 10 mi ou 1%)
rp23 <- c2[rp %in% c("2", "3")][, .(ano, rp, orgao_cod, nome, siga_bi = bi(siga_emp), siop_bi = bi(siop_emp),
                                    dif_bi = bi(dif), dif_pct = round(100 * dif / pmax(abs(siop_emp), 1), 2))]
salva(rp23[abs(dif_bi) > 0.01 | abs(dif_pct) > 1][order(-abs(dif_bi))], "02b_C2_rp2_rp3_orgaos_divergentes.csv")
salva(rp23[order(ano, rp, -siop_bi)], "02c_C2_rp2_rp3_por_orgao_completo.csv")

# -----------------------------------------------------------------------------
# 5. C3 — Emendas RP6–RP9: SIGA × SIOP execução × SIOP emendas
# -----------------------------------------------------------------------------
e_siga <- siga[restos_a_pagar == FALSE & rp %in% 6:9, .(siga = sum(empenhado)), by = .(ano, rp)]
e_sx   <- siop_x[rp %in% 6:9, .(siop_execucao = sum(empenhado)), by = .(ano, rp)]
e_se   <- siop_e[rp %in% 6:9, .(siop_emendas = sum(empenhado)), by = .(ano, rp)]
c3 <- Reduce(function(a, b) merge(a, b, by = c("ano", "rp"), all = TRUE), list(e_siga, e_sx, e_se))
for (v in c("siga", "siop_execucao", "siop_emendas")) set(c3, which(is.na(c3[[v]])), v, 0)
c3_out <- c3[, .(ano, rp, siga_bi = bi(siga), siop_execucao_bi = bi(siop_execucao),
                 siop_emendas_bi = bi(siop_emendas),
                 dif_siga_siopexec_bi = bi(siga - siop_execucao),
                 dif_siopemendas_siopexec_bi = bi(siop_emendas - siop_execucao))][order(rp, ano)]
salva(c3_out, "03a_C3_emendas_tres_bases_ano_rp.csv")

# Emendas por órgão nas três bases (para localizar diferenças)
e3o <- Reduce(function(a, b) merge(a, b, by = c("ano", "rp", "orgao_cod"), all = TRUE), list(
  siga[restos_a_pagar == FALSE & rp %in% 6:9, .(siga = sum(empenhado)), by = .(ano, rp, orgao_cod)],
  siop_x[rp %in% 6:9, .(siop_execucao = sum(empenhado)), by = .(ano, rp, orgao_cod)],
  siop_e[rp %in% 6:9, .(siop_emendas = sum(empenhado)), by = .(ano, rp, orgao_cod)]))
for (v in c("siga", "siop_execucao", "siop_emendas")) set(e3o, which(is.na(e3o[[v]])), v, 0)
e3o <- merge(e3o, nomes_org, by = "orgao_cod", all.x = TRUE)
e3o_out <- e3o[abs(siga - siop_execucao) > 1e6 | abs(siop_emendas - siop_execucao) > 1e6,
               .(ano, rp, orgao_cod, nome, siga_bi = bi(siga), siop_execucao_bi = bi(siop_execucao),
                 siop_emendas_bi = bi(siop_emendas))][order(ano, rp, -siop_execucao_bi)]
salva(e3o_out, "03b_C3_emendas_orgaos_divergentes.csv")

# Tipo de autor × RP no SIOP emendas (consistência)
salva(dcast(siop_e[, .(emp_bi = bi(sum(empenhado))), by = .(ano, rp, tipo_autor)],
            ano + tipo_autor ~ rp, value.var = "emp_bi", fill = 0),
      "03c_C3_siop_emendas_tipo_autor_x_rp.csv")

# Emendas sem partido ou com partido ausente (para a proxy partidária)
salva(siop_e[rp %in% 6:9, .(n_linhas = .N, emp_bi = bi(sum(empenhado))),
             by = .(ano, rp, tipo_autor, sem_partido = partido %in% c("", "S/PARTIDO", "-", NA))][order(ano, rp)],
      "03d_C3_siop_emendas_cobertura_partido.csv")

# -----------------------------------------------------------------------------
# 6. C4 — Créditos extraordinários e especiais
# -----------------------------------------------------------------------------
c4 <- merge(
  siga[restos_a_pagar == FALSE, .(siga_bi = bi(sum(empenhado))), by = .(ano, rp, credito)],
  siop_x[, .(siop_bi = bi(sum(empenhado))), by = .(ano, rp, credito)],
  by = c("ano", "rp", "credito"), all = TRUE)
salva(c4[order(rp, ano, credito)], "04a_C4_credito_por_tipo_ano_rp.csv")
salva(siop_x[credito == "extraordinario" & rp %in% c("2", "3"),
             .(emp_bi = bi(sum(empenhado))), by = .(ano, rp, orgao)][emp_bi > 0.05][order(ano, -emp_bi)],
      "04b_C4_extraordinario_rp2_rp3_por_orgao.csv")

# -----------------------------------------------------------------------------
# 7. Anomalias no SIGA mensal
# -----------------------------------------------------------------------------
# 7a. Linhas com RP não informado ou fora de 0–9
salva(siga[!rp %in% as.character(0:9), .(n = .N, emp_bi = bi(sum(empenhado))), by = .(ano, rp)],
      "05a_siga_rp_nao_informado.csv")

# 7b. Meses com empenho líquido negativo por órgão × RP (cancelamentos > novos empenhos)
mens <- siga[restos_a_pagar == FALSE, .(emp = sum(empenhado)), by = .(ano, mes, orgao_cod, rp)]
salva(mens[emp < -1e6 & rp %in% c("2", "3", "6", "7", "8", "9"),
           .(ano, mes, orgao_cod, rp, emp_mi = round(emp / 1e6, 1))][order(emp_mi)],
      "05b_siga_meses_negativos_orgao_rp.csv")

# 7c. Mês 00 (abertura) — deve estar vazio de execução
salva(siga[mes == 0, .(n = .N, dot_ini_bi = bi(sum(dot_ini)), emp_bi = bi(sum(empenhado))), by = ano],
      "05c_siga_mes_00.csv")

# 7d. Perfil mensal do empenho por RP (% do ano)
perfil <- siga[restos_a_pagar == FALSE & mes > 0, .(emp = sum(empenhado)), by = .(ano, mes, rp)]
perfil[, pct := round(100 * emp / sum(emp), 1), by = .(ano, rp)]
salva(dcast(perfil[rp %in% c("2", "3", "6", "7", "8", "9")], rp + ano ~ mes, value.var = "pct", fill = 0),
      "05d_siga_perfil_mensal_pct_ano.csv")

# 7e. Maiores UOs em RP2 por ano (identifica valores atípicos)
top_uo <- siga[restos_a_pagar == FALSE & rp == "2", .(emp_bi = bi(sum(empenhado))), by = .(ano, credito, uo_cod, uo)]
salva(top_uo[order(ano, -emp_bi)][, head(.SD, 5), by = ano], "05e_siga_top5_uo_rp2_por_ano.csv")

# 7f. Mês-base do IPCA: fator = valor IPCA / valor nominal, por ano-mês
fator <- siga[mes > 0 & abs(empenhado) > 1e6, .(fator = median(empenhado_ipca / empenhado)), by = .(ano, mes)]
salva(fator[order(ano, mes)], "05f_siga_fator_ipca_por_mes.csv")

# -----------------------------------------------------------------------------
# 8. Bases derivadas (versão 0, não congeladas)
# -----------------------------------------------------------------------------
painel <- siga[restos_a_pagar == FALSE & mes > 0,
               .(empenhado = sum(empenhado), empenhado_ipca = sum(empenhado_ipca),
                 liquidado = sum(liquidado), liquidado_ipca = sum(liquidado_ipca),
                 pago = sum(pago), pago_ipca = sum(pago_ipca)),
               by = .(ano, mes, orgao_cod, orgao, rp, credito)]
salva(painel[order(ano, mes, orgao_cod, rp)], "10_painel_mensal_siga_orgao_rp_v0.csv")

cat("Conciliação concluída. Saídas em", normalizePath(OUT), "\n")
