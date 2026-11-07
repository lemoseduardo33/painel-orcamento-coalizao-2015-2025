# =============================================================================
# ICG+ v2 — 08: Atribuição partidária do RP8 e do RP9 (2020–2025)
# -----------------------------------------------------------------------------
# Regras (decisoes.md):
#   D-12 um registro por nota de empenho (valor bruto do empenho)
#   D-13 empenho com vários apoiadores -> rateio igual entre os apoiadores ativos
#        (robustez: primeiro apoiador)
#   D-14 partido do apoiador = proxy em cascata (script 04, 05_proxy_...csv);
#        comissões: rateio igual entre todos os parlamentares ("comissao")
#   D-15 apoiador de código 4325 excluído (sem mandato federal no ano das emendas; registros em 28 UFs)
#   D-17 RP9 (e RP8) sem apoiador = "não atribuído" (cenário: rateio entre todos)
# Limitação: a planilha de apoiamentos não traz a data do empenho (só a da
# última movimentação). A atribuição é feita por ano; a série mensal usa a
# participação anual de cada partido, combinada com a coalizão de cada mês.
# Entradas
#   dados_brutos/apoiamentos/apoiamentos_AAAA.csv
#   icg_plus_v2/saidas/04_lideres_coapoio/05_proxy_partidaria_regra_revista.csv
#   icg_plus_v2/saidas/06_base_ministerial/03_partido_mes.csv
#   icg_plus_v2/saidas/07_painel_v1/painel_componentes.csv
# Saídas: icg_plus_v2/saidas/08_atribuicao_rp8_rp9/
#   01_apoio_por_empenho_apoiador.csv  empenho × apoiador: valor rateado e partido
#   02_partido_ano_rp.csv              partido × ano × RP (rateio igual e 1º apoiador)
#   03_rp8_rp9_coalizao_mensal.csv     mês: RP8/RP9 por grupo (presidente, coalizão
#                                      D1/D2, fora da coalizão, comissão, não atribuído)
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(stringr) })
OUT <- "icg_plus_v2/saidas/08_atribuicao_rp8_rp9"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
salva <- function(x, f) fwrite(x, file.path(OUT, f), sep = ";")
num_br <- function(x) { x <- str_trim(x); neg <- str_detect(x, "^-")
  v <- suppressWarnings(as.numeric(str_replace(str_replace_all(str_remove(x, "^-\\s*"), "\\.", ""), ",", ".")))
  v[is.na(v)] <- 0; ifelse(neg, -v, v) }
EXCLUIDO_D15 <- "4325"

# Harmonização de siglas com a base ministerial (rótulo vigente no meio do ano)
harmoniza <- function(p, ano) {
  p <- toupper(str_trim(str_remove(p, "\\(.*\\)")))
  rec <- c(PRB = "REPUBLICANOS", PR = "PL", PMDB = "MDB", PPS = "CIDADANIA", SD = "SOLIDARIEDADE",
           PODE = "PODEMOS", PCDOB = "PCdoB")
  p <- ifelse(p %in% names(rec), rec[p], p)
  p[p == ""] <- NA_character_
  fifelse(p %in% c("DEM", "PSL") & ano >= 2022, "UNIÃO", p)
}

NOMES <- c("cod_apoiador", "apoiador", "data_apoio", "data_retirada", "empenho",
  "data_mov", "cod_favorecido", "favorecido", "tipo_favorecido", "uf_favorecido",
  "mun_favorecido", "cod_emenda", "rp", "ano_emenda", "cod_autor", "autor",
  "num_emenda", "localidade", "cod_ug", "ug", "cod_uo", "uo", "cod_orgao", "orgao",
  "cod_orgao_sup", "orgao_sup", "cod_acao", "acao", "v_empenhado", "v_cancelado", "v_pago")
ap <- rbindlist(lapply(sort(list.files("dados_brutos/apoiamentos", "^apoiamentos_\\d{4}\\.csv$", full.names = TRUE)),
  function(f) { d <- fread(f, sep = ";", encoding = "Latin-1", colClasses = "character")
    setnames(d, NOMES); d[, (NOMES) := lapply(.SD, enc2utf8)] }))
ap[, `:=`(v = num_br(v_empenhado), ano = as.integer(ano_emenda), retirado = data_retirada != "",
          rp = fifelse(str_detect(rp, "9|[Rr]elator"), "RP9", fifelse(str_detect(rp, "8|[Cc]omiss"), "RP8", rp)))]
print(ap[, .N, by = rp])

# Data do apoio: o Portal exporta "AAAA-MM-DD hh:mm:ss" (corrigido na sessão 5;
# antes o formato esperado era "DD/MM/AAAA" e o 1º apoiador saía vazio).
# Empate de horário (ex.: cadastro retroativo do RP9 em 06/03/2025): vale a ordem do arquivo.
dh_apoio <- function(x) { t <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  i <- is.na(t); t[i] <- as.POSIXct(x[i], format = "%d/%m/%Y %H:%M:%S", tz = "UTC"); t }
# Um registro por empenho × apoiador ativo; valor bruto do empenho (D-12)
at <- unique(ap[retirado == FALSE & cod_apoiador != EXCLUIDO_D15 & rp %in% c("RP8", "RP9"),
                .(empenho, cod_apoiador, apoiador, ano, rp, v, data_apoio)])
emp <- at[, .(ano = first(ano), rp = first(rp), v = first(v), n_ap = uniqueN(cod_apoiador),
              primeiro = cod_apoiador[which.min(dh_apoio(data_apoio))],
              empate_1o = sum(dh_apoio(data_apoio) == min(dh_apoio(data_apoio))) > 1),
          by = empenho]
stopifnot(!anyNA(emp$primeiro))
print(emp[n_ap > 1, .(empenhos_multiplos = .N, com_empate_no_1o = sum(empate_1o)), by = .(ano, rp)])
at <- merge(unique(at[, .(empenho, cod_apoiador, apoiador)], by = c("empenho", "cod_apoiador")), emp, by = "empenho")
at[, `:=`(v_rateio = v / n_ap, v_primeiro = fifelse(cod_apoiador == primeiro, v, 0))]

px <- fread("icg_plus_v2/saidas/04_lideres_coapoio/05_proxy_partidaria_regra_revista.csv", sep = ";",
            encoding = "UTF-8", colClasses = "character")
setnames(px, function(n) str_remove(n, "^﻿"))
px <- unique(px[, .(cod_apoiador, ano = as.integer(ano), partido, regra)], by = c("cod_apoiador", "ano"))
at <- merge(at, px, by = c("cod_apoiador", "ano"), all.x = TRUE)
at[, partido := fifelse(str_detect(fcoalesce(regra, ""), "comiss"), "COMISSÃO", harmoniza(partido, ano))]
at[is.na(partido), partido := "SEM PARTIDO IDENTIFICADO"]
salva(at[order(ano, rp, empenho)], "01_apoio_por_empenho_apoiador.csv")

pa <- at[, .(v_rateio = sum(v_rateio), v_primeiro = sum(v_primeiro)), by = .(ano, rp, partido)]
pa[, `:=`(sh_rateio = v_rateio / sum(v_rateio), sh_primeiro = v_primeiro / sum(v_primeiro)), by = .(ano, rp)]
salva(pa[order(ano, rp, -v_rateio)], "02_partido_ano_rp.csv")

# Conferência: total com apoiador × total do painel (o resto é "não atribuído")
painel <- fread("icg_plus_v2/saidas/07_painel_v1/painel_componentes.csv", sep = ";")
tot_ap <- at[, .(com_apoiador_mi = sum(v_rateio) / 1e6), by = .(ano, rp)]

# --- Série mensal por grupo ---------------------------------------------------
pm <- fread("icg_plus_v2/saidas/06_base_ministerial/03_partido_mes.csv", sep = ";")
pres <- function(a, m) { d <- as.Date(sprintf("%d-%02d-15", a, m))
  fcase(d < as.Date("2016-05-12"), "PT", d < as.Date("2019-01-01"), "MDB", d < as.Date("2019-11-20"), "PSL",
        d < as.Date("2021-11-30"), "SEM PARTIDO", d < as.Date("2023-01-01"), "PL", default = "PT") }
meses <- CJ(ano = 2020:2025, mes = 1:12, rp = c("RP8", "RP9"))
g <- merge(meses, pa[, .(ano, rp, partido, sh = sh_rateio)], by = c("ano", "rp"), allow.cartesian = TRUE)
g[, pp := pres(ano, mes)]
g <- merge(g, pm[def == "D1", .(ano, mes, partido, D1 = TRUE)], by = c("ano", "mes", "partido"), all.x = TRUE)
g <- merge(g, pm[def == "D2", .(ano, mes, partido, D2 = TRUE)], by = c("ano", "mes", "partido"), all.x = TRUE)
g[, grupo := fcase(partido == "COMISSÃO", "comissao", partido == "SEM PARTIDO IDENTIFICADO", "sem_partido_identificado",
                   partido == pp, "partido_presidente", D1 %in% TRUE, "coalizao_D1", default = "fora_coalizao_D1")]
g[, grupo_D2 := fcase(grupo %in% c("comissao", "sem_partido_identificado", "partido_presidente"), grupo,
                      D2 %in% TRUE, "coalizao_D2", default = "fora_coalizao_D2")]
sh1 <- g[, .(sh = sum(sh)), by = .(ano, mes, rp, grupo)]
sh2 <- g[, .(sh = sum(sh)), by = .(ano, mes, rp, grupo = grupo_D2)]

# Valor mensal do painel × fração com apoiador no ano × participação do grupo
pv <- melt(painel[ano >= 2020, .(ano, mes, RP8 = rp8, RP9 = rp9)], id.vars = c("ano", "mes"),
           variable.name = "rp", value.name = "valor_mi")
c1 <- fread("icg_plus_v2/saidas/01_conciliacao/01_C1_siga_x_siop_ano_rp.csv", sep = ";", dec = ",",
            colClasses = list(character = "rp"))
setnames(c1, function(n) str_remove(n, "^\ufeff"))
fr <- merge(c1[rp %in% c("8", "9") & ano >= 2020, .(ano, rp = paste0("RP", rp), tot = siop_emp_bi * 1e3)],
            tot_ap, by = c("ano", "rp"), all.x = TRUE)   # ambos nominais, R$ mi
fr[, frac_atrib := fifelse(tot > 0, pmin(1, fcoalesce(com_apoiador_mi, 0) / tot), 0)]
salva(fr, "00_fracao_atribuida_ano_rp.csv")
pv <- merge(pv, fr[, .(ano, rp, frac_atrib)], by = c("ano", "rp"))
mk <- function(sh, def) {
  x <- merge(pv, sh, by = c("ano", "mes", "rp"), allow.cartesian = TRUE)
  x[, v := valor_mi * frac_atrib * sh]
  na <- pv[, .(ano, mes, rp, grupo = "nao_atribuido", v = valor_mi * (1 - frac_atrib))]
  x <- rbind(x[, .(ano, mes, rp, grupo, v)], na)
  dcast(x, ano + mes ~ paste0(tolower(rp), "_", def, "_", grupo), value.var = "v", fun.aggregate = sum, fill = 0)
}
out <- merge(mk(sh1, "D1"), mk(sh2, "D2"), by = c("ano", "mes"))
salva(out, "03_rp8_rp9_coalizao_mensal.csv")
print(fr)
print(pa[, .(top = paste(head(partido[order(-v_rateio)], 5), collapse = ", ")), by = .(ano, rp)])
