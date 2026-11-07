# =============================================================================
# ICG+ v2 — 02: Diagnóstico das bases de apoiamento (RP8 e RP9, 2020–2025)
#
# Pergunta: as planilhas de apoiamento do Portal da Transparência (download de
# 18/10/2026) reproduzem a execução oficial de RP8 e RP9 do SIOP (mesma data de
# corte)? Onde não reproduzem, por quê? E dá para atribuir partido aos apoiadores?
#
# A planilha de apoiamento é uma lista de ADESÕES: cada linha diz "o parlamentar
# X apoiou o empenho Y", e o valor repete o valor cheio do empenho em todas as
# linhas em que ele aparece. Por isso o total se calcula UM POR EMPENHO
# (aprendizado do pipeline de agosto/2026, retestado aqui).
#
# Entradas: dados_brutos/apoiamentos/apoiamentos_AAAA.csv
#           dados_brutos/siop/siop_emendas_2015a2025_outubro26.csv
# Saídas:   icg_plus_v2/saidas/02_apoiamentos/
# =============================================================================

suppressPackageStartupMessages({ library(data.table); library(stringr); library(stringi) })

RAIZ <- if (dir.exists("dados_brutos")) "." else ".."
DB   <- file.path(RAIZ, "dados_brutos")
OUT  <- file.path(RAIZ, "icg_plus_v2", "saidas", "02_apoiamentos")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
salva <- function(x, nome) fwrite(x, file.path(OUT, nome), sep = ";", dec = ",", bom = TRUE)
num_br <- function(x) { x <- str_trim(x); neg <- str_detect(x, "^-")
  v <- suppressWarnings(as.numeric(str_replace(str_replace_all(str_remove(x, "^-\\s*"), "\\.", ""), ",", ".")))
  v[is.na(v)] <- 0; ifelse(neg, -v, v) }
mi <- function(x) round(x / 1e6, 1)
valor_de <- function(ids) mi(ap[empenho %in% ids, .(v = max(v_empenhado)), by = empenho][, sum(v)])
norm <- function(x) toupper(str_squish(stri_trans_general(x, "Latin-ASCII")))

NOMES <- c("cod_apoiador", "apoiador", "data_apoio", "data_retirada", "empenho",
  "data_mov", "cod_favorecido", "favorecido", "tipo_favorecido", "uf_favorecido",
  "mun_favorecido", "cod_emenda", "rp", "ano_emenda", "cod_autor", "autor",
  "num_emenda", "localidade", "cod_ug", "ug", "cod_uo", "uo", "cod_orgao", "orgao",
  "cod_orgao_sup", "orgao_sup", "cod_acao", "acao", "v_empenhado", "v_cancelado", "v_pago")

# -----------------------------------------------------------------------------
# 1. Leitura e estrutura
# -----------------------------------------------------------------------------
arqs <- sort(list.files(file.path(DB, "apoiamentos"), "^apoiamentos_\\d{4}\\.csv$", full.names = TRUE))
ap <- rbindlist(lapply(arqs, function(f) {
  d <- fread(f, sep = ";", encoding = "Latin-1", colClasses = "character", quote = "\"")
  stopifnot(ncol(d) == length(NOMES)); setnames(d, NOMES)
  d[, (NOMES) := lapply(.SD, enc2utf8)]   # Portal exporta em latin1; grava tudo em UTF-8
  d[, ano_arquivo := as.integer(str_extract(basename(f), "\\d{4}"))]
}))
ap[, `:=`(v_empenhado = num_br(v_empenhado), v_cancelado = num_br(v_cancelado), v_pago = num_br(v_pago),
          ano_emenda = as.integer(ano_emenda),
          ano_ne = as.integer(str_sub(empenho, 12, 15)),
          ano_apoio = as.integer(str_sub(data_apoio, 1, 4)),
          retirado = data_retirada != "")]
ap[, liquido := v_empenhado - v_cancelado]

estrutura <- ap[, .(linhas = .N, empenhos = uniqueN(empenho), emendas = uniqueN(cod_emenda),
                    apoiadores = uniqueN(cod_apoiador),
                    rps = paste(sort(unique(rp)), collapse = ","),
                    linhas_apoio_retirado = sum(retirado)), by = ano_arquivo]
salva(estrutura, "00_estrutura_por_arquivo.csv")

# -----------------------------------------------------------------------------
# 2. Premissas da deduplicação (retestadas)
# -----------------------------------------------------------------------------
p1 <- ap[, .(n_valores = uniqueN(paste(v_empenhado, v_cancelado, v_pago))), by = empenho][n_valores > 1]
p2 <- ap[, .(n_anos = uniqueN(ano_arquivo)), by = empenho][n_anos > 1]
p3 <- ap[ano_arquivo != ano_emenda, .N]
p4 <- ap[, .(n_emendas = uniqueN(cod_emenda)), by = empenho][n_emendas > 1]
premissas <- data.table(
  premissa = c("P1 valor do empenho é o mesmo em todas as suas linhas",
               "P2 empenho aparece em um só ano",
               "P3 ano do arquivo = ano da emenda",
               "P4 empenhos ligados a mais de uma emenda",
               "P5 ano do empenho (NE) = ano da emenda"),
  casos_que_violam = c(nrow(p1), nrow(p2), p3, nrow(p4), ap[ano_ne != ano_emenda, uniqueN(empenho)]),
  valor_envolvido_mi = c(valor_de(p1$empenho), valor_de(p2$empenho),
                         valor_de(ap[ano_arquivo != ano_emenda, unique(empenho)]),
                         valor_de(p4$empenho), valor_de(ap[ano_ne != ano_emenda, unique(empenho)])))
salva(premissas, "01_premissas_deduplicacao.csv")
salva(ap[empenho %in% p4$empenho, .(emendas = paste(sort(unique(cod_emenda)), collapse = " | "),
                                     v_empenhado = max(v_empenhado)), by = .(ano_arquivo, rp, empenho)],
      "01b_empenhos_com_varias_emendas.csv")

# -----------------------------------------------------------------------------
# 3. Redundância interna
# -----------------------------------------------------------------------------
ident <- ap[duplicated(ap[, ..NOMES]), .N, by = ano_arquivo]
rep_ap <- ap[, .N, by = .(ano_arquivo, empenho, cod_apoiador)][N > 1, .(pares_repetidos = .N, linhas_extras = sum(N - 1)), by = ano_arquivo]
salva(merge(ident[, .(ano_arquivo, linhas_identicas = N)], rep_ap, by = "ano_arquivo", all = TRUE),
      "02_redundancia_interna.csv")

# -----------------------------------------------------------------------------
# 4. Base deduplicada por empenho e totais
# -----------------------------------------------------------------------------
emp <- ap[, .(rp = first(rp), ano = first(ano_emenda), cod_emenda = first(cod_emenda),
              n_emendas = uniqueN(cod_emenda),
              cod_orgao_sup = first(cod_orgao_sup), orgao_sup = first(orgao_sup),
              v_empenhado = max(v_empenhado), v_cancelado = max(v_cancelado),
              v_pago = max(v_pago), n_apoiadores = uniqueN(cod_apoiador),
              n_apoiadores_ativos = uniqueN(cod_apoiador[!retirado]),
              ano_ne = first(ano_ne), primeiro_apoio = min(data_apoio)), by = empenho]
emp[, liquido := v_empenhado - v_cancelado]

metodos <- rbind(
  ap[, .(metodo = "1 soma direta das linhas (errado)", v = sum(v_empenhado)), by = .(ano = ano_emenda, rp)],
  unique(ap[, ..NOMES])[, .(metodo = "2 remove linhas idênticas (errado)", v = sum(v_empenhado)), by = .(ano = as.integer(ano_emenda), rp)],
  unique(ap, by = c("empenho", "cod_emenda"))[, .(metodo = "3 um por empenho-emenda (errado)", v = sum(v_empenhado)), by = .(ano = ano_emenda, rp)],
  emp[, .(metodo = "4 um por empenho, bruto", v = sum(v_empenhado)), by = .(ano, rp)],
  emp[, .(metodo = "5 um por empenho, líquido de cancelamentos", v = sum(liquido)), by = .(ano, rp)])
salva(dcast(metodos[, .(ano, rp, metodo, v_mi = mi(v))], metodo ~ ano + rp, value.var = "v_mi"),
      "03_comparacao_de_metodos.csv")

# -----------------------------------------------------------------------------
# 5. Conciliação com o SIOP (empenhado, ano × RP; ano × RP × órgão; emenda a emenda)
# -----------------------------------------------------------------------------
se <- fread(file.path(DB, "siop", "siop_emendas_2015a2025_outubro26.csv"), sep = ";",
            encoding = "UTF-8", colClasses = "character")
setnames(se, function(n) str_remove(n, "^﻿"))
se <- se[grepl("^\\d{4}$", Ano)]
se[, `:=`(ano = as.integer(Ano), rp = paste0("RP", str_sub(RP, 1, 1)), emp = num_br(Empenhado),
          pago = num_br(Pago), cod_orgao_sup = str_sub(`Órgão`, 1, 5),
          chave = paste0(Ano, `Nro. Emenda`), cod_autor = str_sub(`Nro. Emenda`, 1, 4))]
se89 <- se[rp %in% c("RP8", "RP9") & ano >= 2020]

c_ano <- merge(se89[, .(siop_mi = mi(sum(emp))), by = .(ano, rp)],
               emp[, .(apoio_bruto_mi = mi(sum(v_empenhado)), apoio_liquido_mi = mi(sum(liquido)),
                       n_empenhos = .N), by = .(ano, rp)], by = c("ano", "rp"), all = TRUE)
c_ano[, cobertura_bruto_pct := round(100 * apoio_bruto_mi / siop_mi, 1)]
# Comparador: o "Empenhado" do SIOP é o empenhado do exercício. O "Valor Cancelado" do
# apoiamento acumula cancelamentos de qualquer ano (inclusive de restos a pagar), por isso
# a comparação principal usa o valor bruto do empenho; o líquido fica como informação.
salva(c_ano[order(rp, ano)], "04a_conciliacao_ano_rp.csv")

c_org <- merge(se89[, .(siop_mi = mi(sum(emp)), orgao = first(`Órgão`)), by = .(ano, rp, cod_orgao_sup)],
               emp[, .(apoio_bruto_mi = mi(sum(v_empenhado))), by = .(ano, rp, cod_orgao_sup)],
               by = c("ano", "rp", "cod_orgao_sup"), all = TRUE)
for (v in c("siop_mi", "apoio_bruto_mi")) set(c_org, which(is.na(c_org[[v]])), v, 0)
c_org[, `:=`(dif_mi = apoio_bruto_mi - siop_mi, cobertura_pct = round(100 * apoio_bruto_mi / pmax(siop_mi, 0.1), 1))]
salva(c_org[order(ano, rp, -siop_mi)], "04b_conciliacao_ano_rp_orgao.csv")

# Emenda a emenda: chave = ano + número da emenda (8 dígitos: autor + sequencial)
emp[, chave := paste0(ano, str_sub(cod_emenda, 5, 12))]
c_em <- merge(se89[, .(siop = sum(emp), pago_siop = sum(pago), autor = first(Autor)), by = .(chave, ano, rp)],
              emp[, .(apoio = sum(v_empenhado), n_empenhos = .N), by = .(chave)], by = "chave", all = TRUE)
for (v in c("siop", "apoio")) set(c_em, which(is.na(c_em[[v]])), v, 0)
c_em[is.na(ano), ano := as.integer(str_sub(chave, 1, 4))]
c_em[, situacao := fcase(siop > 0 & apoio == 0, "só no SIOP (emenda sem apoiamento)",
                         siop == 0 & apoio > 0, "só no apoiamento",
                         abs(apoio - siop) <= 1, "bate (dif. ≤ R$ 1)",
                         abs(apoio - siop) / pmax(abs(siop), 1) <= 0.01, "bate (dif. ≤ 1%)",
                         apoio < siop, "apoiamento menor",
                         default = "apoiamento maior")]
salva(c_em[, .(n_emendas = .N, siop_mi = mi(sum(siop)), apoio_mi = mi(sum(apoio))),
           by = .(ano, rp, situacao)][order(ano, rp, situacao)], "04c_conciliacao_emenda_a_emenda_resumo.csv")
salva(c_em[situacao != "bate (dif. ≤ R$ 1)"][order(-abs(apoio - siop))][, .(ano, rp, chave, autor, situacao,
           siop_mi = mi(siop), apoio_mi = mi(apoio))], "04d_emendas_divergentes.csv")

# -----------------------------------------------------------------------------
# 6. Apoiadores: quantos por empenho, retiradas, datas, quem são
# -----------------------------------------------------------------------------
salva(emp[, .(n_empenhos = .N, valor_mi = mi(sum(v_empenhado))), by = .(ano, rp, faixa_apoiadores =
        fcase(n_apoiadores == 1, "1", n_apoiadores == 2, "2", n_apoiadores <= 5, "3–5",
              n_apoiadores <= 20, "6–20", default = "mais de 20"))][order(ano, rp, faixa_apoiadores)],
      "05a_apoiadores_por_empenho.csv")

# Defasagem entre a emenda e o registro do apoio (transparência retroativa)
salva(emp[, .(n_empenhos = .N, valor_mi = mi(sum(v_empenhado))),
          by = .(ano, rp, ano_primeiro_apoio = as.integer(str_sub(primeiro_apoio, 1, 4)))][order(ano, rp, ano_primeiro_apoio)],
      "05b_ano_do_registro_do_apoio.csv")

# Apoios retirados
salva(ap[, .(linhas = .N, linhas_retiradas = sum(retirado),
             empenhos_so_com_apoio_retirado = uniqueN(empenho[retirado]) -
               uniqueN(intersect(empenho[retirado], empenho[!retirado]))), by = .(ano_arquivo, rp)],
      "05c_apoios_retirados.csv")

# -----------------------------------------------------------------------------
# 7. Proxy partidária: apoiador → autor no SIOP (código de 4 dígitos)
# -----------------------------------------------------------------------------
# No SIOP, os 4 primeiros dígitos do número da emenda são o código do autor.
# Para cada ano, o partido do autor é o declarado nas emendas RP6 daquele ano.
autores <- se[str_sub(RP, 1, 1) == "6", .(nome_siop = first(Autor), tipo = first(`Tipo Autor`),
                                          partido = paste(sort(unique(Partido)), collapse = "/"),
                                          uf = first(`UF Autor`)), by = .(ano, cod_autor)]
salva(autores[, .(autores = .N, com_mais_de_um_partido = sum(str_detect(partido, "/"))), by = ano],
      "06a_proxy_autores_siop_por_ano.csv")

# Valor de cada empenho rateado igualmente entre apoiadores ativos (só para medir cobertura)
ap_at <- unique(ap[retirado == FALSE, .(empenho, cod_apoiador, apoiador, ano = ano_emenda, rp)])
ap_at <- merge(ap_at, emp[, .(empenho, v_empenhado, n_ativos = n_apoiadores_ativos)], by = "empenho")
ap_at[, valor_rateado := v_empenhado / n_ativos]
ap_at <- merge(ap_at, autores, by.x = c("ano", "cod_apoiador"), by.y = c("ano", "cod_autor"), all.x = TRUE)
# segunda tentativa: mesmo código em qualquer ano (parlamentar sem RP6 naquele ano)
qualquer <- autores[order(-ano)][, .(partido_outro_ano = first(partido), ano_ref = first(ano)), by = cod_autor]
ap_at <- merge(ap_at, qualquer, by.x = "cod_apoiador", by.y = "cod_autor", all.x = TRUE)
ap_at[, match := fcase(!is.na(partido), "código e ano", !is.na(partido_outro_ano), "código, outro ano",
                       default = "sem correspondência")]
ap_at[, nome_confere := is.na(nome_siop) | norm(apoiador) == norm(nome_siop)]
salva(ap_at[, .(apoiadores = uniqueN(cod_apoiador), valor_rateado_mi = mi(sum(valor_rateado))),
            by = .(ano, rp, match)][order(ano, rp, match)], "06b_cobertura_proxy_partidaria.csv")
salva(unique(ap_at[match == "sem correspondência", .(cod_apoiador, apoiador)])[
        ap_at[match == "sem correspondência", .(valor_mi = mi(sum(valor_rateado))), by = cod_apoiador], on = "cod_apoiador"][order(-valor_mi)],
      "06c_apoiadores_sem_correspondencia.csv")
salva(ap_at[match == "código e ano" & !nome_confere, .(valor_mi = mi(sum(valor_rateado))),
            by = .(ano, cod_apoiador, apoiador, nome_siop)][order(-valor_mi)],
      "06d_codigo_igual_nome_diferente.csv")

# Base deduplicada por empenho (v0) para as próximas etapas
salva(emp[order(ano, rp, empenho)], "10_apoiamentos_por_empenho_v0.csv")

cat("Diagnóstico concluído. Saídas em", normalizePath(OUT), "\n")
