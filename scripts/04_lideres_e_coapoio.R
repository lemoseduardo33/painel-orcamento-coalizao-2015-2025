# =============================================================================
# ICG+ v2 — 04: Líderes (2020–2025) e padrão de coapoio nas emendas RP8/RP9
#
# 1. Baixa as lideranças da Câmara (Dados Abertos, legislaturas 56 e 57) e do
#    Senado (Dados Abertos, por senador das legislaturas 56 e 57) e monta a
#    lista de líderes com mandato de liderança entre 2020 e 2025.
# 2. Liga cada líder ao código de apoiador da planilha de apoiamentos (por nome).
# 3. Mede quanto do RP8/RP9 tem mais de um apoiador e quanto disso tem um líder
#    como coapoiador. O apoiador de código 4325 (sem mandato federal no ano das emendas, com registros em 28 UFs) é tratado como outlier e sai da conta (D-15).
# 4. Proxy partidária com a regra revista: mesmo ano > ano seguinte > mais próximo.
# 5. Lista quem apoiou sem ter mandato federal no ano da emenda.
# 6. Confere a hipótese RP9 → RP6 (PEC da Transição).
#
# Os JSON brutos baixados ficam em dados_brutos/lideres/ (data de download no nome).
# Saídas: icg_plus_v2/saidas/04_lideres_coapoio/
# =============================================================================

suppressPackageStartupMessages({ library(data.table); library(stringr); library(stringi); library(jsonlite) })

RAIZ <- if (dir.exists("dados_brutos")) "." else ".."
DB   <- file.path(RAIZ, "dados_brutos")
DL   <- file.path(DB, "lideres"); dir.create(DL, showWarnings = FALSE)
OUT  <- file.path(RAIZ, "icg_plus_v2", "saidas", "04_lideres_coapoio")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
salva <- function(x, nome) fwrite(x, file.path(OUT, nome), sep = ";", dec = ",", bom = TRUE)
norm  <- function(x) toupper(str_squish(str_remove_all(stri_trans_general(x, "Latin-ASCII"), "[\\.']")))
num_br <- function(x) { x <- str_trim(x); neg <- str_detect(x, "^-")
  v <- suppressWarnings(as.numeric(str_replace(str_replace_all(str_remove(x, "^-\\s*"), "\\.", ""), ",", ".")))
  v[is.na(v)] <- 0; ifelse(neg, -v, v) }
mi <- function(x) round(x / 1e6, 1)
pct <- function(a, b) round(100 * a / pmax(b, 1e-9), 1)

# ------------------------------------------------- 1. Download dos líderes ----
# O servidor do Senado às vezes derruba a conexão: tenta até 6 vezes.
get_json <- function(u) { for (i in 1:6) { r <- tryCatch(fromJSON(u), error = function(e) NULL)
  if (!is.null(r)) return(r); Sys.sleep(3) }; stop("falha ao baixar: ", u) }
arq_cam <- file.path(DL, "camara_lideres_leg56_57.json")
arq_sen <- file.path(DL, "senado_liderancas_leg56_57.json")
if (!file.exists(arq_cam)) {
  rows <- list()
  for (L in c(56, 57)) { p <- 1
    repeat {
      d <- get_json(sprintf("https://dadosabertos.camara.leg.br/api/v2/legislaturas/%d/lideres?itens=100&pagina=%d", L, p))$dados
      if (length(d) == 0 || NROW(d) == 0) break
      rows[[length(rows) + 1]] <- flatten(d); p <- p + 1 } }
  write_json(rbindlist(rows, fill = TRUE), arq_cam)
}
if (!file.exists(arq_sen)) {
  sen <- get_json("https://legis.senado.leg.br/dadosabertos/senador/lista/legislatura/56/57.json")
  cods <- sen$ListaParlamentarLegislatura$Parlamentares$Parlamentar$IdentificacaoParlamentar$CodigoParlamentar
  rows <- lapply(cods, function(c) tryCatch(as.data.table(get_json(paste0(
    "https://legis.senado.leg.br/dadosabertos/composicao/lideranca.json?codigoParlamentar=", c))), error = function(e) NULL))
  write_json(rbindlist(rows, fill = TRUE), arq_sen)
}
cam <- as.data.table(fromJSON(arq_cam))
sen <- as.data.table(fromJSON(arq_sen))

lid <- rbind(
  cam[titulo %in% c("Líder", "Representante"),
      .(casa = "Câmara", cargo = titulo, tipo = bancada.tipo, bancada = bancada.nome,
        nome = str_trim(parlamentar.nome), partido = parlamentar.siglaPartido, uf = parlamentar.siglaUf,
        inicio = as.IDate(dataInicio), fim = as.IDate(fifelse(is.na(dataFim) | dataFim == "", "2026-12-31", dataFim)))],
  sen[str_starts(descricaoTipoLideranca, "Líder") & !str_detect(descricaoTipoLideranca, "Câmara"),
      .(casa = fifelse(casa == "CN", "Congresso", "Senado"), cargo = descricaoTipoLideranca,
        tipo = descricaoTipoUnidadeLideranca,
        bancada = fcoalesce(fifelse(siglaTipoUnidadeLideranca == "P", siglaPartido, NA_character_), siglaBloco, nomeBloco,
                            descricaoTipoUnidadeLideranca),
        nome = nomeParlamentar, partido = siglaPartidoFiliacao, uf = uf,
        inicio = as.IDate(dataDesignacao), fim = as.IDate(fifelse(is.na(dataTermino) | dataTermino == "", "2026-12-31", dataTermino)))])
lid <- unique(lid[inicio <= as.IDate("2025-12-31") & fim >= as.IDate("2020-01-01")])
lid[, tipo_resumo := fcase(str_detect(tipo, "Partido"), "partido", str_detect(tipo, "Bloco"), "bloco",
                           str_detect(tipo, "Governo"), "governo", str_detect(tipo, "Maioria"), "maioria",
                           str_detect(tipo, "Minoria"), "minoria", str_detect(tipo, "Oposi"), "oposição",
                           str_detect(tipo, "Feminina"), "bancada feminina", default = NA_character_)]
lid[is.na(tipo_resumo), tipo_resumo := tipo]
for (a in 2020:2025) set(lid, j = paste0("lider_", a),
                         value = lid$inicio <= as.IDate(paste0(a, "-12-31")) & lid$fim >= as.IDate(paste0(a, "-01-01")))
setorder(lid, casa, tipo_resumo, bancada, inicio)
salva(lid, "01_lideres_2020_2025.csv")

# ---------------------------------------- 2. Apoiamentos e ligação por nome ---
NOMES <- c("cod_apoiador", "apoiador", "data_apoio", "data_retirada", "empenho",
  "data_mov", "cod_favorecido", "favorecido", "tipo_favorecido", "uf_favorecido",
  "mun_favorecido", "cod_emenda", "rp", "ano_emenda", "cod_autor", "autor",
  "num_emenda", "localidade", "cod_ug", "ug", "cod_uo", "uo", "cod_orgao", "orgao",
  "cod_orgao_sup", "orgao_sup", "cod_acao", "acao", "v_empenhado", "v_cancelado", "v_pago")
ap <- rbindlist(lapply(sort(list.files(file.path(DB, "apoiamentos"), "^apoiamentos_\\d{4}\\.csv$", full.names = TRUE)),
  function(f) { d <- fread(f, sep = ";", encoding = "Latin-1", colClasses = "character")
    setnames(d, NOMES); d[, (NOMES) := lapply(.SD, enc2utf8)] }))
ap[, `:=`(v = num_br(v_empenhado), ano = as.integer(ano_emenda), retirado = data_retirada != "")]
EXCLUIDO_D15 <- "4325"

# 2015–2025 + 2026 (a base de 2026 entra só para a proxy partidária de quem tomou posse em 2024–2025)
se <- rbindlist(lapply(c("siop_emendas_2015a2025_outubro26.csv", "2026_siop_emendas.csv"), function(f)
  fread(file.path(DB, "siop", f), sep = ";", encoding = "UTF-8", colClasses = "character")), fill = TRUE)
setnames(se, function(n) str_remove(n, "^﻿"))
se <- se[grepl("^\\d{4}$", Ano)]
se[, `:=`(ano = as.integer(Ano), rpn = str_sub(RP, 1, 1), emp = num_br(Empenhado),
          cod_autor = str_sub(`Nro. Emenda`, 1, 4))]
autores <- se[rpn == "6", .(nome_siop = first(Autor), partido = first(Partido), uf_autor = first(`UF Autor`)),
              by = .(ano_ref = ano, cod_autor)]

# Nomes candidatos por código: o da planilha de apoio e os do SIOP
nomes_cod <- unique(rbind(unique(ap[, .(cod = cod_apoiador, n = norm(apoiador))]),
                          unique(autores[, .(cod = cod_autor, n = norm(nome_siop))])))
lid[, n := norm(nome)]
lig <- merge(unique(lid[, .(n, nome, casa)]), nomes_cod, by = "n", all.x = TRUE, allow.cartesian = TRUE)
salva(lig[is.na(cod), .(nome, casa)], "02b_lideres_sem_codigo_de_apoiador.csv")
lid <- merge(lid, unique(lig[!is.na(cod), .(n, cod)]), by = "n", all.x = TRUE, allow.cartesian = TRUE)
salva(lid[, .(casa, cargo, tipo_resumo, bancada, nome, partido, uf, inicio, fim, cod_apoiador = cod)][order(casa, tipo_resumo, bancada, inicio)],
      "02_lideres_2020_2025_com_codigo.csv")

lid_ano <- unique(rbindlist(lapply(2020:2025, function(a)
  lid[get(paste0("lider_", a)) == TRUE & !is.na(cod),
      .(ano = a, cargo_lider = paste(unique(paste(tipo_resumo, bancada)), collapse = " / ")), by = .(cod_apoiador = cod)])),
  by = c("ano", "cod_apoiador"))

# ------------------------------------------------------- 3. Coapoio e líderes -
ap_at <- unique(ap[retirado == FALSE & cod_apoiador != EXCLUIDO_D15,
                   .(empenho, cod_apoiador, apoiador, ano, rp, uf_fav = uf_favorecido, v)])
emp <- ap_at[, .(ano = first(ano), rp = first(rp), v = first(v), uf_fav = first(uf_fav),
                 n_ap = uniqueN(cod_apoiador)), by = empenho]
ap_at <- merge(ap_at, lid_ano, by = c("ano", "cod_apoiador"), all.x = TRUE)
ap_at[, lider := !is.na(cargo_lider)]
ap_at <- merge(ap_at, autores[, .(cod_apoiador = cod_autor, ano_ref, uf_autor)][order(-ano_ref)][, .SD[1], by = cod_apoiador],
               by = "cod_apoiador", all.x = TRUE)
ap_at[, fora_uf := !is.na(uf_autor) & uf_fav != "" & uf_fav != uf_autor]
por_emp <- ap_at[, .(tem_lider = any(lider), tem_nao_lider = any(!lider),
                     lider_fora_uf = any(lider & fora_uf)), by = empenho]
emp <- merge(emp, por_emp, by = "empenho")

emp[, faixa := fcase(n_ap == 1, "1 apoiador", n_ap <= 5, "2 a 5 apoiadores", default = "6 ou mais (coletivo)")]
salva(emp[, .(empenhos = .N, valor_mi = mi(sum(v)),
              com_lider_mi = mi(sum(v[tem_lider])),
              lider_mais_nao_lider_mi = mi(sum(v[tem_lider & tem_nao_lider])),
              lider_fora_uf_mi = mi(sum(v[lider_fora_uf]))), by = .(ano, rp, faixa)][order(rp, ano, faixa)],
      "03b_coapoio_por_faixa_de_apoiadores.csv")
co <- emp[, .(total_mi = mi(sum(v)),
              multi_mi = mi(sum(v[n_ap > 1])),
              multi_com_lider_mi = mi(sum(v[n_ap > 1 & tem_lider])),
              multi_lider_mais_nao_lider_mi = mi(sum(v[n_ap > 1 & tem_lider & tem_nao_lider])),
              multi_lider_fora_uf_mi = mi(sum(v[n_ap > 1 & lider_fora_uf])),
              unico_lider_mi = mi(sum(v[n_ap == 1 & tem_lider]))), by = .(ano, rp)]
co[, `:=`(pct_multi = pct(multi_mi, total_mi),
          pct_do_multi_com_lider = pct(multi_com_lider_mi, multi_mi),
          pct_do_multi_lider_coapoiador = pct(multi_lider_mais_nao_lider_mi, multi_mi),
          pct_do_multi_lider_fora_uf = pct(multi_lider_fora_uf_mi, multi_mi),
          pct_total_lider_sozinho = pct(unico_lider_mi, total_mi))]
salva(co[order(rp, ano)], "03_coapoio_e_lideres_por_ano_rp.csv")

# Quais líderes mais aparecem como coapoiadores (empenhos com líder + não líder)
lc <- ap_at[lider == TRUE][emp[n_ap > 1 & tem_lider & tem_nao_lider, .(empenho)], on = "empenho", nomatch = 0]
salva(lc[, .(empenhos = .N, valor_mi = mi(sum(v / emp[.SD, on = "empenho", n_ap])),
             pct_fora_uf = round(100 * mean(fora_uf), 1)),
         by = .(ano, rp, cod_apoiador, apoiador, cargo_lider)][order(ano, rp, -empenhos)],
      "04_lideres_como_coapoiadores.csv")

# ------------------------------------------- 4. Proxy partidária, regra revista
apo <- unique(ap[cod_apoiador != EXCLUIDO_D15, .(cod_apoiador, apoiador, ano)])
cand <- merge(apo, autores, by.x = "cod_apoiador", by.y = "cod_autor", allow.cartesian = TRUE)
cand[, prioridade := fcase(ano_ref == ano, 0L, ano_ref == ano + 1L, 1L, default = NA_integer_)]
cand[is.na(prioridade), prioridade := 2L + 2L * abs(ano_ref - ano) - as.integer(ano_ref > ano)]
setorder(cand, cod_apoiador, ano, prioridade)
proxy <- cand[, .SD[1], by = .(cod_apoiador, ano)]
proxy[, regra := fifelse(ano_ref == ano, "mesmo ano", fifelse(ano_ref == ano + 1L, "ano seguinte", paste0("mais próximo (", ano_ref, ")")))]
proxy <- merge(apo, proxy[, .(cod_apoiador, ano, partido, uf_autor, ano_ref, regra)], by = c("cod_apoiador", "ano"), all.x = TRUE)
proxy[is.na(regra), regra := "sem correspondência"]

# Partidos conferidos manualmente (20/10/2026) para quem não aparece como autor de RP6
# em nenhum ano de 2015 a 2026. Comissões ficam sem partido: o valor delas é rateado igualmente
# entre todos os parlamentares (decisão de pesquisa; valor pouco significativo).
man <- fread(file.path(DB, "parlamentares", "partidos_manuais_apoiadores.csv"), sep = ";",
             encoding = "UTF-8", colClasses = "character")
setnames(man, function(n) str_remove(n, "^\ufeff"))
man <- man[, .(cod_apoiador, apoiador_m = norm(apoiador), partido_m = fifelse(partido_confirmado == "", NA_character_, partido_confirmado),
               uf_m = uf, comissao = observacao %like% "comiss")]
proxy[, apoiador_m := norm(apoiador)]
proxy <- merge(proxy, man[cod_apoiador != "", .(cod_apoiador, partido_c = partido_m, uf_c = uf_m, com_c = comissao)], by = "cod_apoiador", all.x = TRUE)
proxy <- merge(proxy, man[cod_apoiador == "", .(apoiador_m, partido_n = partido_m, uf_n = uf_m)], by = "apoiador_m", all.x = TRUE)
proxy[regra == "sem correspondência" & com_c %in% TRUE, regra := "comissão: rateio igual entre todos os parlamentares"]
proxy[regra == "sem correspondência" & !is.na(fcoalesce(partido_c, partido_n)),
      `:=`(partido = fcoalesce(partido_c, partido_n), uf_autor = fcoalesce(uf_c, uf_n), regra = "manual (conferido)")]
proxy[, c("apoiador_m", "partido_c", "uf_c", "com_c", "partido_n", "uf_n") := NULL]
salva(proxy[order(ano, cod_apoiador)], "05_proxy_partidaria_regra_revista.csv")
# Quem continua sem correspondência mesmo com a base de 2026 (valor rateado entre apoiadores ativos)
vr <- ap[retirado == FALSE & cod_apoiador != EXCLUIDO_D15, .(empenho, cod_apoiador, v)][, n := uniqueN(cod_apoiador), by = empenho]
semc <- merge(proxy[regra == "sem correspondência", .(cod_apoiador, ano, apoiador)],
              unique(vr)[, .(valor_mi = mi(sum(v / n)), empenhos = .N), by = cod_apoiador], by = "cod_apoiador")
salva(semc[, .(anos = paste(sort(unique(ano)), collapse = ","), valor_mi = first(valor_mi), empenhos = first(empenhos)),
           by = .(cod_apoiador, apoiador)][order(-valor_mi)], "05b_sem_correspondencia_apos_2026.csv")
salva(proxy[, .N, by = .(ano, regra = fifelse(str_detect(regra, "próximo"), "mais próximo", regra))][order(ano, regra)],
      "05c_proxy_resumo_regras.csv")

# ---------------------------- 5. Apoio sem mandato federal no ano da emenda ---
# Teve mandato no ano t quem é autor de RP6 em t (LOA votada no fim de t-1) ou em t+1
# (LOA votada durante t). Para 2025, a LOA 2026 não está na base: só vale o próprio ano.
mand <- unique(autores[, .(cod_apoiador = cod_autor, ano_ref)])
ap_v <- ap[retirado == FALSE, .(v = first(v), n = uniqueN(cod_apoiador)), by = empenho]
apoio_cod <- unique(ap[retirado == FALSE, .(empenho, cod_apoiador, apoiador, ano, rp)])
apoio_cod <- merge(apoio_cod, ap_v, by = "empenho")
apoio_cod[, com_mandato := mapply(function(c, a) any(mand$cod_apoiador == c & mand$ano_ref %in% c(a, a + 1)),
                                  cod_apoiador, ano)]
sm <- apoio_cod[com_mandato == FALSE & cod_apoiador %in% mand$cod_apoiador,
                .(empenhos = .N, valor_rateado_mi = mi(sum(v / n))), by = .(ano, rp, cod_apoiador, apoiador)]
salva(sm[order(-valor_rateado_mi)], "06_apoio_sem_mandato_no_ano.csv")
salva(sm[, .(apoiadores = .N, valor_rateado_mi = sum(valor_rateado_mi),
             sem_excluido_d15_mi = sum(valor_rateado_mi[cod_apoiador != EXCLUIDO_D15])), by = .(ano, rp)][order(ano, rp)],
      "06b_apoio_sem_mandato_resumo.csv")

# ------------------------------------------- 6. Hipótese RP9 → RP6 (PEC) ------
h <- dcast(se[rpn %in% c("6", "7", "8", "9") & ano >= 2019 & ano <= 2025, .(bi = round(sum(emp) / 1e9, 2)), by = .(ano, rp = paste0("RP", rpn))],
           ano ~ rp, value.var = "bi", fill = 0)
h[, `:=`(RP6_mais_RP9 = RP6 + RP9, total_emendas = RP6 + RP7 + RP8 + RP9)]
salva(h, "07_hipotese_rp9_para_rp6.csv")


# ------------------------- 8. Apoiador único × coapoio; partido do líder coapoiador
# Partidos harmonizados pela linhagem atual, porque a proxy pode vir de anos diferentes
familia <- function(x) fcase(x %in% c("PR", "PL", "PL(1985)"), "PL", x %in% c("PRB", "REPUBLICANOS"), "REPUBLICANOS",
                             x %in% c("DEM", "PSL", "UNIÃO"), "UNIÃO", x %in% c("PTB", "PATRIOTA", "PRD"), "PRD",
                             x %in% c("PHS", "PSC", "PODE"), "PODE", x %in% c("Pros", "SD"), "SD", default = NA_character_) |> fcoalesce(x)
ap_at <- merge(ap_at, proxy[, .(cod_apoiador, ano, partido_proxy = familia(partido))], by = c("cod_apoiador", "ano"), all.x = TRUE)

salva(emp[, .(empenhos = .N, valor_mi = mi(sum(v))), by = .(ano, rp, faixa)][
        , `:=`(pct_empenhos = round(100 * empenhos / sum(empenhos), 1), pct_valor = round(100 * valor_mi / sum(valor_mi), 1)), by = .(ano, rp)][order(rp, ano, faixa)],
      "08a_apoiador_unico_x_coapoio.csv")

# Para cada empenho com líder + não líder: o líder é do mesmo partido de algum/todos os não líderes?
mix <- emp[n_ap > 1 & tem_lider & tem_nao_lider, .(empenho, ano, rp, v, faixa)]
pp <- ap_at[mix[, .(empenho)], on = "empenho", nomatch = 0]
res <- pp[, {
  pl <- unique(na.omit(partido_proxy[lider])); pn <- unique(na.omit(partido_proxy[!lider]))
  tipos <- unique(str_extract(na.omit(cargo_lider[lider]), "^[^ ]+"))
  .(mesmo_partido_algum = length(intersect(pl, pn)) > 0,
    mesmo_partido_todos = length(pn) > 0 && all(pn %in% pl),
    so_lider_de_partido = length(tipos) > 0 && all(tipos == "partido"),
    sem_partido_conhecido = length(pl) == 0 || length(pn) == 0) }, by = empenho]
mix <- merge(mix, res, by = "empenho")
salva(mix[, .(empenhos = .N, valor_mi = mi(sum(v)),
              pct_valor_mesmo_partido_algum = round(100 * sum(v[mesmo_partido_algum]) / sum(v), 1),
              pct_valor_mesmo_partido_todos = round(100 * sum(v[mesmo_partido_todos]) / sum(v), 1),
              pct_valor_partido_desconhecido = round(100 * sum(v[sem_partido_conhecido]) / sum(v), 1)),
          by = .(ano, rp, faixa)][order(rp, ano, faixa)], "08b_lider_coapoiador_mesmo_partido.csv")
salva(mix[faixa == "2 a 5 apoiadores", .(empenhos = .N, valor_mi = mi(sum(v)),
              pct_valor_mesmo_partido_algum = round(100 * sum(v[mesmo_partido_algum]) / sum(v), 1)),
          by = .(ano, rp, tipo_lideranca = fifelse(so_lider_de_partido, "só líder de partido", "bloco/governo/maioria etc."))][order(rp, ano)],
      "08c_mesmo_partido_por_tipo_de_lideranca.csv")

# Empenhos coletivos (6+ apoiadores): quem assina e quantos são líderes ou presidentes de comissão
col <- ap_at[emp[faixa == "6 ou mais (coletivo)", .(empenho)], on = "empenho", nomatch = 0]
salva(col[, .(empenhos = uniqueN(empenho), e_lider = any(lider)), by = .(ano, rp, cod_apoiador, apoiador, partido_proxy)][order(ano, -empenhos)],
      "08d_empenhos_coletivos_assinantes.csv")

cat("Concluído:", normalizePath(OUT), "\n")
