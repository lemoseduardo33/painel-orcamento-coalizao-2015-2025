# =============================================================================
# ICG+ v2 — 06a: Download do histórico dos deputados (Câmara, legislaturas 55–57)
# Fonte: API de Dados Abertos da Câmara (dadosabertos.camara.leg.br/api/v2)
#   /deputados?idLegislatura=L  -> lista de quem exerceu mandato na legislatura
#   /deputados/{id}/historico   -> eventos de status (posse, licença, troca de
#                                  partido, afastamento), com data e partido
# Saída: dados_brutos/camara/historico_deputados_leg55_57.csv (bruto, sem edição)
# Rodar do diretório Mestrado/. Leva ~10 min (uma chamada por deputado).
# =============================================================================
suppressPackageStartupMessages({library(jsonlite); library(dplyr); library(purrr); library(readr)})
API <- "https://dadosabertos.camara.leg.br/api/v2"
pega <- function(url, tent = 4) {
  for (i in seq_len(tent)) {
    r <- tryCatch(fromJSON(url), error = function(e) NULL)
    if (!is.null(r)) return(r)
    Sys.sleep(2 * i)
  }
  stop("falhou: ", url)
}
lista <- map_dfr(55:57, function(L) {
  out <- list(); p <- 1
  repeat {
    x <- pega(sprintf("%s/deputados?idLegislatura=%d&itens=100&pagina=%d", API, L, p))
    if (length(x$dados) == 0) break
    out[[p]] <- as_tibble(x$dados) |> transmute(id, nome, siglaPartido, siglaUf, idLegislatura = L)
    p <- p + 1
  }
  bind_rows(out)
})
ids <- unique(lista$id)
message(length(ids), " deputados distintos")
hist <- map_dfr(ids, function(i) {
  h <- pega(sprintf("%s/deputados/%d/historico", API, i))$dados
  if (length(h) == 0) return(NULL)
  as_tibble(h) |> mutate(across(everything(), as.character))
}, .progress = TRUE)
dir.create("dados_brutos/camara", showWarnings = FALSE)
write_csv(lista, "dados_brutos/camara/deputados_leg55_57.csv")
write_csv(hist, "dados_brutos/camara/historico_deputados_leg55_57.csv")
message("ok: ", nrow(hist), " eventos; baixado em ", Sys.Date())
