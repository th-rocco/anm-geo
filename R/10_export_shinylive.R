################################################################################
# 10_export_shinylive.R
#
# Gera uma versao REDUZIDA de shiny_dashboard/ para publicacao estatica via
# shinylive (GitHub Pages). NAO toca na versao completa: le de
# shiny_dashboard/ e escreve em shinylive_app/.
#
# POR QUE ISTO EXISTE
# O shinylive empacota o app inteiro -- codigo E dados -- num unico app.json,
# em base64 (que infla ~33%). Com a base completa, esse arquivo passou de
# 178 MB, e o limite do GitHub por arquivo e 100 MB. Colocar app.json no
# .gitignore faz o push passar, mas publica so a casca: a pagina fica em
# branco, porque o app.json E o app.
#
# RECORTE (decisao 2026-08): OURO, CASSITERITA e COLUMBITA, em TODAS as fases.
# E o foco declarado do projeto (cadeia do ouro e minerais estrategicos), nao
# um corte arbitrario para caber no limite. Fica registrado na tela do app,
# para ninguem confundir a versao publica com a base completa.
#
# O QUE MAIS ENXUGA, alem do filtro:
#   1. pma.rds / ti.rds / uc.rds / qui.rds -- geometrias NAO simplificadas.
#      O app carrega apenas as versoes *_simpl. Estavam no repositorio sem
#      ninguem ler.
#   2. Todas as tabelas por processo sao filtradas para os processos que
#      sobreviveram ao recorte.
#   3. compress = "xz" na gravacao (mais lento de escrever, bem menor).
#
# USO:
#   source(here::here("R", "10_export_shinylive.R"))
#   depois, no terminal, a partir da raiz do projeto:
#     "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" -e "shinylive::export('shinylive_app','docs')"




# shinylive::export("shinylive_app", "C:/GP/_sentinelas-da-amazonia/docs")

# cd C:\GP\_sentinelas-da-amazonia
# git add .
# git commit -m "descrição da mudança"
# git push

################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(here)
})

SRC <- here::here("shiny_dashboard")
OUT <- here::here("shinylive_app")

if (!dir.exists(SRC)) {
  stop("[10] shiny_dashboard/ nao encontrado -- rodar 07 e 08 antes.", call. = FALSE)
}
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# --- Recorte -----------------------------------------------------------------
# SUBSarrSIM e o grupo (OURO, NIOBIO...); SUBSarr e a substancia declarada.
# COLUMBITA entra pelo nome exato porque o grupo NIOBIO inclui outras que nao
# corrigimos (ver 06_correcao_cfem.R).
GRUPOS_MANTER <- c("OURO")
SUBS_MANTER   <- c("CASSITERITA", "COLUMBITA")

ler <- function(nome) {
  p <- file.path(SRC, nome)
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}

# xz e bem mais lento para escrever, mas o ganho aqui vale: sao arquivos
# escritos uma vez e lidos muitas.
gravar <- function(obj, nome) {
  if (is.null(obj)) return(invisible(NULL))
  saveRDS(obj, file.path(OUT, nome), compress = "xz")
  invisible(NULL)
}

filtra_proc <- function(df, procs, col = "processo") {
  if (is.null(df) || !col %in% names(df)) return(df)
  df[as.character(df[[col]]) %in% procs, , drop = FALSE]
}

message("\n[10] lendo base completa de: ", SRC)

# =============================================================================
# 1 — CFEM e o conjunto de processos que sobrevive ao recorte
# =============================================================================

cfem <- ler("cfem.rds")
if (is.null(cfem)) stop("[10] cfem.rds ausente.", call. = FALSE)

n_cfem_antes <- nrow(cfem)
cfem <- cfem |>
  dplyr::filter(SUBSarrSIM %in% GRUPOS_MANTER | SUBSarr %in% SUBS_MANTER)

procs_manter <- unique(as.character(cfem$PROCESSO))

message(sprintf(
  "[10] recorte por substancia | declaracoes: %d -> %d (%.1f%%) | processos: %d",
  n_cfem_antes, nrow(cfem), 100 * nrow(cfem) / n_cfem_antes, length(procs_manter)
))

if (length(procs_manter) == 0) {
  stop("[10] o recorte nao devolveu nenhum processo -- conferir GRUPOS_MANTER/SUBS_MANTER.",
       call. = FALSE)
}

gravar(cfem, "cfem.rds")

# cfem_mensal e cfem + a coluna 'data'. Mantido porque a aba 3 e o detalhe do
# processo na aba 4 leem dele; derivado aqui em vez de refiltrado do disco.
cfem_mensal <- cfem |>
  dplyr::mutate(data = as.Date(sprintf("%04d-%02d-01", ANO, MES)))
gravar(cfem_mensal, "cfem_mensal.rds")
rm(cfem_mensal)

cfem_anual <- ler("cfem_anual.rds") |>
  (\(d) if (is.null(d)) NULL else
     dplyr::filter(d, SUBSarrSIM %in% GRUPOS_MANTER | SUBSarr %in% SUBS_MANTER))()
gravar(cfem_anual, "cfem_anual.rds")

# --- lookups dos filtros (regenerados do dado ja recortado) -------------------
# Nao adianta copiar os lk_* originais: eles listam municipios/titulares/
# declarantes que nao existem mais depois do recorte, e a interface ofereceria
# opcao que devolve tabela vazia.
gerar_lks <- function(df, sufixo) {
  if (is.null(df)) return(invisible(NULL))
  gravar(dplyr::distinct(df, SUBSarrSIM, SUBSarr, FASE, abbrev_state, ANO, name_muni),
         paste0("lk_mun_",      sufixo, ".rds"))
  gravar(dplyr::distinct(df, abbrev_state, name_muni, TITULAR, PROCESSO),
         paste0("lk_tit_proc_", sufixo, ".rds"))
  gravar(dplyr::distinct(df, abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr),
         paste0("lk_decl_",     sufixo, ".rds"))
}
gerar_lks(cfem,       "tab1")
gerar_lks(cfem_anual, "tab2")
gerar_lks(cfem,       "tab3")
rm(cfem, cfem_anual)
invisible(gc())

# =============================================================================
# 2 — Geometrias
# =============================================================================
# pma.rds / ti.rds / uc.rds / qui.rds (NAO simplificadas) ficam de fora: o app
# carrega apenas as versoes *_simpl. Estavam sendo versionadas sem uso.

pma_simpl <- ler("pma_simpl.rds")
if (!is.null(pma_simpl)) {
  n_antes <- nrow(pma_simpl)
  pma_simpl <- pma_simpl[as.character(pma_simpl$PROCESSO) %in% procs_manter, , drop = FALSE]
  message(sprintf("[10] pma_simpl | poligonos: %d -> %d", n_antes, nrow(pma_simpl)))
  gravar(pma_simpl, "pma_simpl.rds")
}

# Territorios: recortados pelo bbox dos processos que sobraram. Nao ha por que
# carregar UC do Brasil inteiro se o app so desenha a vizinhanca desses
# poligonos. Se pma_simpl estiver vazio, mantem inteiro (nao ha o que recortar).
recorta_bbox <- function(camada, ref) {
  if (is.null(camada) || is.null(ref) || nrow(ref) == 0) return(camada)
  bb <- sf::st_as_sfc(sf::st_bbox(ref))
  sf::st_crs(bb) <- sf::st_crs(ref)
  suppressWarnings(camada[sf::st_intersects(camada, bb, sparse = FALSE)[, 1], , drop = FALSE])
}

for (nm in c("ti_simpl", "uc_simpl", "qui_simpl")) {
  cam <- ler(paste0(nm, ".rds"))
  if (is.null(cam)) next
  n_antes <- nrow(cam)
  cam <- recorta_bbox(cam, pma_simpl)
  message(sprintf("[10] %-10s | feicoes: %d -> %d", nm, n_antes, nrow(cam)))
  gravar(cam, paste0(nm, ".rds"))
}
rm(pma_simpl)
invisible(gc())

# =============================================================================
# 3 — Tabelas por processo (aba 4)
# =============================================================================

tabelas_por_processo <- c(
  "micro_processos", "micro_pessoas", "micro_substancias", "micro_titulos",
  "micro_municipios", "micro_documentacao", "micro_associacoes",
  "micro_propsolo", "eventos_serie", "situacao_atual",
  "fases_processo_tabela", "multas_infracoes_tabela", "dossie_resumo_processo"
)

for (nm in tabelas_por_processo) {
  df <- ler(paste0(nm, ".rds"))
  if (is.null(df)) { message("[10] ausente, pulando: ", nm); next }
  n_antes <- nrow(df)
  df <- filtra_proc(df, procs_manter)
  message(sprintf("[10] %-24s | linhas: %8d -> %8d", nm, n_antes, nrow(df)))
  gravar(df, paste0(nm, ".rds"))
  rm(df); invisible(gc())
}

# micro_pessoa_resumo nao tem coluna 'processo' -- e agregado por pessoa.
# Refiltrado pelos idpessoa que sobraram em micro_pessoas.
pes <- ler("micro_pessoas.rds")
res <- ler("micro_pessoa_resumo.rds")
if (!is.null(pes) && !is.null(res)) {
  ids <- unique(as.character(filtra_proc(pes, procs_manter)$idpessoa))
  n_antes <- nrow(res)
  res <- res[as.character(res$idpessoa) %in% ids, , drop = FALSE]
  message(sprintf("[10] %-24s | linhas: %8d -> %8d", "micro_pessoa_resumo", n_antes, nrow(res)))
  gravar(res, "micro_pessoa_resumo.rds")
}
rm(pes, res); invisible(gc())

# =============================================================================
# 4 — app.R e arquivos avulsos
# =============================================================================

app_src <- file.path(SRC, "app.R")
if (!file.exists(app_src)) {
  stop("[10] app.R nao encontrado em ", app_src,
       " -- sem ele o export gera app vazio.", call. = FALSE)
}

# bindCache() exige um backend de cache que nao existe no webR, e o app morre
# com: `cache` must either be "app", "session", or a cache object.
#
# Neutralizado AQUI, no export, e nao no app.R original: localmente o cache
# continua valendo. Tentamos antes condicionar por plataforma
# (Sys.info()[["sysname"]] == "Emscripten") e nao funcionou -- nao ha string
# confiavel para identificar o webR, e a condicao ficava sempre falsa. Como a
# versao exportada SEMPRE roda no navegador, a desativacao aqui e incondicional
# e nao depende de adivinhar nada.
patch <- c(
  "# === INSERIDO POR 10_export_shinylive.R -- nao editar aqui ===",
  "# bindCache desativado: sem backend de cache no webR.",
  "bindCache <- function(x, ...) x",
  ""
)
writeLines(c(patch, readLines(app_src, warn = FALSE)),
           file.path(OUT, "app.R"))
message("[10] app.R copiado com bindCache neutralizado (", length(patch), " linhas no topo).")

# =============================================================================
# 5 — Relatorio de tamanho
# =============================================================================
# O numero que importa nao e a soma dos .rds, e a estimativa do app.json:
# o shinylive junta tudo em base64, que infla ~33%. Limite do GitHub: 100 MB.

tam <- function(dir) {
  arqs <- list.files(dir, full.names = TRUE, recursive = TRUE)
  if (length(arqs) == 0) return(0)
  sum(file.info(arqs)$size, na.rm = TRUE)
}
mb <- function(x) round(x / 1024^2, 1)

tam_src <- tam(SRC)
tam_out <- tam(OUT)
est_json <- tam_out * 4 / 3

detalhe <- data.frame(
  arquivo = list.files(OUT),
  mb      = mb(file.info(list.files(OUT, full.names = TRUE))$size)
)
detalhe <- detalhe[order(-detalhe$mb), ]
print(head(detalhe, 12), row.names = FALSE)

message(sprintf(
  "\n[10] shiny_dashboard/: %s MB  ->  shinylive_app/: %s MB",
  mb(tam_src), mb(tam_out)
))
message(sprintf("[10] estimativa do app.json (base64, +33%%): %s MB | limite do GitHub: 100 MB",
                mb(est_json)))

if (est_json > 100 * 1024^2) {
  message("\n[10] AINDA ACIMA DO LIMITE. Proximas alavancas, em ordem de ganho:")
  message("     1. Restringir as fases (so LAVRA GARIMPEIRA e REQ LAVRA GARIMPEIRA).")
  message("     2. Restringir o periodo (ex: 2018+).")
  message("     3. Descartar colunas nao exibidas de cfem.rds.")
  message("     4. Publicar os dados fora do repositorio e baixar por URL no app.")
} else {
  message("\n[10] Dentro do limite. Proximo passo, na raiz do projeto:")
  message('     Rscript -e "shinylive::export(\'shinylive_app\', \'docs\')"')
}

message("\n=== 10_export_shinylive.R — CONCLUIDO ===")