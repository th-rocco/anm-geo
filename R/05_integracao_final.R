################################################################################
# 05_integracao_final.R
#
# Municipio (Bloco 4), intersecao espacial TI/UC/Quilombola + embargos
# (Bloco 5), e preparo do CFEM ate o ponto anterior a correcao de peso/preco
# (leitura, limpeza, conversao de unidade, aliquota, VALORtot, preco_g_orig).
#
# Revisão 2026-08-25 (auditoria Sentinela da Amazônia):
#   F-02  pma_attrs levava 8 colunas e NENHUMA flag de sobreposição — as flags
#         eram calculadas, conferidas no QA e não viajavam para a tabela de
#         CFEM. Como o filtro do dashboard ignora coluna ausente e devolve o
#         conjunto inteiro, os botões de território não filtravam nada. A lista
#         de colunas passa a derivar de FLAGS_SOBREPOSICAO (R/utils.R).
#   F-01  Autos de infração do ICMBio incorporados por sobreposição espacial
#         única (ponto contra polígono), com contagem, soma de multa,
#         rastreabilidade por numero_ai e validação cruzada por nome_uc.
#         Inclui teste de sensibilidade a 500 m (não entra no resultado).
#   F-05  O CSV de disponibilidade de fontes passa a ser exportado também para
#         a pasta do dashboard — diagnóstico que fica só em QA é igual a não
#         existir, e foi por isso que a queda do ICMBio passou despercebida.
################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)

Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("GDAL_DATA")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(tidyterra)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(arrow)
  library(readr)
  library(purrr)
  library(stringr)
  library(here)
})

source(here::here("R", "utils.R"))

# --- Caminhos -----------------------------------------------------------------
RAW_DIR       <- here::here("data", "raw_data")
PRE_PROC_DIR  <- here::here("data", "pre_proc_data")
MICRO_OUT_DIR <- here::here("data", "result_db", "microdados")  # parquets do 04
QA_DIR        <- here::here("data", "_qa", "05_integracao_final")
MUNI_DIR      <- here::here("data", "raw_data", "BR_Municipios_2025")

dir.create(QA_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# BLOCO 4 — MUNICÍPIO
# =============================================================================

pma_amzl <- load_ckpt("03_pma_amzl")
proc_mun <- arrow::read_parquet(file.path(MICRO_OUT_DIR, "micro_processo_municipio.parquet"))
muni_lk  <- arrow::read_parquet(file.path(MICRO_OUT_DIR, "micro_municipio.parquet"))
muni_lk2 <- muni_lk |>
  dplyr::transmute(
    idmunicipio = idmunicipio,
    munic_micro = toupper(nmmunicipio),
    uf_micro    = toupper(sguf)
  )

proc_mun_resumo <- proc_mun |>
  dplyr::left_join(muni_lk2, by = "idmunicipio") |>
  dplyr::group_by(processo) |>
  dplyr::summarise(
    n_munic     = dplyr::n_distinct(idmunicipio),
    munic_unico = dplyr::first(munic_micro),
    uf_unico    = dplyr::first(uf_micro),
    .groups = "drop"
  )

muni_ibge <- terra::vect(list.files(MUNI_DIR, pattern = "\\.shp$", full.names = TRUE)[1]) |>
  terra::project(terra::crs(pma_amzl))

centroides <- terra::centroids(pma_amzl, inside = TRUE)
col_nmmun  <- "NM_MUN"
col_ufmun  <- "SIGLA_UF"

cent_mun <- terra::intersect(centroides, muni_ibge) |>
  as.data.frame() |>
  dplyr::transmute(
    PROCESSO,
    munic_centroide = toupper(.data[[col_nmmun]]),
    uf_centroide    = toupper(.data[[col_ufmun]])
  ) |>
  dplyr::distinct(PROCESSO, .keep_all = TRUE)

munic_final <- as.data.frame(pma_amzl) |>
  dplyr::select(PROCESSO) |>
  dplyr::left_join(proc_mun_resumo, by = c("PROCESSO" = "processo")) |>
  dplyr::left_join(cent_mun, by = "PROCESSO") |>
  dplyr::mutate(
    n_munic = tidyr::replace_na(n_munic, 0L),
    micro_ok = n_munic == 1 & !is.na(munic_unico),
    munic = dplyr::case_when(
      micro_ok                       ~ munic_unico,
      !is.na(munic_centroide)        ~ munic_centroide,
      TRUE                           ~ NA_character_),
    uf = dplyr::case_when(
      micro_ok                       ~ uf_unico,
      !is.na(uf_centroide)           ~ uf_centroide,
      TRUE                           ~ NA_character_),
    munic_fonte = dplyr::case_when(
      micro_ok                       ~ "microdado",
      n_munic > 1 & !is.na(munic_centroide)  ~ "centroide",
      n_munic == 1 & !is.na(munic_centroide) ~ "centroide_micro_sem_nome",
      n_munic == 0 & !is.na(munic_centroide) ~ "centroide_sem_micro",
      TRUE                           ~ NA_character_)
  ) |>
  dplyr::select(PROCESSO, munic, uf, n_munic, munic_fonte)

ids_na <- munic_final |> dplyr::filter(is.na(munic)) |> dplyr::pull(PROCESSO)

if (length(ids_na) > 0) {
  cent_na  <- centroides[centroides$PROCESSO %in% ids_na, ]
  nr       <- terra::nearest(cent_na, muni_ibge)
  idx_near <- terra::values(nr)$to_id
  near_df <- tibble::tibble(
    PROCESSO   = cent_na$PROCESSO,
    munic_near = toupper(terra::values(muni_ibge)[[col_nmmun]][idx_near]),
    uf_near    = toupper(terra::values(muni_ibge)[[col_ufmun]][idx_near])
  )

  munic_final <- munic_final |>
    dplyr::left_join(near_df, by = "PROCESSO") |>
    dplyr::mutate(
      preencheu_near = is.na(munic) & !is.na(munic_near),
      munic       = dplyr::if_else(preencheu_near, munic_near, munic),
      uf          = dplyr::if_else(preencheu_near, uf_near,    uf),
      munic_fonte = dplyr::if_else(preencheu_near, "centroide_nearest", munic_fonte)
    ) |>
    dplyr::select(-munic_near, -uf_near, -preencheu_near)
}

# --- Check/parecer: distribuição da fonte do município ------------------------
munic_check <- munic_final |> dplyr::count(munic_fonte, sort = TRUE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 2))
readr::write_csv(munic_check, file.path(QA_DIR, "municipio_fonte_distribuicao.csv"))
message("[05][municipio] distribuicao de fonte:")
print(munic_check)
message("[05][municipio] processos sem municipio (mesmo apos nearest): ", sum(is.na(munic_final$munic)))

pma_amzl <- tidyterra::left_join(pma_amzl, munic_final, by = "PROCESSO")
save_ckpt(pma_amzl, "05_pma_munic")

# =============================================================================
# BLOCO 5 — INTERSEÇÃO ESPACIAL (TI/UC/Quilombola + embargos)
# =============================================================================

pma_amzl <- load_ckpt("05_pma_munic")
ti_amzl  <- load_ckpt("03_ti_amzl")
uc_amzl  <- load_ckpt("03_uc_amzl")
qui_amzl <- load_ckpt("03_qui_amzl")

invalid_geom <- !terra::is.valid(pma_amzl)
if (any(invalid_geom)) {
  pma_amzl <- rbind(terra::buffer(pma_amzl[invalid_geom, ], 0), pma_amzl[!invalid_geom, ])
}

n_grupo_na <- sum(is.na(uc_amzl$grupo))
if (n_grupo_na > 0) {
  message(sprintf("[05][uc] AVISO: %d de %d UC(s) sem 'grupo' preenchido -- tratadas como NAO proteção integral.",
                  n_grupo_na, length(uc_amzl)))
}
uc_pi_resex <- uc_amzl[(!is.na(uc_amzl$grupo) & uc_amzl$grupo == "Proteção Integral") |
                       (!is.na(uc_amzl$sigla_snuc) & uc_amzl$sigla_snuc == "RESEX"), ]
qui_amzl    <- qui_amzl |> dplyr::select(nm_comunid)

qui_bf <- terra::buffer(qui_amzl, width = 10000)
ti_bf  <- terra::buffer(ti_amzl,  width = 10000)
# Buffer de UC fixo em 2km (decisao 2026-08): antes variava 2km/10km conforme
# 'pl_manejo' == "SIM", mas esse campo pode vir NA na fonte (o que quebrava o
# buffer -- ifelse(NA, ...) devolve NA, nao o fallback). Simplificado pra 2km
# fixo em todas as UC de protecao integral/RESEX ate decidirmos reintroduzir
# a diferenciacao por plano de manejo com um tratamento de NA explicito.
uc_bf  <- terra::buffer(uc_pi_resex, width = 2000)

qui_bf_only <- terra::erase(qui_bf, qui_amzl)
ti_bf_only  <- terra::erase(ti_bf,  ti_amzl)
uc_bf_only  <- terra::erase(uc_bf,  uc_pi_resex)

calc_overlap_named <- function(pma_lyr, tp_lyr, flag_name, name_col, out_name,
                               fix_encoding = FALSE, extra_cols = NULL, min_prop = 0.05) {
  inter <- terra::intersect(pma_lyr, tp_lyr)
  inter$area_inter <- terra::expanse(inter, unit = "ha")

  keep_cols <- c("PROCESSO", "AREA_HA", "area_inter", name_col, extra_cols)
  inter <- inter |> tidyterra::select(dplyr::all_of(keep_cols)) |> as.data.frame()
  inter$Propor <- as.numeric(inter$area_inter / inter$AREA_HA)
  inter <- inter |> dplyr::filter(Propor >= min_prop)

  if (nrow(inter) == 0) {
    base <- tibble::tibble(PROCESSO = character(0))
    base[[flag_name]] <- integer(0)
    base[[out_name]]  <- character(0)
    if (!is.null(extra_cols)) for (ec in extra_cols) base[[ec]] <- character(0)
    return(base)
  }

  inter <- inter |> dplyr::rename(!!out_name := dplyr::all_of(name_col))
  if (fix_encoding) {
    inter[[out_name]] <- stringi::stri_encode(inter[[out_name]], from = "Windows-1252", to = "UTF-8")
  }

  inter |>
    dplyr::group_by(PROCESSO) |>
    dplyr::summarise(
      !!flag_name := 1L,
      !!out_name  := paste(unique(.data[[out_name]]), collapse = "|"),
      dplyr::across(dplyr::all_of(extra_cols), ~ paste(unique(.x), collapse = "|")),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
    dplyr::distinct(PROCESSO, .keep_all = TRUE)
}

df_uc_pma <- calc_overlap_named(pma_amzl, uc_pi_resex, "UCov", "nome_uc", "UCname",
                                extra_cols = c("sigla_snuc", "cd_cnuc")) |>
  dplyr::rename(UCtype = sigla_snuc, UCcnuc = cd_cnuc)
df_ti_pma <- calc_overlap_named(pma_amzl, ti_amzl, "TIov", "terrai_nom", "TIname")
df_qui_pma <- calc_overlap_named(pma_amzl, qui_amzl, "QUIov", "nm_comunid", "QUIname")

pma_tp1 <- pma_amzl |>
  tidyterra::left_join(df_ti_pma,  by = "PROCESSO") |>
  tidyterra::left_join(df_uc_pma,  by = "PROCESSO") |>
  tidyterra::left_join(df_qui_pma, by = "PROCESSO") |>
  dplyr::mutate(
    TIov  = tidyr::replace_na(TIov,  0L),
    UCov  = tidyr::replace_na(UCov,  0L),
    QUIov = tidyr::replace_na(QUIov, 0L)
  )

# min_prop = 0: proximidade e flag de alerta, nao sobreposicao real -- decisao
# 2026-08, manter o limiar de 5% so pra dentro da area protegida (acima).
df_uc_donut <- calc_overlap_named(pma_tp1, uc_bf_only, "UCov2km", "nome_uc", "UCname_ov",
                                  extra_cols = "sigla_snuc", min_prop = 0) |> dplyr::rename(UCtype_ov = sigla_snuc)
df_ti_donut <- calc_overlap_named(pma_tp1, ti_bf_only, "TIov10km", "terrai_nom", "TIname_ov", min_prop = 0)
df_qui_donut <- calc_overlap_named(pma_tp1, qui_bf_only, "QUIov10km", "nm_comunid", "QUIname_ov", min_prop = 0)

pma_tp <- pma_tp1 |>
  tidyterra::left_join(df_ti_donut,  by = "PROCESSO") |>
  tidyterra::left_join(df_uc_donut,  by = "PROCESSO") |>
  tidyterra::left_join(df_qui_donut, by = "PROCESSO") |>
  dplyr::mutate(
    TIov10km   = tidyr::replace_na(TIov10km,   0L),
    UCov2km = tidyr::replace_na(UCov2km, 0L),
    QUIov10km  = tidyr::replace_na(QUIov10km,  0L)
  )

EMBmtSEMA <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "sema_mt_embargos.shp"))
EMBmtSIGA <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "sema_mt_embargos_siga.shp"))
EMBib     <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "ibama_embargos.shp"))
EMBic     <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "icmbio_embargos.shp"))
INFmtSIGA <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "sema_mt_infracoes_siga.shp"))
INFic     <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "icmbio_infracoes.shp"))
 
pma_tp$inf_MT  <- relacionar_flag_opcional(pma_tp, INFmtSIGA)
pma_tp$inf_IC  <- relacionar_flag_opcional(pma_tp, INFic)
pma_tp$emb_MTa <- relacionar_flag_opcional(pma_tp, EMBmtSEMA)
pma_tp$emb_MTb <- relacionar_flag_opcional(pma_tp, EMBmtSIGA)
pma_tp$emb_IB  <- relacionar_flag_opcional(pma_tp, EMBib)
pma_tp$emb_IC  <- relacionar_flag_opcional(pma_tp, EMBic)

# =============================================================================
# AUTOS DE INFRACAO DO ICMBIO — SOBREPOSICAO ESPACIAL UNICA (auditoria F-01)
# =============================================================================
# Decisao 2026-08-25: a camada ENTRA. O shapefile e de PONTOS (um por auto
# lavrado), entao o cruzamento e ponto-dentro-do-poligono do processo. O flag
# binario inf_IC ja sai do relacionar_flag_opcional acima; aqui derivamos o que
# ele nao da: quantos autos e quanto de multa por processo.
#
# Contexto do achado: o terra descartava a camada em silencio e ela nunca
# contribuiu com nada. Confirmado no cruzamento final -- zero processos
# sinalizados por esta fonte. Ver 02_pre_proc.R para o inventario do arquivo.
#
# TESTE DE SENSIBILIDADE (500 m): a coordenada marca o ponto do auto lavrado,
# nao o centroide do dano, entao a sobreposicao estrita tende a SUBcontar. O
# numero com tolerancia NAO entra no resultado -- e so para dimensionar o
# efeito antes de fixar o criterio. Se a diferenca for marginal, fechamos no
# estrito com respaldo empirico em vez de escolha default.
resumir_infracoes_pontos <- function(pma, pts, label = "icmbio_infracoes",
                                     col_valor = "valor_mult", col_id = "numero_ai",
                                     tolerancias_m = c(50, 100, 250, 500, 1000),
                                     qa_dir = QA_DIR) {

  vazio <- tibble::tibble(PROCESSO = character(0), inf_IC_n = integer(0),
                          inf_IC_multa = numeric(0), inf_IC_ais = character(0),
                          inf_IC_cnucs = character(0))
  if (is.null(pts) || nrow(pts) == 0) {
    message("[05][", label, "] camada ausente ou vazia -- sem contagem por processo.")
    return(vazio)
  }

  # valor_mult vem como texto na fonte; parse tolerante a separador BR e US.
  v <- as.character(terra::values(pts)[[col_valor]])
  v <- gsub("[^0-9,.-]", "", v)
  v <- ifelse(grepl(",\\d{1,2}$", v), gsub("\\.", "", v), v)   # 1.234,56 -> 1234,56
  v <- gsub(",", ".", v)
  pts$.valor_num <- suppressWarnings(as.numeric(v))
  n_valor_na <- sum(is.na(pts$.valor_num))
  if (n_valor_na > 0) {
    message("[05][", label, "] ", n_valor_na, " de ", nrow(pts),
            " auto(s) sem valor de multa interpretavel -- somados como 0.")
  }

  # --- criterio adotado: sobreposicao estrita (ponto dentro do poligono) ------
  rel <- terra::relate(pts, pma, "intersects")   # linhas = pontos, colunas = processos
  hits <- which(rel, arr.ind = TRUE)

  if (nrow(hits) == 0) {
    message("[05][", label, "] nenhum auto caiu dentro de poligono de processo.")
    return(vazio)
  }

  df_hits <- tibble::tibble(
    PROCESSO = as.character(pma$PROCESSO)[hits[, "col"]],
    ai       = as.character(terra::values(pts)[[col_id]])[hits[, "row"]],
    valor    = tidyr::replace_na(pts$.valor_num[hits[, "row"]], 0),
    cnuc     = trimws(as.character(terra::values(pts)[["cnuc"]])[hits[, "row"]])
  )

  resumo <- df_hits |>
    dplyr::group_by(PROCESSO) |>
    dplyr::summarise(
      inf_IC_n     = dplyr::n_distinct(ai),
      inf_IC_multa = sum(valor, na.rm = TRUE),
      inf_IC_ais   = paste(unique(ai[!is.na(ai) & nzchar(ai)]), collapse = "|"),
      inf_IC_cnucs = paste(unique(cnuc[!is.na(cnuc) & nzchar(cnuc)]), collapse = "|"),
      .groups = "drop"
    )

  # --- teste de sensibilidade (nao entra no resultado) -----------------------
  # AMPLIADO 2026-08-25: a primeira rodada testou so 500 m e deu +112% (347 ->
  # 735 processos). Longe de marginal, entao a curva importa: varios raios
  # mostram onde ela sobe e se ha patamar. 500 m em area de garimpo ja pode
  # capturar processo vizinho, entao o numero sozinho nao decide.
  n_estrito <- nrow(resumo)
  curva <- purrr::map_dfr(tolerancias_m, \(w) {
    n <- tryCatch({
      sum(terra::is.related(pma, terra::buffer(pts, width = w), "intersects"))
    }, error = function(e) NA_integer_)
    tibble::tibble(criterio = paste0("buffer_", w, "m"), raio_m = w, n_processos = n)
  })

  curva <- dplyr::bind_rows(
    tibble::tibble(criterio = "estrito", raio_m = 0L, n_processos = n_estrito),
    curva
  ) |>
    dplyr::mutate(delta_pct = round(100 * (n_processos - n_estrito) / max(n_estrito, 1), 1))

  message("[05][", label, "] SENSIBILIDADE (nao entra no resultado):")
  print(as.data.frame(curva), row.names = FALSE)

  readr::write_csv(curva, file.path(qa_dir, "icmbio_infracoes_sensibilidade_buffer.csv"))

  resumo
}

df_inf_ic <- resumir_infracoes_pontos(pma_tp, INFic)

pma_tp <- pma_tp |>
  tidyterra::left_join(df_inf_ic, by = "PROCESSO") |>
  dplyr::mutate(
    inf_IC_n     = tidyr::replace_na(inf_IC_n, 0L),
    inf_IC_multa = tidyr::replace_na(inf_IC_multa, 0)
  )

# VALIDACAO CRUZADA, independente da geometria: se o ponto caiu no poligono do
# processo E o codigo CNUC declarado no auto bate com o codigo da UC que o
# processo sobrepoe, ha confirmacao por dois caminhos distintos.
#
# POR CODIGO, NAO POR NOME (correcao 2026-08-25): os nomes nao sao comparaveis
# entre as duas fontes -- o ICMBio abrevia ("FLONA do Jamari") e o CNUC escreve
# por extenso ("FLORESTA NACIONAL DO JAMARI"). A primeira versao comparava
# texto e devolvia 0 acertos em 347 processos, o que parecia achado e era
# artefato de vocabulario. O campo cd_cnuc (CNUC) e o campo cnuc (autos) usam o
# MESMO formato, ex: "0000.00.0118".
#
# NA = nao avaliavel (processo sem auto, ou sem UC sobreposta). Divergencia nao
# invalida o flag: pode ser auto lavrado fora de UC, ou processo sobrepondo mais
# de uma. Serve como grau de confianca para a investigacao.
if (!"UCcnuc" %in% names(pma_tp)) {
  warning("[05] UCcnuc ausente -- validacao cruzada indisponivel. ",
          "Conferir se o 02 preservou cd_cnuc no CNUC.")
  pma_tp$inf_IC_uc_confere <- NA
} else {
  pma_tp$inf_IC_uc_confere <- mapply(
    function(cnucs_auto, cnuc_proc) {
      if (is.na(cnucs_auto) || !nzchar(cnucs_auto) ||
          is.na(cnuc_proc)  || !nzchar(cnuc_proc)) return(NA)
      any(strsplit(cnucs_auto, "\\|")[[1]] %in% strsplit(cnuc_proc, "\\|")[[1]])
    },
    as.character(pma_tp$inf_IC_cnucs),
    as.character(pma_tp$UCcnuc)
  )
}

n_com_auto  <- sum(pma_tp$inf_IC_n > 0, na.rm = TRUE)
n_avaliavel <- sum(!is.na(pma_tp$inf_IC_uc_confere))
n_confere   <- sum(pma_tp$inf_IC_uc_confere, na.rm = TRUE)
message(sprintf(
  "[05][icmbio_infracoes] processos com auto: %d | avaliaveis (tem auto E UC sobreposta): %d | codigo CNUC confere: %d",
  n_com_auto, n_avaliavel, n_confere))

# --- Registro em QA: quais fontes de embargo/infracao faltaram nesta execucao
fontes_check <- tibble::tibble(
  fonte = c("sema_mt_embargos", "sema_mt_embargos_siga", "ibama_embargos", "icmbio_embargos",
           "sema_mt_infracoes_siga", "icmbio_infracoes"),
  disponivel = c(!is.null(EMBmtSEMA), !is.null(EMBmtSIGA), !is.null(EMBib), !is.null(EMBic),
                !is.null(INFmtSIGA), !is.null(INFic))
)
readr::write_csv(fontes_check, file.path(QA_DIR, "fontes_embargo_infracao_disponibilidade.csv"))
# Tambem para a pasta do dashboard: diagnostico que fica so em CSV de QA e o
# mesmo que nao existir. Foi por isso que a queda do ICMBio (F-05) passou
# despercebida -- o mecanismo de deteccao existia e ninguem leu.
SHINY_DIR_05 <- here::here("data", "result_shiny")
dir.create(SHINY_DIR_05, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(fontes_check, file.path(SHINY_DIR_05, "fontes_disponibilidade.csv"))
if (any(!fontes_check$disponivel)) {
  message("[05][embargos] ATENCAO — fonte(s) indisponivel(is) nesta execucao: ",
          paste(fontes_check$fonte[!fontes_check$disponivel], collapse = ", "),
          " | flag(s) correspondente(s) gravada(s) como NA (nao 0) em pma_tp.")
}

# --- Check/parecer: quantos processos carregam cada flag de sobreposição ------
# flags_sobrep agora vem de R/utils.R (FLAGS_SOBREPOSICAO), a mesma fonte usada
# na propagacao de colunas do Bloco 6 -- ver auditoria F-02.
flags_sobrep <- FLAGS_SOBREPOSICAO
sobrep_check <- as.data.frame(pma_tp) |>
  dplyr::summarise(dplyr::across(dplyr::all_of(flags_sobrep), ~ sum(.x == 1L, na.rm = TRUE))) |>
  tidyr::pivot_longer(everything(), names_to = "flag", values_to = "n_processos") |>
  dplyr::mutate(pct = round(100 * n_processos / nrow(pma_tp), 2))
readr::write_csv(sobrep_check, file.path(QA_DIR, "sobreposicao_flags_distribuicao.csv"))
message("[05][sobreposicao] flags de sobreposicao/embargo:")
print(sobrep_check)

save_ckpt(pma_tp, "05_pma_tp")
# BLOCO 6 — CFEM: LEITURA, LIMPEZA E PREPARO (sem correcao de peso/preco)
# =============================================================================

processos_amzl <- load_ckpt("03_processos_amzl")
pma_tp         <- load_ckpt("05_pma_tp")

cfem_aut <- readr::read_csv(file.path(PRE_PROC_DIR, "CFEM_Autuacao.csv"), show_col_types = FALSE) |>
  dplyr::mutate(AnoPublicação = as.numeric(AnoPublicação), MêsPublicação = as.numeric(MêsPublicação)) |>
  dplyr::select(-dplyr::any_of(c("ProcessoCobrança", "Tipo_PF_PJ", "NúmeroAuto"))) |>
  dplyr::rename(TITULARaut = NomeTitular, PROCESSO = ProcessoMinerário, SUBSaut = Substância,
               name_muni = Município, abbrev_state = UF, ANO = AnoPublicação,
               MES = MêsPublicação, VALORaut = Valor, CPF_CNPJaut = CPF_CNPJ) |>
  dplyr::filter(!is.na(PROCESSO), !is.na(ANO), !is.na(MES), !is.na(SUBSaut), !is.na(CPF_CNPJaut), PROCESSO != "NA/NA") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::mutate(CPF_CNPJaut = padroniza_doc(CPF_CNPJaut))

cfem_arr <- readr::read_csv(file.path(PRE_PROC_DIR, "CFEM_Arrecadacao.csv"),
                            col_types = readr::cols(ValorRecolhido = readr::col_double(),
                                                     QuantidadeComercializada = readr::col_double()))
names(cfem_arr)[names(cfem_arr) == "Processo"] <- "ProcSemNum"
cfem_arr$PROCESSO <- paste(cfem_arr$ProcSemNum, cfem_arr$AnoDoProcesso, sep = "/")

cfem_arr <- cfem_arr |>
  dplyr::select(-dplyr::any_of(c("ProcSemNum", "Tipo_PF_PJ", "AnoDoProcesso", "DataCriacao"))) |>
  dplyr::rename(SUBSarr = Substância, name_muni = Município, code_muni = CodigoMunicipio,
               abbrev_state = UF, ANO = Ano, MES = Mês, QTD_MINERIO = QuantidadeComercializada,
               VALORarr = ValorRecolhido, CPF_CNPJarr = CPF_CNPJ, UM = UnidadeDeMedida) |>
  dplyr::filter(!is.na(PROCESSO), !is.na(ANO), !is.na(MES), !is.na(SUBSarr), !is.na(CPF_CNPJarr), PROCESSO != "NA/NA") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::mutate(CPF_CNPJarr = padroniza_doc(CPF_CNPJarr))

# Razao social: primeiro tenta os microdados (fonte oficial, ja no pipeline
# via 04); CSV auxiliar so entra pra CPF/CNPJ que o microdado nao resolveu.
pessoa <- arrow::read_parquet(file.path(MICRO_OUT_DIR, "micro_pessoa.parquet")) |>
  dplyr::rename(CPF_CNPJarr = nrcpfcnpj, NOME_arr = nmpessoa) |>
  dplyr::mutate(CPF_CNPJarr = padroniza_doc(CPF_CNPJarr)) |>
  dplyr::select(CPF_CNPJarr, NOME_arr) |>
  dplyr::distinct(CPF_CNPJarr, .keep_all = TRUE) |>
  dplyr::mutate(NOME_arr = toupper(NOME_arr))

razao_social_path <- file.path(RAW_DIR, "cefem_arrecadacao(semshapes).csv")
if (!file.exists(razao_social_path)) {
  warning("[05][CFEM] Arquivo auxiliar de razao social nao encontrado (fonte fora do 01_download.R): ",
          razao_social_path, " — prosseguindo sem o fallback.")
  RazaoSocial <- tibble::tibble(CPF_CNPJarr = character(0), NOME_arr_alt = character(0))
} else {
  RazaoSocial <- readr::read_csv(razao_social_path, show_col_types = FALSE) |>
    dplyr::rename(CPF_CNPJarr = cnpj_cpf, NOME_arr_alt = razao_social) |>
    dplyr::mutate(CPF_CNPJarr = padroniza_doc(CPF_CNPJarr)) |>
    dplyr::select(CPF_CNPJarr, NOME_arr_alt) |>
    dplyr::distinct(CPF_CNPJarr, .keep_all = TRUE) |>
    dplyr::mutate(NOME_arr_alt = toupper(NOME_arr_alt))
}

cfem_arr_pre_fallback <- cfem_arr |>
  dplyr::left_join(pessoa, by = "CPF_CNPJarr")

# Checagem (decisao pendente 2026-08): quantos CPF/CNPJ so ganham nome por
# causa do CSV auxiliar (fonte fora do 01_download.R)? Se for perto de zero,
# o fallback pode ser removido -- os microdados (04) ja bastariam sozinhos.
n_so_fallback <- cfem_arr_pre_fallback |>
  dplyr::filter(is.na(NOME_arr)) |>
  dplyr::distinct(CPF_CNPJarr) |>
  dplyr::inner_join(RazaoSocial, by = "CPF_CNPJarr") |>
  nrow()
n_cpf_sem_nome <- cfem_arr_pre_fallback |>
  dplyr::filter(is.na(NOME_arr)) |>
  dplyr::distinct(CPF_CNPJarr) |>
  nrow()
message(sprintf(
  "[05][CFEM] razao social: %d CPF/CNPJ distinto(s) sem nome no microdado, dos quais %d resolvido(s) so pelo CSV auxiliar.",
  n_cpf_sem_nome, n_so_fallback
))

cfem_arr <- cfem_arr_pre_fallback |>
  dplyr::left_join(RazaoSocial, by = "CPF_CNPJarr") |>
  dplyr::mutate(NOME_arr = dplyr::coalesce(NOME_arr, NOME_arr_alt, "NOME DESCONHECIDO")) |>
  dplyr::select(-NOME_arr_alt)

cfem_arr_amzl0 <- cfem_arr |>
  dplyr::filter(PROCESSO %in% processos_amzl) |>
  dplyr::mutate(row_id = dplyr::row_number())

cfem_aut_amzl <- cfem_aut |> dplyr::filter(PROCESSO %in% processos_amzl)

fatores_kg <- c("KG" = 1, "T" = 1000, "G" = 0.001, "CT" = 0.0002)
fatores_g  <- c("KG" = 1000, "T" = 1e6, "G" = 1, "CT" = 0.2)

cfem_arr_amzl1 <- cfem_arr_amzl0 |>
  dplyr::mutate(
    PESO_KG    = round(as.double(QTD_MINERIO) * unname(fatores_kg[UM]), 10),
    PESO_G     = round(as.double(QTD_MINERIO) * unname(fatores_g[UM]), 10),
    SUBSarrSIM = classificar_grupo(SUBSarr)
  )

# --- Colunas propagadas para a tabela de CFEM (auditoria F-02) ----------------
# Antes esta selecao levava 8 colunas e NENHUMA flag de sobreposicao. As flags
# eram calculadas logo acima, conferidas no QA, e simplesmente nao viajavam.
# Como o filtro do dashboard ignora coluna ausente e devolve o conjunto
# inteiro, os botoes "Territorios Protegidos" nao davam erro: nao filtravam.
#
# A lista NAO e escrita a mao: deriva de flags_sobrep (o mesmo vetor usado no
# check de QA) mais as colunas de nome/tipo e as derivadas do ICMBio. Assim,
# incluir uma fonte nova nao depende de alguem lembrar de atualizar um select()
# -- que foi exatamente como o bug nasceu.
cols_nomes_terr <- c("UCname", "UCtype", "UCcnuc", "TIname", "QUIname",
                     "UCname_ov", "UCtype_ov", "TIname_ov", "QUIname_ov")
cols_inf_ic     <- c("inf_IC_n", "inf_IC_multa", "inf_IC_ais", "inf_IC_cnucs",
                     "inf_IC_uc_confere")
cols_base       <- c("PROCESSO", "AREA_HA", "FASE", "ULT_EVENTO", "TITULAR",
                     "SUBS", "TIPO_REQcm", "CPF_CNPJcm")

cols_propagar <- c(cols_base, FLAGS_SOBREPOSICAO, cols_nomes_terr, cols_inf_ic)

ausentes_prop <- setdiff(cols_propagar, names(as.data.frame(pma_tp)))
if (length(ausentes_prop) > 0) {
  stop("[05] colunas previstas para propagacao nao existem em pma_tp: ",
       paste(ausentes_prop, collapse = ", "),
       " -- conferir os blocos de sobreposicao acima.")
}

pma_attrs <- as.data.frame(pma_tp) |>
  dplyr::select(dplyr::all_of(cols_propagar)) |>
  dplyr::distinct(PROCESSO, .keep_all = TRUE)

message("[05][propagacao] ", length(cols_propagar),
        " colunas levadas para a tabela de CFEM (antes: 8, sem nenhuma flag).")

cfem_arr_amzl2 <- dplyr::left_join(cfem_arr_amzl1, pma_attrs, by = "PROCESSO") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)))

# Data de corte da aliquota CFEM (Lei 13.540/2017, vigencia 01/11/2017).
corte <- as.Date("2017-11-01")

cfem_arr_amzl3 <- cfem_arr_amzl2 |>
  dplyr::mutate(
    ANO = as.integer(ANO), MES = as.integer(MES), VALORarr = as.numeric(VALORarr),
    PESO_KG = as.numeric(PESO_KG), PESO_G = as.numeric(PESO_G),
    data = as.Date(sprintf("%04d-%02d-01", ANO, MES))
  ) |>
  dplyr::mutate(
    ALIQUOTA_PCT = dplyr::case_when(
      data >= corte & SUBSarrSIM == "OURO"     ~ 1.5,
      data >= corte & SUBSarrSIM == "DIAMANTE" ~ 2.0,
      data >= corte & SUBSarrSIM == "NIÓBIO"   ~ 3.0,
      data >= corte                            ~ 2.0,
      data <  corte & SUBSarrSIM == "OURO"     & grepl("GARIMPEIRA", ifelse(is.na(FASE), "", FASE), ignore.case = TRUE) ~ 0.2,
      data <  corte & SUBSarrSIM == "DIAMANTE" & grepl("GARIMPEIRA", ifelse(is.na(FASE), "", FASE), ignore.case = TRUE) ~ 0.2,
      data <  corte & SUBSarrSIM == "OURO"     ~ 2.0,
      data <  corte & SUBSarrSIM == "DIAMANTE" ~ 3.0,
      data <  corte & SUBSarrSIM == "NIÓBIO"   ~ 3.0,
      data <  corte                            ~ 2.0,
      TRUE                                     ~ NA_real_
    ),
    VALORtot     = round(VALORarr * (100 / ALIQUOTA_PCT), 2),
    preco_g_orig = dplyr::if_else(!is.na(PESO_G) & PESO_G > 0, VALORtot / PESO_G, NA_real_)
  )

# cfem_bruto: ponto de partida da correcao (06) -- ainda sem nenhuma
# correcao aplicada (PESO_*_final = PESO_*, corr = "original").
cfem_bruto <- cfem_arr_amzl3 |>
  dplyr::mutate(PESO_G_final = PESO_G, PESO_KG_final = PESO_KG, preco_g_final = preco_g_orig, corr = "original")

save_ckpt(cfem_bruto,    "05_cfem_bruto")
save_ckpt(cfem_aut_amzl, "05_cfem_aut_amzl")

message(sprintf("[05][CFEM] preparo concluido | declaracoes (Amazonia): %d | autuacoes (Amazonia): %d",
                nrow(cfem_bruto), nrow(cfem_aut_amzl)))

message("\n=== 05_integracao_final.R — CONCLUÍDO ===")