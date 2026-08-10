################################################################################
# 08_proc_shiny.R
#
# Empacota tudo que vai pro Shiny e que NAO depende de logica temporal:
#   PARTE 1 — catalogo relacional (micro_*), direto dos microdados do 04.
#   PARTE 2 — CFEM base/anual/mensal + geometrias (PMA/TI/UC/Quilombolas),
#             direto do result_shiny do 06.
#
# ESCOPO (decisao 2026-08): fusao dos antigos "07_proc_shiny_dossie.R Parte 1"
# (catalogo relacional) + "08_proc_shiny_geo.R" (geo) inteiro. A Parte 2 do
# antigo 07_proc_shiny_dossie.R (dossie de alertas: situacao_atual,
# segmentos_aptidao_processo, cfem_motivo_ref etc.) NAO esta aqui -- foi pro
# 07_serie_temporal.R (Bloco E), que ja produz o cruzamento CFEM x aptidao
# direto. Os dois pedacos deste arquivo sao independentes entre si e
# independentes do 07 -- podem rodar em qualquer ordem em relacao a ele.
################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)
options(arrow.use_altrep = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(readr)
  library(here)
  library(stringr)
  library(sf)
  library(rmapshaper)
})

source(here::here("R", "utils.R"))

# --- Caminhos -----------------------------------------------------------------
MICRO_DIR    <- here::here("data", "result_db", "microdados")
RESULT_SHINY <- here::here("data", "result_shiny")
OUTPUT_DIR   <- here::here("shiny_dashboard")
QA_DIR       <- here::here("data", "_qa", "08_proc_shiny")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(QA_DIR,     recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# PARTE 1 — CATALOGO RELACIONAL (micro_*), direto do 04
# ==============================================================================
message("[08][parte1] lendo microdados relacionais...")

ler_parquet <- function(dir, nome) {
  caminho <- file.path(dir, paste0(nome, ".parquet"))
  if (!file.exists(caminho)) {
    message("[08] ausente, pulando: ", caminho)
    return(NULL)
  }
  df <- as.data.frame(arrow::read_parquet(caminho))
  cols_dt <- names(df)[stringr::str_starts(names(df), "dt")]
  for (cc in cols_dt) {
    if (is.character(df[[cc]])) {
      df[[cc]] <- suppressWarnings(as.Date(df[[cc]]))
    }
  }
  df
}

catalogo <- function(df, id_col, novo_nome) {
  if (is.null(df)) return(NULL)
  nms <- names(df)
  if (!id_col %in% nms) {
    cand_id <- nms[stringr::str_starts(nms, "id")]
    if (length(cand_id)) id_col <- cand_id[1] else return(NULL)
  }
  desc_col <- nms[stringr::str_starts(nms, "ds") | stringr::str_starts(nms, "nm") | stringr::str_starts(nms, "no")]
  desc_col <- setdiff(desc_col, id_col)
  desc_col <- if (length(desc_col)) desc_col[1] else setdiff(nms, id_col)[1]
  out <- df[, c(id_col, desc_col)]
  names(out) <- c(id_col, novo_nome)
  out[[id_col]] <- as.character(out[[id_col]])
  dplyr::distinct(out)
}
jl <- function(df, cat, id_col) {
  if (is.null(df) || is.null(cat)) return(df)
  if (!id_col %in% names(df)) return(df)
  df[[id_col]] <- as.character(df[[id_col]])
  dplyr::left_join(df, cat, by = id_col)
}
to_chr_proc <- function(df) {
  if (!is.null(df) && "processo" %in% names(df)) df$processo <- as.character(df$processo)
  df
}

p_processo     <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo"))
p_pessoa       <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_pessoa"))
p_substancia   <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_substancia"))
p_municipio    <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_municipio"))
p_titulo       <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_titulo"))
p_documentacao <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_documentacao"))
p_associacao   <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_associacao"))
p_propsolo     <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_propriedade_solo"))

d_pessoa       <- ler_parquet(MICRO_DIR, "micro_pessoa")
d_municipio    <- ler_parquet(MICRO_DIR, "micro_municipio")
d_fase         <- ler_parquet(MICRO_DIR, "micro_fase_processo")
d_substancia   <- ler_parquet(MICRO_DIR, "micro_substancia")
d_tiporeq      <- ler_parquet(MICRO_DIR, "micro_tipo_requerimento")
d_tipoassoc    <- ler_parquet(MICRO_DIR, "micro_tipo_associacao")
d_tipodoc      <- ler_parquet(MICRO_DIR, "micro_tipo_documento")
d_tipodoclegal <- ler_parquet(MICRO_DIR, "micro_tipo_documento_legal")
d_tiporel      <- ler_parquet(MICRO_DIR, "micro_tipo_relacao")
d_tiporepleg   <- ler_parquet(MICRO_DIR, "micro_tipo_representacao_legal")
d_tiporesptec  <- ler_parquet(MICRO_DIR, "micro_tipo_responsabilidade_tecnica")
d_tipouso      <- ler_parquet(MICRO_DIR, "micro_tipo_uso_substancia")
d_condsolo     <- ler_parquet(MICRO_DIR, "micro_condicao_propriedade_solo")
d_motivoenc    <- ler_parquet(MICRO_DIR, "micro_motivo_encerramento_substancia")
d_sitdoclegal  <- ler_parquet(MICRO_DIR, "micro_situacao_documento_legal")

# --- micro_processos: 1 linha por processo (catalogo) ------------------------
processos <- p_processo |>
  jl(catalogo(d_fase, "idfaseprocesso", "fase"), "idfaseprocesso") |>
  jl(catalogo(d_tiporeq, "idtiporequerimento", "tipo_requerimento"), "idtiporequerimento") |>
  dplyr::transmute(
    processo, nup = nrnup, ativo = ifelse(btativo == "S", "Sim", "Nao"),
    tipo_requerimento, fase, area_ha = suppressWarnings(as.numeric(qtareaha)),
    dt_protocolo = dtprotocolo, dt_prioridade = dtprioridade
  )

if (!is.null(p_municipio) && !is.null(d_municipio)) {
  mc <- d_municipio; mc$idmunicipio <- as.character(mc$idmunicipio)
  processos <- processos |> dplyr::left_join(
    p_municipio |> dplyr::mutate(idmunicipio = as.character(idmunicipio)) |>
      dplyr::left_join(mc, by = "idmunicipio") |> dplyr::group_by(processo) |>
      dplyr::summarise(uf = paste(sort(unique(na.omit(sguf))), collapse = ", "),
                        municipios = paste(sort(unique(na.omit(nmmunicipio))), collapse = ", "), .groups = "drop"),
    by = "processo"
  )
}
if (!is.null(p_substancia) && !is.null(d_substancia)) {
  sc <- catalogo(d_substancia, "idsubstancia", "substancia")
  processos <- processos |> dplyr::left_join(
    p_substancia |> dplyr::mutate(idsubstancia = as.character(idsubstancia)) |>
      dplyr::left_join(sc, by = "idsubstancia") |> dplyr::group_by(processo) |>
      dplyr::summarise(substancias = paste(sort(unique(na.omit(substancia))), collapse = "; "), .groups = "drop"),
    by = "processo"
  )
}
if (!is.null(p_pessoa) && !is.null(d_pessoa) && !is.null(d_tiporel)) {
  rel <- catalogo(d_tiporel, "idtiporelacao", "relacao")
  pc  <- d_pessoa; pc$idpessoa <- as.character(pc$idpessoa)
  processos <- processos |> dplyr::left_join(
    p_pessoa |> dplyr::mutate(idpessoa = as.character(idpessoa), idtiporelacao = as.character(idtiporelacao)) |>
      dplyr::left_join(rel, by = "idtiporelacao") |> dplyr::left_join(pc, by = "idpessoa") |>
      dplyr::filter(grepl("titular", relacao, ignore.case = TRUE)) |> dplyr::group_by(processo) |>
      dplyr::summarise(titular = paste(sort(unique(na.omit(nmpessoa))), collapse = "; "), .groups = "drop"),
    by = "processo"
  )
}
processos$ano_protocolo <- suppressWarnings(as.integer(format(processos$dt_protocolo, "%Y")))
saveRDS(processos, file.path(OUTPUT_DIR, "micro_processos.rds"))
message(sprintf("[08][parte1] micro_processos.rds: %d processos", nrow(processos)))

# --- micro_pessoas / micro_pessoa_resumo -------------------------------------
pessoas <- p_pessoa |> dplyr::mutate(idpessoa = as.character(idpessoa)) |>
  jl(catalogo(d_tiporel, "idtiporelacao", "relacao"), "idtiporelacao") |>
  jl(catalogo(d_tiporesptec, "idtiporesponsabilidadetecnica", "resp_tecnica"), "idtiporesponsabilidadetecnica") |>
  jl(catalogo(d_tiporepleg, "idtiporepresentacaolegal", "repr_legal"), "idtiporepresentacaolegal")

if (!is.null(d_pessoa)) { pc <- d_pessoa; pc$idpessoa <- as.character(pc$idpessoa); pessoas <- dplyr::left_join(pessoas, pc, by = "idpessoa") }
pessoas <- pessoas |> dplyr::transmute(
  processo, idpessoa,
  nome        = if ("nmpessoa" %in% names(pessoas)) nmpessoa else NA_character_,
  cpf_cnpj    = if ("nrcpfcnpj" %in% names(pessoas)) nrcpfcnpj else NA_character_,
  tipo_pessoa = if ("tppessoa" %in% names(pessoas)) tppessoa else NA_character_,
  relacao, resp_tecnica, repr_legal,
  dt_inicio = dtiniciovigencia, dt_fim = dtfimvigencia
)
saveRDS(pessoas, file.path(OUTPUT_DIR, "micro_pessoas.rds"))

pessoa_resumo <- pessoas |>
  dplyr::group_by(idpessoa, nome, cpf_cnpj) |>
  dplyr::summarise(n_processos = dplyr::n_distinct(processo), n_vinculos = dplyr::n(),
                    papeis = paste(sort(unique(na.omit(relacao))), collapse = ", "), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(n_processos))
saveRDS(pessoa_resumo, file.path(OUTPUT_DIR, "micro_pessoa_resumo.rds"))
message(sprintf("[08][parte1] micro_pessoas.rds: %d vinculos | micro_pessoa_resumo.rds: %d pessoas", nrow(pessoas), nrow(pessoa_resumo)))

# --- micro_substancias / micro_titulos / micro_municipios / documentacao / associacoes / propsolo ---
substancias <- p_substancia |>
  jl(catalogo(d_substancia, "idsubstancia", "substancia"), "idsubstancia") |>
  jl(catalogo(d_tipouso, "idtipousosubstancia", "tipo_uso"), "idtipousosubstancia") |>
  jl(catalogo(d_motivoenc, "idmotivoencerramentosubstancia", "motivo_encerramento"), "idmotivoencerramentosubstancia") |>
  dplyr::transmute(processo, substancia, tipo_uso, motivo_encerramento, dt_inicio = dtiniciovigencia, dt_fim = dtfimvigencia)
saveRDS(substancias, file.path(OUTPUT_DIR, "micro_substancias.rds"))

titulos <- p_titulo |>
  jl(catalogo(d_tipodoclegal, "idtipodocumentolegal", "tipo_documento"), "idtipodocumentolegal") |>
  jl(catalogo(d_sitdoclegal, "idsituacaodocumentolegal", "situacao"), "idsituacaodocumentolegal") |>
  dplyr::transmute(processo, nr_titulo = nrtitulo, tipo_documento, situacao,
                    dt_publicacao = dtpublicacao, dt_vencimento = dtvencimento)
saveRDS(titulos, file.path(OUTPUT_DIR, "micro_titulos.rds"))

if (!is.null(p_municipio) && !is.null(d_municipio)) {
  mc <- d_municipio; mc$idmunicipio <- as.character(mc$idmunicipio)
  municipios_proc <- p_municipio |> dplyr::mutate(idmunicipio = as.character(idmunicipio)) |>
    dplyr::left_join(mc, by = "idmunicipio") |> dplyr::transmute(processo, municipio = nmmunicipio, uf = sguf)
  saveRDS(municipios_proc, file.path(OUTPUT_DIR, "micro_municipios.rds"))
}
documentacao <- p_documentacao |>
  jl(catalogo(d_tipodoc, "idtipodocumento", "tipo_documento"), "idtipodocumento") |>
  dplyr::transmute(processo, tipo_documento, dt_protocolo = dtprotocolo)
saveRDS(documentacao, file.path(OUTPUT_DIR, "micro_documentacao.rds"))

associacoes <- p_associacao |>
  jl(catalogo(d_tipoassoc, "idtipoassociacao", "tipo_associacao"), "idtipoassociacao") |>
  dplyr::transmute(processo, processo_associado = dsprocessoassociado, tipo_associacao,
                    dt_associacao = dtassociacao, dt_desassociacao = dtdesassociacao, obs = obassociacao)
saveRDS(associacoes, file.path(OUTPUT_DIR, "micro_associacoes.rds"))

propsolo <- p_propsolo |>
  jl(catalogo(d_condsolo, "idcondicaopropriedadesolo", "condicao_solo"), "idcondicaopropriedadesolo") |>
  dplyr::transmute(processo, condicao_solo)
saveRDS(propsolo, file.path(OUTPUT_DIR, "micro_propsolo.rds"))

message("[08][parte1] concluida.\n")

# ==============================================================================
# PARTE 2 — CFEM (base/anual/mensal) + GEOMETRIAS, direto do result_shiny do 06
# ==============================================================================
message("[08][parte2] lendo CFEM + geometrias...")

cfem_csv_path     <- file.path(RESULT_SHINY, "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv")
pma_geojson_path  <- file.path(RESULT_SHINY, "pma_amzl_ALLminerals_final.geojson")

if (!file.exists(cfem_csv_path))    stop("[08] CFEM nao encontrado: ", cfem_csv_path,    " — rode o 06_correcao_cfem.R primeiro.")
if (!file.exists(pma_geojson_path)) stop("[08] PMA nao encontrado: ",  pma_geojson_path, " — rode o 06_correcao_cfem.R primeiro.")

# --- 2.1) CFEM base (declaracao a declaracao, sem agregacao) ----------------
cfem <- readr::read_csv(cfem_csv_path, show_col_types = FALSE) |>
  dplyr::mutate(
    ANO           = as.integer(ANO),
    MES           = as.integer(MES),
    VALORarr      = as.numeric(VALORarr),
    PESO_KG       = as.numeric(PESO_KG),
    PESO_G        = as.numeric(PESO_G),
    preco_g_orig  = as.numeric(preco_g_orig),
    PESO_G_final  = as.numeric(PESO_G_final),
    PESO_KG_final = as.numeric(PESO_KG_final),
    preco_g_final = as.numeric(preco_g_final),
    PESO_G_final_limpo  = as.numeric(PESO_G_final_limpo),
    PESO_KG_final_limpo = as.numeric(PESO_KG_final_limpo),
    proc_ano      = paste0(trimws(PROCESSO), "/", ANO)
  )

saveRDS(cfem, file.path(OUTPUT_DIR, "cfem.rds"))
message(sprintf("[08][parte2] cfem.rds: %d declaracoes", nrow(cfem)))

lk_mun_tab1      <- cfem |> dplyr::distinct(SUBSarrSIM, SUBSarr, FASE, abbrev_state, ANO, name_muni)
lk_tit_proc_tab1 <- cfem |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO)
lk_decl_tab1     <- cfem |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr)
saveRDS(lk_mun_tab1,      file.path(OUTPUT_DIR, "lk_mun_tab1.rds"))
saveRDS(lk_tit_proc_tab1, file.path(OUTPUT_DIR, "lk_tit_proc_tab1.rds"))
saveRDS(lk_decl_tab1,     file.path(OUTPUT_DIR, "lk_decl_tab1.rds"))

# --- 2.2) CFEM anual (agregacao por ano -- legitima aqui, e o proposito
#     da aba "Anual" do app) ---------------------------------------------------
cfem_anual <- cfem |>
  dplyr::group_by(ANO, abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr, SUBSarrSIM, SUBSarr) |>
  dplyr::summarise(
    VALORarr      = sum(VALORarr,      na.rm = TRUE),
    VALORtot      = sum(VALORtot,      na.rm = TRUE),
    PESO_KG       = sum(PESO_KG,       na.rm = TRUE),
    PESO_G        = sum(PESO_G,        na.rm = TRUE),
    PESO_G_final  = sum(PESO_G_final,  na.rm = TRUE),
    PESO_KG_final = sum(PESO_KG_final, na.rm = TRUE),
    PESO_G_final_limpo  = sum(PESO_G_final_limpo,  na.rm = TRUE),
    PESO_KG_final_limpo = sum(PESO_KG_final_limpo, na.rm = TRUE),
    .groups       = "drop"
  )

pma_ocd_attr <- sf::st_read(pma_geojson_path, quiet = TRUE) |>
  dplyr::select(
    PROCESSO, foco, AREA_HA, FASE, ULT_EV_DAT, ULT_EV_DES,
    TIov, UCov, QUIov, TIov10km, UCov2km, QUIov10km,
    UCtype, UCname, TIname, QUIname
  ) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(dplyr::across(c(TIov, UCov, QUIov, TIov10km, UCov2km, QUIov10km), ~ as.logical(.x)))

# CORRECAO (achado 2026-08): join so por PROCESSO causava produto cartesiano
# nos processos multi-substancia (pma_full tem 1 linha por PROCESSO x foco,
# ver NOTA 2026-07-20 no 06_correcao_cfem.R) -- cada declaracao de CFEM
# recebia atributos geograficos de TODOS os focos do processo, nao so do
# seu proprio. Agora casa tambem por foco.
cfem_anual <- cfem_anual |>
  dplyr::mutate(foco = categorizar_foco(SUBSarr, SUBSarrSIM))

cfem_anual <- dplyr::inner_join(cfem_anual, pma_ocd_attr, by = c("PROCESSO", "foco"))
saveRDS(cfem_anual, file.path(OUTPUT_DIR, "cfem_anual.rds"))
message(sprintf("[08][parte2] cfem_anual.rds: %d linhas (ano x processo x substancia)", nrow(cfem_anual)))

lk_mun_tab2      <- cfem_anual |> dplyr::distinct(SUBSarrSIM, SUBSarr, FASE, abbrev_state, ANO, name_muni)
lk_tit_proc_tab2 <- cfem_anual |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO)
lk_decl_tab2     <- cfem_anual |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr)
saveRDS(lk_mun_tab2,      file.path(OUTPUT_DIR, "lk_mun_tab2.rds"))
saveRDS(lk_tit_proc_tab2, file.path(OUTPUT_DIR, "lk_tit_proc_tab2.rds"))
saveRDS(lk_decl_tab2,     file.path(OUTPUT_DIR, "lk_decl_tab2.rds"))

# --- 2.3) CFEM mensal (so adiciona a coluna "data" -- nenhuma agregacao,
#     continua 1 linha = 1 declaracao) -----------------------------------------
cfem_mensal <- cfem |>
  dplyr::mutate(data = as.Date(sprintf("%04d-%02d-01", ANO, MES)))

saveRDS(cfem_mensal, file.path(OUTPUT_DIR, "cfem_mensal.rds"))

lk_mun_tab3      <- cfem_mensal |> dplyr::distinct(SUBSarrSIM, SUBSarr, FASE, abbrev_state, ANO, name_muni)
lk_tit_proc_tab3 <- cfem_mensal |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO)
lk_decl_tab3     <- cfem_mensal |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr)
saveRDS(lk_mun_tab3,      file.path(OUTPUT_DIR, "lk_mun_tab3.rds"))
saveRDS(lk_tit_proc_tab3, file.path(OUTPUT_DIR, "lk_tit_proc_tab3.rds"))
saveRDS(lk_decl_tab3,     file.path(OUTPUT_DIR, "lk_decl_tab3.rds"))

# --- 2.4) Geometrias (PMA / TI / UC / Quilombolas) -- brutas + simplificadas --
message("[08][parte2] processando camadas geoespaciais...")

to_wgs84 <- function(x) {
  cr <- sf::st_crs(x)
  if (is.na(cr) || isFALSE(sf::st_is_longlat(cr))) suppressWarnings(sf::st_transform(x, 4326)) else x
}

# st_make_valid() ANTES de simplificar: sem isso, geometrias invalidas/mistas
# do shapefile de origem podem virar GEOMETRYCOLLECTION apos rmapshaper::
# ms_simplify() -- o leaflet::addPolygons() no app.R nao sabe desenhar isso e
# trava ("Don't know how to get polygon data from object of class
# XY,GEOMETRYCOLLECTION,sfg").
#
# CORRECAO (achado real, PMA/2026-07): aplicar st_make_valid() em TODAS as
# geometrias, mesmo nas ja validas, pode destruir geometrias corretas.
# Confirmado no shapefile do PMA: 54497 de 55415 poligonos ja eram
# "Valid Geometry" na origem (MULTIPOLYGON), mas ao rodar st_make_valid()
# em cima de TODAS mesmo assim, 6 delas (todas com area < 0.07 ha -- REQ LAVRA
# GARIMPEIRA/REQ PESQUISA recem-protocolados) viraram GEOMETRYCOLLECTION
# VAZIA. Efeito colateral conhecido do GEOS por tras do st_make_valid() em
# poligonos muito pequenos/proximos da tolerancia numerica: ele renoda a
# geometria mesmo quando nao precisa, e pode colapsa-la. Teste que confirmou
# (pulando make_valid() nessas 6): permanecem MULTIPOLYGON validos, nrow
# preservado. Por isso agora so aplicamos st_make_valid() no subconjunto que
# st_is_valid() de fato aponta como invalido -- o resto fica intocado.
tornar_valido <- function(x) {
  invalidas <- !sf::st_is_valid(x)
  if (any(invalidas)) {
    x[invalidas, ] <- suppressWarnings(sf::st_make_valid(x[invalidas, ]))
  }

  # So o st_make_valid() acima (aplicado apenas ao subconjunto que era
  # invalido) pode gerar GEOMETRYCOLLECTION (poligono + fragmentos residuais
  # de linha/ponto). Isolamos so essas para extrair a parte poligonal --
  # tudo que ja era (multi)poligono valido (a imensa maioria) permanece
  # intocado, sem passar de novo por st_collection_extract()/st_make_valid().
  mistas <- sf::st_geometry_type(x) == "GEOMETRYCOLLECTION"
  if (any(mistas)) {
    x_intocado <- x[!mistas, ]
    x_extraido <- suppressWarnings(sf::st_collection_extract(x[mistas, ], "POLYGON"))
    x_extraido <- sf::st_make_valid(x_extraido)
    x <- rbind(x_intocado, x_extraido)
  }
  x
}

pma <- sf::st_read(pma_geojson_path, quiet = TRUE) |> to_wgs84() |> tornar_valido()
uc  <- sf::st_read(file.path(RESULT_SHINY, "uc_amzl.shp"),  quiet = TRUE) |> to_wgs84() |> tornar_valido() |> dplyr::select("nome_uc", "sigla_snuc")
qui <- sf::st_read(file.path(RESULT_SHINY, "qui_amzl.shp"), quiet = TRUE) |> to_wgs84() |> tornar_valido() |> dplyr::select("nm_comunid")
ti  <- sf::st_read(file.path(RESULT_SHINY, "ti_amzl.shp"),  quiet = TRUE) |> to_wgs84() |> tornar_valido() |> dplyr::select("terrai_nom")

saveRDS(pma, file.path(OUTPUT_DIR, "pma.rds"))
saveRDS(ti,  file.path(OUTPUT_DIR, "ti.rds"))
saveRDS(uc,  file.path(OUTPUT_DIR, "uc.rds"))
saveRDS(qui, file.path(OUTPUT_DIR, "qui.rds"))

message("[08][parte2] simplificando geometrias (rmapshaper)...")
# CORRECAO (achado real, crash em producao 2026-07): tornar_valido() roda
# ANTES do ms_simplify(), mas o proprio ms_simplify() pode reintroduzir
# GEOMETRYCOLLECTION como efeito colateral da simplificacao (comportamento
# conhecido do mapshaper/rmapshaper -- simplificar uma geometria valida pode
# gerar auto-intersecoes ou fragmentos). O crash reportado veio do .rds JA
# simplificado, nao do pma bruto -- por isso precisa re-sanear DEPOIS de
# simplificar tambem, nao so antes.
simplify_and_save <- function(sf_obj, out_path, keep_ratio) {
  if (nrow(sf_obj) > 0) {
    simplified <- rmapshaper::ms_simplify(sf_obj, keep = keep_ratio, keep_shapes = TRUE)
    simplified <- tornar_valido(simplified)
    saveRDS(simplified, out_path)
  }
}

simplify_and_save(ti,  file.path(OUTPUT_DIR, "ti_simpl.rds"),  0.3)
simplify_and_save(uc,  file.path(OUTPUT_DIR, "uc_simpl.rds"),  0.3)
simplify_and_save(qui, file.path(OUTPUT_DIR, "qui_simpl.rds"), 0.3)
simplify_and_save(pma, file.path(OUTPUT_DIR, "pma_simpl.rds"), 0.1)

message("\n=== 08_proc_shiny.R — CONCLUIDO ===")


################################################################################
# checar_rds_antes_deploy.R
#
# Checagem robusta de TODOS os .rds em shiny_dashboard antes do scp pro
# droplet. Roda 100% local, so leitura -- nao altera nenhum arquivo.
#
# O que detecta:
#   1) Erro de leitura (arquivo corrompido/ausente)
#   2) Warnings emitidos durante o readRDS() -- pega qualquer warning, incluindo
#      o "cannot unserialize ALTVEC object... returning length zero vector"
#      (bug do arrow ALTREP ja identificado), sem depender do texto exato da
#      mensagem continuar igual em versoes futuras do pacote.
#   3) Checagem ESTRUTURAL independente do warning: compara o tamanho de CADA
#      coluna com nrow(df). Isso pega o bug mesmo se o R um dia parar de
#      emitir warning nesse caso -- e o teste que realmente importa, o warning
#      e so um sintoma.
#   4) Tabelas com 0 linhas (pode ser esperado ou sintoma de outro problema
#      upstream -- sinalizado, nao tratado como erro automatico).
################################################################################

pasta <- "C:/GP/anm-geo/shiny_dashboard"   # <-- ajuste aqui se necessario

arquivos_rds <- list.files(pasta, pattern = "\\.rds$", full.names = TRUE)

if (length(arquivos_rds) == 0) {
  stop("Nenhum .rds encontrado em: ", pasta)
}

checar_rds <- function(caminho) {
  nome <- basename(caminho)
  warnings_capturados <- character(0)

  obj <- withCallingHandlers(
    tryCatch(readRDS(caminho), error = function(e) e),
    warning = function(w) {
      warnings_capturados <<- c(warnings_capturados, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (inherits(obj, "error")) {
    return(data.frame(arquivo = nome, status = "ERRO_LEITURA",
                       detalhe = conditionMessage(obj), stringsAsFactors = FALSE))
  }

  detalhe <- character(0)

  if (length(warnings_capturados) > 0) {
    detalhe <- c(detalhe, paste0("WARNING NA LEITURA: ", paste(unique(warnings_capturados), collapse = " || ")))
  }

  if (is.data.frame(obj)) {
    n <- nrow(obj)
    tam_colunas <- vapply(obj, length, integer(1))

    colunas_zeradas      <- names(obj)[tam_colunas == 0 & n > 0]
    colunas_incompativeis <- setdiff(names(obj)[tam_colunas != n], colunas_zeradas)

    if (length(colunas_zeradas) > 0) {
      detalhe <- c(detalhe, paste0("COLUNA(S) DE TAMANHO 0 (nrow=", n, "): ",
                                    paste(colunas_zeradas, collapse = ", ")))
    }
    if (length(colunas_incompativeis) > 0) {
      detalhe <- c(detalhe, paste0("COLUNA(S) COM TAMANHO != nrow: ",
                                    paste(colunas_incompativeis, collapse = ", ")))
    }
    if (n == 0) {
      detalhe <- c(detalhe, "AVISO: 0 linhas (confirmar se e esperado)")
    }

    if (inherits(obj, "sf")) {
      tipos <- as.character(sf::st_geometry_type(obj))
      tipos_ok <- c("POLYGON", "MULTIPOLYGON")
      tipos_ruins <- setdiff(unique(tipos), tipos_ok)
      if (length(tipos_ruins) > 0) {
        n_ruins <- sum(tipos %in% tipos_ruins)
        detalhe <- c(detalhe, paste0("GEOMETRIA INCOMPATIVEL COM leaflet::addPolygons -- ",
                                      n_ruins, " linha(s) do tipo ",
                                      paste(tipos_ruins, collapse = ", ")))
      }
    }
  } else if (is.list(obj) && length(obj) == 0) {
    detalhe <- c(detalhe, "LISTA VAZIA")
  }

  status <- if (length(detalhe) == 0) "OK" else "PROBLEMA"
  data.frame(arquivo = nome, status = status,
             detalhe = paste(detalhe, collapse = " | "), stringsAsFactors = FALSE)
}

resultado <- do.call(rbind, lapply(arquivos_rds, checar_rds))

cat("\n================ RESUMO (", nrow(resultado), "arquivos ) ================\n")
print(resultado[, c("arquivo", "status")], row.names = FALSE)

problemas <- resultado[resultado$status != "OK", ]
if (nrow(problemas) > 0) {
  cat("\n================ DETALHES DOS PROBLEMAS ================\n")
  for (i in seq_len(nrow(problemas))) {
    cat("\n->", problemas$arquivo[i], "\n   ", problemas$detalhe[i], "\n")
  }
  cat("\n", nrow(problemas), "de", nrow(resultado), "arquivo(s) com problema -- NAO subir pro droplet ainda.\n")
} else {
  cat("\nTodos os", nrow(resultado), "arquivos passaram limpo. Pode subir pro droplet.\n")
}